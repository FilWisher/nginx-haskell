{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Nginx.HTTP.Request where

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Marshal (copyArray, with)
import Foreign.C.Types
import Foreign.C.String

import Control.Monad (forM_)

import Nginx.Core
import Nginx.HTTP.Request.Headers

import Network.Wai (StreamingBody(..))

import Data.ByteString.Char8 (ByteString(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL

#include "ngx_http.h"
#include "ngx_http_haskell_module.h"

data NgxRequest = NgxRequest
  { ngxMethod :: NgxString
  , ngxHttpVersion :: NgxString
  , ngxUri :: NgxString
  , ngxQueryString :: NgxString
  , ngxHeadersIn :: NgxHeadersIn

  , ngxPool :: Ptr ()
  , rawRequest :: Ptr ()
  }
  deriving (Show)

data Request = Request
  { method :: ByteString
  , httpVersion :: ByteString
  , uri :: ByteString
  , queryString :: ByteString
  , headersIn :: [(ByteString, ByteString)]
  }
  deriving (Show)

toRequest :: (NgxString -> ByteString) -> NgxRequest -> Request
toRequest toBS NgxRequest{..} = Request
  { method      = toBS ngxMethod
  , httpVersion = toBS ngxHttpVersion
  , uri         = toBS ngxUri
  , queryString = toBS ngxQueryString
  , headersIn = map (\(NgxTableElt _ key val) -> (toBS key, toBS val)) (toList $ headers ngxHeadersIn)
  }

instance Storable NgxRequest where
  sizeOf _ = (#size ngx_http_request_t)
  alignment _ = 8
  peek ptr =
    NgxRequest
      <$> (#peek ngx_http_request_t, method_name) ptr
      <*> (#peek ngx_http_request_t, http_protocol) ptr
      <*> (#peek ngx_http_request_t, uri) ptr
      <*> (#peek ngx_http_request_t, args) ptr
      <*> (#peek ngx_http_request_t, headers_in) ptr
      <*> (#peek ngx_http_request_t, pool) ptr
      <*> return (castPtr ptr)
  poke = undefined


-- typedef ngx_int_t (*handler_fn)(ngx_http_request_t *, ngx_haskell_response_t *);

handle :: Request -> IO Response
handle req =
    -- return $ ResponseStream (Status 200 "OK") [] $ \write flush -> do
    --     write "hello"
    --     write "That's really nice"
    --     write "This is neat as fuck"
    --     flush
    return $ ResponseBuilder (Status 200 "OK") [] $ BB.byteString "Hello haskell"
    
print_request :: Ptr NgxRequest -> IO CInt
print_request ptr = do
  print "printing request"
  print (#offset ngx_http_request_t, headers_in)
  ngx_request <- peek ptr
  forM_ (toList $ headers $ ngxHeadersIn ngx_request) printElt 
  print (toRequest toInternalByteString ngx_request)
  res <- handle (toRequest toInternalByteString ngx_request)
  send_response res ngx_request
  return 42
  where
    printElt (NgxTableElt _ key val) = do
      print $ toInternalByteString key
      print $ toInternalByteString val

foreign export ccall print_request :: Ptr NgxRequest -> IO CInt

-- RESPONSE

foreign import ccall ""
  ngx_http_haskell_write_chunk :: Ptr a -> Ptr CChar -> IO ()

foreign import ccall "" 
    ngx_http_haskell_send_headers :: Ptr a -> CInt -> IO ()

foreign import ccall ""
  ngx_http_haskell_flush :: Ptr a -> IO ()

sendHeaders :: NgxRequest -> Status -> IO ()
sendHeaders req (Status status _) = do
    print "sending em now"
    ngx_http_haskell_send_headers (rawRequest req) (fromIntegral status)


writeChunk :: NgxRequest -> BB.Builder -> IO ()
writeChunk req builder = do
    BS.useAsCString bs writeChunk
    where
        bs         = BL.toStrict $ BB.toLazyByteString builder
        writeChunk = ngx_http_haskell_write_chunk (rawRequest req)

flush :: NgxRequest -> IO ()
flush = ngx_http_haskell_flush . castPtr . rawRequest

send_response :: Response -> NgxRequest -> IO ()
send_response resp req = do
    case resp of
        ResponseStream status _ fn -> do
            print "sending response"
            sendHeaders req status
            -- TODO: send status
            -- TODO: send headers
            fn (\ch -> writeChunk req ch) (flush req)
        ResponseBuilder status _ chunk -> do
            sendHeaders req status
            writeChunk req chunk
            flush req

            


data Status = Status
  { statusCode    :: Int
  , statusMessage :: ByteString
  }
  deriving (Show)

instance Storable Status where
  sizeOf _ = (#size ngx_haskell_status_t)
  alignment _ = 8
  peek ptr =
    Status
      <$> (#peek ngx_haskell_status_t, status_code) ptr
      <*> return "hello"

  poke ptr (Status code msg) = do
    ((#poke ngx_haskell_status_t, status_code) ptr) (fromIntegral code :: CUInt)
    BS.useAsCStringLen msg $ \(cstr, len) ->
      copyArray msgPtr cstr (min (#const MAX_STATUS_MESSAGE) len)
    where
      msgPtr = ptr `plusPtr` (#offset ngx_haskell_status_t, msg)

-- TODO: foreign export function for taking a raw request pointer and a response and sending it
-- TODO: call the request handler and fill the C-allocated response using it
-- TODO: call the exported function from C passing it the raw request and the filled response

data Response
  = ResponseStream Status [(ByteString, ByteString)] StreamingBody
  | ResponseBuilder Status [(ByteString, ByteString)] BB.Builder