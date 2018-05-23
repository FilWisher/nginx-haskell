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

import qualified Nginx.Core as Nginx
import Nginx.HTTP.Request.Headers

import Network.Wai (StreamingBody(..))
import Network.HTTP.Types (Status(..))

import Data.ByteString.Char8 (ByteString(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL

#include "ngx_http.h"
#include "ngx_http_haskell_module.h"

data NgxRequest = NgxRequest
  { ngxMethod :: Nginx.String
  , ngxHttpVersion :: Nginx.String
  , ngxUri :: Nginx.String
  , ngxQueryString :: Nginx.String
  , ngxHeadersIn :: HeadersIn

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

toRequest :: (Nginx.String -> ByteString) -> NgxRequest -> Request
toRequest toBS NgxRequest{..} = Request
  { method      = toBS ngxMethod
  , httpVersion = toBS ngxHttpVersion
  , uri         = toBS ngxUri
  , queryString = toBS ngxQueryString
  , headersIn = map (\(Nginx.TableElt _ key val) -> (toBS key, toBS val)) (Nginx.toList $ headers ngxHeadersIn)
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
    --     write "Hello"
    --     write " Haskell!"
    --     flush
    return $ ResponseBuilder (Status 200 "OK") [] $ BB.byteString "Hello haskell"
    
print_request :: Ptr NgxRequest -> IO CInt
print_request ptr = do
  print "printing request"
  print (#offset ngx_http_request_t, headers_in)
  ngx_request <- peek ptr
  forM_ (Nginx.toList $ headers $ ngxHeadersIn ngx_request) printElt 
  print (toRequest Nginx.toInternalByteString ngx_request)
  res <- handle (toRequest Nginx.toInternalByteString ngx_request)
  send_response res ngx_request
  return 42
  where
    printElt (Nginx.TableElt _ key val) = do
      print $ Nginx.toInternalByteString key
      print $ Nginx.toInternalByteString val

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

-- TODO: foreign export function for taking a raw request pointer and a response and sending it
-- TODO: call the request handler and fill the C-allocated response using it
-- TODO: call the exported function from C passing it the raw request and the filled response

data Response
  = ResponseStream Status [(ByteString, ByteString)] StreamingBody
  | ResponseBuilder Status [(ByteString, ByteString)] BB.Builder
