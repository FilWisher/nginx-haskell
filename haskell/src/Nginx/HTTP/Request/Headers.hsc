{-# LANGUAGE ForeignFunctionInterface #-}

module Nginx.HTTP.Request.Headers where

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Control.Monad (forM_)

import qualified Nginx.Core.String as Nginx
import qualified Nginx.Core.List as Nginx
import qualified Nginx.Core.Hash as Nginx

#include "ngx_http.h"

data HeadersIn = HeadersIn
  { headers :: Nginx.List Nginx.TableElt
  }
  deriving (Show)

instance Storable HeadersIn where
  sizeOf _ = (#size ngx_http_headers_in_t)
  alignment _ = 8
  peek ptr =
    HeadersIn <$> (#peek ngx_http_headers_in_t, headers) ptr
  poke = undefined

print_headers :: Ptr HeadersIn -> IO ()
print_headers ptr = do
  ngx_headers <- peek ptr
  forM_ (Nginx.toList $ headers ngx_headers) printElt 
  print =<< peek ptr
  where
    printElt (Nginx.TableElt _ key val) = do
      print $ Nginx.toInternalByteString key
      print $ Nginx.toInternalByteString val

foreign export ccall print_headers :: Ptr HeadersIn -> IO ()
