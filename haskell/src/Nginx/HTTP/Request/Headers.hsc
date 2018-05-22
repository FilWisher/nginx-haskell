{-# LANGUAGE ForeignFunctionInterface #-}

module Nginx.HTTP.Request.Headers where

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Control.Monad (forM_)

import Nginx.Core.String
import Nginx.Core.List
import Nginx.Core.Hash

#include "ngx_http.h"

data NgxHeadersIn = NgxHeadersIn
  { headers :: NgxList NgxTableElt
  }
  deriving (Show)

instance Storable NgxHeadersIn where
  sizeOf _ = (#size ngx_http_headers_in_t)
  alignment _ = 8
  peek ptr =
    NgxHeadersIn <$> (#peek ngx_http_headers_in_t, headers) ptr
  poke = undefined

print_headers :: Ptr NgxHeadersIn -> IO ()
print_headers ptr = do
  ngx_headers <- peek ptr
  forM_ (toList $ headers ngx_headers) printElt 
  print =<< peek ptr
  where
    printElt (NgxTableElt _ key val) = do
      print $ toInternalByteString key
      print $ toInternalByteString val

foreign export ccall print_headers :: Ptr NgxHeadersIn -> IO ()
