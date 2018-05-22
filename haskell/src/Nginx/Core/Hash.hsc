{-# LANGUAGE ForeignFunctionInterface #-}

module Nginx.Core.Hash where

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Control.Monad (forM_)

#include "ngx_core.h"

import Nginx.Core.String

data NgxTableElt = NgxTableElt
  { hash :: CUInt
  , key :: NgxString
  , value :: NgxString
  }
  deriving (Show)

instance Storable NgxTableElt where
  sizeOf _ = (#size ngx_table_elt_t)
  alignment _ = 8
  peek ptr = do
    NgxTableElt
      <$> (#peek ngx_table_elt_t, hash) ptr
      <*> (#peek ngx_table_elt_t, key) ptr
      <*> (#peek ngx_table_elt_t, value) ptr
  poke = undefined

print_hash :: Ptr NgxTableElt -> IO ()
print_hash ptr = do
  elt <- peek ptr
  print $ elt
  print $ toInternalByteString (key elt)
  print $ toInternalByteString (value elt)

foreign export ccall print_hash :: Ptr NgxTableElt -> IO ()
