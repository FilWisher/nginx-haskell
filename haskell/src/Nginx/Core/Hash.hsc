{-# LANGUAGE ForeignFunctionInterface #-}

module Nginx.Core.Hash where

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Control.Monad (forM_)

#include "ngx_core.h"

import qualified Nginx.Core.String as Nginx

data TableElt = TableElt
  { hash :: CUInt
  , key :: Nginx.String
  , value :: Nginx.String
  }
  deriving (Show)

instance Storable TableElt where
  sizeOf _ = (#size ngx_table_elt_t)
  alignment _ = 8
  peek ptr = do
    TableElt
      <$> (#peek ngx_table_elt_t, hash) ptr
      <*> (#peek ngx_table_elt_t, key) ptr
      <*> (#peek ngx_table_elt_t, value) ptr
  poke = undefined

print_hash :: Ptr TableElt -> IO ()
print_hash ptr = do
  elt <- peek ptr
  print $ elt
  print $ Nginx.toInternalByteString (key elt)
  print $ Nginx.toInternalByteString (value elt)

foreign export ccall print_hash :: Ptr TableElt -> IO ()
