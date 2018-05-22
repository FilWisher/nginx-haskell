{-# LANGUAGE ForeignFunctionInterface #-}

module Nginx.Core.List where

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

#include "ngx_core.h"

data NgxListPart a = NgxListPart [a] (Ptr (NgxListPart a))
  deriving (Show)

data NgxList a = NgxList [NgxListPart a]
  deriving (Show)

toList :: NgxList a -> [a]
toList (NgxList parts) =
  foldr (\(NgxListPart ls _) acc -> acc ++ ls) [] parts

instance Storable a => Storable (NgxListPart a) where
  sizeOf _ = (#size ngx_list_part_t)
  alignment _ = 8
  peek ptr = do
    nelts <- (#peek ngx_list_part_t, nelts) (castPtr ptr :: Ptr CUInt)
    eltsPtr <- peek (castPtr ptr :: Ptr (Ptr a))
    NgxListPart
      <$> go eltsPtr nelts []
      <*> (#peek ngx_list_part_t, next) (castPtr ptr)
    where
      go :: Storable a => Ptr a -> CUInt -> [a] -> IO [a]
      go _ 0 ls = return ls
      go ptr off ls = do
        l <- peek ptr
        (l:) <$> go (ptr `plusPtr` sizeOf l) (off-1) ls
  poke = undefined

instance Storable a => Storable (NgxList a) where
  sizeOf _ = (#size ngx_list_t)
  alignment _ = 8
  peek ptr = do
    elms <- go [] =<< (#peek ngx_list_t, part) (castPtr ptr :: Ptr (NgxListPart a))
    return (NgxList elms)
    where
      go ls part@(NgxListPart _ next)
        | next == nullPtr = return (part:ls)
        | otherwise = go (part:ls) =<< peek next
  poke ptr part = undefined
