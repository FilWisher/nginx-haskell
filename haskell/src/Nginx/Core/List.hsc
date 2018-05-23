{-# LANGUAGE ForeignFunctionInterface #-}

module Nginx.Core.List where

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

#include "ngx_core.h"

data ListPart a = ListPart [a] (Ptr (ListPart a))
  deriving (Show)

data List a = List [ListPart a]
  deriving (Show)

toList :: List a -> [a]
toList (List parts) =
  foldr (\(ListPart ls _) acc -> acc ++ ls) [] parts

instance Storable a => Storable (ListPart a) where
  sizeOf _ = (#size ngx_list_part_t)
  alignment _ = 8
  peek ptr = do
    nelts <- (#peek ngx_list_part_t, nelts) (castPtr ptr :: Ptr CUInt)
    eltsPtr <- peek (castPtr ptr :: Ptr (Ptr a))
    ListPart
      <$> go eltsPtr nelts []
      <*> (#peek ngx_list_part_t, next) (castPtr ptr)
    where
      go :: Storable a => Ptr a -> CUInt -> [a] -> IO [a]
      go _ 0 ls = return ls
      go ptr off ls = do
        l <- peek ptr
        (l:) <$> go (ptr `plusPtr` sizeOf l) (off-1) ls
  poke = undefined

instance Storable a => Storable (List a) where
  sizeOf _ = (#size ngx_list_t)
  alignment _ = 8
  peek ptr = do
    elms <- go [] =<< (#peek ngx_list_t, part) (castPtr ptr :: Ptr (ListPart a))
    return (List elms)
    where
      go ls part@(ListPart _ next)
        | next == nullPtr = return (part:ls)
        | otherwise = go (part:ls) =<< peek next
  poke ptr part = undefined
