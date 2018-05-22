{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Types.Extended
    ( module Network.HTTP.Types
    ) where

import Network.HTTP.Types
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import qualified Data.ByteString as BS

#include "ngx_http.h"
#include "ngx_http_haskell_module.h"

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
