{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Nginx.Core.String where

import Prelude hiding (String)

import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Data.ByteString.Char8 (ByteString(..))
import qualified Data.ByteString.Internal as BSInternal
import qualified Data.ByteString.Unsafe as BSUnsafe
import qualified Data.ByteString as BS

import System.IO.Unsafe (unsafePerformIO)

#include "ngx_core.h"

-- | A foreign string allocated and managed by NGINX. The `cptr` points to the
-- beginning of the buffer allocated by NGINX containing the string data.
data String = String
  { len :: CSize
  , cptr :: Ptr CChar
  }
  deriving (Show)

-- | O(1). Construct a ByteString from an String. The underlying bytes are
-- not copied and must remain allocated and unchanged while the ByteString is
-- still in use. For example, references to the original NGINX request imported
-- using this function should not outlive the request.
toForeignByteString :: String -> ByteString
toForeignByteString (String len cptr) = 
  unsafePerformIO
    $ BSUnsafe.unsafePackCStringLen (cptr, fromIntegral len)

-- | O(n). Construct a ByteString from an String. The underlying bytes are
-- copied and managed by the Haskell runtime. References to the original NGINX
-- request that must outlive the request itself should be interned using this
-- function.
toInternalByteString :: String -> ByteString
toInternalByteString (String len cptr) = 
  unsafePerformIO
    $ BS.packCStringLen (cptr, fromIntegral len)

fromByteString :: ByteString -> IO String
fromByteString (BSInternal.PS fptr _ len) =
  withForeignPtr fptr $ \ptr -> return $ String (fromIntegral len) (castPtr ptr)

instance Storable String where
  sizeOf _ = (#size ngx_str_t)
  alignment _ = 8
  peek ptr = do
    String
      <$> (#peek ngx_str_t, len) ptr
      <*> (#peek ngx_str_t, data) ptr
  poke ptr (String len cptr) = do
    ((#poke ngx_str_t, len) ptr) len
    ((#poke ngx_str_t, data) ptr) cptr
