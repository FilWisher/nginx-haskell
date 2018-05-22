{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Nginx.Core.String where

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
data NgxString = NgxString
  { len :: CSize
  , cptr :: Ptr CChar
  }
  deriving (Show)

-- | O(1). Construct a ByteString from an NgxString. The underlying bytes are
-- not copied and must remain allocated and unchanged while the ByteString is
-- still in use. For example, references to the original NGINX request imported
-- using this function should not outlive the request.
toForeignByteString :: NgxString -> ByteString
toForeignByteString (NgxString len cptr) = 
  unsafePerformIO
    $ BSUnsafe.unsafePackCStringLen (cptr, fromIntegral len)

-- | O(n). Construct a ByteString from an NgxString. The underlying bytes are
-- copied and managed by the Haskell runtime. References to the original NGINX
-- request that must outlive the request itself should be interned using this
-- function.
toInternalByteString :: NgxString -> ByteString
toInternalByteString (NgxString len cptr) = 
  unsafePerformIO
    $ BS.packCStringLen (cptr, fromIntegral len)

fromByteString :: ByteString -> IO NgxString
fromByteString (BSInternal.PS fptr _ len) =
  withForeignPtr fptr $ \ptr -> return $ NgxString (fromIntegral len) (castPtr ptr)

instance Storable NgxString where
  sizeOf _ = (#size ngx_str_t)
  alignment _ = 8
  peek ptr = do
    NgxString
      <$> (#peek ngx_str_t, len) ptr
      <*> (#peek ngx_str_t, data) ptr
  poke ptr (NgxString len cptr) = do
    ((#poke ngx_str_t, len) ptr) len
    ((#poke ngx_str_t, data) ptr) cptr
