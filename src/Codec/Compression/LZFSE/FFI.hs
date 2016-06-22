-- |LZFSE C bindings
-- 
-- Currently supports block compression/decompression only
--
module Codec.Compression.LZFSE.FFI
       ( 
         compress'
       , decompress'
       ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString      as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Unsafe
import           System.IO.Unsafe
import           Control.Monad (when)
import           Data.Monoid

import Foreign
import Foreign.C
import Foreign.C.String

foreign import ccall safe "lzfse_encode_scratch_size" c_lzfse_encode_scratch_size :: CInt
foreign import ccall safe "lzfse_decode_scratch_size" c_lzfse_decode_scratch_size :: CInt

foreign import ccall safe "lzfse_encode_buffer" c_lzfse_encode_buffer :: CString -> CInt -> CString -> CInt -> Ptr () -> IO CInt
foreign import ccall safe "lzfse_decode_buffer" c_lzfse_decode_buffer :: CString -> CInt -> CString -> CInt -> Ptr () -> IO CInt

type EncodeDecodeFunc = CString -> CInt -> CString -> CInt -> Ptr () -> IO CInt

{-# INLINE lzfseWith #-}
lzfseWith :: EncodeDecodeFunc -> CString -> CInt -> Ptr () -> ByteString -> IO ByteString
lzfseWith f outp outlen aux ins =  
  unsafeUseAsCStringLen ins $ \(input, inlen) -> do
  nb <- f outp outlen input (fromIntegral inlen) aux
  B.packCStringLen (outp, fromIntegral nb)

allocSize = 256 * 1024

{-# INLINABLE blockEncodeDecodeWith_ #-}
blockEncodeDecodeWith_ :: CInt -> EncodeDecodeFunc -> ByteString -> IO ByteString
blockEncodeDecodeWith_ prealloc f bs = 
  allocaBytes (fromIntegral (prealloc+4096)) $ \aux ->
  allocaBytes (fromIntegral (allocSize+4096)) $ \outp ->
  lzfseWith f outp allocSize aux bs

-- |compress strict bytestring with block compression.
compress' :: ByteString -> ByteString
compress' = unsafePerformIO . blockEncodeDecodeWith_ c_lzfse_encode_scratch_size c_lzfse_encode_buffer

-- |decompress strict bytestring with block decompression.
decompress' :: ByteString -> ByteString
decompress' = unsafePerformIO . blockEncodeDecodeWith_ c_lzfse_decode_scratch_size c_lzfse_decode_buffer
