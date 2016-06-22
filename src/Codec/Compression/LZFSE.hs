-- |LZFSE C bindings
-- 
-- Currently supports block compression/decompression only
--
module Codec.Compression.LZFSE
       ( 
         compress'
       , decompress'
       ) where

import Codec.Compression.LZFSE.FFI
