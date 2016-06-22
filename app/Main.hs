module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as B

import Codec.Compression.LZFSE

s1 = LBS.pack [1,2,3,4]

main :: IO ()
main = B.getContents >>= B.putStr . decompress'
