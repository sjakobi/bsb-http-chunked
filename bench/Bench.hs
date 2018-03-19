{-# language OverloadedStrings #-}
module Main where

import Gauge

import qualified Blaze.ByteString.Builder.HTTP as Blaze
import Data.ByteString.Builder.HTTP.Chunked

import Control.DeepSeq
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Lazy as L
import Data.Semigroup

main :: IO ()
main = defaultMain
  [ benchEncode "200kB strict bytestring"
                (S.replicate (200 * 1000) 95)
                B.byteString
  , benchEncode "1000 small chunks"
                "Hello"
                (stimes (1000 :: Int) . B.byteString)
  , benchEncode "1000 small chunks nocopy"
                "Hello"
                (stimes (1000 :: Int) . B.byteStringInsert)
  , benchEncode "100 4kB chunks"
                (S.replicate 4096 95)
                (stimes (100 :: Int) . B.byteString)
  ]

benchEncode :: NFData input => String -> input -> (input -> B.Builder) -> Benchmark
benchEncode name input mkBuilder =
  env (return input) $ \input' -> bgroup name
    [ bench "bsbhc" $ nf (encode . mkBuilder) input'
    , bench "Blaze" $ nf (encodeBlaze . mkBuilder) input'
    ]

encode :: B.Builder -> L.ByteString
encode = B.toLazyByteString . chunkedTransferEncoding

encodeBlaze :: B.Builder -> L.ByteString
encodeBlaze = B.toLazyByteString . Blaze.chunkedTransferEncoding
