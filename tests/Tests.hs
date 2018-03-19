{-# language OverloadedStrings, MultiWayIf #-}
module Main where

import qualified Data.ByteString.Builder as B

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.Attoparsec.ByteString.Lazy as AL
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.ByteString.Builder.HTTP.Chunked
import qualified Data.ByteString.Lazy as L
import Data.Functor
import Data.Maybe

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain $ testGroup "Tests" [properties, unitTests]

chunkedTransferEncodingL :: L.ByteString -> L.ByteString
chunkedTransferEncodingL = B.toLazyByteString . chunkedTransferEncoding . B.lazyByteString

properties :: TestTree
properties = testGroup "Properties"
  [ p "Encoding and parsing roundtrips" $ do
      lbs <- forAll genLS
      tripping lbs
               chunkedTransferEncodingL
               parseTransferChunks
  ]
  where
    p name = testProperty name . property

genLS :: Gen L.ByteString
genLS = L.fromChunks <$> genSs

genSs :: Gen [ByteString]
genSs = Gen.list (Range.linear 0 10) genSnippedS

genSnippedS :: Gen ByteString
genSnippedS = do
  d <- Gen.int (Range.linear 0 8)
  e <- Gen.int (Range.linear 0 8)
  S.drop d . dropEnd e <$> genPackedS
  where
    dropEnd n bs = S.take m bs
      where m = S.length bs - n

genPackedS :: Gen ByteString
genPackedS = S.replicate <$> Gen.int (Range.linear 0 mAX_CHUNK_SIZE) <*> Gen.enumBounded

parseTransferChunks :: L.ByteString -> Either String L.ByteString
parseTransferChunks = AL.eitherResult .
                      fmap (L.fromChunks . catMaybes) .
                      AL.parse (many transferChunkParser)

-- Adapted from snap-server
transferChunkParser :: Parser (Maybe ByteString)
transferChunkParser = parser <?> "encodedChunkParser"
  where
    parser = do
        hex <- A.hexadecimal <?> "hexadecimal"
        -- skipWhile (/= '\r') <?> "skipToEOL" -- We don't add chunk extensions
        void crlf <?> "linefeed"
        if | hex > mAX_CHUNK_SIZE
            -> fail $ "Chunk of size " ++ show hex ++
                 " is too long. Max chunk size is " ++ show mAX_CHUNK_SIZE
           | hex < 0
             -> fail $ "Negative chunk size: " ++ show hex
           | hex == 0
             -> (crlf >> return Nothing) <?> "terminal crlf after 0 length"
           | otherwise
             -> do
                x <- A.take hex <?> "reading data chunk"
                void crlf <?> "linefeed after data chunk"
                return $! Just x

    crlf = A.string "\r\n"

-- Chunks larger than this may indicate denial-of-service attack.
mAX_CHUNK_SIZE :: Int
mAX_CHUNK_SIZE = 256 * 1024 - 1

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Encoding an empty builder returns an empty builder" $
      chunkedTransferEncodingL "" @?= ""
  ]
