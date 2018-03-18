{-# language OverloadedStrings, MultiWayIf #-}
module Main where

import qualified Data.ByteString.Builder as B

import Data.Attoparsec.ByteString.Char8 (Parser, (<?>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.ByteString.Builder.HTTP.Chunked
import qualified Data.ByteString.Lazy as L
import Data.Foldable
import Data.Functor

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain $ testGroup "Tests" [properties]

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
genSs = Gen.list (Range.linear 0 3) genSnippedS

genSnippedS :: Gen ByteString
genSnippedS = do
  d <- Gen.int (Range.linear 0 8)
  e <- Gen.int (Range.linear 0 8)
  S.drop d . dropEnd e <$> genPackedS
  where
    dropEnd n bs = S.take m bs
      where m = S.length bs - n

genPackedS :: Gen ByteString
genPackedS = S.pack <$> Gen.list (Range.linear 0 (maxSafeBsSize + 2000)) Gen.enumBounded

-- | FIXME: Some larger inputs break the parser.
-- See https://github.com/sjakobi/bsb-http-chunked/issues/9
maxSafeBsSize :: Int
maxSafeBsSize = 8160

parseTransferChunks :: L.ByteString -> Either String L.ByteString
parseTransferChunks = fmap (L.fromChunks . concat) .
                      traverse (A.eitherResult . fmap toList . A.parse transferChunkParser) .
                      L.toChunks

-- Adapted from snap-server
transferChunkParser :: Parser (Maybe ByteString)
transferChunkParser = parser <?> "encodedChunkParser"
  where
    parser = do
        hex <- A.hexadecimal <?> "hexadecimal"
        -- skipWhile (/= '\r') <?> "skipToEOL" -- We don't add chunk extensions
        void crlf <?> "linefeed"
        if | hex >= mAX_CHUNK_SIZE
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

    -- Chunks larger than this may indicate denial-of-service attack.
    mAX_CHUNK_SIZE = 2^(18::Int)

    crlf = A.string "\r\n"
