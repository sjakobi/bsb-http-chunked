{-# language OverloadedStrings #-}
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Builder.HTTP.Chunked
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.List.NonEmpty (NonEmpty)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit (testCase, (@?=))

main :: IO ()
main = defaultMain properties

properties :: TestTree
properties = testGroup "Properties"
  [ p "Non-empty inputs get CRLF suffix" $ do
      bss <- forAll $ genBSs
      assert $ all ("\r\n" `BS.isSuffixOf`) (encode bss)
  ]

genPackedBS :: Gen ByteString
genPackedBS = BS.pack <$> Gen.list (Range.linear 0 100) (Gen.word8 Range.constantBounded)

genSnippedBS :: Gen ByteString
genSnippedBS = do
  d <- Gen.int (Range.linear 0 5)
  t <- Gen.int (Range.linear 3 95)
  BS.drop d . BS.take t <$> genPackedBS

genBSs :: Gen (NonEmpty ByteString)
genBSs = Gen.nonEmpty (Range.linear 1 10) genSnippedBS

toBB :: Foldable t => t ByteString -> Builder
toBB = foldMap BB.byteString

p :: TestName -> PropertyT IO () -> TestTree
p name = testProperty name . property

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Encoding an empty BS returns an empty BS" $
      encode [""] @?= []
  ]

encode :: Foldable t => t ByteString -> [ByteString]
encode = BL.toChunks . BB.toLazyByteString . chunkedTransferEncoding . toBB

encodedLength :: ByteString -> Int
encodedLength = read . ("0x" ++) . C8.unpack . C8.takeWhile isHexDigit
