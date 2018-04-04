{-# LANGUAGE BangPatterns, CPP, MagicHash, OverloadedStrings #-}
-- | HTTP/1.1 chunked transfer encoding as defined
-- in [RFC 7230 Section 4.1](https://tools.ietf.org/html/rfc7230#section-4.1)

module Data.ByteString.Builder.HTTP.Chunked (
    chunkedTransferEncoding
  , chunkedTransferTerminator
  ) where

import           Control.Applicative                   (pure)
import           Control.Monad                         (void)
import           Foreign                               (Ptr, Word8, (.&.))
import qualified Foreign                               as F
import           GHC.Base                              (Int(..), uncheckedShiftRL#)
import           GHC.Word                              (Word(..))

import qualified Data.ByteString                       as S
import           Data.ByteString.Builder               (Builder)
import           Data.ByteString.Builder.Internal      (BufferRange(..), BuildSignal)
import qualified Data.ByteString.Builder.Internal      as B
import qualified Data.ByteString.Builder.Prim          as P
import qualified Data.ByteString.Builder.Prim.Internal as P
import           Data.ByteString.Char8                 () -- For the IsString instance

------------------------------------------------------------------------------
-- CRLF utils
------------------------------------------------------------------------------

{-# INLINE writeCRLF #-}
writeCRLF :: Ptr Word8 -> IO (Ptr Word8)
writeCRLF op = do
    P.runF twoChar8sPrim ('\r', '\n') op
    pure $! op `F.plusPtr` 2

{-# INLINE crlfBuilder #-}
crlfBuilder :: Builder
crlfBuilder = P.primFixed twoChar8sPrim ('\r', '\n')

twoChar8sPrim :: P.FixedPrim (Char, Char)
twoChar8sPrim = P.char8 P.>*< P.char8

-----------------------------------------------------------------------------
-- Hex Encoding Infrastructure
------------------------------------------------------------------------------

-- TODO: Use Data.Bits.unsafeShiftR once compatibility with base < 4.5 is
-- dropped.
{-# INLINE shiftr #-}
shiftr :: Word -> Int -> Word
shiftr (W# w) (I# i) = W# (w `uncheckedShiftRL#` i)

-- | @writeHex len w op@ writes the hex encoding of @w@ to @op@ and 
-- returns @op `'F.plusPtr'` len@.
--
-- If writing @w@ doesn't consume all @len@ bytes, leading zeros are added. 
{-# INLINE writeHex #-}
writeHex :: Int -> Word -> Ptr Word8 -> IO (Ptr Word8)
writeHex len w0 op0 = do
    go w0 (op0 `F.plusPtr` (len - 1))
    pure $! op0 `F.plusPtr` len
  where
    go !w !op
      | op < op0  = pure ()
      | otherwise = do
          let nibble :: Word8
              nibble = fromIntegral w .&. 0xF
              hex | nibble < 10 = 48 + nibble
                  | otherwise   = 55 + nibble
          F.poke op hex
          go (w `shiftr` 4) (op `F.plusPtr` (-1))

-- | Length of the hex-string required to encode the given 'Word'.
{-# INLINE hexLength #-}
hexLength :: Word -> Int
#if MIN_VERSION_base(4,8,0)
hexLength w = (2 * F.sizeOf w) - (F.countLeadingZeros w `F.unsafeShiftR` 2)
#else
hexLength = max 1 . iterationsUntilZero (`shiftr` 4)

{-# INLINE iterationsUntilZero #-}
iterationsUntilZero :: Integral a => (a -> a) -> a -> Int
iterationsUntilZero f = go 0
  where
    go !count 0  = count
    go !count !x = go (count+1) (f x)
#endif

------------------------------------------------------------------------------
-- Chunked transfer encoding
------------------------------------------------------------------------------

-- | Transform a builder such that it uses chunked HTTP transfer encoding.
--
-- Chunk sizes up to 16 TB are supported.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString.Builder as B
-- >>> let f = B.toLazyByteString . chunkedTransferEncoding . B.lazyByteString
-- >>> f "data"
-- "004\r\ndata\r\n"
--
-- >>> f ""
-- ""
--
-- /Note/: While for many inputs, the bytestring chunks that can be obtained from the output
-- via @'Data.ByteString.Lazy.toChunks' . 'Data.ByteString.Builder.toLazyByteString'@
-- each form a chunk in the sense
-- of [RFC 7230 Section 4.1](https://tools.ietf.org/html/rfc7230#section-4.1),
-- this correspondence doesn't hold in general.
chunkedTransferEncoding :: Builder -> Builder
chunkedTransferEncoding innerBuilder =
    B.builder transferEncodingStep
  where
    transferEncodingStep k =
        go (B.runBuilder innerBuilder)
      where
        go innerStep (BufferRange op ope)
          | ope `F.minusPtr` op < minimalBufferSize =
              pure $ B.bufferFull minimalBufferSize op (go innerStep)
          | otherwise = do
              let !maxWritableChunkSize = (ope `F.minusPtr` op)
                                          - 1 -- Minimal chunk size length
                                          - 2 -- CRLF after chunk size
                                          - maxAfterBufferOverhead
              if F.sizeOf (undefined :: Int) > 4 && maxWritableChunkSize > maxConceivableChunkSize
               then fail "bla" -- why not just truncate the chunksize? And split the input?
               else do
                let
                  !maxWritableChunkSizeLength = hexLength $ fromIntegral maxWritableChunkSize
                  !brInner@(BufferRange opInner _) = BufferRange
                     (op  `F.plusPtr` (maxWritableChunkSizeLength + 2)) -- leave space for chunk header
                     (ope `F.plusPtr` (-maxAfterBufferOverhead)) -- leave space at end of data

                  -- wraps the chunk, if it is non-empty, and returns the
                  -- signal constructed with the correct end-of-data pointer
                  {-# INLINE wrapChunk #-}
                  wrapChunk :: Ptr Word8 -> (Ptr Word8 -> IO (BuildSignal a))
                            -> IO (BuildSignal a)
                  wrapChunk !chunkDataEnd mkSignal
                    | chunkDataEnd == opInner = mkSignal op
                    | otherwise           = do
                        let chunkSize = fromIntegral $ chunkDataEnd `F.minusPtr` opInner
                        -- If the hex of chunkSize requires less space than
                        -- maxWritableChunkSizeLength, we get leading zeros.
                        void $ writeHex maxWritableChunkSizeLength chunkSize op
                        void $ writeCRLF (opInner `F.plusPtr` (-2))
                        void $ writeCRLF chunkDataEnd
                        mkSignal (chunkDataEnd `F.plusPtr` 2)

                  doneH opInner' _ = wrapChunk opInner' $ \op' -> do
                                         let !br' = BufferRange op' ope
                                         k br'

                  fullH opInner' minRequiredSize nextInnerStep =
                      wrapChunk opInner' $ \op' ->
                        pure $! B.bufferFull
                          (minRequiredSize + maxEncodingOverhead)
                          op'
                          (go nextInnerStep)
 
                  insertChunkH opInner' bs nextInnerStep
                    | S.null bs =                         -- flush
                        wrapChunk opInner' $ \op' ->
                          pure $! B.insertChunk op' S.empty (go nextInnerStep)

                    | otherwise =                         -- insert non-empty bytestring
                        wrapChunk opInner' $ \op' -> do
                          -- add header for inserted bytestring
                          let chunkSize = fromIntegral $ S.length bs
                          !op'' <- writeHex (hexLength chunkSize) chunkSize op'
                          !op''' <- writeCRLF op''

                          -- insert bytestring and write CRLF in next buildstep
                          pure $! B.insertChunk
                            op''' bs
                           (B.runBuilderWith crlfBuilder $ go nextInnerStep)

                -- execute inner builder with reduced boundaries
                B.fillWithBuildStep innerStep doneH fullH insertChunkH brInner

-- | Minimal useful buffer size
minimalBufferSize :: Int
minimalBufferSize = minimalChunkSize + maxEncodingOverhead
  where
    -- Minimal size guaranteed for actual data. No need to require more
    -- than 1 byte to guarantee progress. The larger sizes will be
    -- hopefully provided by the driver or requested by the wrapped builders.
    minimalChunkSize = 1

-- | Overhead before and after buffer
maxEncodingOverhead :: Int
maxEncodingOverhead = maxBeforeBufferOverhead + maxAfterBufferOverhead

-- | Max chunk size and CRLF after header
maxBeforeBufferOverhead :: Int
maxBeforeBufferOverhead = maxConceivableChunkSizeLength + 2

-- | CRLF after data, max chunk size for next chunk, and CRLF after header
maxAfterBufferOverhead :: Int
maxAfterBufferOverhead  = 2 + maxConceivableChunkSizeLength + 2

-- | Enough bytes to write the hex size for a chunk of the size of your entire
--  memory (on 32-bit platforms) or 16 TB on 64-bit platforms.
maxConceivableChunkSizeLength :: Int
maxConceivableChunkSizeLength = (2 * F.sizeOf (undefined :: Word)) `min` 11

maxConceivableChunkSize :: Int
maxConceivableChunkSize = (1 `F.shiftL` (maxConceivableChunkSizeLength * 4)) - 1

------------------------------------------------------------------------------
-- Chunked transfer terminator
------------------------------------------------------------------------------

-- | The zero-length chunk @0\\r\\n\\r\\n@ signaling the termination of the data transfer.
chunkedTransferTerminator :: Builder
chunkedTransferTerminator = B.byteStringCopy "0\r\n\r\n"
