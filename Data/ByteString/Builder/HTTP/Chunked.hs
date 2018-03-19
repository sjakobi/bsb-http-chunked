{-# LANGUAGE BangPatterns, CPP, MagicHash, OverloadedStrings #-}
-- | Chunked HTTP transfer encoding

module Data.ByteString.Builder.HTTP.Chunked (
    chunkedTransferEncoding
  , chunkedTransferTerminator
  ) where

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word (Word32(..))
#else
import Data.Word
#endif

import Control.Monad
import Foreign

import qualified Data.ByteString                       as S
import           Data.ByteString.Char8                 () -- for IsString instance
import           Data.ByteString.Builder               (Builder)
import           Data.ByteString.Builder.Internal      (BufferRange(..), BuildSignal)
import qualified Data.ByteString.Builder.Internal      as B
import qualified Data.ByteString.Builder.Prim          as P
import           Data.ByteString.Builder.Prim.Internal (FixedPrim)
import qualified Data.ByteString.Builder.Prim.Internal as P

------------------------------------------------------------------------------
-- Poking a buffer
------------------------------------------------------------------------------

-- | Changing a sequence of bytes starting from the given pointer. 'Poke's are
-- the most primitive buffer manipulation. In most cases, you don't use the
-- explicitely but as part of a 'Write', which also tells how many bytes will
-- be changed at most.
newtype Poke =
    Poke { runPoke :: Ptr Word8 -> IO (Ptr Word8) }

-- | @mappend@ for 'Poke's
{-# INLINE appendP #-}
appendP :: Poke -> Poke -> Poke
(Poke po1) `appendP` (Poke po2) = Poke $ po1 >=> po2

-- | @pokeN size io@ creates a write that denotes the writing of @size@ bytes
-- to a buffer using the IO action @io@. Note that @io@ MUST write EXACTLY @size@
-- bytes to the buffer!
{-# INLINE pokeN #-}
pokeN :: Int -> (Ptr Word8 -> IO ()) -> Poke
pokeN size io = Poke $ \op -> io op >> (return $! (op `plusPtr` size))

------------------------------------------------------------------------------
-- Write
------------------------------------------------------------------------------

-- | A write of a bounded number of bytes.
--
-- A general and efficient write type that allows for the easy construction of
-- builders for (smallish) bounded size writes to a buffer.
--
-- When defining a function @write :: a -> Write@ for some @a@, then it is
-- important to ensure that the bound on the number of bytes written is
-- data-independent. Formally,
--
--  @ forall x y. getBound (write x) = getBound (write y) @
--
-- The idea is that this data-independent bound is specified such that the
-- compiler can optimize the check, if there are enough free bytes in the buffer,
-- to a single subtraction between the pointer to the next free byte and the
-- pointer to the end of the buffer with this constant bound of the maximal
-- number of bytes to be written.
data Write = Write {-# UNPACK #-} !Int Poke

-- | @mappend@ for 'Write's
{-# INLINE appendW #-}
appendW :: Write -> Write -> Write
(Write bound1 w1) `appendW` (Write bound2 w2) =
    Write (bound1 + bound2) (w1 `appendP` w2)

-- | Extract the 'Poke' action of a write.
{-# INLINE getPoke #-}
getPoke :: Write -> Poke
getPoke (Write _ wio) = wio

-- | @exactWrite size io@ creates a bounded write that can later be converted to
-- a builder that writes exactly @size@ bytes. Note that @io@ MUST write
-- EXACTLY @size@ bytes to the buffer!
{-# INLINE exactWrite #-}
exactWrite :: Int -> (Ptr Word8 -> IO ()) -> Write
exactWrite size io = Write size (pokeN size io)

------------------------------------------------------------------------------
-- Converting 'Write's to 'Builder's
------------------------------------------------------------------------------

-- | Create a builder that execute a single 'Write'.
{-# INLINE fromWrite #-}
fromWrite :: Write -> Builder
fromWrite (Write maxSize wio) =
    B.builder step
  where
    step k (BufferRange op ope)
      | op `plusPtr` maxSize <= ope = do
          op' <- runPoke wio op
          let !br' = BufferRange op' ope
          k br'
      | otherwise = return $ B.bufferFull maxSize op (step k)

------------------------------------------------------------------------------
-- Write utils
------------------------------------------------------------------------------

-- | Write a CRLF sequence.
{-# INLINE writeCRLF #-}
writeCRLF :: Write
writeCRLF = writeChar8 '\r' `appendW` writeChar8 '\n'
  where writeChar8 = writePrimFixed P.char8

{-# INLINE writePrimFixed #-}
writePrimFixed :: FixedPrim a -> a -> Write
writePrimFixed fe a = exactWrite (P.size fe) (P.runF fe a)

-- | Execute a write
{-# INLINE execWrite #-}
execWrite :: Write -> Ptr Word8 -> IO ()
execWrite w op = runPoke (getPoke w) op >> return ()

------------------------------------------------------------------------------
-- Hex Encoding Infrastructure
------------------------------------------------------------------------------

{-# INLINE shiftr_w32 #-}
shiftr_w32 :: Word32 -> Int -> Word32
#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`   i)
#else
shiftr_w32 = shiftR
#endif

pokeWord32HexN :: Int -> Word32 -> Ptr Word8 -> IO ()
pokeWord32HexN n0 w0 op0 =
    go w0 (op0 `plusPtr` (n0 - 1))
  where
    go !w !op
      | op < op0  = return ()
      | otherwise = do
          let nibble :: Word8
              nibble = fromIntegral w .&. 0xF
              hex | nibble < 10 = 48 + nibble
                  | otherwise   = 55 + nibble
          poke op hex
          go (w `shiftr_w32` 4) (op `plusPtr` (-1))
{-# INLINE pokeWord32HexN #-}

iterationsUntilZero :: Integral a => (a -> a) -> a -> Int
iterationsUntilZero f = go 0
  where
    go !count 0  = count
    go !count !x = go (count+1) (f x)
{-# INLINE iterationsUntilZero #-}

-- | Length of the hex-string required to encode the given 'Word32'.
word32HexLength :: Word32 -> Int
word32HexLength = max 1 . iterationsUntilZero (`shiftr_w32` 4)
{-# INLINE word32HexLength #-}

writeWord32Hex :: Word32 -> Write
writeWord32Hex w =
    Write (2 * sizeOf w) (pokeN len $ pokeWord32HexN len w)
  where
    len = word32HexLength w
{-# INLINE writeWord32Hex #-}

------------------------------------------------------------------------------
-- Chunked transfer encoding
------------------------------------------------------------------------------

-- | Transform a builder such that it uses chunked HTTP transfer encoding.
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
          -- FIXME: Assert that outRemaining < maxBound :: Word32
          | outRemaining < minimalBufferSize =
              return $ B.bufferFull minimalBufferSize op (go innerStep)
          | otherwise = do
              let !brInner@(BufferRange opInner _) = BufferRange
                     (op  `plusPtr` (chunkSizeLength + 2))     -- leave space for chunk header
                     (ope `plusPtr` (-maxAfterBufferOverhead)) -- leave space at end of data

                  -- wraps the chunk, if it is non-empty, and returns the
                  -- signal constructed with the correct end-of-data pointer
                  {-# INLINE wrapChunk #-}
                  wrapChunk :: Ptr Word8 -> (Ptr Word8 -> IO (BuildSignal a))
                            -> IO (BuildSignal a)
                  wrapChunk !opInner' mkSignal
                    | opInner' == opInner = mkSignal op
                    | otherwise           = do
                        pokeWord32HexN chunkSizeLength
                            (fromIntegral $ opInner' `minusPtr` opInner)
                            op
                        execWrite writeCRLF (opInner `plusPtr` (-2))
                        execWrite writeCRLF opInner'
                        mkSignal (opInner' `plusPtr` 2)

                  -- prepare handlers
                  doneH opInner' _ = wrapChunk opInner' $ \op' -> do
                                         let !br' = BufferRange op' ope
                                         k br'

                  fullH opInner' minRequiredSize nextInnerStep =
                      wrapChunk opInner' $ \op' ->
                        return $! B.bufferFull
                          (minRequiredSize + maxEncodingOverhead)
                          op'
                          (go nextInnerStep)

                  insertChunkH opInner' bs nextInnerStep
                    | S.null bs =                         -- flush
                        wrapChunk opInner' $ \op' ->
                          return $! B.insertChunk op' S.empty (go nextInnerStep)

                    | otherwise =                         -- insert non-empty bytestring
                        wrapChunk opInner' $ \op' -> do
                          -- add header for inserted bytestring
                          -- FIXME: assert(S.length bs < maxBound :: Word32)
                          !op'' <- (`runPoke` op') $ getPoke $
                              writeWord32Hex (fromIntegral $ S.length bs)
                              `appendW` writeCRLF

                          -- insert bytestring and write CRLF in next buildstep
                          return $! B.insertChunk
                            op'' bs
                            (B.runBuilderWith (fromWrite writeCRLF) $ go nextInnerStep)

              -- execute inner builder with reduced boundaries
              B.fillWithBuildStep innerStep doneH fullH insertChunkH brInner
          where
            -- minimal size guaranteed for actual data no need to require more
            -- than 1 byte to guarantee progress the larger sizes will be
            -- hopefully provided by the driver or requested by the wrapped
            -- builders.
            minimalChunkSize  = 1

            -- overhead computation
            maxBeforeBufferOverhead = sizeOf (undefined :: Int) + 2 -- max chunk size and CRLF after header
            maxAfterBufferOverhead  = 2 +                           -- CRLF after data
                                      sizeOf (undefined :: Int) + 2 -- max bytestring size, CRLF after header

            maxEncodingOverhead = maxBeforeBufferOverhead + maxAfterBufferOverhead

            minimalBufferSize = minimalChunkSize + maxEncodingOverhead

            -- remaining and required space computation
            outRemaining :: Int
            outRemaining    = ope `minusPtr` op
            chunkSizeLength = word32HexLength $ fromIntegral outRemaining

-- | The zero-length chunk '0\r\n\r\n' signaling the termination of the data transfer.
chunkedTransferTerminator :: Builder
chunkedTransferTerminator = B.byteStringCopy "0\r\n\r\n"
