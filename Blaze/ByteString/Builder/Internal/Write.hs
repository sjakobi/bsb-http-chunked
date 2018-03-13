{-# LANGUAGE CPP, BangPatterns #-}

-- |
-- Module      : Blaze.ByteString.Builder.Internal.Poke
-- Copyright   : (c) 2010 Simon Meier
--               (c) 2010 Jasper van der Jeugt
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Leon Smith <leon@melding-monads.com>
-- Stability   : experimental
-- Portability : tested on GHC only
--
-- A general and efficient write type that allows for the easy construction of
-- builders for (smallish) bounded size writes to a buffer.
--
-- FIXME: Improve documentation.
--
module Blaze.ByteString.Builder.Internal.Write (
  -- * Poking a buffer
    Poke(..)
  , pokeN

  -- * Writing to abuffer
  , Write(..)
  , getPoke

  , exactWrite
  , boundedWrite

  -- * Constructing builders from writes
  , fromWrite

  -- * Writing 'Storable's
  ) where

import Foreign

import Control.Monad

import Data.ByteString.Builder.Internal

import Data.Monoid (Monoid(..))
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup
#endif

------------------------------------------------------------------------------
-- Poking a buffer and writing to a buffer
------------------------------------------------------------------------------

-- Sadly GHC is not smart enough: code where we branch and each branch should
-- execute a few IO actions and then return a value cannot be taught to GHC. At
-- least not such that it returns the value of the branches unpacked.
--
-- Hmm.. at least he behaves much better for the Monoid instance of Write
-- than the one for Poke. Serializing UTF-8 chars gets a slowdown of a
-- factor 2 when 2 chars are composed. Perhaps I should try out the writeList
-- instances also, as they may be more sensitive to to much work per Char.
--

-- | Changing a sequence of bytes starting from the given pointer. 'Poke's are
-- the most primitive buffer manipulation. In most cases, you don't use the
-- explicitely but as part of a 'Write', which also tells how many bytes will
-- be changed at most.
newtype Poke =
    Poke { runPoke :: Ptr Word8 -> IO (Ptr Word8) }

-- | A write of a bounded number of bytes.
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
--
data Write = Write {-# UNPACK #-} !Int Poke

-- | Extract the 'Poke' action of a write.
{-# INLINE getPoke #-}
getPoke :: Write -> Poke
getPoke (Write _ wio) = wio

#if MIN_VERSION_base(4,9,0)
instance Semigroup Poke where
  {-# INLINE (<>) #-}
  (Poke po1) <> (Poke po2) = Poke $ po1 >=> po2

  {-# INLINE sconcat #-}
  sconcat = foldr (<>) mempty
#endif

instance Monoid Poke where
  {-# INLINE mempty #-}
  mempty = Poke return

#if !(MIN_VERSION_base(4,11,0))
  {-# INLINE mappend #-}
  (Poke po1) `mappend` (Poke po2) = Poke $ po1 >=> po2
#endif

  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

#if MIN_VERSION_base(4,9,0)
instance Semigroup Write where
  {-# INLINE (<>) #-}
  (Write bound1 w1) <> (Write bound2 w2) =
    Write (bound1 + bound2) (w1 <> w2)

  {-# INLINE sconcat #-}
  sconcat = foldr (<>) mempty
#endif

instance Monoid Write where
  {-# INLINE mempty #-}
  mempty = Write 0 mempty

#if !(MIN_VERSION_base(4,11,0))
  {-# INLINE mappend #-}
  (Write bound1 w1) `mappend` (Write bound2 w2) =
    Write (bound1 + bound2) (w1 `mappend` w2)
#endif

  {-# INLINE mconcat #-}
  mconcat = foldr mappend mempty

-- | @pokeN size io@ creates a write that denotes the writing of @size@ bytes
-- to a buffer using the IO action @io@. Note that @io@ MUST write EXACTLY @size@
-- bytes to the buffer!
{-# INLINE pokeN #-}
pokeN :: Int
       -> (Ptr Word8 -> IO ()) -> Poke
pokeN size io = Poke $ \op -> io op >> (return $! (op `plusPtr` size))


-- | @exactWrite size io@ creates a bounded write that can later be converted to
-- a builder that writes exactly @size@ bytes. Note that @io@ MUST write
-- EXACTLY @size@ bytes to the buffer!
{-# INLINE exactWrite #-}
exactWrite :: Int
           -> (Ptr Word8 -> IO ())
           -> Write
exactWrite size io = Write size (pokeN size io)

-- | @boundedWrite size write@ creates a bounded write from a @write@ that does
-- not write more than @size@ bytes.
{-# INLINE boundedWrite #-}
boundedWrite :: Int -> Poke -> Write
boundedWrite = Write

-- | Create a builder that execute a single 'Write'.
{-# INLINE fromWrite #-}
fromWrite :: Write -> Builder
fromWrite (Write maxSize wio) =
    builder step
  where
    step k (BufferRange op ope)
      | op `plusPtr` maxSize <= ope = do
          op' <- runPoke wio op
          let !br' = BufferRange op' ope
          k br'
      | otherwise = return $ bufferFull maxSize op (step k)
