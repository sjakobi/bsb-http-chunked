------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Compat.Write
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- Conversions from the new Prims to the old Writes.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Compat.Write
    ( Write
    , writePrimFixed
    ) where

import Data.ByteString.Builder.Prim.Internal (FixedPrim, runF, size)
import Blaze.ByteString.Builder.Internal.Write (Write, exactWrite)

writePrimFixed :: FixedPrim a -> a -> Write
writePrimFixed fe a = exactWrite (size fe) (runF fe a)
{-# INLINE writePrimFixed #-}
