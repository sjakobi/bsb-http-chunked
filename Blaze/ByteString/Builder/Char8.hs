------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Char8
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  Leon P Smith <leon@melding-monads.com>
-- Stability:   experimental
--
-- //Note:// This package is intended for low-level use like implementing
-- protocols. If you need to //serialize// Unicode characters use one of the
-- UTF encodings (e.g. 'Blaze.ByteString.Builder.Char.UTF-8').
--
-- 'Write's and 'Builder's for serializing the lower 8-bits of characters.
--
-- This corresponds to what the 'bytestring' package offer in
-- 'Data.ByteString.Char8'.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Char8
    (
      -- * Writing Latin-1 (ISO 8859-1) encodable characters to a buffer
      writeChar
    ) where

import Blaze.ByteString.Builder.Compat.Write ( Write, writePrimFixed )
import qualified Data.ByteString.Builder.Prim as P

-- | Write the lower 8-bits of a character to a buffer.
writeChar :: Char -> Write
writeChar = writePrimFixed P.char8
{-# INLINE writeChar #-}
