{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Jitterpug.Hashable
-- Description : Fast hashing (for image sampling).
--
-- Fast hashing for image sampling purposes.
module Jitterpug.Hashable
  ( -- * Guide
    -- $guide

    -- * Types
    Hashable (hash),
    Hash (Hash, unHash),
  )
where

import Data.Bits ((.&.), complement, finiteBitSize, shiftL, shiftR, xor)
import Data.Word (Word16, Word32, Word64, Word8)
import Text.Printf (printf)
import Unsafe.Coerce (unsafeCoerce)

-- | Hash value.
newtype Hash
  = Hash
      { -- | Deconstruct the 'Hash' value to a 'Word32'.
        unHash :: Word32
      }

instance Show Hash where
  show (Hash x) = printf "Hash 0x%08x" x

-- | Type class for 32-bit hashing.
class Hashable a where
  -- | Compute a 32-bit hash for a given value.
  --
  -- The hash functions used here are quick-and-dirty. They are suitable for
  -- the type of noise generation required by Jitterpug, and may not be
  -- appropriate for other purposes.
  hash :: a -> Hash

instance Hashable Bool where
  hash = \case
    True -> Hash 0x967a889b
    False -> Hash 0x368cc8b7

instance Hashable Char where
  hash = Hash . word32Hash . unsafeCoerce

instance Hashable Word8 where
  hash = Hash . word32Hash . fromIntegral

instance Hashable Word16 where
  hash = Hash . word32Hash . fromIntegral

instance Hashable Word32 where
  hash = Hash . word32Hash

instance Hashable Word64 where
  hash = Hash . word64Hash

instance Hashable Int where
  hash = case finiteBitSize (0 :: Int) of
    32 -> Hash . word32Hash . unsafeCoerce
    64 -> Hash . word64Hash . unsafeCoerce
    x ->
      error $
        "Unexpected finiteBitSize for Int: "
          <> show x
          <> "; we only handle 32 or 64 bits."

instance Hashable Float where
  hash = Hash . word32Hash . unsafeCoerce

instance Hashable Double where
  hash = Hash . word64Hash . unsafeCoerce

-- | Hashing function for 'Word32'.
--
-- Based on Robert Jenkins' 32-bit integer hash function from here:
-- http://web.archive.org/web/20071223173210/http://www.concentric.net/~Ttwang/tech/inthash.htm
word32Hash :: Word32 -> Word32
word32Hash x0 =
  let x1, x2, x3, x4, x5, x6 :: Word32
      x1 = x0 + 0x7ed55d16 + shiftL x0 12
      x2 = xor (xor x1 0xc761c23c) (shiftR x1 19)
      x3 = x2 + 0x165667b1 + shiftL x2 5
      x4 = xor (x3 + 0xd3a2646c) (shiftL x3 9)
      x5 = x4 + 0xfd7046c5 + shiftL x4 3
      x6 = xor (xor x5 0xb55a4f09) (shiftR x5 16)
   in x6

-- | Hashing function for 'Word64'.
--
-- Based on the 64 bit to 32 bit hash function from here:
-- http://web.archive.org/web/20071223173210/http://www.concentric.net/~Ttwang/tech/inthash.htm
word64Hash :: Word64 -> Word32
word64Hash x0 =
  let x1 = complement x0 + shiftL x0 18
      x2 = xor x1 (shiftR x1 31)
      x3 = x2 * 21
      x4 = xor x3 (shiftR x3 11)
      x5 = x4 + shiftL x4 6
      x6 = xor x5 (shiftR x5 22)
   in fromIntegral (x6 .&. 0xFFFFFFFF)

-- $guide
--
-- Examples:
--
-- >>> hash (42.0 :: Float)
-- Hash 0x56371023
-- >>> hash (1 :: Int)
-- Hash 0x15515fbc
-- >>> hash (2 :: Int)
-- Hash 0x2aa2ba14
--
-- Inside Jitterpug, hashes are used to introduce deterministic,
-- pseudo-randomness into sampling. See also the 'Jitterpug.PRNG.jump' function.
