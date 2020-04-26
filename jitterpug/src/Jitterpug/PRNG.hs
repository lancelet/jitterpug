{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module      : Jitterpug.PRNG
-- Description : Hash-based PRNG for image sampling.
--
-- The pseudo-random number generation in this module is based on the concept of
-- requesting a particular 'Pattern' of random samples, where the random samples
-- for a given 'Pattern' will always be the same. 'Pattern' operates somewhat like
-- a random seed. The 'Index' type specifies an index for a value within a given
-- pattern that should be returned.
--
-- For example, we can request different 'Pattern's of permutations of indices
-- using the 'permuteIndex' function:
--
-- >>> permuteIndex (Pattern 4) (NSamples 3) . Index <$> [0 .. 2]
-- [Index 1,Index 0,Index 2]
--
-- 'Pattern', 'Index' and 'NSamples' are all instances of 'Num', so this can also
-- be shortened to:
--
-- >>> permuteIndex 4 3 <$> [0 .. 2]
-- [Index 1,Index 0,Index 2]
--
-- We can request samples of 'Float's in a similar way using 'randFloat':
--
-- >>> randFloat 5 <$> [0 .. 5]
-- [0.66834587,0.5991194,0.54418945,0.21984401,2.0471262e-2,0.8727778]
--
-- The pseudo-random number generators in this file are close copies of those
-- described here:
--
--   - <https://graphics.pixar.com/library/MultiJitteredSampling/ Kensler A (2013) Correlated Multi-Jittered Sampling. Pixar Technical Memo 13-01.>
module Jitterpug.PRNG
  ( -- * Types
    Pattern (Pattern, unPattern),
    Index (Index, unIndex),
    NSamples (NSamples, unNSamples),

    -- * Functions
    offsetForPixel,
    randFloat,
    permuteIndex,
  )
where

import Data.Bits
  ( (.&.),
    (.|.),
    shiftR,
    xor,
  )
import Data.Word (Word32)
import Jitterpug.Geom (Pxc)
import qualified Jitterpug.Geom as Geom

-- | Pattern in a pseudo-random number generator.
--
-- Each pattern indexes a new sequence of pseudo-random output. It operates
-- somewhat like a random seed.
newtype Pattern = Pattern {unPattern :: Word32}
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral)

-- | Index into a pattern.
newtype Index = Index {unIndex :: Word32}
  deriving (Eq, Ord, Num, Real, Enum, Integral)

instance Show Index where
  show index = "Index " <> show (unIndex index)

-- | Total number of samples or total length.
newtype NSamples = NSamples {unNSamples :: Word32}
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral)

-- | Standardised way to offset a pattern using pixel coordinates.
offsetForPixel ::
  -- | Pixel coordinates (x, y).
  Pxc ->
  -- | Input pattern value.
  Pattern ->
  -- | Output pattern value.
  Pattern
offsetForPixel pxc = (*) (0xabcdef0 * Geom.pxcY pxc + Geom.pxcX pxc)
{-# INLINE offsetForPixel #-}

-- | Pseudo-random 'Float' from the specified index in a particular pattern.
--
-- The 'Float' values produced are always in the range @[0, 1)@.
--
-- For example, we can generate a list of 'Float' values from a given pattern
-- number (eg. 'Pattern' 0) as follows:
--
-- >>> randFloat 0 <$> [0 .. 5]
-- [0.8727778,2.0471262e-2,0.5252205,0.964735,0.5991194,0.66834587]
--
-- For a different 'Pattern' number, we expect that (usually) the generated
-- 'Float' values will be different:
--
-- >>> randFloat 1 <$> [0 .. 5]
-- [2.0471262e-2,0.8727778,0.964735,0.5252205,0.66834587,0.5991194]
randFloat ::
  -- | Pattern from which to request a 'Float'.
  Pattern ->
  -- | Sample index.
  Index ->
  -- | Floating-point value from the given pattern at the specified index.
  Float
randFloat p i = randFloat' (unIndex i) (unPattern p)
{-# INLINE randFloat #-}

-- | Pseudo-random permutation of indices.
--
-- For a given 'Pattern', all index-to-index permutations can be obtained by
-- iterating over the input index.
--
-- For example, for pattern number 0, we can request a permutation of 5 index
-- values as follows:
--
-- >>> permuteIndex 0 5 <$> [ 0 .. 4 ]
-- [Index 0,Index 4,Index 2,Index 3,Index 1]
--
-- For a different pattern (eg. pattern number 1) we (might) obtain a different
-- permutation:
--
-- >>> permuteIndex 1 5 <$> [ 0 .. 4 ]
-- [Index 0,Index 1,Index 2,Index 4,Index 3]
--
-- In each case, for a permutation of length @N@ (of type 'NSamples'), the
-- indices 0 to @N-1@ appear permuted in the output.
permuteIndex ::
  -- | Pattern from which to request permuted indices.
  Pattern ->
  -- | Total length of the permutation.
  NSamples ->
  -- | Input sample index.
  Index ->
  -- | Output (permuted) sample index.
  Index
permuteIndex p l i =
  Index $ permute' (unIndex i) (unNSamples l) (unPattern p)
{-# INLINE permuteIndex #-}

-- | Pseudo-random 'Float'.
--
-- This is a Haskell transcription of @randfloat@ from Kensler (2013).
randFloat' ::
  -- | Index.
  Word32 ->
  -- | Pattern.
  Word32 ->
  Float
randFloat' i p =
  let xorSelfShift :: Word32 -> Int -> Word32
      xorSelfShift x n = x `xor` (x `shiftR` n)
      coef :: Float
      coef = 1.0 / 4294967808.0
      i1, i2, i3, i4, i5, i6, i7, i8, i9, i10 :: Word32
      i1 = i `xor` p
      i2 = i1 `xorSelfShift` 17
      i3 = i2 `xorSelfShift` 10
      i4 = i3 * 0xb36534e5
      i5 = i4 `xorSelfShift` 12
      i6 = i5 `xorSelfShift` 21
      i7 = i6 * 0x93fc4795
      i8 = i7 `xor` 0xdf6e307f
      i9 = i8 `xorSelfShift` 17
      i10 = i9 * (1 .|. (p `shiftR` 18))
   in fromIntegral i10 * coef
{-# INLINE randFloat' #-}

-- | Pseudo-random permutation index.
--
-- This is a Haskell transcription of @permute@ from Kensler (2013).
permute' ::
  -- | Index.
  Word32 ->
  -- | Length of the permutation.
  Word32 ->
  -- | Pattern.
  Word32 ->
  Word32
permute' i l p =
  let orSelfShift :: Word32 -> Int -> Word32
      orSelfShift x n = x .|. (x `shiftR` n)
      w0, w1, w2, w3, w4, w5, w :: Word32
      w0 = l - 1
      w1 = w0 `orSelfShift` 1
      w2 = w1 `orSelfShift` 2
      w3 = w2 `orSelfShift` 4
      w4 = w3 `orSelfShift` 8
      w5 = w4 `orSelfShift` 16
      w = w5
      go :: Word32 -> Word32
      go i' =
        let i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10 :: Word32
            i11, i12, i13, i14, i15, i16, i17 :: Word32
            i0 = i' `xor` p
            i1 = i0 * 0xe170893d
            i2 = i1 `xor` (p `shiftR` 16)
            i3 = i2 `xor` ((i2 .&. w) `shiftR` 4)
            i4 = i3 `xor` (p `shiftR` 8)
            i5 = i4 * 0x0929eb3f
            i6 = i5 `xor` (p `shiftR` 23)
            i7 = i6 `xor` ((i6 .&. w) `shiftR` 1)
            i8 = i7 * (1 .|. (p `shiftR` 27))
            i9 = i8 * 0x6935fa69
            i10 = i9 `xor` ((i9 .&. w) `shiftR` 11)
            i11 = i10 * 0x74dcb303
            i12 = i11 `xor` ((i11 .&. w) `shiftR` 2)
            i13 = i12 * 0x9e501cc3
            i14 = i13 `xor` ((i13 .&. w) `shiftR` 2)
            i15 = i14 * 0xc860a3df
            i16 = i15 .&. w
            i17 = i16 `xor` (i16 `shiftR` 5)
         in if i17 < l then i17 else go i17
   in (go i + p) `mod` l
{-# INLINE permute' #-}
