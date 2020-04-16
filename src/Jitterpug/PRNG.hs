{-|
Module      : Jitterpug.PRNG
Description :

Pseudo-random number generation for image sampling.

The functions in this file are Haskell transcriptions of the pseudo-random
number generators found in:

  - Kensler A (2013) Correlated Multi-Jittered Sampling.
    Pixar Technical Memo 13-01.
-}
module Jitterpug.PRNG
    ( Pattern(Pattern)
    , Index(Index)
    , PermutationLength(PermutationLength, unPermutationLength)
    , randFloat
    , permuteIndexWord32
    )
where

import           Data.Bits                      ( shiftR
                                                , xor
                                                , (.&.)
                                                , (.|.)
                                                )
import           Data.Word                      ( Word32 )

-- | Pattern in a pseudo-random number generator.
--
-- Each pattern indexes a new sequence of pseudo-random output.
newtype Pattern = Pattern { unPattern :: Word32 } deriving (Show)

-- | Index into a pattern.
newtype Index = Index { unIndex :: Word32 } deriving (Show)

-- | Length of a permutation.
newtype PermutationLength =
  PermutationLength { unPermutationLength :: Word32 } deriving (Show)

-- | Pseudo-random 'Float' from the specified index in a particular pattern.
randFloat :: Pattern -> Index -> Float
randFloat p i = randFloat' (unIndex i) (unPattern p)
{-# INLINABLE randFloat #-}

-- | Pseudo-random permutation indices.
permuteIndexWord32 :: Pattern -> PermutationLength -> Index -> Word32
permuteIndexWord32 p l i =
    permute' (unIndex i) (unPermutationLength l) (unPattern p)
{-# INLINABLE permuteIndexWord32 #-}

-- | Pseudo-random 'Float'.
--
-- This is a Haskell transcription of @randfloat@ from Kensler (2013).
randFloat'
    :: Word32  -- ^ Index.
    -> Word32  -- ^ Pattern.
    -> Float
randFloat' i p =
    let xorSelfShift :: Word32 -> Int -> Word32
        xorSelfShift x n = x `xor` (x `shiftR` n)

        coef :: Float
        coef = 1.0 / 4294967808.0

        i1, i2, i3, i4, i5, i6, i7, i8, i9, i10 :: Word32
        i1  = i `xor` p
        i2  = i1 `xorSelfShift` 17
        i3  = i2 `xorSelfShift` 10
        i4  = i3 * 0xb36534e5
        i5  = i4 `xorSelfShift` 12
        i6  = i5 `xorSelfShift` 21
        i7  = i6 * 0x93fc4795
        i8  = i7 `xor` 0xdf6e307f
        i9  = i8 `xorSelfShift` 17
        i10 = i9 * (1 .|. (p `shiftR` 18))
    in  fromIntegral i10 * coef
{-# INLINE randFloat' #-}

-- | Pseudo-random permutation index.
--
-- This is a Haskell transcription of @permute@ from Kensler (2013).
permute'
    :: Word32  -- ^ Index.
    -> Word32  -- ^ Length of the permutation.
    -> Word32  -- ^ Pattern.
    -> Word32
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
        w  = w5

        go :: Word32 -> Word32
        go i' =
                let i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10 :: Word32
                    i11, i12, i13, i14, i15, i16, i17 :: Word32
                    i0  = i' `xor` p
                    i1  = i0 * 0xe170893d
                    i2  = i1 `xor` (p `shiftR` 16)
                    i3  = i2 `xor` ((i2 .&. w) `shiftR` 4)
                    i4  = i3 `xor` (p `shiftR` 8)
                    i5  = i4 * 0x0929eb3f
                    i6  = i5 `xor` (p `shiftR` 23)
                    i7  = i6 `xor` ((i6 .&. w) `shiftR` 1)
                    i8  = i7 * (1 .|. (p `shiftR` 27))
                    i9  = i8 * 0x6935fa69
                    i10 = i9 `xor` ((i9 .&. w) `shiftR` 11)
                    i11 = i10 * 0x74dcb303
                    i12 = i11 `xor` ((i11 .&. w) `shiftR` 2)
                    i13 = i12 * 0x9e501cc3
                    i14 = i13 `xor` ((i13 .&. w) `shiftR` 2)
                    i15 = i14 * 0xc860a3df
                    i16 = i15 .&. w
                    i17 = i16 `xor` (i16 `shiftR` 5)
                in  if (i17 < l) then i17 else go i17
    in  (go i + p) `mod` l
{-# INLINE permute' #-}
