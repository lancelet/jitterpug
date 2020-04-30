{-# OPTIONS_HADDOCK hide #-}

module Jitterpug.PRNG.Kensler
  ( -- * Functions
    kensler,
  )
where

import Data.Bits ((.&.), (.|.), shiftR, xor)
import Data.Word (Word32)
import Jitterpug.PRNG.Types
  ( Index (Index),
    NSamples,
    PRNG (PRNG),
    Pattern,
    unIndex,
    unNSamples,
    unPattern,
  )

-- | 'PRNG' implementation based on Kensler (2013).
--
-- This 'PRNG' contains implementations of the random number generators from
-- the following paper:
--
--   * [Kensler, A](https://graphics.pixar.com/library/MultiJitteredSampling/)
--     (2013) Correlated Multi-Jittered Sampling.
--     Pixar Technical Memo 13-01.
kensler :: PRNG
kensler = PRNG randFloatKensler randPermuteKensler
{-# INLINEABLE kensler #-}

randWord32Kensler ::
  Pattern ->
  Index ->
  Word32
randWord32Kensler pat idx =
  let -- computation steps
      i1, i2, i3, i4, i5, i6, i7, i8 :: Word32
      i1 = xor (unIndex idx) (unPattern pat)
      i2 = xorsr i1 17
      i3 = 0xb36534e5 * xorsr i2 10
      i4 = xorsr i3 12
      i5 = 0x93fc4795 * xorsr i4 21
      i6 = xor i5 0xdf6e307f
      i7 = xorsr i6 17
      i8 = i7 * (1 .|. shiftR (unPattern pat) 18)
   in i8
{-# INLINEABLE randWord32Kensler #-}

randFloatKensler ::
  Pattern ->
  Index ->
  Float
randFloatKensler pat idx =
  fromIntegral (randWord32Kensler pat idx) / fromIntegral (maxBound :: Word32)
{-# INLINEABLE randFloatKensler #-}

randPermuteKensler ::
  Pattern ->
  Index ->
  NSamples ->
  Index ->
  Index
randPermuteKensler pat idx ns pIdx =
  let w :: Word32
      w = lenShift ns
      --
      p :: Word32
      p = randWord32Kensler pat idx
      --
      go :: Word32 -> Word32
      go i0 =
        let i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12 :: Word32
            i1 = 0xe170893d * xor i0 p
            i2 = xor i1 (shiftR p 16)
            i3 = xor i2 (shiftR (i2 .&. w) 4)
            i4 = 0x0929eb3f * xor i3 (shiftR p 8)
            i5 = xor i4 (shiftR p 23)
            i6 = xor i5 (shiftR (i5 .&. w) 1)
            i7 = 0x6935fa69 * i6 * (1 .|. shiftR p 27)
            i8 = 0x74dcb303 * xor i7 (shiftR (i7 .&. w) 11)
            i9 = 0x9e501cc3 * xor i8 (shiftR (i8 .&. w) 2)
            i10 = 0xc860a3df * xor i9 (shiftR (i9 .&. w) 2)
            i11 = i10 .&. w
            i12 = xor i11 (shiftR i11 5)
         in if i12 >= unNSamples ns
              then go i12
              else i12
   in Index $ (p + go (unIndex pIdx)) `mod` unNSamples ns
{-# INLINEABLE randPermuteKensler #-}

lenShift :: NSamples -> Word32
lenShift ns =
  let w0, w1, w2, w3, w4, w5 :: Word32
      w0 = unNSamples ns - 1
      w1 = w0 .|. shiftR w0 1
      w2 = w1 .|. shiftR w1 2
      w3 = w2 .|. shiftR w2 4
      w4 = w3 .|. shiftR w3 8
      w5 = w4 .|. shiftR w4 16
   in w5
{-# INLINE lenShift #-}

xorsr :: Word32 -> Int -> Word32
xorsr x n = xor x (shiftR x n)
{-# INLINE xorsr #-}
