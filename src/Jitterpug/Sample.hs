{-|
Module      : Jitterpug.Sample
Description :

Sample generation and image sampling.

The 'cmj' function in this module is a Haskell transcriptions of code listing 6
in:

  - Kensler A (2013) Correlated Multi-Jittered Sampling.
    Pixar Technical Memo 13-01.
-}
{-# LANGUAGE ScopedTypeVariables #-}
module Jitterpug.Sample
    ( SampleOps(SampleOps, sampleOpsScale, sampleOpsAdd)
    , Jittering(unJittering)
    , NSamples(NSamples, unNSamples)
    , AspectRatio(AspectRatio, unAspectRatio)
    , mkJittering
    , cmj
    )
where

import           Data.Word                      ( Word32 )

import           Jitterpug.Geom                 ( V2(V2) )
import           Jitterpug.PRNG                 ( Index
                                                , Pattern
                                                )
import qualified Jitterpug.PRNG                as PRNG

-- | A vector space for samples.
data SampleOps s v
  = SampleOps
    { sampleOpsScale :: s -> v -> v
    , sampleOpsAdd   :: v -> v -> v
    }

-- | Amount of jittering.
newtype Jittering = Jittering { unJittering :: Float }

-- | Create a 'Jittering' value.
--
-- Values are clamped to the range [0.0, 1.0].
mkJittering :: Float -> Jittering
mkJittering c | c > 1     = Jittering 1
              | c < 0     = Jittering 0
              | otherwise = Jittering c

-- | Total number of samples.
newtype NSamples = NSamples { unNSamples :: Word32 }

-- | Aspect ratio of samples within a unit square.
newtype AspectRatio = AspectRatio { unAspectRatio :: Float }

-- | Correlated Multi-Jittered Sampling.
--
-- Generate a sample in a unit square, using the correlated multi-jittered
-- sampling technique.
--
-- This is a Haskell transcription of code listing 6 from Kensler (2013).
cmj
    :: Jittering     -- ^ Jittering multiplication factor.
    -> NSamples      -- ^ Total number of samples.
    -> Pattern       -- ^ Pattern index (should be unique per pixel per frame).
    -> AspectRatio   -- ^ Aspect ratio for the samples within the square.
    -> Index         -- ^ Index of the sample to generate [0 .. (NSamples - 1)].
    -> V2 Float      -- ^ Generated sample.
cmj (Jittering jit) (NSamples nn) pat (AspectRatio a) sampleIndex =
    let
        m, n :: Word32
        m = round $ sqrt ((fromIntegral nn) * a)
        n = (nn + m - 1) `div` m

        s, smodm, sdivm, sx, sy :: Word32
        s = PRNG.permuteIndexWord32 (PRNG.patternMul 0x51633e2d pat)
                                    (PRNG.PermutationLength nn)
                                    sampleIndex
        (smodm, sdivm) = s `quotRem` m
        sx = PRNG.permuteIndexWord32 (PRNG.patternMul 0x68bc21eb pat)
                                     (PRNG.PermutationLength m)
                                     (PRNG.Index smodm)
        sy = PRNG.permuteIndexWord32 (PRNG.patternMul 0x02e5be93 pat)
                                     (PRNG.PermutationLength n)
                                     (PRNG.Index sdivm)

        jx, jy :: Float
        jx = jit
            * PRNG.randFloat (PRNG.patternMul 0x967a889b pat) (PRNG.Index s)
        jy =
            jit * PRNG.randFloat (PRNG.patternMul 0x368cc8b7 pat) (PRNG.Index s)

        x, y :: Float
        x = (fromIntegral sx + (fromIntegral sy + jx) / fromIntegral n)
            / fromIntegral m
        y = (fromIntegral s + jy) / fromIntegral nn
    in
        V2 x y
{-# INLINABLE cmj #-}
