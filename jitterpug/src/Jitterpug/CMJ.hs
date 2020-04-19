{-|
Module      : Jitterpug.CMJ
Description :

Correlated multi-jittered sampling position generation.

The 'cmj' function in this module is based on code listing 6 in:

  - Kensler A (2013) Correlated Multi-Jittered Sampling.
    Pixar Technical Memo 13-01.
-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Jitterpug.CMJ
    ( Jittering(unJittering)
    , NSamples(NSamples, unNSamples)
    , AspectRatio(AspectRatio, unAspectRatio)
    , mkJittering
    , cmj
    )
where

import           Jitterpug.Geom                 ( V2(V2) )
import           Jitterpug.PRNG                 ( Index
                                                , Pattern
                                                , NSamples
                                                )
import qualified Jitterpug.PRNG                as PRNG

-- | Amount of jittering.
--
-- To create a 'Jittering' value, use the 'mkJittering' function.
newtype Jittering = Jittering { unJittering :: Float }
  deriving (Eq, Show, Num, Fractional)

-- | Create a 'Jittering' value.
--
-- Values are clamped to the range [0.0, 1.0]. 0.0 indicates no jittering,
-- while 1.0 indicates maximum jittering.
mkJittering :: Float -> Jittering
mkJittering c | c > 1     = Jittering 1
              | c < 0     = Jittering 0
              | otherwise = Jittering c

-- | Aspect ratio of samples within a unit square.
--
-- This is calculated as the width (x) divided by the height (y).
newtype AspectRatio = AspectRatio { unAspectRatio :: Float }
  deriving (Eq, Show, Num, Fractional)

-- | Correlated Multi-Jittered Sampling.
--
-- Generate the coordinates of a sample position in a unit square, using the
-- correlated multi-jittered sampling technique.
--
-- This is a Haskell transcription of code listing 6 from Kensler (2013),
-- with the addition of modulated jittering.
cmj
    :: Jittering    -- ^ Jittering multiplication factor.
    -> NSamples     -- ^ Total number of samples.
    -> Pattern      -- ^ Pattern index.
    -> AspectRatio  -- ^ Aspect ratio for the samples within the square.
    -> Index        -- ^ Index of the sample to generate.
    -> V2 Float     -- ^ Generated sample coordinates.
cmj (Jittering jit) nTotalSamples pat (AspectRatio a) sampleIndex =
    let
        -- find the number of samples in the x-direction (n) and the
        -- y-direction (m) based on the total number of samples and the aspect
        -- ratio
        m, n :: NSamples
        m = floor $ sqrt (fromIntegral nTotalSamples * a)
        n = (nTotalSamples + m - 1) `quot` m

        -- shuffle the sample output order
        s :: Index
        s = PRNG.permuteIndex (0x51633e2d * pat) nTotalSamples sampleIndex

        -- find s % m (smodm) and s / m (sdivm)
        smodm, sdivm :: Index
        (sdivm, smodm) = s `quotRem` (fromIntegral m)

        -- the sample positions as indexes in x and y
        sx, sy :: Index
        sx = PRNG.permuteIndex (0x68bc21eb * pat) m smodm
        sy = PRNG.permuteIndex (0x02e5be93 * pat) n sdivm

        -- the jitter in x and y; modulated by the Jittering amount
        jx, jy, jx', jy' :: Float
        jx' = PRNG.randFloat (0x967a889b * pat) s
        jy' = PRNG.randFloat (0x368cc8b7 * pat) s
        jx  = 0.5 + (jit * (jx' - 0.5))
        jy  = 0.5 + (jit * (jy' - 0.5))

        -- convert everything to floating-point
        sf, sxf, syf, nf, mf, nnf :: Float
        sf  = fromIntegral s
        sxf = fromIntegral sx
        syf = fromIntegral sy
        nf  = fromIntegral n
        mf  = fromIntegral m
        nnf = fromIntegral nTotalSamples

        -- final x and y coordinates
        x, y :: Float
        x = (sxf + (syf + jx) / nf) / mf
        y = (sf + jy) / nnf
    in
        V2 x y
{-# INLINABLE cmj #-}
