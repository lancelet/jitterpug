{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jitterpug.SampleGen.Types
  ( -- ** Types
    Aspect (Aspect, unAspect),
    Jitter (UseJitter, NoJitter),
    JitterCoeff (unJitterCoeff),
    SampleCount (SampleCount, unSampleCount),
    SampleGen (SampleGen, sampleGen),
    SizedSampleGen (SizedSampleGen, sampleCount, samples),

    -- ** Smart constructors
    mkJitter,
  )
where

import Data.Word (Word16)
import Jitterpug.PRNG (PRN)

-- | Aspect ratio of sample generation.
--
-- This is the (approximate) number of horizontal samples divided by the
-- number of vertical samples.
newtype Aspect = Aspect {unAspect :: Float}
  deriving (Show, Eq, Ord, Num, Real, Fractional, Floating)

-- | Amount of jitter per sample.
data Jitter
  = -- | Use some specified amount of jitter.
    UseJitter JitterCoeff
  | -- | Do not use jitter.
    NoJitter
  deriving (Show, Eq)

-- | Fractional amount of jitter; in the range @[0.0, 1.0]@
newtype JitterCoeff = JitterCoeff {unJitterCoeff :: Float}
  deriving (Show, Eq)

-- | Create a Jitter value, clamped to the range @[0.0, 1.0]@
--
-- If this function is passed a value of @0.0@ then it uses the 'NoJitter'
-- constructor; otherwise it uses the 'UseJitter' constructor.
mkJitter :: Float -> Jitter
mkJitter amount
  | coeff == JitterCoeff 0 = NoJitter
  | otherwise = UseJitter coeff
  where
    coeff :: JitterCoeff
    coeff = mkJitterCoeff amount
{-# INLINE mkJitter #-}

-- | Create a jitter coefficient, clamped to the range @[0.0, 1.0]@.
mkJitterCoeff :: Float -> JitterCoeff
mkJitterCoeff = JitterCoeff . clamp 0.0 1.0
{-# INLINE mkJitterCoeff #-}

-- | Number of samples to generate.
newtype SampleCount = SampleCount {unSampleCount :: Word16}
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral, Bounded)

-- | Generator of samples.
newtype SampleGen a
  = SampleGen
      { -- | Produces a sample generator with a known size.
        sampleGen :: Aspect -> SampleCount -> SizedSampleGen a
      }

instance Functor SampleGen where
  fmap f sg = SampleGen $ \aspect n -> f <$> sampleGen sg aspect n

-- | Sample generator that is fixed to a particular number of samples.
data SizedSampleGen a
  = SizedSampleGen
      { -- | Number of samples per pixel (final).
        sampleCount :: SampleCount,
        -- | Pseudo-random generator for the samples.
        samples :: PRN [a]
      }

instance Functor SizedSampleGen where
  fmap f ssg = SizedSampleGen (sampleCount ssg) (fmap f <$> samples ssg)

-- | Clamp a value between a minimum and maximum value.
clamp ::
  (Ord a) =>
  -- | Minimum allowed value.
  a ->
  -- | Maximum allowed value.
  a ->
  -- | Input value.
  a ->
  -- | Output value.
  a
clamp cmin cmax q
  | q < cmin = cmin
  | q > cmax = cmax
  | otherwise = q
{-# INLINE clamp #-}
