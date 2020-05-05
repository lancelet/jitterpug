{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Jitterpug.SampleGen.Types
  ( -- * Types
    Smc (Smc, x, y),
    Aspect (Aspect, unAspect),
    Jitter (UseJitter, NoJitter),
    JitterCoeff (unJitterCoeff),
    SamplesPerPixel (SamplesPerPixel, unSamplesPerPixel),
    SampleGen (SampleGen, sampleGen),
    SizedSampleGen (SizedSampleGen, spp, samplesForPixel),

    -- * Functions
    mkJitter,
  )
where

import Data.Word (Word16)
import Jitterpug.Image.Types (Pxc)

-- | Sample coordinates.
data Smc
  = Smc
      { x :: {-# UNPACK #-} !Float,
        y :: {-# UNPACK #-} !Float
      }
  deriving (Eq, Show)

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

-- | Number of samples per pixel.
newtype SamplesPerPixel = SamplesPerPixel {unSamplesPerPixel :: Word16}
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral, Bounded)

-- | Generator of sample coordinates for a pixel.
newtype SampleGen
  = SampleGen
      { -- | Produces a sample generator with a known size.
        sampleGen :: Aspect -> SamplesPerPixel -> SizedSampleGen
      }

-- | Sample generator that is fixed to a particular number of samples.
data SizedSampleGen
  = SizedSampleGen
      { -- | Number of samples per pixel (final).
        spp :: SamplesPerPixel,
        -- | Generate the samples for a single pixel.
        samplesForPixel :: Pxc -> [Smc]
      }

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
