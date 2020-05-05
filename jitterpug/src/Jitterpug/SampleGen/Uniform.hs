{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK not-home #-}

module Jitterpug.SampleGen.Uniform
  ( uniform,
  )
where

import Data.Word (Word16)
import qualified Jitterpug.Image as Image
import Jitterpug.Image.Types (Pxc)
import Jitterpug.PRNG (Index, PRNG, Pattern)
import qualified Jitterpug.PRNG as PRNG
import Jitterpug.SampleGen.Types
  ( Aspect,
    Jitter (NoJitter, UseJitter),
    SampleGen (SampleGen),
    SamplesPerPixel (SamplesPerPixel),
    SizedSampleGen (SizedSampleGen),
    Smc (Smc),
    unAspect,
    unJitterCoeff,
  )

-- | Uniform sample generator, with jitter.
uniform ::
  -- | Pseudo-random number generator for jitter.
  PRNG ->
  -- | Jitter amount.
  Jitter ->
  -- | Sample generator.
  SampleGen
uniform prng jitter = SampleGen (uniformSized prng jitter)
{-# INLINEABLE uniform #-}

-- | Create a uniform sized sample generator.
uniformSized :: PRNG -> Jitter -> Aspect -> SamplesPerPixel -> SizedSampleGen
uniformSized prng jitter aspect spp =
  SizedSampleGen spp' (samplesForPixel prng jitter counts)
  where
    counts :: SampleCounts
    counts = resolveSampleCounts aspect spp
    spp' :: SamplesPerPixel
    spp' = sampleCountsToSpp counts
{-# INLINEABLE uniformSized #-}

-- | Create all samples for a pixel in the uniform SampleGen.
samplesForPixel :: PRNG -> Jitter -> SampleCounts -> Pxc -> [Smc]
samplesForPixel prng jitter counts pxc =
  sample prng jitter counts pxc
    <$> [(i, j) | j <- [0 .. cy counts - 1], i <- [0 .. cx counts - 1]]
{-# INLINEABLE samplesForPixel #-}

-- | Create an individual sample for the uniform SampleGen.
sample :: PRNG -> Jitter -> SampleCounts -> Pxc -> (Word16, Word16) -> Smc
sample prng jitter counts pxc (i, j) = Smc x y
  where
    dx, dy :: Float
    dx = 1.0 / fromIntegral (cx counts)
    dy = 1.0 / fromIntegral (cy counts)
    --
    x0, y0 :: Float
    x0 = dx * (fromIntegral i + 0.5)
    y0 = dy * (fromIntegral j + 0.5)
    --
    jitx, jity :: Float
    (jitx, jity) =
      case jitter of
        NoJitter -> (0, 0)
        UseJitter coeff ->
          let jx, jy, c :: Float
              (jx, jy) = jitxy prng counts pxc (i, j)
              c = unJitterCoeff coeff
           in (dx * c * (jx - 0.5), dy * c * (jy - 0.5))
    --
    x , y :: Float
    x = x0 + jitx + fromIntegral (Image.x pxc)
    y = y0 + jity + fromIntegral (Image.y pxc)
{-# INLINEABLE sample #-}

-- | Obtain x and y jitter random values from the PRNG.
--
-- The output values are in the range @[0, 1)@.
jitxy :: PRNG -> SampleCounts -> Pxc -> (Word16, Word16) -> (Float, Float)
jitxy prng counts pxc (i, j) = (jx, jy)
  where
    pat :: Pattern
    pat = pxcToPattern pxc
    --
    s :: Index
    s = fromIntegral (i + j * cy counts)
    --
    jx, jy :: Float
    jx = PRNG.randFloat prng (pat * 0x967a889b) s
    jy = PRNG.randFloat prng (pat * 0x368cc8b7) s
{-# INLINE jitxy #-}

-- | Obtain a random pattern for a pixel coordinate.
pxcToPattern :: Pxc -> Pattern
pxcToPattern pxc =
  0x367a889b * fromIntegral (Image.x pxc)
    + 0x768cc8b7 * fromIntegral (Image.y pxc)
{-# INLINE pxcToPattern #-}

-- | Internal class describing the number of samples in the x and y directions
--   of the uniform sampler.
data SampleCounts
  = SampleCounts
      { cx :: {-# UNPACK #-} !Word16,
        cy :: {-# UNPACK #-} !Word16
      }
  deriving (Eq, Show)

-- | Convert the SampleCounts of x and y samples into the total number of
--   samples per pixel.
sampleCountsToSpp :: SampleCounts -> SamplesPerPixel
sampleCountsToSpp sc = SamplesPerPixel (cx sc * cy sc)
{-# INLINE sampleCountsToSpp #-}

-- | Resolve the number of x and y samples that we're going to use in the
--   uniform sampler.
resolveSampleCounts :: Aspect -> SamplesPerPixel -> SampleCounts
resolveSampleCounts aspect spp = SampleCounts nx ny
  where
    nx, ny :: Word16
    nx = ceiling @Float $ sqrt $ (unAspect aspect) * fromIntegral spp
    ny = ceiling @Float $ fromIntegral spp / fromIntegral nx
{-# INLINE resolveSampleCounts #-}
