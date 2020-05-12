{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK not-home #-}

module Jitterpug.SampleGen.Stratified
  ( stratified,
    uniform,
  )
where

import Data.Word (Word16)
import Jitterpug.PRNG (PRN)
import qualified Jitterpug.PRNG as PRNG
import Jitterpug.SampleGen.Types
  ( Aspect,
    Jitter (NoJitter, UseJitter),
    JitterCoeff,
    SampleCount (SampleCount),
    SampleGen (SampleGen),
    SizedSampleGen (SizedSampleGen),
    unAspect,
    unJitterCoeff,
  )
import Jitterpug.UV (UV (UV))

-- | Uniform sample generator.
uniform :: SampleGen [] UV
uniform = stratified NoJitter

-- | Stratified sample generator.
stratified ::
  -- | Jitter amount.
  Jitter ->
  -- | Sample generator.
  SampleGen [] UV
stratified jitter = SampleGen (stratifiedSized jitter)

-- | Create a uniform sized sample generator.
stratifiedSized :: Jitter -> Aspect -> SampleCount -> SizedSampleGen [] UV
stratifiedSized jitter aspect n =
  SizedSampleGen n' (samplesForUnitSquare jitter counts)
  where
    counts :: SampleCountsXY
    counts = resolveSampleCounts aspect n
    n' :: SampleCount
    n' = sampleCountsToSpp counts

-- | Create samples for a unit square.
samplesForUnitSquare ::
  -- | Amount of jitter.
  Jitter ->
  -- | Sample counts in the x and y directions.
  SampleCountsXY ->
  -- | Random-generation action.
  PRN [UV]
samplesForUnitSquare jitter counts =
  sequence $
    sample jitter counts
      <$> [(i, j) | j <- [0 .. cy counts - 1], i <- [0 .. cx counts - 1]]

-- | Create a single sample.
sample ::
  -- | Amount of jitter.
  Jitter ->
  -- | Sample counts in the x and y directions.
  SampleCountsXY ->
  -- | Sample coordinate index.
  (Word16, Word16) ->
  -- | Random-generation action.
  PRN UV
sample jitter counts (i, j) = do
  (jitx, jity) <- case jitter of
    NoJitter -> pure (0, 0)
    UseJitter coeff -> jitxy coeff
  pure $
    UV
      ((0.5 + jitx + fromIntegral i) / fromIntegral (cx counts))
      ((0.5 + jity + fromIntegral j) / fromIntegral (cy counts))

-- | Compute the jitter for a single pixel.
--
-- The jitter returned is in the range @[-0.5, 0.5)@ for the x and y directions.
jitxy ::
  -- | Jitter coefficient (amount of jitter in the range @[0.0, 1.0]@).
  JitterCoeff ->
  -- | Jitter in x and y directions.
  PRN (Float, Float)
jitxy coeff =
  (,) <$> scale zRandFloat <*> scale zRandFloat
  where
    scale :: PRN Float -> PRN Float
    scale = fmap (unJitterCoeff coeff *)

-- | randFloat in the range @[-0.5, 0.5)@.
zRandFloat :: PRN Float
zRandFloat = (\x -> x - 0.5) <$> PRNG.float

-- | Internal class describing the number of samples in the x and y directions
--   of the uniform sampler.
data SampleCountsXY
  = SampleCountsXY
      { cx :: {-# UNPACK #-} !Word16,
        cy :: {-# UNPACK #-} !Word16
      }
  deriving (Eq, Show)

-- | Convert the SampleCounts of x and y samples into the total number of
--   samples per pixel.
sampleCountsToSpp :: SampleCountsXY -> SampleCount
sampleCountsToSpp sc = SampleCount (cx sc * cy sc)

-- | Resolve the number of x and y samples that we're going to use in the
--   uniform sampler.
resolveSampleCounts :: Aspect -> SampleCount -> SampleCountsXY
resolveSampleCounts aspect spp = SampleCountsXY nx ny
  where
    nx, ny :: Word16
    nx = ceiling @Float $ sqrt $ unAspect aspect * fromIntegral spp
    ny = ceiling @Float $ fromIntegral spp / fromIntegral nx
