{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Jitterpug.SampleGen
-- Description : Sample generation.
--
-- This module contains functions for generating samples.
module Jitterpug.SampleGen
  ( -- * Guide
    -- $guide

    -- * Types
    module Jitterpug.SampleGen.Types,

    -- * Functions

    -- ** Sample generators
    uniform,
    stratified,

    -- ** Utilities
    addPattern,
    mapPRN,
    mapPRNSSG,
    preComposePRN,
    preComposePRNSSG,
  )
where

import Jitterpug.PRNG (PRN, Pattern)
import qualified Jitterpug.PRNG as PRNG
import Jitterpug.SampleGen.Stratified (stratified, uniform)
import Jitterpug.SampleGen.Types

-- | Add a 'Pattern' to each sample output by a generator.
addPattern ::
  forall a.
  SampleGen a ->
  SampleGen (Pattern, a)
addPattern = mapPRN (PRNG.addPattern . pure)

-- | Map a pseudo-random-number effect over all generated samples.
mapPRN ::
  forall a b.
  -- | Effect to map over all values.
  (a -> PRN b) ->
  -- | Initial sample generator.
  SampleGen a ->
  -- | Final sample generator.
  SampleGen b
mapPRN fb ingen =
  SampleGen $ \aspect n -> mapPRNSSG fb (sampleGen ingen aspect n)

-- | Map a pseudo-random-number effect over all generated samples for the
--   'SizedSampleGen'.
mapPRNSSG ::
  forall a b.
  -- | Effect to map over all values.
  (a -> PRN b) ->
  -- | Initial sample generator.
  SizedSampleGen a ->
  -- | Final sample generator.
  SizedSampleGen b
mapPRNSSG fb ingen =
  SizedSampleGen (sampleCount ingen) (samples ingen >>= traverse fb)

-- | Compose a 'PRN' action before the same generation 'PRN'.
preComposePRN :: PRN () -> SampleGen a -> SampleGen a
preComposePRN prnUnit sg =
  SampleGen $ \aspect n -> preComposePRNSSG prnUnit (sampleGen sg aspect n)

-- | Compose a 'PRN' action before the sample generation 'PRN'.
preComposePRNSSG :: PRN () -> SizedSampleGen a -> SizedSampleGen a
preComposePRNSSG prnUnit ssg =
  SizedSampleGen
    (sampleCount ssg)
    (prnUnit >> samples ssg)

-- $guide
--
-- == Generating 'Jitterpug.UV.UV' samples
--
-- As an example, consider the task of generating uniform 'Jitterpug.UV.UV'
-- samples on a unit square. We start with the 'uniform' sample generator:
--
-- >>> import Jitterpug.UV (UV)
-- >>> sg = uniform :: SampleGen [] UV
--
-- Next we have to request the number of samples we want to generate, and their
-- aspect ratio:
--
-- >>> aspect = 1 :: Aspect
-- >>> n = 8 :: SampleCount
-- >>> ssg = sampleGen sg aspect n :: SizedSampleGen [] UV
--
-- The number of samples requested from a sample generator and the actual
-- number that it is able to provide may be different. One purpose of the
-- 'SizedSampleGen' is to resolve the actual number. We can query that number
-- using the 'sampleCount' function:
--
-- >>> sampleCount ssg
-- SampleCount {unSampleCount = 9}
--
-- In this case, we requested 8 samples, but the generator will produce 9,
-- because it has to generate a rectangular number of samples. In general,
-- the number of samples that will actually be produced is greater than or
-- equal to the number requested.
--
-- To generate the samples, call the 'samples' function, and run the supplied
-- 'PRN':
--
-- >>> import qualified Jitterpug.PRNG as PRNG
-- >>> uvs = PRNG.runPRN' (PRNG.kensler 0) (samples ssg)
--
-- The number of samples generated should match the number we expect (9):
--
-- >>> length uvs
-- 9
--
-- And we can inspect a couple of them:
--
-- >>> take 2 uvs
-- [UV {u = 0.16666667, v = 0.16666667},UV {u = 0.5, v = 0.16666667}]
