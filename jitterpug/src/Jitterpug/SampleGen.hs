-- |
-- Module      : Jitterpug.SampleGen
-- Description : Sample generation.
--
-- This module contains functions for generating samples inside a single pixel.
--
-- == Guide
--
-- To generate samples, we start with a 'SampleGen' value. In the example
-- here, we will use a 'uniform' 'SampleGen':
--
-- >>> import qualified Jitterpug.PRNG as PRNG
-- >>> smGen = uniform PRNG.kensler (mkJitter 1.0) :: SampleGen
--
-- We then configure the 'SampleGen' by indicating the 'Aspect' ratio of the
-- generated samples and the number of 'SamplesPerPixel' we wish to generate:
--
-- >>> sizedSmGen = sampleGen smGen 1 3 :: SizedSampleGen
--
-- Depending upon the sample generator that is used, the desired number of
-- samples per pixel may not exactly match the number that the generator can
-- provide. We can inspect the actual number of samples from the
-- 'SizedSampleGen' that we obtained above:
--
-- >>> spp sizedSmGen
-- SamplesPerPixel {unSamplesPerPixel = 4}
--
-- In this case, we can see that 4 samples will be generated instead of the 3
-- that we originally requested. This is because the uniform sampler must
-- produce a fixed number of samples in the x and y directions across the
-- pixel, so with an aspect ratio of 1, it generates 2x2 = 4 samples.
--
-- Finally, to generate the actual samples for a particular pixel, we use the
-- 'samplesForPixel' function:
--
-- >>> import qualified Jitterpug.Image as Image
-- >>> samples = samplesForPixel sizedSmGen (Image.Pxc 0 0)
-- >>> length samples
-- 4
-- >>> head samples
-- Smc {x = 0.43638897, y = 0.43638897}
--
-- All the samples for a particular pixel will lie inside the boundary
-- rectangle of that pixel.
module Jitterpug.SampleGen
  ( module Jitterpug.SampleGen.Types,
    module Jitterpug.SampleGen.Uniform,
  )
where

import Jitterpug.SampleGen.Types
import Jitterpug.SampleGen.Uniform
