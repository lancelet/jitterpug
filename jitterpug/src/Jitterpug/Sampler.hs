{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jitterpug.Sampler
  ( SamplePos (SamplePos, unSamplePos),
    Sample (samplePosn, sampleValue),
    SampleGenerator (SampleGenerator, unSampleGenerator),
    samplePositions,
    sampleImage,
  )
where

import Control.DeepSeq
  ( NFData,
    rnf,
  )
import Data.Massiv.Array (Array, Construct, D, Ix3, Sz)
import qualified Data.Massiv.Array as Array
import Jitterpug.Geom (PxRect, Pxc, V2)
import qualified Jitterpug.Geom as Geom
import Jitterpug.ImageFn (ImageFn)
import qualified Jitterpug.ImageFn as ImageFn
import Jitterpug.PRNG (Index, NSamples)

data Sample e
  = Sample
      { samplePosn  :: {-# UNPACK #-} !SamplePos,
        sampleValue :: !e
      }

sample :: SamplePos -> e -> Sample e
sample = Sample

newtype SamplePos = SamplePos {unSamplePos :: V2 Float}

samplePos :: Float -> Float -> SamplePos
samplePos x y = SamplePos (Geom.v2 x y)

samplePosX :: SamplePos -> Float
samplePosX = Geom.v2x . unSamplePos

samplePosY :: SamplePos -> Float
samplePosY = Geom.v2y . unSamplePos

offsetSamplePosForPixel :: Pxc -> SamplePos -> SamplePos
offsetSamplePosForPixel pxc sp =
  samplePos (samplePosX sp + Geom.pxcX pxc) (samplePosY sp + Geom.pxcY pxc)

instance NFData (Sample e) where
  rnf = const ()

newtype SampleGenerator
  = SampleGenerator
      {unSampleGenerator :: NSamples -> Pxc -> Index -> SamplePos}

-- | Generate sample positions for all pixels in an image region.
--
-- Sample positions are generated sequentially.
samplePositions ::
  (Construct r Ix3 SamplePos) =>
  -- | Rectangle of the image in which to create sample positions.
  PxRect ->
  -- | Number of samples per pixel in the region.
  NSamples ->
  -- | Sample generator.
  SampleGenerator ->
  -- | Massiv 'Array' containing sample positions.
  Array r Ix3 SamplePos
samplePositions rect spp gen = Array.makeArray Array.Seq sz f
  where
    sz :: Sz Ix3
    sz =
      Array.Sz
        ( Array.Ix3
            (Geom.pxRectWidth rect)
            (Geom.pxRectHeight rect)
            (fromIntegral spp)
        )
    f :: Ix3 -> SamplePos
    f (Array.Ix3 i j s) =
      let pxc :: Pxc
          pxc = Geom.pxc i j
       in offsetSamplePosForPixel pxc $
            unSampleGenerator gen spp pxc (fromIntegral s)
{-# INLINEABLE samplePositions #-}

sampleImage ::
  forall c.
  -- | Rectangle in which to sample the image.
  PxRect ->
  -- | Number of samples per pixel.
  NSamples ->
  -- | Sample generator.
  SampleGenerator ->
  -- | Image function; returning a color @c@ from given coordinates.
  ImageFn c ->
  -- | Massiv 'Array' containing samples for all pixels.
  Array D Ix3 (Sample c)
sampleImage rect spp gen imageFn =
  let positions :: Array D Ix3 SamplePos
      positions = samplePositions rect spp gen
      sp :: SamplePos -> Sample c
      sp p = sample p (ImageFn.unImageFn imageFn (samplePosX p) (samplePosY p))
   in Array.map sp positions
{-# INLINEABLE sampleImage #-}
