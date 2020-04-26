{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jitterpug.Imager where

import Data.Foldable (foldl')
import Data.Massiv.Array (Array, Ix2, Ix3)
import qualified Data.Massiv.Array as Array
import Jitterpug.Filter (Filter)
import Jitterpug.Geom (PxRect, Pxc, Size)
import qualified Jitterpug.Geom as Geom
import Jitterpug.ImageFn (ImageFn)
import Jitterpug.PRNG (NSamples)
import Jitterpug.Sampler (Sample, SampleGenerator)
import qualified Jitterpug.Sampler as Sampler

newtype FilterSize = FilterSize {unFilterSize :: Size Float}

class CVec c where
  cvecZero :: c
  cvecScale :: Float -> c -> c
  cvecAdd :: c -> c -> c

-- | Detailed filter parameters.
data FilterParams
  = FilterParams
      { filterParamsSize :: FilterSize,
        filterParamsPxSize :: Size Int,
        filterParamsBorderX :: Int,
        filterParamsBorderY :: Int
      }

-- | Compute detailed filter parameters from the filter size.
filterParams :: FilterSize -> FilterParams
filterParams fltSize = FilterParams fltSize pxSize borderX borderY
  where
    fltW, fltH :: Float
    fltW = Geom.sizeWidth . unFilterSize $ fltSize
    fltH = Geom.sizeHeight . unFilterSize $ fltSize
    pxSize :: Size Int
    pxSize =
      Geom.size
        (1 + 2 * ceiling ((fltW - 1) / 2))
        (1 + 2 * ceiling ((fltH - 1) / 2))
    borderX, borderY :: Int
    borderX = (Geom.sizeWidth pxSize - 1) `quot` 2
    borderY = (Geom.sizeHeight pxSize - 1) `quot` 2

-- | Sample and filter the image for a single tile.
imageTile ::
  forall r c.
  (Array.Mutable r Ix3 (Sample c)) =>
  FilterSize ->
  Filter ->
  SampleGenerator ->
  NSamples ->
  PxRect ->
  ImageFn c ->
  Array r Ix2 c
imageTile fltSize flt gen spp pxRect imgFn =
  let -- detailed filter parameters
      params :: FilterParams
      params = filterParams fltSize
      -- sampling rectangle; adjusted for filter size
      sampleRect :: PxRect
      sampleRect =
        Geom.pxRectSizeDelta
          (filterParamsBorderX params)
          (filterParamsBorderY params)
          pxRect
      -- array of samples
      samples :: Array r Ix3 (Sample c)
      samples = Array.compute $ Sampler.sampleImage sampleRect spp gen imgFn
   in filterSamples params flt samples

-- | Filter the samples for a single tile.
filterSamples ::
  FilterParams ->
  Filter ->
  Array r Ix3 (Sample c) ->
  Array r Ix2 c
filterSamples = undefined

-- | Filter the samples to obtain a single pixel color.
filterPixel ::
  forall r c.
  (Array.Manifest r Ix3 (Sample c), CVec c) =>
  FilterParams ->
  Filter ->
  Array r Ix3 (Sample c) ->
  Pxc ->
  c
filterPixel params flt samples pxCoord =
  let pxRect :: PxRect
      pxRect =
        Geom.pxRectSizeDelta
          (filterParamsBorderX params)
          (filterParamsBorderY params)
          (Geom.rectForPixel pxCoord)
      accum :: (Float, c) -> Sample c -> (Float, c)
      accum = undefined
      initVal :: (Float, c)
      initVal = (0, cvecZero)
      totalWeight :: Float
      totalColor :: c
      (totalWeight, totalColor) = foldSampleRect pxRect accum samples initVal
   in cvecScale (1 / totalWeight) totalColor

foldSampleRect ::
  forall r a c.
  (Array.Manifest r Ix3 (Sample c)) =>
  PxRect ->
  (a -> Sample c -> a) ->
  Array r Ix3 (Sample c) ->
  a ->
  a
foldSampleRect rect f samples initVal =
  foldl' f initVal (Array.index' samples <$> coords)
  where
    nSamples :: Int
    Array.Sz (Array.Ix3 _ _ nSamples) = Array.size samples
    coords :: [Ix3]
    coords =
      [ Array.Ix3 (Geom.pxcX pxc) (Geom.pxcY pxc) k
        | pxc <- Geom.pxRectPixels rect,
          k <- [0 .. nSamples - 1]
      ]
