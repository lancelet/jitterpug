module Jitterpug.Image
  ( module Jitterpug.Image.Types,
    imageSampleGen,
  )
where

import Jitterpug.Image.Types
import qualified Jitterpug.PRNG as PRNG
import Jitterpug.Raster (Pxc)
import qualified Jitterpug.Raster as Raster
import Jitterpug.SampleGen (SampleGen)
import qualified Jitterpug.SampleGen as SampleGen
import Jitterpug.UV (UV)
import qualified Jitterpug.UV as UV

-- | Convert a UV coordinate (on the unit square) to an image coordinate, by
--   offsetting it using a pixel coordinate.
uvToImc :: Pxc -> UV -> Imc
uvToImc pxc uv =
  Imc
    (fromIntegral (Raster.x pxc) + UV.u uv)
    (fromIntegral (Raster.y pxc) + UV.v uv)

-- | Transform a UV sample generator to an image coordinate sample generator.
--
-- This function pre-composes a 'PRN' jump based on the pixel coordinates, and
-- converts UV coordinates to pixel coordinates, offsetting them by the pixel
-- coordinates.
imageSampleGen :: (Functor f) => SampleGen f UV -> Pxc -> SampleGen f Imc
imageSampleGen sg pxc =
  uvToImc pxc <$> SampleGen.preComposePRN (PRNG.jump pxc) sg
