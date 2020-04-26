module Jitterpug.Doc.ZonePlate where

{-

import Data.Massiv.Array
  ( Array,
    D,
    Ix2 (Ix2),
    N,
    Sz (Sz),
  )
import qualified Data.Massiv.Array as Array
import Data.Massiv.Array.IO (writeImage)
import Data.Word (Word16)
import Graphics.ColorModel (RGB)
import Graphics.Pixel (Pixel (PixelRGB))
import qualified Jitterpug.CMJ as CMJ
import qualified Jitterpug.Filter as Filter
import Jitterpug.Geom (V2)
import qualified Jitterpug.Geom as Geom
import qualified Jitterpug.PRNG as PRNG
import Jitterpug.Tile
  ( ImplicitImage,
    Tile,
  )
import qualified Jitterpug.Tile as Tile

test :: IO ()
test = writeImage "test-zoneplate.png" cImage -- (Tile.unTile testImage)
  where
    cImage :: Array D Ix2 (Pixel RGB Word16)
    cImage = Array.map f (Tile.unTile testImage)
    f :: Float -> Pixel RGB Word16
    f x = PixelRGB w w w
      where
        w :: Word16
        w = floor (65535 * x)

testImage :: Tile N Float
testImage =
  Tile.sampleAndFilterTile
    (CMJ.mkJittering 1)
    (CMJ.NSamples 100)
    (PRNG.Pattern 1)
    (CMJ.AspectRatio 1.0)
    (Filter.Filter (Geom.Size 1 1) Filter.boxFilter)
    (Sz (Ix2 256 256))
    ii
  where
    ii :: ImplicitImage Float
    ii = Tile.ImplicitImage $ zpi
      where
        zpi :: V2 Float -> Float
        zpi (Geom.V2 x y) = zonePlate 2 (Geom.V2 x' y')
          where
            scale :: Float
            scale = 1 / 40
            x', y' :: Float
            x' = x * scale
            y' = y * scale

-- | Binary zone plate image.
zonePlate :: Float -> V2 Float -> Float
zonePlate power (Geom.V2 x y) = fromIntegral (1 - qi `mod` 2)
  where
    qi :: Int
    qi = floor q
    q :: Float
    q = r2 ** power
    r2 :: Float
    r2 = x * x + y * y

-}
