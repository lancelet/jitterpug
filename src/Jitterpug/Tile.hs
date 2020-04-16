module Jitterpug.Tile where

import           Data.Massiv.Array              ( Array
                                                , D(D)
                                                , Sz2
                                                , U
                                                , makeArrayR
                                                , Comp(Seq)
                                                , Ix1
                                                , Ix2
                                                )

import           Jitterpug.Geom                 ( V2 )
import           Jitterpug.PRNG                 ( Pattern )
import           Jitterpug.SamplePositions      ( AspectRatio
                                                , Jittering
                                                , NSamples
                                                )

newtype Tile r e = Tile { unTile :: Array r Ix2 e }

tileSamplePositions
    :: Jittering
    -> NSamples
    -> Pattern
    -> AspectRatio
    -> Sz2
    -> Tile D (Array U Ix1 (V2 Float))
tileSamplePositions jittering nSamples pat aspect size = Tile $ tileArray
  where
    tileArray :: Array D Ix2 (Array U Ix1 (V2 Float))
    tileArray = makeArrayR D Seq size mkElement

    mkElement :: Ix2 -> Array U Ix1 (V2 Float)
    mkElement = undefined
