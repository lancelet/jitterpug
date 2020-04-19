{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Jitterpug.Tile where

import           Control.DeepSeq                ( NFData )
import           Data.Massiv.Array              ( Array
                                                , Comp(Seq)
                                                , D(D)
                                                , Ix1
                                                , Ix2(Ix2)
                                                , Load
                                                , Manifest
                                                , N(N)
                                                , Sz(Sz)
                                                , Sz1
                                                , Sz2
                                                , computeAs
                                                , imap
                                                , index'
                                                , makeArrayR
                                                , makeVectorR
                                                , size
                                                , toManifest
                                                )
import qualified Data.Massiv.Array             as Array
                                                ( map )

import           Jitterpug.CMJ                  ( AspectRatio
                                                , Jittering
                                                , NSamples
                                                )
import qualified Jitterpug.CMJ                 as CMJ
import           Jitterpug.Filter               ( Filter
                                                , Filterable
                                                , PartialFilterResult
                                                )
import qualified Jitterpug.Filter              as Filter
import           Jitterpug.Geom                 ( V2(V2) )
import qualified Jitterpug.Geom                as Geom
import           Jitterpug.PRNG                 ( Pattern )
import qualified Jitterpug.PRNG                as PRNG
import           Jitterpug.Sample               ( Sample )
import qualified Jitterpug.Sample              as Sample

newtype Tile r e = Tile { unTile :: Array r Ix2 e }

tileSz :: (Load r Ix2 e) => Tile r e -> Sz Ix2
tileSz = size . unTile

tilePx :: Manifest r Ix2 e => Tile r e -> Ix2 -> e
tilePx tile = index' (unTile tile)

tileIxValid :: (Load r Ix2 e) => Tile r e -> Ix2 -> Bool
tileIxValid tile (Ix2 i j) = i >= 0 && i <= maxi && j >= 0 && j <= maxj
  where
    w, h :: Int
    Sz (Ix2 w h) = tileSz tile

    maxi, maxj :: Int
    maxi = w - 1
    maxj = h - 1

newtype ImplicitImage e = ImplicitImage { unImplicitImage :: V2 Float -> e }

sampleImplicitImage :: ImplicitImage e -> V2 Float -> Sample e
sampleImplicitImage img p = Sample.Sample p (unImplicitImage img p)

translateImplicitImage :: V2 Int -> ImplicitImage e -> ImplicitImage e
translateImplicitImage t image =
    ImplicitImage $ unImplicitImage image . Geom.addV2 t'
  where
    t' :: V2 Float
    t' = fromIntegral <$> t

sampleAndFilterTile
    :: (NFData e, Filterable e)
    => Jittering
    -> NSamples
    -> Pattern
    -> AspectRatio
    -> Filter
    -> Sz2
    -> ImplicitImage e
    -> Tile N e
sampleAndFilterTile jittering nSamples pat aspect flt sz image =
    filterTile flt (sampleTile jittering nSamples pat aspect sz image)

sampleTile
    :: forall e
     . Jittering
    -> NSamples
    -> Pattern
    -> AspectRatio
    -> Sz2
    -> ImplicitImage e
    -> Tile D (Array D Ix1 (Sample e))
sampleTile jittering nSamples pat aspect sz image = Tile
    (samplePx <$> unTile sampleps)
  where
    samplePx :: Array D Ix1 (V2 Float) -> Array D Ix1 (Sample e)
    samplePx = Array.map (sampleImplicitImage image)

    sampleps :: Tile D (Array D Ix1 (V2 Float))
    sampleps = tileSamplePositions jittering nSamples pat aspect sz

filterTile
    :: forall e
     . (NFData e, Filterable e)
    => Filter
    -> Tile D (Array D Ix1 (Sample e))
    -> Tile N e
filterTile flt tile = Tile $ computeAs N $ imap mapFn (unTile evald)
  where
    evald :: Tile N (Array N Ix1 (Sample e))
    evald = Tile $ computeAs N $ Array.map (computeAs N) (unTile tile)

    mapFn :: Ix2 -> Array N Ix1 (Sample e) -> e
    mapFn ix _ = filterPx flt evald ix

filterPx
    :: (Filterable e) => Filter -> Tile N (Array N Ix1 (Sample e)) -> Ix2 -> e
filterPx flt tile ix@(Ix2 i j) =
    Filter.finalize $ mconcat $ filterSubSamples flt ix . tilePx tile <$> ixes
  where
    ixes :: [Ix2]
    ixes =
        filter (tileIxValid tile)
            $ [ Ix2 i' j'
              | j' <- [j - fhi .. j + fhi]
              , i' <- [i - fwi .. i + fwi]
              ]

    fwi, fhi :: Int
    fwi = ceiling (Geom.sizeWidth (Filter.filterSize flt) / 2)
    fhi = ceiling (Geom.sizeHeight (Filter.filterSize flt) / 2)

filterSubSamples
    :: Filterable e
    => Filter
    -> Ix2
    -> Array N Ix1 (Sample e)
    -> PartialFilterResult e
filterSubSamples flt (Ix2 i j) samples = foldMap
    (Filter.filterSample flt origin)
    (toManifest samples)
  where
    origin :: V2 Float
    origin = V2 (fromIntegral i + 0.5) (fromIntegral j + 0.5)

tileSamplePositions
    :: Jittering
    -> NSamples
    -> Pattern
    -> AspectRatio
    -> Sz2
    -> Tile D (Array D Ix1 (V2 Float))
tileSamplePositions jittering nSamples pat aspect sz = Tile tileArray
  where
    tileArray :: Array D Ix2 (Array D Ix1 (V2 Float))
    tileArray = makeArrayR D Seq sz mkElement

    mkElement :: Ix2 -> Array D Ix1 (V2 Float)
    mkElement ix2 =
        let ixV2 :: V2 Int
            ixV2 = ix2ToV2 ix2

            pat' :: Pattern
            pat' = PRNG.offsetForPixel ixV2 pat

            pixelOrigin :: V2 Float
            pixelOrigin = fmap fromIntegral ixV2

            makeSample :: Ix1 -> V2 Float
            makeSample sampleIdx = Geom.addV2 pixelOrigin curSamplePos
                  where
                    curSamplePos :: V2 Float
                    curSamplePos = CMJ.cmj
                        jittering
                        nSamples
                        pat'
                        aspect
                        (PRNG.Index (fromIntegral sampleIdx))
        in  makeVectorR D Seq (nSamplesToSz1 nSamples) makeSample

nSamplesToSz1 :: NSamples -> Sz1
nSamplesToSz1 = Sz . fromIntegral . CMJ.unNSamples

ix2ToV2 :: Ix2 -> V2 Int
ix2ToV2 (Ix2 i j) = V2 i j
