{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Jitterpug.Tile where

import           Data.Massiv.Array              ( Array
                                                , Comp(Seq)
                                                , D(D)
                                                , Ix1
                                                , Ix2(Ix2)
                                                , N(N)
                                                , Sz(Sz)
                                                , Sz1
                                                , Sz2
                                                , U(U)
                                                , computeAs
                                                , makeArrayR
                                                , makeVectorR
                                                )
import qualified Data.Massiv.Array             as Array
                                                ( map )
import           Data.Vector.Unboxed            ( Unbox )
import           Data.Vector.Unboxed.Deriving   ( derivingUnbox )

import           Jitterpug.Geom                 ( V2(V2) )
import qualified Jitterpug.Geom                as Geom
import           Jitterpug.PRNG                 ( Pattern )
import qualified Jitterpug.PRNG                as PRNG
import           Jitterpug.SamplePositions      ( AspectRatio
                                                , Jittering
                                                , NSamples
                                                )
import qualified Jitterpug.SamplePositions     as SamplePositions

newtype Tile r e = Tile { unTile :: Array r Ix2 e }

data Sample e
  = Sample
    { samplePos   :: {-# UNPACK #-} !(V2 Float)
    , sampleValue :: !e
    }

newtype ImplicitImage e = ImplicitImage { unImplicitImage :: V2 Float -> e }

sampleImplicitImage :: ImplicitImage e -> V2 Float -> Sample e
sampleImplicitImage img p = Sample p (unImplicitImage img p)

translateImplicitImage :: V2 Int -> ImplicitImage e -> ImplicitImage e
translateImplicitImage t image =
    ImplicitImage $ unImplicitImage image . Geom.addV2 t'
  where
    t' :: V2 Float
    t' = fromIntegral <$> t

derivingUnbox "Sample"
  [t| forall e. (Unbox e) => Sample e -> (V2 Float, e) |]
  [| \(Sample p v) -> (p, v) |]
  [| \(p, v) -> Sample p v |]

sampleTile
    :: forall e
     . (Unbox e)
    => Jittering
    -> NSamples
    -> Pattern
    -> AspectRatio
    -> Sz2
    -> ImplicitImage e
    -> Tile N (Array U Ix1 (Sample e))
sampleTile jittering nSamples pat aspect size image = (Tile . computeAs N)
    (samplePx <$> unTile sampleps)
  where
    samplePx :: Array U Ix1 (V2 Float) -> Array U Ix1 (Sample e)
    samplePx ps = computeAs U $ Array.map (sampleImplicitImage image) ps

    sampleps :: Tile D (Array U Ix1 (V2 Float))
    sampleps = tileSamplePositions jittering nSamples pat aspect size

tileSamplePositions
    :: Jittering
    -> NSamples
    -> Pattern
    -> AspectRatio
    -> Sz2
    -> Tile D (Array U Ix1 (V2 Float))
tileSamplePositions jittering nSamples pat aspect size = Tile tileArray
  where
    tileArray :: Array D Ix2 (Array U Ix1 (V2 Float))
    tileArray = makeArrayR D Seq size mkElement

    mkElement :: Ix2 -> Array U Ix1 (V2 Float)
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
                    curSamplePos = SamplePositions.cmj
                        jittering
                        nSamples
                        pat'
                        aspect
                        (PRNG.Index (fromIntegral sampleIdx))
        in  makeVectorR U Seq (nSamplesToSz1 nSamples) makeSample

nSamplesToSz1 :: NSamples -> Sz1
nSamplesToSz1 = Sz . fromIntegral . SamplePositions.unNSamples

ix2ToV2 :: Ix2 -> V2 Int
ix2ToV2 (Ix2 i j) = V2 i j
