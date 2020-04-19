{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Jitterpug.Doc.CMJPixel where

import           Data.Colour.SRGB               ( sRGB24 )
import           Diagrams                       ( Diagram
                                                , SizeSpec
                                                , circle
                                                , fc
                                                , frame
                                                , lwO
                                                , mkSizeSpec
                                                , scaleX
                                                , scaleY
                                                , translateX
                                                , translateY
                                                , (#)
                                                )
import qualified Diagrams                      as Diagrams
                                                ( V2(V2) )
import qualified Diagrams.Backend.SVG          as SVG
                                                ( B
                                                , renderSVG
                                                )

import           Jitterpug.CMJ                  ( AspectRatio(AspectRatio)
                                                , Jittering
                                                , NSamples(NSamples)
                                                , cmj
                                                )
import qualified Jitterpug.CMJ                 as CMJ
import           Jitterpug.Doc.Grid             ( DiaScalar
                                                , GridGeom(GridGeom)
                                                , GridStyle
                                                )
import qualified Jitterpug.Doc.Grid            as Grid
import           Jitterpug.Geom                 ( V2(V2) )
import           Jitterpug.PRNG                 ( Index(Index)
                                                , Pattern
                                                )
import qualified Jitterpug.PRNG                as PRNG


data CMJPixelDiagramGeom n
  = CMJPixelDiagramGeom
    { cmjPixelDiagramGeomNSamplesX :: Int
    , cmjPixelDiagramGeomNSamplesY :: Int
    , cmjPixelDiagramSampleRadius  :: n
    }

newtype SampleStyle n b =
  SampleStyle { unSampleStyle :: Diagram b -> Diagram b }

defaultSampleStyle :: DiaScalar n b => SampleStyle n b
defaultSampleStyle = SampleStyle $ lwO 0 # fc (sRGB24 0x00 0x7E 0x9C)

render5x5Pixel :: FilePath -> IO ()
render5x5Pixel filePath =
    let
        geom :: CMJPixelDiagramGeom Double
        geom = CMJPixelDiagramGeom 5 5 0.01

        gridStyle :: GridStyle Double SVG.B
        gridStyle = Grid.defaultGridStyle

        sampleStyle :: SampleStyle Double SVG.B
        sampleStyle = defaultSampleStyle

        size :: SizeSpec Diagrams.V2 Double
        size = mkSizeSpec (Diagrams.V2 (Just 320) Nothing)

        pat :: Pattern
        pat = PRNG.Pattern 1

        jittering :: Jittering
        jittering = CMJ.mkJittering 0.5

        diagram :: Diagram SVG.B
        diagram = frame
            0.02
            (cmjPixelDiagram geom gridStyle sampleStyle pat jittering)
    in
        SVG.renderSVG filePath size diagram

render5x3Pixel :: FilePath -> IO ()
render5x3Pixel filePath =
    let
        geom :: CMJPixelDiagramGeom Double
        geom = CMJPixelDiagramGeom 5 3 0.01

        gridStyle :: GridStyle Double SVG.B
        gridStyle = Grid.defaultGridStyle

        sampleStyle :: SampleStyle Double SVG.B
        sampleStyle = defaultSampleStyle

        size :: SizeSpec Diagrams.V2 Double
        size = mkSizeSpec (Diagrams.V2 (Just 320) Nothing)

        pat :: Pattern
        pat = PRNG.Pattern 1

        jittering :: Jittering
        jittering = CMJ.mkJittering 0.5

        diagram :: Diagram SVG.B
        diagram = frame
            0.02
            (cmjPixelDiagram geom gridStyle sampleStyle pat jittering)
    in
        SVG.renderSVG filePath size diagram

cmjPixelDiagram
    :: forall b n
     . DiaScalar n b
    => CMJPixelDiagramGeom n
    -> GridStyle n b
    -> SampleStyle n b
    -> Pattern
    -> Jittering
    -> Diagram b
cmjPixelDiagram geom gridStyle sampleStyle pat jittering =
    sampleCircles <> gridDia
  where
    nxi, nyi :: Int
    nxi = cmjPixelDiagramGeomNSamplesX geom
    nyi = cmjPixelDiagramGeomNSamplesY geom

    nxf, nyf :: n
    nxf = fromIntegral nxi
    nyf = fromIntegral nyi

    nSamples :: NSamples
    nSamples = NSamples $ fromIntegral (nxi * nyi)

    aspect :: AspectRatio
    aspect = AspectRatio $ fromIntegral nyi / fromIntegral nxi

    sampleCircles :: Diagram b
    sampleCircles =
        mconcat
                [ circle (cmjPixelDiagramSampleRadius geom)
                  # translateX (realToFrac x)
                  # translateY (realToFrac y)
                | V2 x y <- ps
                ]
            # unSampleStyle sampleStyle

    ps :: [V2 Float]
    ps =
        [ cmj jittering nSamples pat aspect (Index i)
        | i <- [0 .. fromIntegral (nxi * nyi) - 1]
        ]

    gridGeom :: GridGeom
    gridGeom = GridGeom nxi nyi nyi nxi

    gridDia :: Diagram b
    gridDia =
        Grid.grid gridGeom gridStyle # scaleX (1 / nxf) # scaleY (1 / nyf)
