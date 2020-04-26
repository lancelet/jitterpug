{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Jitterpug.Doc.CMJPixel where

import Data.Colour.SRGB (sRGB24)
import Diagrams
  ( (#),
    Diagram,
    SizeSpec,
    circle,
    fc,
    frame,
    lwO,
    mkSizeSpec,
    scaleX,
    scaleY,
    translateX,
    translateY,
  )
import qualified Diagrams as Diagrams
  ( V2 (V2),
  )
import qualified Diagrams.Backend.SVG as SVG
  ( B,
    renderSVG,
  )
import Jitterpug.CMJ
  ( AspectRatio,
    Jittering,
    NSamples,
    cmj,
  )
import Jitterpug.Doc.Grid
  ( DiaScalar,
    GridGeom (GridGeom),
    GridStyle,
  )
import qualified Jitterpug.Doc.Grid as Grid
import Jitterpug.Geom (V2)
import qualified Jitterpug.Geom as Geom
import Jitterpug.PRNG (Pattern)

data CMJPixelDiagramGeom n
  = CMJPixelDiagramGeom
      { cmjPixelDiagramGeomNSamplesX :: Int,
        cmjPixelDiagramGeomNSamplesY :: Int,
        cmjPixelDiagramSampleRadius :: n
      }

newtype SampleStyle n b
  = SampleStyle {unSampleStyle :: Diagram b -> Diagram b}

defaultSampleStyle :: DiaScalar n b => SampleStyle n b
defaultSampleStyle = SampleStyle $ lwO 0 # fc (sRGB24 0x00 0x7E 0x9C)

render5x5Pixel :: FilePath -> IO ()
render5x5Pixel filePath =
  let geom :: CMJPixelDiagramGeom Double
      geom = CMJPixelDiagramGeom 5 5 0.01
      gridStyle :: GridStyle Double SVG.B
      gridStyle = Grid.defaultGridStyle
      sampleStyle :: SampleStyle Double SVG.B
      sampleStyle = defaultSampleStyle
      size :: SizeSpec Diagrams.V2 Double
      size = mkSizeSpec (Diagrams.V2 (Just 320) Nothing)
      diagram :: Diagram SVG.B
      diagram = frame 0.02 (cmjPixelDiagram geom gridStyle sampleStyle 1 0.5)
   in SVG.renderSVG filePath size diagram

render5x3Pixel :: FilePath -> IO ()
render5x3Pixel filePath =
  let geom :: CMJPixelDiagramGeom Double
      geom = CMJPixelDiagramGeom 5 3 0.01
      gridStyle :: GridStyle Double SVG.B
      gridStyle = Grid.defaultGridStyle
      sampleStyle :: SampleStyle Double SVG.B
      sampleStyle = defaultSampleStyle
      size :: SizeSpec Diagrams.V2 Double
      size = mkSizeSpec (Diagrams.V2 (Just 320) Nothing)
      diagram :: Diagram SVG.B
      diagram = frame 0.02 (cmjPixelDiagram geom gridStyle sampleStyle 1 0.5)
   in SVG.renderSVG filePath size diagram

cmjPixelDiagram ::
  forall b n.
  DiaScalar n b =>
  CMJPixelDiagramGeom n ->
  GridStyle n b ->
  SampleStyle n b ->
  Pattern ->
  Jittering ->
  Diagram b
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
    nSamples = fromIntegral (nxi * nyi)
    aspect :: AspectRatio
    aspect = fromIntegral nxi / fromIntegral nyi
    sampleCircles :: Diagram b
    sampleCircles =
      mconcat
        [ circle (cmjPixelDiagramSampleRadius geom)
            # translateX (realToFrac (Geom.v2x p))
            # translateY (realToFrac (Geom.v2y p))
          | p <- ps
        ]
        # unSampleStyle sampleStyle
    ps :: [V2 Float]
    ps =
      [ cmj jittering nSamples pat aspect i
        | i <- fromIntegral <$> [0 .. (nxi * nyi) - 1]
      ]
    gridGeom :: GridGeom
    gridGeom = GridGeom nxi nyi nyi nxi
    gridDia :: Diagram b
    gridDia =
      Grid.grid gridGeom gridStyle # scaleX (1 / nxf) # scaleY (1 / nyf)
