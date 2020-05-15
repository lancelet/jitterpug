{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Jitterpug.SampleGen.Doc
  ( renderUniform,
    renderStratified,
  )
where

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
    translateX,
    translateY,
  )
import qualified Diagrams (V2 (V2))
import Diagrams.Backend.SVG as SVG (B, renderSVG)
import Jitterpug.Doc.Grid
  ( DiaScalar,
    GridGeom (GridGeom),
    GridStyle,
    defaultGridStyle,
    grid,
    gridGeomNCellsX,
    gridGeomNCellsY,
    gridGeomNSubCellsX,
    gridGeomNSubCellsY,
  )
import Jitterpug.Image (Imc (Imc))
import qualified Jitterpug.Image as Image
import qualified Jitterpug.PRNG as PRNG
import Jitterpug.Raster (Pxc (Pxc))
import Jitterpug.SampleGen
  ( Aspect,
    SampleCount,
    SampleGen,
  )
import qualified Jitterpug.SampleGen as SampleGen
import Jitterpug.UV (UV)

renderUniform :: FilePath -> IO ()
renderUniform filepath = renderSampleGen filepath SampleGen.uniform

renderStratified :: FilePath -> IO ()
renderStratified filepath =
  renderSampleGen filepath (SampleGen.stratified (SampleGen.mkJitter 0.7))

renderSampleGen :: FilePath -> SampleGen UV -> IO ()
renderSampleGen filepath sguv =
  let geom :: SampleGridGeom Double
      geom = SampleGridGeom 4 4 4 3 0.03
      --
      style :: SampleGridStyle Double B
      style = defaultSampleGridStyle
      --
      size :: SizeSpec Diagrams.V2 Double
      size = mkSizeSpec (Diagrams.V2 (Just 320) Nothing)
      --
      sg :: Pxc -> SampleGen Imc
      sg = Image.imageSampleGen sguv
      --
      diagram :: Diagram B
      diagram = frame 0.02 (renderSamples geom style sg)
   in SVG.renderSVG filepath size diagram

data SampleGridGeom n
  = SampleGridGeom
      { nSamplesX :: Int,
        nSamplesY :: Int,
        nPixelsX :: Int,
        nPixelsY :: Int,
        sampleRadius :: n
      }

data SampleGridStyle n b
  = SampleGridStyle
      { gridStyle :: GridStyle n b,
        sampleStyle :: Diagram b -> Diagram b
      }

defaultSampleGridStyle :: DiaScalar n b => SampleGridStyle n b
defaultSampleGridStyle =
  SampleGridStyle
    { gridStyle = defaultGridStyle,
      sampleStyle = lwO 0 # fc (sRGB24 0x00 0x7E 0x9C)
    }

renderSamples ::
  forall b n.
  DiaScalar n b =>
  SampleGridGeom n ->
  SampleGridStyle n b ->
  (Pxc -> SampleGen Imc) ->
  Diagram b
renderSamples geom style sg = sampleDots <> dgrid
  where
    dgrid :: Diagram b
    dgrid = grid gridGeom (gridStyle style)
    --
    gridGeom :: GridGeom
    gridGeom = GridGeom
      { gridGeomNCellsX = nPixelsX geom,
        gridGeomNCellsY = nPixelsY geom,
        gridGeomNSubCellsX = nSamplesX geom,
        gridGeomNSubCellsY = nSamplesY geom
      }
    --
    n' :: SampleCount
    n' = fromIntegral (nSamplesX geom * nSamplesY geom)
    --
    aspect :: Aspect
    aspect = fromIntegral (nSamplesX geom) / fromIntegral (nSamplesY geom)
    --
    pxcs :: [Pxc]
    pxcs =
      [ Pxc i j
        | j <- [0 .. fromIntegral (nPixelsY geom) - 1],
          i <- [0 .. fromIntegral (nPixelsX geom) - 1]
      ]
    --
    samplesForPxc :: Pxc -> [Imc]
    samplesForPxc pxc =
      snd $
        PRNG.runPRN
          (PRNG.kensler 0)
          (SampleGen.samples (SampleGen.sampleGen (sg pxc) aspect n'))
    --
    samples :: [Imc]
    samples = concat $ samplesForPxc <$> pxcs
    --
    sampleDots :: Diagram b
    sampleDots = mconcat $ sampleDot <$> samples
    --
    sampleDot :: Imc -> Diagram b
    sampleDot (Imc x y) =
      circle (sampleRadius geom)
        # translateX (realToFrac x)
        # translateY (realToFrac y)
        # sampleStyle style
