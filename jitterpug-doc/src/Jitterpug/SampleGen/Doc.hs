{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Jitterpug.SampleGen.Doc
  ( renderUniformNoJitter,
    renderUniformJitter,
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
import Jitterpug.Image (Pxc (Pxc))
import Jitterpug.PRNG (kensler)
import Jitterpug.SampleGen
  ( Aspect,
    Jitter,
    SampleGen,
    SamplesPerPixel,
    Smc (Smc),
    mkJitter,
    sampleGen,
    samplesForPixel,
    uniform,
  )

renderUniformNoJitter :: FilePath -> IO ()
renderUniformNoJitter filepath = renderUniform filepath (mkJitter 0)

renderUniformJitter :: FilePath -> IO ()
renderUniformJitter filepath = renderUniform filepath (mkJitter 0.8)

renderUniform :: FilePath -> Jitter -> IO ()
renderUniform filepath jitter =
  let geom :: SampleGridGeom Double
      geom = SampleGridGeom 4 4 4 3 0.03
      --
      style :: SampleGridStyle Double B
      style = defaultSampleGridStyle
      --
      size :: SizeSpec Diagrams.V2 Double
      size = mkSizeSpec (Diagrams.V2 (Just 320) Nothing)
      --
      sg :: SampleGen
      sg = uniform kensler jitter
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
  SampleGen ->
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
    spp' :: SamplesPerPixel
    spp' = fromIntegral (nSamplesX geom * nSamplesY geom)
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
    samples :: [Smc]
    samples = concat $ samplesForPixel (sampleGen sg aspect spp') <$> pxcs
    --
    sampleDots :: Diagram b
    sampleDots = mconcat $ sampleDot <$> samples
    --
    sampleDot :: Smc -> Diagram b
    sampleDot (Smc x y) =
      circle (sampleRadius geom)
        # translateX (realToFrac x)
        # translateY (realToFrac y)
        # sampleStyle style
