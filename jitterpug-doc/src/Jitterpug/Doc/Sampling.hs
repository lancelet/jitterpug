{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Jitterpug.Doc.Sampling where

import           Data.Colour                    ( Colour
                                                , blend
                                                )
import           Data.Colour.SRGB               ( sRGB24 )
import           Data.List.Extra                ( mconcatMap )
import           Data.Massiv.Array              ( Ix2(Ix2)
                                                , Sz(Sz)
                                                )
import qualified Data.Massiv.Array             as Array
import           Diagrams                       ( Diagram
                                                , SizeSpec
                                                , circle
                                                , direction
                                                , fc
                                                , frame
                                                , lwO
                                                , mkSizeSpec
                                                , rect
                                                , translateX
                                                , translateY
                                                , turn
                                                , wedge
                                                , (#)
                                                , (@@)
                                                )
import qualified Diagrams                      as Diagrams
                                                ( V2(V2) )
import qualified Diagrams.Backend.SVG          as SVG
                                                ( B
                                                , renderSVG
                                                )

import qualified Jitterpug.Geom                as Geom
import           Jitterpug.Sample               ( Sample )
import qualified Jitterpug.Sample              as Sample
import qualified Jitterpug.Tile                as Tile

import           Jitterpug.Doc.Grid             ( DiaScalar
                                                , GridStyle
                                                )
import qualified Jitterpug.Doc.Grid            as Grid


renderSamplingImage :: FilePath -> IO ()
renderSamplingImage filePath =
    let style :: Style Double SVG.B
        style = defaultStyle

        size :: SizeSpec Diagrams.V2 Double
        size = mkSizeSpec (Diagrams.V2 (Just 400) Nothing)

        diagram :: Diagram SVG.B
        diagram = frame 0.2 (samplingImageDiagram style)
    in  SVG.renderSVG filePath size diagram

data Style n b
  = Style
    { styleGridStyle    :: GridStyle n b
    , styleArc          :: Diagram b -> Diagram b
    , styleBg           :: Diagram b -> Diagram b
    , styleFilledSample :: Diagram b -> Diagram b
    , styleEmptySample  :: Diagram b -> Diagram b
    , styleSampleRadius :: n
    }

white :: Colour Double
white = sRGB24 0xFF 0xFF 0xFF

defaultStyle :: DiaScalar n b => Style n b
defaultStyle = Style
    { styleGridStyle    = Grid.defaultGridStyle
    , styleBg           = lwO 0 # fc (blend 0.3 (sRGB24 0x00 0x7E 0x9C) white)
    , styleArc          = lwO 0 # fc (blend 0.3 (sRGB24 0xFC 0xAF 0x00) white)
    , styleFilledSample = lwO 0 # fc (sRGB24 0xFC 0xAF 0x00)
    , styleEmptySample  = lwO 0 # fc (sRGB24 0x00 0x7E 0x9C)
    , styleSampleRadius = 0.04
    }

samplingImageDiagram :: forall  b n . DiaScalar n b => Style n b -> Diagram b
samplingImageDiagram style = grid <> sampleCircles <> arcInGrid <> background
  where
    radius :: n
    radius = 6.6

    background :: Diagram b
    background = rect 8 8 # translateX 4 # translateY 4 # styleBg style

    implImage :: Tile.ImplicitImage Float
    implImage = Tile.ImplicitImage
        (\(Geom.V2 x y) ->
            let l2 = realToFrac (x * x + y * y)
                r2 = radius * radius
            in  if l2 < r2 then 1.0 else 0.0
        )

    sampleCircles :: Diagram b
    sampleCircles = mconcatMap sampleToCircle samples

    sampleToCircle :: Sample Float -> Diagram b
    sampleToCircle sample =
        circle (styleSampleRadius style)
            # translateX (realToFrac (Geom.v2x (Sample.samplePos sample)))
            # translateY (realToFrac (Geom.v2y (Sample.samplePos sample)))
            # if Sample.sampleValue sample > 0.5
                  then styleFilledSample style
                  else styleEmptySample style

    samples :: [Sample Float]
    samples = concatMap
        Array.toList
        (Array.toList
            (Tile.unTile (Tile.sampleTile 0 25 42 1 (Sz (Ix2 8 8)) implImage))
        )

    arcInGrid :: Diagram b
    arcInGrid = wedge radius (direction (Diagrams.V2 1 0)) (1 / 4 @@ turn)
        # styleArc style

    grid :: Diagram b
    grid = Grid.grid (Grid.GridGeom 8 8 5 5) (styleGridStyle style)
