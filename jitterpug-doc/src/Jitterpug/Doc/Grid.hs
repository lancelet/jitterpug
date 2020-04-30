{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Jitterpug.Doc.Grid where

import Data.Colour.SRGB (sRGB24)
import Data.List.Extra (mconcatMap)
import Data.Typeable (Typeable)
import Diagrams
  ( (#),
    Diagram,
    LineCap (LineCapButt),
    LineJoin (LineJoinRound),
    N,
    Path,
    Renderable,
    V,
    V2,
    fromVertices,
    lc,
    lineCap,
    lineJoin,
    lwO,
    opacity,
    p2,
    rect,
    translateX,
    translateY,
  )

-- | Type constraint indicating that @n@ is the numerical field for diagram
--   @b@.
type DiaScalar n b =
  (V b ~ V2, N b ~ n, Typeable n, RealFloat n, Renderable (Path V2 n) b)

-- | Geometric parameters of a grid.
data GridGeom
  = GridGeom
      { gridGeomNCellsX :: Int,
        gridGeomNCellsY :: Int,
        gridGeomNSubCellsX :: Int,
        gridGeomNSubCellsY :: Int
      }

-- | Style parameters of a grid.
data GridStyle n b
  = GridStyle
      { gridStyleBorder :: Diagram b -> Diagram b,
        gridStyleMajorLine :: Diagram b -> Diagram b,
        gridStyleMinorLine :: Diagram b -> Diagram b
      }

-- | Default style for a grid.
defaultGridStyle :: DiaScalar n b => GridStyle n b
defaultGridStyle =
  GridStyle
    { gridStyleBorder =
        lwO 1.6
          # lc (sRGB24 0x00 0x00 0x00)
          # lineJoin LineJoinRound,
      gridStyleMajorLine =
        lwO 1.0
          # lc (sRGB24 0x00 0x00 0x00)
          # lineCap LineCapButt,
      gridStyleMinorLine =
        lwO 0.6
          # lc (sRGB24 0x00 0x00 0x00)
          # opacity 0.2
          # lineCap LineCapButt
    }

-- | Create a rendering of a grid with major and minor lines.
--
-- The grid extends from the origin with a single unit for each major grid
-- cell along the positive x and y axes.
grid :: forall b n. DiaScalar n b => GridGeom -> GridStyle n b -> Diagram b
grid geom style =
  border
    <> majorVertLines
    <> majorHorizLines
    <> minorVertLines
    <> minorHorizLines
  where
    fWidth, fHeight :: n
    fWidth = fromIntegral (gridGeomNCellsX geom)
    fHeight = fromIntegral (gridGeomNCellsY geom)
    border :: Diagram b
    border =
      rect fWidth fHeight
        # translateX (fWidth / 2)
        # translateY (fHeight / 2)
        # gridStyleBorder style
    majorVertLines :: Diagram b
    majorVertLines =
      mconcatMap
        fromVertices
        [ [p2 (x, 0), p2 (x, fHeight)]
          | x <- fromIntegral <$> [1 .. (gridGeomNCellsX geom - 1)]
        ]
        # gridStyleMajorLine style
    majorHorizLines :: Diagram b
    majorHorizLines =
      mconcatMap
        fromVertices
        [ [p2 (0, y), p2 (fWidth, y)]
          | y <- fromIntegral <$> [1 .. (gridGeomNCellsY geom - 1)]
        ]
        # gridStyleMajorLine style
    nMinorVertLines, nMinorHorizLines :: Int
    nMinorVertLines = gridGeomNCellsX geom * gridGeomNSubCellsX geom
    nMinorHorizLines = gridGeomNCellsY geom * gridGeomNSubCellsY geom
    fSubCellsX, fSubCellsY :: n
    fSubCellsX = fromIntegral (gridGeomNSubCellsX geom)
    fSubCellsY = fromIntegral (gridGeomNSubCellsY geom)
    minorVertLines :: Diagram b
    minorVertLines =
      mconcatMap
        fromVertices
        [ [p2 (x, 0), p2 (x, fHeight)]
          | i <- [1 .. nMinorVertLines - 1],
            i `mod` (gridGeomNSubCellsX geom) /= 0,
            let x = fromIntegral i / fSubCellsX
        ]
        # gridStyleMinorLine style
    minorHorizLines :: Diagram b
    minorHorizLines =
      mconcatMap
        fromVertices
        [ [p2 (0, y), p2 (fWidth, y)]
          | j <- [1 .. nMinorHorizLines - 1],
            j `mod` (gridGeomNSubCellsY geom) /= 0,
            let y = fromIntegral j / fSubCellsY
        ]
        # gridStyleMinorLine style
