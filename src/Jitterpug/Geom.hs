module Jitterpug.Geom
    ( V2(V2, v2x, v2y)
    , Size(Size, sizeWidth, sizeHeight)
    )
where

data V2 a
  = V2
    { v2x :: a
    , v2y :: a
    }
  deriving (Eq, Show)

data Size a
  = Size
    { sizeWidth  :: a
    , sizeHeight :: a
    }
