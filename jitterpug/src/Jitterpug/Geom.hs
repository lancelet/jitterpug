{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Jitterpug.Geom
    ( V2(V2, v2x, v2y)
    , Size(Size, sizeWidth, sizeHeight)
    , Rect(Rect, rectX, rectY, rectWidth, rectHeight)
    , addV2
    , subV2
    , ptInRect
    , rectCentered
    )
where

data V2 a
  = V2
    { v2x :: !a
    , v2y :: !a
    }
  deriving (Eq, Show)

instance Functor V2 where
    {-# INLINE fmap #-}
    fmap f (V2 x y) = V2 (f x) (f y)

addV2 :: Num a => V2 a -> V2 a -> V2 a
addV2 (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
{-# INLINE addV2 #-}

subV2 :: Num a => V2 a -> V2 a -> V2 a
subV2 (V2 x1 y1) (V2 x2 y2) = V2 (x1 - x2) (y1 - y2)
{-# INLINE subV2 #-}

data Size a
  = Size
    { sizeWidth  :: !a
    , sizeHeight :: !a
    }

data Rect a
  = Rect
    { rectX      :: !a
    , rectY      :: !a
    , rectWidth  :: !a
    , rectHeight :: !a
    }

ptInRect :: forall  a . (Ord a, Num a) => Rect a -> V2 a -> Bool
ptInRect (Rect x y w h) (V2 px py) =
    let xmin, xmax, ymin, ymax :: a
        xmin = x
        xmax = x + w
        ymin = y
        ymax = y + h
    in  px >= xmin && px <= xmax && py >= ymin && py <= ymax
{-# INLINE ptInRect #-}

rectCentered :: forall  a . (Fractional a) => V2 a -> Size a -> Rect a
rectCentered (V2 x y) (Size w h) = Rect (x - w2) (y - h2) w h
  where
    w2, h2 :: a
    w2 = w / 2
    h2 = h / 2
{-# INLINE rectCentered #-}
