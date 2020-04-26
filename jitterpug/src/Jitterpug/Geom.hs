{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Jitterpug.Geom
  ( V2 (v2x, v2y),
    Size (sizeWidth, sizeHeight),
    Rect (rectX, rectY, rectWidth, rectHeight),
    Pxc,
    PxRect,
    v2,
    pxc,
    pxcX,
    pxcY,
    addV2,
    subV2,
    ptInRect,
    rectCentered,
    rectSizeDelta,
    rectForPixel,
    size,
    rect,
    pxRectSizeDelta,
    pxRectPixels,
    pxRect,
    pxRectWidth,
    pxRectHeight,
  )
where

data V2 a
  = V2
      { v2x :: !a,
        v2y :: !a
      }
  deriving (Eq, Show)

v2 :: a -> a -> V2 a
v2 = V2
{-# INLINE v2 #-}

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
      { sizeWidth :: !a,
        sizeHeight :: !a
      }

size :: a -> a -> Size a
size = Size
{-# INLINE size #-}

data Rect a
  = Rect
      { rectX :: !a,
        rectY :: !a,
        rectWidth :: !a,
        rectHeight :: !a
      }
  deriving (Eq, Show)

rect :: a -> a -> a -> a -> Rect a
rect = Rect
{-# INLINE rect #-}

rectEnumX :: (Enum a) => Rect a -> [a]
rectEnumX r = toEnum <$> [xmin .. xmax]
  where
    xmin, xmax :: Int
    xmin = fromEnum . rectX $ r
    xmax = xmin + (fromEnum . rectWidth $ r)
{-# INLINE rectEnumX #-}

rectEnumY :: (Enum a) => Rect a -> [a]
rectEnumY r = toEnum <$> [ymin .. ymax]
  where
    ymin, ymax :: Int
    ymin = fromEnum . rectY $ r
    ymax = ymin + (fromEnum . rectHeight $ r)
{-# INLINE rectEnumY #-}

rectEnumV :: (Enum a) => Rect a -> [V2 a]
rectEnumV r = [V2 i j | j <- rectEnumY r, i <- rectEnumX r]
{-# INLINE rectEnumV #-}

ptInRect :: forall a. (Ord a, Num a) => Rect a -> V2 a -> Bool
ptInRect (Rect x y w h) (V2 px py) =
  let xmin, xmax, ymin, ymax :: a
      xmin = x
      xmax = x + w
      ymin = y
      ymax = y + h
   in px >= xmin && px <= xmax && py >= ymin && py <= ymax
{-# INLINE ptInRect #-}

rectCentered :: forall a. (Fractional a) => V2 a -> Size a -> Rect a
rectCentered (V2 x y) (Size w h) = Rect (x - w2) (y - h2) w h
  where
    w2, h2 :: a
    w2 = w / 2
    h2 = h / 2
{-# INLINE rectCentered #-}

rectSizeDelta :: forall a. (Num a) => a -> a -> Rect a -> Rect a
rectSizeDelta dw dh (Rect x y w h) = Rect x' y' w' h'
  where
    x', y', w', h' :: a
    x' = x - dw
    y' = y - dh
    w' = w + 2 * dw
    h' = h + 2 * dh
{-# INLINE rectSizeDelta #-}

-- | Coordinates of a pixel.
newtype Pxc = Pxc {unPxc :: V2 Int}
  deriving (Eq, Show)

pxc :: Int -> Int -> Pxc
pxc i j = Pxc (V2 i j)
{-# INLINE pxc #-}

pxcX :: Num a => Pxc -> a
pxcX = fromIntegral . v2x . unPxc
{-# INLINE pxcX #-}

pxcY :: Num a => Pxc -> a
pxcY = fromIntegral . v2y . unPxc
{-# INLINE pxcY #-}

-- | Rectangle within an image, using pixel coordinates.
newtype PxRect = PxRect {unPxRect :: Rect Int}
  deriving (Eq, Show)

pxRect :: Int -> Int -> Int -> Int -> PxRect
pxRect x y w h = PxRect (rect x y w h)
{-# INLINE pxRect #-}

pxRectWidth :: PxRect -> Int
pxRectWidth = rectWidth . unPxRect
{-# INLINE pxRectWidth #-}

pxRectHeight :: PxRect -> Int
pxRectHeight = rectHeight . unPxRect
{-# INLINE pxRectHeight #-}

-- | Create a single pixel rectangle.
rectForPixel :: Pxc -> PxRect
rectForPixel (Pxc (V2 i j)) =
  PxRect $
    Rect
      { rectX = i,
        rectY = j,
        rectWidth = 1,
        rectHeight = 1
      }
{-# INLINE rectForPixel #-}

pxRectSizeDelta :: Int -> Int -> PxRect -> PxRect
pxRectSizeDelta dw dh = PxRect . rectSizeDelta dw dh . unPxRect
{-# INLINE pxRectSizeDelta #-}

pxRectPixels :: PxRect -> [Pxc]
pxRectPixels = fmap Pxc . rectEnumV . unPxRect
{-# INLINE pxRectPixels #-}
