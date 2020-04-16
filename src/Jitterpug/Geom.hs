{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Jitterpug.Geom
    ( V2(V2, v2x, v2y)
    , Size(Size, sizeWidth, sizeHeight)
    , addV2
    )
where

import           Data.Vector.Unboxed            ( Unbox )
import           Data.Vector.Unboxed.Deriving   ( derivingUnbox )

data V2 a
  = V2
    { v2x :: !a
    , v2y :: !a
    }
  deriving (Eq, Show)

instance Functor V2 where
    {-# INLINE fmap #-}
    fmap f (V2 x y) = V2 (f x) (f y)

derivingUnbox "V2"
  [t| forall a. (Unbox a) => V2 a -> (a, a) |]
  [| \(V2 x y) -> (x, y) |]
  [| \(x, y) -> V2 x y |]

addV2 :: Num a => V2 a -> V2 a -> V2 a
addV2 (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)
{-# INLINE addV2 #-}

data Size a
  = Size
    { sizeWidth  :: a
    , sizeHeight :: a
    }
