{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Jitterpug.Geom
    ( V2(V2, v2x, v2y)
    , Size(Size, sizeWidth, sizeHeight)
    )
where

import qualified Data.Vector.Generic           as G
                                                ( Vector
                                                , basicLength
                                                , basicUnsafeFreeze
                                                , basicUnsafeIndexM
                                                , basicUnsafeSlice
                                                , basicUnsafeThaw
                                                )
import qualified Data.Vector.Generic.Mutable   as M
                                                ( MVector
                                                , basicInitialize
                                                , basicLength
                                                , basicOverlaps
                                                , basicUnsafeNew
                                                , basicUnsafeRead
                                                , basicUnsafeSlice
                                                , basicUnsafeWrite
                                                )
import qualified Data.Vector.Unboxed.Base      as U
                                                ( MVector
                                                , Unbox
                                                , Vector
                                                )

data V2 a
  = V2
    { v2x :: a
    , v2y :: a
    }
  deriving (Eq, Show)

data instance U.Vector (V2 a) = V_V2 {-# UNPACK #-} !Int !(U.Vector a)
data instance U.MVector s (V2 a) = MV_V2 {-# UNPACK #-} !Int !(U.MVector s a)
instance U.Unbox a => U.Unbox (V2 a)

instance U.Unbox a => M.MVector U.MVector (V2 a) where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength (MV_V2 n _) = n
    basicUnsafeSlice m n (MV_V2 _ v) =
        MV_V2 n (M.basicUnsafeSlice (2 * m) (2 * n) v)
    basicOverlaps (MV_V2 _ v) (MV_V2 _ u) = M.basicOverlaps v u
    basicUnsafeNew n = fmap (MV_V2 n) (M.basicUnsafeNew (2 * n))
    basicUnsafeRead (MV_V2 _ v) i = do
        let o = 2 * i
        x <- M.basicUnsafeRead v o
        y <- M.basicUnsafeRead v (o + 1)
        return (V2 x y)
    basicUnsafeWrite (MV_V2 _ v) i (V2 x y) = do
        let o = 2 * i
        M.basicUnsafeWrite v o x
        M.basicUnsafeWrite v (o + 1) y
    basicInitialize (MV_V2 _ v) = M.basicInitialize v

instance U.Unbox a => G.Vector U.Vector (V2 a) where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MV_V2 n v) = fmap (V_V2 n) (G.basicUnsafeFreeze v)
    basicUnsafeThaw (V_V2 n v) = fmap (MV_V2 n) (G.basicUnsafeThaw v)
    basicLength (V_V2 n _) = n
    basicUnsafeSlice m n (V_V2 _ v) =
        V_V2 n (G.basicUnsafeSlice (2 * m) (2 * n) v)
    basicUnsafeIndexM (V_V2 _ v) i = do
        let o = 2 * i
        x <- G.basicUnsafeIndexM v o
        y <- G.basicUnsafeIndexM v (o + 1)
        return (V2 x y)

data Size a
  = Size
    { sizeWidth  :: a
    , sizeHeight :: a
    }
