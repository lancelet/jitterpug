{-# LANGUAGE ScopedTypeVariables #-}
module Jitterpug.Test.Util
    ( approxEq
    , approxEqList
    , approxEqListV2
    )
where

import           Control.Applicative            ( liftA2 )
import           Data.Foldable                  ( toList )
import           Data.Maybe                     ( fromMaybe )

import           Jitterpug.Geom                 ( V2
                                                , v2x
                                                , v2y
                                                )

-- | Check if two floats are equal up to some @eps@ value.
approxEq :: forall  n . (Ord n, Num n) => n -> n -> n -> Bool
approxEq eps x y = x + eps > y && x - eps < y

-- | Check if all elements of two @Float@ lists are equal up to some @eps@
--   value.
approxEqList
    :: forall f n
     . (Foldable f, Functor f, Ord n, Num n)
    => n
    -> f n
    -> f n
    -> Bool
approxEqList eps xs' ys' =
    let xs, ys :: [Maybe n]
        xs = toList (fmap Just xs') <> repeat Nothing
        ys = toList (fmap Just ys')

        pairEq :: Maybe n -> Maybe n -> Maybe Bool
        pairEq = liftA2 (approxEq eps)
    in  all (fromMaybe False) (zipWith pairEq xs ys)

approxEqListV2
    :: forall f n
     . (Foldable f, Functor f, Ord n, Num n)
    => n
    -> f (V2 n)
    -> f (V2 n)
    -> Bool
approxEqListV2 eps xs ys =
    approxEqList eps (v2x <$> xs) (v2x <$> ys)
        && approxEqList eps (v2y <$> xs) (v2y <$> ys)
