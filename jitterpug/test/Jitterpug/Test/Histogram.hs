{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Jitterpug.Test.Histogram
  ( fromList,
    fromListI,
    fromListIBound,
    isUniform,
    Histogram,
    Distribution,
    NBuckets (NBuckets, unNBuckets),
    EpsilonFrac (EpsilonFrac, unEpsilonFrac),
  )
where

import Control.Monad.Extra (anyM)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word (Word16, Word32, Word64)

type Histogram n = Bucketed n Word32

type Distribution n = Bucketed n Float

data Bucketed n c
  = Bucketed
      { buckets :: V.Vector c,
        minX :: n,
        maxX :: n
      }

newtype NBuckets = NBuckets {unNBuckets :: Word16}
  deriving (Eq, Ord, Num, Real, Enum, Integral)

newtype EpsilonFrac = EpsilonFrac {unEpsilonFrac :: Float}
  deriving (Eq, Ord, Num, Fractional)

isUniform :: EpsilonFrac -> Histogram n -> Bool
isUniform eps hist =
  V.all (>= allowedMin) $ buckets (histogramToDistribution hist)
  where
    n :: Int
    n = V.length . buckets $ hist
    --
    allowedMin :: Float
    allowedMin = (1.0 / fromIntegral n) - abs (unEpsilonFrac eps)

histogramToDistribution :: Histogram n -> Distribution n
histogramToDistribution hist =
  Bucketed (V.map realToFrac dblvec) (minX hist) (maxX hist)
  where
    dblvec :: V.Vector Double
    dblvec = V.map (\x -> fromIntegral x / n) $ buckets hist
    --
    n :: Double
    n =
      fromIntegral
        $ V.sum
        $ V.map (fromIntegral @Word32 @Word64)
        $ buckets hist

fromListIBound ::
  forall n.
  (Integral n, Bounded n) =>
  NBuckets ->
  [n] ->
  Maybe (Histogram Double)
fromListIBound nBuckets = fromListI nBuckets minBound maxBound

fromListI ::
  forall n.
  (Integral n) =>
  NBuckets ->
  n ->
  n ->
  [n] ->
  Maybe (Histogram Double)
fromListI nBuckets minX' maxX' xs = fromList nBuckets minX'' maxX'' xs'
  where
    minX'', maxX'' :: Double
    minX'' = fromIntegral minX'
    maxX'' = fromIntegral maxX'
    --
    xs' :: [Double]
    xs' = fromIntegral <$> xs

fromList ::
  forall n.
  (RealFrac n) =>
  NBuckets ->
  n ->
  n ->
  [n] ->
  Maybe (Histogram n)
fromList nBuckets minX' maxX' xs =
  if not (fst buckets')
    then Just (Bucketed (snd buckets') minX' maxX')
    else Nothing
  where
    bucketPlacer :: n -> Maybe Int
    bucketPlacer = bucketIndex nBuckets minX' maxX'
    --
    buckets' :: (Bool, V.Vector Word32)
    buckets' = runST $ do
      vec <- MV.replicate (fromIntegral nBuckets) (0 :: Word32)
      failed <- anyM (addSample bucketPlacer vec) xs
      final <- V.unsafeFreeze vec
      pure (failed, final)

bucketIndex :: forall n. (RealFrac n) => NBuckets -> n -> n -> n -> Maybe Int
bucketIndex nBuckets minX' maxX' x
  | x >= minX' && x <= maxX' = Just (floor (x / dx))
  | otherwise = Nothing
  where
    dx :: n
    dx = (maxX' - minX') / fromIntegral nBuckets

addSample ::
  (PrimMonad m, s ~ PrimState m) =>
  (n -> Maybe Int) ->
  MV.MVector s Word32 ->
  n ->
  m Bool
addSample bucketPlacer vec x =
  case bucketPlacer x of
    Nothing -> pure True
    Just i -> MV.modify vec (+ 1) i >> pure False
