{-# LANGUAGE ScopedTypeVariables #-}

module Jitterpug.Filter where

import Jitterpug.Geom (V2)

newtype FilterCoord = FilterCoord {unFilterCoord :: V2 Float}

newtype Filter = Filter {unFilter :: FilterCoord -> Float}
{-

import Jitterpug.Geom
  ( Rect,
    Size,
    V2,
  )
import qualified Jitterpug.Geom as Geom
import Jitterpug.Sampler (Sample)
import qualified Jitterpug.Sampler as Sampler

class Filterable e where
  filterableZero :: e
  filterableSum :: e -> e -> e
  filterableScale :: Float -> e -> e

instance Filterable Float where
  {-# INLINE filterableZero #-}
  {-# INLINE filterableSum #-}
  {-# INLINE filterableScale #-}
  filterableZero = 0.0
  filterableSum = (+)
  filterableScale = (*)

data Filter
  = Filter
      { filterSize :: Size Float,
        filterFunction :: V2 Float -> Float
      }

boxFilter :: V2 Float -> Float
boxFilter = const 1.0

data PartialFilterResult e
  = PartialFilterResult
      { partialFilterResultWeightSum :: {-# UNPACK #-} !Float,
        partialFilterResultValue :: !e
      }

instance forall e. Filterable e => Semigroup (PartialFilterResult e) where
  {-# INLINE (<>) #-}
  a <> b = PartialFilterResult weight value
    where
      weight :: Float
      weight =
        partialFilterResultWeightSum a + partialFilterResultWeightSum b
      value :: e
      value =
        filterableSum
          (partialFilterResultValue a)
          (partialFilterResultValue b)

instance Filterable e => Monoid (PartialFilterResult e) where
  {-# INLINE mempty #-}
  mempty = PartialFilterResult 0.0 filterableZero

finalize :: forall e. (Filterable e) => PartialFilterResult e -> e
finalize pfr = filterableScale (1 / weight) value
  where
    weight :: Float
    weight = partialFilterResultWeightSum pfr
    value :: e
    value = partialFilterResultValue pfr
{-# INLINEABLE finalize #-}

filterSample ::
  forall e.
  (Filterable e) =>
  Filter ->
  V2 Float ->
  Sample e ->
  PartialFilterResult e
filterSample flt origin sample =
  let fr :: Rect Float
      fr = Geom.rectCentered (Geom.V2 0.0 0.0) (Geom.Size 2.0 2.0)
      dx, dy :: Float
      Geom.V2 dx dy = Geom.subV2 (Sampler.samplePos sample) origin
      fw, fh :: Float
      Geom.Size fw fh = filterSize flt
      pn :: V2 Float
      pn = Geom.V2 (2 * dx / fw) (2 * dy / fh)
   in if Geom.ptInRect fr pn
        then
          let weight :: Float
              weight = filterFunction flt pn
              value :: e
              value = filterableScale weight (Sampler.sampleValue sample)
           in PartialFilterResult weight value
        else mempty
{-# INLINEABLE filterSample #-}

-}
