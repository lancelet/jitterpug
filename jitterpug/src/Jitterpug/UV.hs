-- |
-- Module      : Jitterpug.UV
-- Description : UV coordinates.
module Jitterpug.UV
  ( UV (UV, u, v),
    inUnitSquare,
  )
where

-- | UV coordinate.
--
-- UV coordinates that are "in the unit square" exist in the range
-- @[0.0, 1.0]@. However, they are not clamped to this range.
data UV
  = UV
      { u :: {-# UNPACK #-} !Float,
        v :: {-# UNPACK #-} !Float
      }
  deriving (Eq, Show)

-- | Check if a 'UV' is inside the unit square.
inUnitSquare :: UV -> Bool
inUnitSquare (UV u' v') =
  u' >= 0
    && u' <= 1
    && v' >= 0
    && v' <= 1
