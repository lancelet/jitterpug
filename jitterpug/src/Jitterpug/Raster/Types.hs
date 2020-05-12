module Jitterpug.Raster.Types
  ( Pxc (Pxc, x, y),
  )
where

import Jitterpug.Hashable (Hash (Hash), Hashable, hash, unHash)

-- | Coordinates of a pixel.
data Pxc
  = Pxc
      { x :: {-# UNPACK #-} !Int,
        y :: {-# UNPACK #-} !Int
      }
  deriving (Eq, Show)

instance Hashable Pxc where
  hash (Pxc px py) =
    Hash $ unHash (hash True) + unHash (hash px) + unHash (hash py)
