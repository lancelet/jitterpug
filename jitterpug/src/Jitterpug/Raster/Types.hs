module Jitterpug.Raster.Types
  ( Pxc (Pxc, x, y),
    RasterRect (RasterRect, rx, ry, rw, rh)
  )
where

import Data.Word (Word32)
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

-- | Rectangle in a raster.
data RasterRect
  = RasterRect
    { -- | x-coordinate.
      rx :: {-# UNPACK #-} !Int,
      -- | y-coordinate.
      ry :: {-# UNPACK #-} !Int,
      -- | Rectangle width.
      rw :: {-# UNPACK #-} !Word32,
      -- | Rectangle height.
      rh :: {-# UNPACK #-} !Word32
    }
