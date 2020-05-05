module Jitterpug.Image.Types
  ( Pxc (Pxc, x, y),
  )
where

-- | Coordinates of a pixel.
data Pxc
  = Pxc
      { x :: {-# UNPACK #-} !Int,
        y :: {-# UNPACK #-} !Int
      }
  deriving (Eq, Show)
