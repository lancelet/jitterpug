module Jitterpug.Image.Types
  ( Imc (Imc, x, y),
  )
where

-- | Coordinates of a point within an image.
data Imc
  = Imc
      { x :: {-# UNPACK #-} !Float,
        y :: {-# UNPACK #-} !Float
      }
