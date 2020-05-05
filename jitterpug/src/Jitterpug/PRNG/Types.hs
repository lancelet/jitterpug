{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK not-home #-}

module Jitterpug.PRNG.Types
  ( -- * Types
    Pattern (Pattern, unPattern),
    Index (Index, unIndex),
    NSamples (NSamples, unNSamples),
    PRNG (PRNG, randFloat, randPermute),
  )
where

import Data.Word (Word32)

-- | Pattern of randomness.
newtype Pattern = Pattern {unPattern :: Word32}
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral, Bounded)

-- | Index into a set of random samples.
newtype Index = Index {unIndex :: Word32}
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral, Bounded)

-- | Number of samples.
newtype NSamples = NSamples {unNSamples :: Word32}
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral, Bounded)

-- | Pseudo-random-number generator.
data PRNG
  = PRNG
      { -- | Generate a random floating-point number.
        randFloat :: Pattern -> Index -> Float,
        -- | Generate a function that randomly permutes indices.
        randPermute :: Pattern -> Index -> NSamples -> Index -> Index
      }
