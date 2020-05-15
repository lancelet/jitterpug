{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK not-home #-}

module Jitterpug.PRNG.Types
  ( -- * Types
    Pattern (Pattern, unPattern),
    Index (Index, unIndex),
    NSamples (NSamples, unNSamples),
    PRNGen (PRNGen, nextFloat, nextPermute, nextPattern, jumpPattern),
    PRN (PRN, unPRN),
  )
where

import Data.Word (Word32)
import Text.Printf (printf)

-- | Pattern of randomness.
newtype Pattern = Pattern {unPattern :: Word32}
  deriving (Eq, Ord, Num, Real, Enum, Integral, Bounded)

instance Show Pattern where
  show (Pattern p) = printf "Pattern 0x%08x" p

-- | Index into a set of random samples.
newtype Index = Index {unIndex :: Word32}
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral, Bounded)

-- | Number of samples.
newtype NSamples = NSamples {unNSamples :: Word32}
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral, Bounded)

-- | Pseudo-random-number generator.
data PRNGen
  = PRNGen
      { -- | Generate a random floating-point number.
        nextFloat :: (PRNGen, Float),
        -- | Generate a function that randomly permutes indices.
        nextPermute :: NSamples -> (PRNGen, Index -> Index),
        -- | Generate a pattern.
        nextPattern :: (PRNGen, Pattern),
        -- | Jump to a new 'PRNGen' using the specified 'Pattern'.
        jumpPattern :: Pattern -> PRNGen
      }

instance Show PRNGen where
  show _ = "<PRNGen>"

-- | Pseudo-random-number monad.
newtype PRN a = PRN {unPRN :: PRNGen -> (PRNGen, a)}

instance Functor PRN where
  fmap f (PRN g) = PRN (fmap f <$> g)

instance Applicative PRN where

  pure x = PRN (,x)

  ff <*> fa =
    PRN
      ( \gen ->
          let (gen', f) = unPRN ff gen
              (gen'', a) = unPRN fa gen'
           in (gen'', f a)
      )

instance Monad PRN where
  ma >>= fmb =
    PRN
      ( \gen ->
          let (gen', a) = unPRN ma gen
           in unPRN (fmb a) gen'
      )
