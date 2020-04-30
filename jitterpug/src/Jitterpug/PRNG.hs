-- |
-- Module      : Jitterpug.PRNG
-- Description : Pseudo-random-number generation.
--
-- == Guide
--
-- This module contains deterministic pseudo-random number generators. They
-- return deterministic pseudo-random values using a combination of these
-- two parameters:
--
--   ['Pattern']: Specifies a "pattern of randomness" from which to select
--                values.
--
--   ['Index']: Index into the 'Pattern' to choose a value.
--
-- When taken together, 'Pattern' and 'Index' can be considered as the two
-- parts of a unique address for a particular random value. The separation
-- aids in many algorithms in which a hierarchical indexing of randomness
-- is desirable. The 'Pattern' can also be thought of as like a random seed,
-- while the 'Index' is a sequence number of a value generated using that
-- seed.
--
-- === Generating random 'Float's
--
-- To generate a random 'Float', supply a 'Pattern' and 'Index', and use the
-- 'randFloat' function. For example, using the 'kensler' PRNG:
--
-- >>> pat = Pattern 42
-- >>> index = Index 0
-- >>> randFloat kensler pat index
-- 0.33162743
--
-- This can be shortened in the case of numeric literals because 'Pattern'
-- and 'Index' both have a 'Num' instance:
--
-- >>> randFloat kensler 42 0
-- 0.33162743
--
-- To generate a sequence of floats from a given 'Pattern', simply apply the
-- function to multiple 'Index' values:
--
-- >>> randFloat kensler 42 <$> [0 .. 5]
-- [0.33162743,0.3776114,0.9006274,0.7080654,0.4385996,7.2427265e-2]
--
-- === Generating random 'Index' permutations
--
-- Random 'Index' permutations map index values uniquely from an 'Index'
-- in a range to another 'Index' in the same range. To do this, supply a
-- 'Pattern', 'Index' and 'NSamples', and use the 'randPermute' functions.
-- For example, using the 'kensler' PRNG:
--
-- >>> pat = Pattern 42
-- >>> index = Index 0
-- >>> nSamples = NSamples 5
-- >>> unIndex <$> randPermute kensler pat index nSamples <$> [0 .. 4]
-- [2,4,3,0,1]
--
-- Different permutation of the indices are available with either a different
-- 'Pattern' or different initial 'Index'. For instance, using different
-- indices:
--
-- >>> unIndex <$> randPermute kensler 42 1 5 <$> [0 .. 4]
-- [2,4,3,0,1]
-- >>> unIndex <$> randPermute kensler 42 2 5 <$> [0 .. 4]
-- [2,0,1,3,4]
module Jitterpug.PRNG
  ( module Jitterpug.PRNG.Types,
    module Jitterpug.PRNG.Kensler,
  )
where

import Jitterpug.PRNG.Kensler
import Jitterpug.PRNG.Types
