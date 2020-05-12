-- |
-- Module      : Jitterpug.PRNG
-- Description : Pseudo-random-number generation.
--
-- This module is similar to the built-in 'System.Random' module, but provides
-- cut-down functionality specifically required by Jitterpug.
module Jitterpug.PRNG
  ( -- * Guide
    -- $guide
    module Jitterpug.PRNG.Types,
    module Jitterpug.PRNG.Kensler,

    -- * Functions

    -- ** Run PRN monad
    runPRN,
    runPRN',

    -- ** Generate values
    float,
    permute,
    pat,

    -- ** Special operations
    addPattern,
    jump,
  )
where

import Data.Tuple (swap)
import Jitterpug.Hashable (Hashable)
import qualified Jitterpug.Hashable as Hashable
import Jitterpug.PRNG.Kensler
import Jitterpug.PRNG.Types

-- | Run the 'PRN' monad, using the specified 'PRNGen'.
runPRN ::
  -- | Initial generator to feed into the 'PRN'.
  PRNGen ->
  -- | The 'PRN' monad to run.
  PRN a ->
  -- | Produced final 'PRNGen' and value.
  (PRNGen, a)
runPRN = flip unPRN

-- | Run the 'PRN' monad, using the specified 'PRNGen', and discard the final
--   'PRNGen' that is produced.
runPRN' ::
  -- | Initial generator to feed into the 'PRN'.
  PRNGen ->
  -- | The 'PRN' monad to run.
  PRN a ->
  -- | Produced final value.
  a
runPRN' gen prn = snd $ runPRN gen prn

-- | Include a random 'Pattern' with a 'PRN' result.
--
-- The random pattern is computed after the value of type 'a' is produced.
addPattern :: PRN a -> PRN (Pattern, a)
addPattern prn = swap <$> ((,) <$> prn <*> pat)

-- | Produce a random 'Float'.
float :: PRN Float
float = PRN nextFloat

-- | Produce a random permutation.
--
-- The permutation returned is a function of type @'Index' -> 'Index'@, where
-- the domain should lie in the range @[0, nSamples - 1]@.
permute ::
  -- | Number of samples in the permutation.
  NSamples ->
  -- | Produced permutation function, in the 'PRN' monad.
  PRN (Index -> Index)
permute nSamples = PRN (`nextPermute` nSamples)

-- | Produce a random 'Pattern'.
pat :: PRN Pattern
pat = PRN nextPattern

-- | Perform a random jump based on a 'Hashable' value in the 'PRN' monad.
jump ::
  (Hashable a) =>
  -- | Value to use for the random jump.
  a ->
  -- | Produced 'PRN' action.
  PRN ()
jump x =
  PRN $
    \gen ->
      (jumpPattern gen (Pattern (Hashable.unHash . Hashable.hash $ x)), ())

-- $guide
--
-- The two core types are:
--
--  ['PRNGen']: Generates a single random value.
--  ['PRN']: A monad to generate multiple random values.
--
-- To generate random values, the basic idea is to run a 'PRN' monad using a
-- 'PRNGen'. For example, first start by creating a 'PRNGen'. In this case,
-- we use the default 'kensler' 'PRNGen':
--
-- >>> gen = kensler 0 :: PRNGen
--
-- Then we need a 'PRN' to run. We can create a 'Float' value using the 'float'
-- 'PRN':
--
-- >>> prn = float :: PRN Float
--
-- Finally, we can run the 'PRN', feeding it the 'PRNGen', using the 'runPRN''
-- function:
--
-- >>> runPRN' gen prn
-- 0.87277794
--
-- Each time that 'runPRN'' is called on the same inputs, it will produce the
-- same output. There is also a 'runPRN' function, which returns a new random
-- generator, which can be used downstream for further random value generation:
--
-- >>> gen0 = kensler 0
-- >>> (gen1, x1) = runPRN gen0 float
-- >>> (gen2, x2) = runPRN gen1 float
-- >>> (x1, x2)
-- (0.87277794,2.0471264e-2)
--
-- The 'PRN' type can be composed in an 'Applicative' fashion:
--
-- >>> runPRN' gen $ (,) <$> float <*> float
-- (0.87277794,2.0471264e-2)
--
-- It can also be composed using 'Monad'ic operations:
--
-- >>> runPRN' gen $ float >>= \x1 -> float >>= \x2 -> pure (x1, x2)
-- (0.87277794,2.0471264e-2)
--
-- == Random seed: 'Pattern'
--
-- The 'Pattern' type works as a random seed. Running the same 'PRN' with
-- different 'Pattern's (usually) results in different values:
--
-- >>> floatPair pat = runPRN' (kensler pat) $ (,) <$> float <*> float
-- >>> floatPair (1 :: Pattern)
-- (2.0471264e-2,0.87277794)
-- >>> floatPair (2 :: Pattern)
-- (0.5252206,0.9647351)
--
-- == Random permutations
--
-- A permutation in our context is an ordering of indices. A permutation can
-- thus be described as a function of type @'Index' -> 'Index'@, where the
-- domain is some previous ordering of indices. To see the indices produced by a
-- permutation, apply the @'Index' -> 'Index'@ function to some prior ordering
-- of indices. For example:
--
-- >>> nSamples = 5 :: NSamples
-- >>> indices = [0 .. fromIntegral nSamples - 1]
-- >>> idxPerm = permute nSamples >>= \f -> pure (f <$> indices)
-- >>> perm pat = unIndex <$> runPRN' (kensler pat) idxPerm
-- >>> perm (0 :: Pattern)
-- [1,3,4,2,0]
-- >>> perm (1 :: Pattern)
-- [2,4,1,3,0]
-- >>> perm (2 :: Pattern)
-- [3,2,0,1,4]
--
-- Each permutation contains the numbers @[0,1,2,3,4]@ exactly once.
--
-- == Random jumps
--
-- Some useful functionality provided by this module is the ability to do a
-- random 'jump'. Random jumps are a way to randomly offset the seed of a random
-- generator, based on both a 'Jitterpug.Hashable.Hashable' value and the
-- current state of the generator. This is useful in cases where we want to use
-- the same 'PRN', but allow it to branch out, or split, at some point. For
-- instance, we may want to generate different values for each pixel in an
-- image, in parallel.
--
-- To see the utility of random jumps, say we start with a 'PRN':
--
-- >>> prn = float :: PRN Float
--
-- We also have a fixed 'PRNGen':
--
-- >>> gen = kensler 0 :: PRNGen
--
-- We can split the @prn@ across multiple values by using 'jump':
--
-- >>> :{
--   prni :: Int -> PRN Float
--   prni i = jump i >> prn
-- :}
--
-- This means that we can now generate different 'Float' values at each index,
-- despite starting with the same random generator:
--
-- >>> runPRN' gen . prni <$> [0 .. 5]
-- [0.9581054,0.39477178,0.2643435,0.61987036,0.46661952,0.2679268]
--
-- These random values can be chosen from a different pattern by using a
-- different 'PRNGen' value:
--
-- >>> runPRN' (kensler 1) . prni <$> [0 .. 5]
-- [0.12153749,0.7268794,0.8944455,0.47504237,0.33605206,0.29878357]
--
-- An important utility obtained by the 'jump' function is the ability to
-- deterministically obtain the same random values in parallel. Ordinarily, new
-- random values require traversing the values produced by a 'PRNGen' in
-- sequence, but with 'jump', an instant offset is obtained, which can then be
-- used for further generation.
