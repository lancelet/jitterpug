{-# LANGUAGE OverloadedStrings #-}

module Jitterpug.PRNG.Tests
  ( tests,
  )
where

import Data.List (sort)
import Hedgehog
  ( (===),
    Property,
    assert,
    cover,
    forAll,
    property,
    withTests,
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Jitterpug.PRNG
  ( Index,
    NSamples,
    Pattern,
  )
import qualified Jitterpug.PRNG as PRNG
import Jitterpug.Test.Util (approxEqList)
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  Tasty.testGroup
    "Jitterpug.PRNG"
    [ testProperty
        "unit: permuteIndexWord32 matches the C version for some examples"
        unit_permutationComparison,
      testProperty
        "unit: randFloat matches the C version for some examples"
        unit_randFloatComparison,
      testProperty
        "prop: randFloat range"
        prop_randFloatRange,
      testProperty
        "prop: permuteIndexWord32 must contain all indices"
        prop_permuteAllIndices
    ]

---- Unit tests

-- | Compare 'PRNG.permuteIndexWord32' against the C version.
--
-- We have some examples of what the C version of the @permute@ function
-- produces. This unit test compares those values against the Haskell version.
--
-- The examples used here are completely arbitrary.
unit_permutationComparison :: Property
unit_permutationComparison = withTests 1 $ property $ do
  let len :: NSamples
      len = 8
      permIndices :: Pattern -> [Index]
      permIndices pat =
        PRNG.permuteIndex pat len
          . fromIntegral
          <$> [0 .. len - 1]
  permIndices 0xa511e9b3 === [7, 4, 1, 6, 5, 3, 0, 2]
  permIndices 0xa511e9b4 === [3, 1, 4, 6, 7, 2, 5, 0]
  permIndices 0xa511e9b5 === [2, 4, 7, 5, 3, 0, 1, 6]
  permIndices 0xa511e9b6 === [6, 0, 5, 3, 7, 2, 1, 4]
  permIndices 0xa511e9b7 === [1, 7, 4, 6, 3, 0, 5, 2]
  permIndices 0xa511e9b8 === [3, 6, 1, 4, 7, 5, 0, 2]
  permIndices 0xa511e9b9 === [7, 4, 5, 2, 6, 0, 3, 1]
  permIndices 0xa511e9ba === [3, 6, 5, 0, 2, 4, 1, 7]
  permIndices 0xa511e9bb === [7, 4, 1, 6, 5, 3, 0, 2]
  permIndices 0xa511e9bc === [3, 1, 4, 6, 7, 2, 5, 0]

-- | Compare 'PRNG.randFloat' against the C version.
--
-- We have some examples of what the C version of the @randfloat@ function
-- produces. This unit test compares those values against the Haskell version.
--
-- The examples used here are completely arbitrary.
unit_randFloatComparison :: Property
unit_randFloatComparison = withTests 1 $ property $ do
  let n :: Index
      n = 10
      floats :: Pattern -> [Float]
      floats pat = PRNG.randFloat pat <$> [0 .. (n - 1)]
      eps :: Float
      eps = 1e-6
  assert $
    approxEqList
      eps
      (floats 0xa399d265)
      [ 0.204491,
        0.951885,
        0.436780,
        0.436960,
        0.824837,
        0.467759,
        0.307512,
        0.940556,
        0.175458,
        0.899137
      ]
  assert $
    approxEqList
      eps
      (floats 0x711ad6a5)
      [ 0.645281,
        0.804511,
        0.134814,
        0.548340,
        0.730665,
        0.861065,
        0.548685,
        0.280748,
        0.056795,
        0.321446
      ]

--- Property tests

-- | Check the range of the 'PRNG.randFloat' function.
--
-- 'PRNG.randFloat' should always return a number in the range [0, 1).
-- If we create a histogram of values (done here using `cover` from Hedgehog),
-- then the values should be distributed approximately evenly.
prop_randFloatRange :: Property
prop_randFloatRange = withTests 10000 $ property $ do
  pat <- forAll $ PRNG.Pattern <$> Gen.word32 Range.linearBounded
  idx <- forAll $ PRNG.Index <$> Gen.word32 Range.linearBounded
  let f :: Float
      f = PRNG.randFloat pat idx
  assert $ f >= 0.0
  assert $ f < 1.0
  let minCov = 18 -- minimum coverage; close to 20%
  cover minCov "0.0 to 0.2" (f >= 0.0 && f <= 0.2)
  cover minCov "0.2 to 0.4" (f >= 0.2 && f <= 0.4)
  cover minCov "0.4 to 0.6" (f >= 0.4 && f <= 0.6)
  cover minCov "0.6 to 0.8" (f >= 0.6 && f <= 0.8)
  cover minCov "0.8 to 1.0" (f >= 0.8 && f <= 1.0)

-- | Permutations of all indices should contain all indices.
--
-- eg. the length 8 index permutation [7, 4, 1, 6, 5, 3, 0, 2] contains all
--     indices from 0 to 7
prop_permuteAllIndices :: Property
prop_permuteAllIndices = property $ do
  pat <- forAll $ fromIntegral <$> Gen.word32 Range.linearBounded
  plen <- forAll $ fromIntegral <$> Gen.word32 (Range.linear 1 256)
  let indices :: [Index]
      indices = fromIntegral <$> [0 .. plen - 1]
      permutations :: [Index]
      permutations = PRNG.permuteIndex pat plen <$> indices
  sort permutations === indices
