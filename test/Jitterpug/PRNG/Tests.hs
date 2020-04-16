{-# LANGUAGE OverloadedStrings #-}
module Jitterpug.PRNG.Tests
    ( tests
    )
where

import           Hedgehog                       ( Property
                                                , property
                                                , withTests
                                                , (===)
                                                )
import           Test.Tasty                     ( TestTree )
import qualified Test.Tasty                    as Tasty
import           Test.Tasty.Hedgehog            ( testProperty )

import           Data.Word                      ( Word32 )

import           Jitterpug.PRNG                 ( Pattern
                                                , PermutationLength
                                                )
import qualified Jitterpug.PRNG                as PRNG

tests :: TestTree
tests = Tasty.testGroup
    "Jitterpug.PRNG"
    [ testProperty
          "unit: permuteIndexWord32 matches the C version for some examples"
          unit_permutationComparison
    ]

---- Unit tests

-- | Compare against the C version.
--
-- We have some examples of what the C version of the 'permute' function
-- produces in a few cases. This unit test compares those values against the
-- Haskell version.
--
-- The examples used here are completely arbitrary.
unit_permutationComparison :: Property
unit_permutationComparison = withTests 1 $ property $ do
    let len :: PermutationLength
        len = PRNG.PermutationLength 8

        permIndices :: Pattern -> [Word32]
        permIndices pat =
            PRNG.permuteIndexWord32 pat len
                .   PRNG.Index
                <$> [0 .. (PRNG.unPermutationLength len - 1)]

    permIndices (PRNG.Pattern 0xa511e9b3) === [7, 4, 1, 6, 5, 3, 0, 2]
    permIndices (PRNG.Pattern 0xa511e9b4) === [3, 1, 4, 6, 7, 2, 5, 0]
    permIndices (PRNG.Pattern 0xa511e9b5) === [2, 4, 7, 5, 3, 0, 1, 6]
    permIndices (PRNG.Pattern 0xa511e9b6) === [6, 0, 5, 3, 7, 2, 1, 4]
    permIndices (PRNG.Pattern 0xa511e9b7) === [1, 7, 4, 6, 3, 0, 5, 2]
    permIndices (PRNG.Pattern 0xa511e9b8) === [3, 6, 1, 4, 7, 5, 0, 2]
    permIndices (PRNG.Pattern 0xa511e9b9) === [7, 4, 5, 2, 6, 0, 3, 1]
    permIndices (PRNG.Pattern 0xa511e9ba) === [3, 6, 5, 0, 2, 4, 1, 7]
    permIndices (PRNG.Pattern 0xa511e9bb) === [7, 4, 1, 6, 5, 3, 0, 2]
    permIndices (PRNG.Pattern 0xa511e9bc) === [3, 1, 4, 6, 7, 2, 5, 0]
