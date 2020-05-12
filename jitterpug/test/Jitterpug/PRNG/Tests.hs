{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jitterpug.PRNG.Tests
  ( tests,
    genKensler,
  )
where

import Data.List (sort)
import Hedgehog
  ( (===),
    Gen,
    MonadTest,
    Property,
    assert,
    failure,
    forAll,
    property,
    withTests,
  )
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Source (HasCallStack)
import qualified Hedgehog.Range as Range
import Jitterpug.PRNG
  ( Index,
    PRNGen,
    Pattern,
  )
import qualified Jitterpug.PRNG as PRNG
import qualified Jitterpug.Test.Histogram as Histogram
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Jitterpug.PRNG"
    [ prngTests "kensler PRNG" genKensler
    ]

genPattern :: Gen Pattern
genPattern = Gen.enumBounded

genKensler :: Gen PRNGen
genKensler = PRNG.kensler <$> genPattern

prngTests :: TestName -> Gen PRNGen -> TestTree
prngTests name prnGenGen =
  testGroup
    name
    [ properties
    ]
  where
    properties :: TestTree
    properties =
      testGroup
        "Properties"
        [ testProperty
            "PRNG.float values are in the range [0, 1)"
            (propFloatRange prnGenGen),
          testProperty
            "PRNG.float values are uniformly-distributed"
            (propFloatUniform prnGenGen),
          testProperty
            "PRNG.permute is a permutation (includes all indices once)"
            (propPermuteIsPermutation prnGenGen),
          testProperty
            "PRNG.pat is uniformly-distributed"
            (propPatUniform prnGenGen)
        ]

propFloatRange :: Gen PRNGen -> Property
propFloatRange prnGenGen = property $ do
  prnGen <- forAll prnGenGen
  let x :: Float
      x = PRNG.runPRN' prnGen PRNG.float
  assert $ x >= 0
  assert $ x < 1

propFloatUniform :: Gen PRNGen -> Property
propFloatUniform prnGenGen = withTests 10 $ property $ do
  prnGen <- forAll prnGenGen
  let nFloats :: Int
      nFloats = 10000
      --
      xs :: [Float]
      xs = take nFloats $ PRNG.runPRN' prnGen (repeatM PRNG.float)
  --
  hist <- fromJustH $ Histogram.fromList 10 0.0 1.0 xs
  assert $ Histogram.isUniform 0.02 hist

propPermuteIsPermutation :: Gen PRNGen -> Property
propPermuteIsPermutation prnGenGen = property $ do
  prnGen <- forAll prnGenGen
  nSamples <- forAll $ Gen.integral (Range.linear 1 1024)
  let indices :: [Index]
      indices = [0 .. fromIntegral (nSamples - 1)]
      --
      permFn :: Index -> Index
      permFn = PRNG.runPRN' prnGen (PRNG.permute nSamples)
      --
      permutation :: [Index]
      permutation = permFn <$> indices
  sort permutation === indices

propPatUniform :: Gen PRNGen -> Property
propPatUniform prnGenGen = withTests 10 $ property $ do
  prnGen <- forAll prnGenGen
  let nPatterns :: Int
      nPatterns = 10000
      --
      pats :: [Pattern]
      pats = take nPatterns $ PRNG.runPRN' prnGen (repeatM PRNG.pat)
  --
  hist <- fromJustH $ Histogram.fromListIBound 10 pats
  assert $ Histogram.isUniform 0.02 hist

repeatM :: forall m a. Monad m => m a -> m [a]
repeatM action = sequence $ repeat action

fromJustH :: (MonadTest m, HasCallStack) => Maybe a -> m a
fromJustH = maybe failure pure
