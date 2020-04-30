{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jitterpug.PRNG.Tests
  ( tests,
  )
where

import Data.List (sort)
import Data.String (fromString)
import Data.Word (Word8)
import Hedgehog
  ( (===),
    Gen,
    LabelName,
    MonadTest,
    Property,
    assert,
    cover,
    forAll,
    property,
    withTests,
  )
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Property (CoverPercentage)
import Hedgehog.Internal.Source (HasCallStack)
import qualified Hedgehog.Range as Range
import Jitterpug.PRNG
  ( Index,
    NSamples,
    PRNG,
    Pattern,
    kensler,
    randFloat,
    randPermute,
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty (TestName)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Jitterpug.PRNG"
    [ prngTests "kensler PRNG" kensler
    ]

prngTests :: TestName -> PRNG -> TestTree
prngTests name prng =
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
            "randFloat values are in the range [0, 1)"
            (propRandFloatRange (randFloat prng)),
          testProperty
            "randFloat values are uniformly-distributed"
            (propRandFloatUniform (randFloat prng)),
          testProperty
            "randPermute is a permutation (includes all required indices once)"
            (propRandPermuteIsPermutation (randPermute prng))
        ]

propRandFloatRange ::
  (Pattern -> Index -> Float) ->
  Property
propRandFloatRange randFloatFn = property $ do
  pat <- forAll $ genPattern
  index <- forAll $ genIndex
  let x = randFloatFn pat index
  assert $ x >= 0
  assert $ x < 1

propRandFloatUniform ::
  (Pattern -> Index -> Float) ->
  Property
propRandFloatUniform randFloatFn = withTests 10000 $ property $ do
  pat <- forAll $ genPattern
  index <- forAll $ genIndex
  let x = randFloatFn pat index
  coverUniformUnit 1 10 x

propRandPermuteIsPermutation ::
  (Pattern -> Index -> NSamples -> Index -> Index) ->
  Property
propRandPermuteIsPermutation randPermuteFn = property $ do
  pat <- forAll $ genPattern
  nSamples <- forAll $ Gen.integral (Range.linear 1 1024)
  index <- forAll $ genIndex
  let indices = [0 .. fromIntegral (nSamples - 1)]
      permutation = randPermuteFn pat index nSamples <$> indices
  sort permutation === indices

genPattern :: Gen Pattern
genPattern = Gen.enumBounded

genIndex :: Gen Index
genIndex = Gen.enumBounded

-- | Test that a quantity is uniformly distributed across the unit interval.
--
-- In this test, a quantity is histogrammed into a number of uniformly-sized
-- buckets which divide the interval @[0, 1)@. Hedgehog coverage (the 'cover'
-- function) is used to test that the quantity is evenly distributed. An
-- epsilon-adjustment is supplied to allow the caller to specify the minimum
-- difference between an even distribution and the actual distribution that
-- is acceptable.
coverUniformUnit ::
  forall m n.
  (MonadTest m, HasCallStack, Show n, Ord n, Fractional n) =>
  -- | Epsilon offset to subtract from the expected percentage in each bucket.
  CoverPercentage ->
  -- | Number of buckets.
  Word8 ->
  -- | Current value.
  n ->
  -- | Resulting 'MonadTest' value.
  m ()
coverUniformUnit dpc nBuckets x =
  sequence_ $ coverBucket <$> [0 .. fromIntegral $ nBuckets - 1]
  where
    -- percentage coverage per bucket
    pc :: CoverPercentage
    pc = 100 / fromIntegral nBuckets - dpc
    -- range of values for a bucket
    bucketRange :: Int -> (n, n)
    bucketRange bucket = (bmin, bmax)
      where
        bmin, bmax :: n
        bmin = fromIntegral bucket / fromIntegral nBuckets
        bmax = fromIntegral (bucket + 1) / fromIntegral nBuckets
    -- bucket range test function
    inBucket :: Int -> Bool
    inBucket bucket = x >= bmin && x < bmax
      where
        bmin, bmax :: n
        (bmin, bmax) = bucketRange bucket
    -- label for a bucket
    labelBucket :: Int -> LabelName
    labelBucket bucket = minLabel <> " <= x <= " <> maxLabel
      where
        bmin, bmax :: n
        (bmin, bmax) = bucketRange bucket
        minLabel, maxLabel :: LabelName
        minLabel = fromString . show $ bmin
        maxLabel = fromString . show $ bmax
    -- bucket coverage test
    coverBucket :: Int -> m ()
    coverBucket bucket = cover pc (labelBucket bucket) (inBucket bucket)
