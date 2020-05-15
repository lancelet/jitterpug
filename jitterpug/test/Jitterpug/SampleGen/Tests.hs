{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jitterpug.SampleGen.Tests
  ( tests,
  )
where

import Hedgehog ((===), Gen, Property, annotateShow, assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Jitterpug.PRNG as PRNG
import Jitterpug.PRNG.Tests (genKensler)
import Jitterpug.SampleGen
  ( Aspect,
    SampleCount,
    SampleGen,
    SizedSampleGen,
  )
import qualified Jitterpug.SampleGen as SampleGen
import Jitterpug.UV (UV)
import qualified Jitterpug.UV as UV
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Jitterpug.SampleGen"
    [ sampleGenTests "Uniform" uniformSampleGen,
      sampleGenTests "Stratified" stratifiedSampleGen
    ]

uniformSampleGen :: SampleGen UV
uniformSampleGen = SampleGen.uniform

stratifiedSampleGen :: SampleGen UV
stratifiedSampleGen = SampleGen.stratified (SampleGen.mkJitter 1)

sampleGenTests ::
  TestName ->
  SampleGen UV ->
  TestTree
sampleGenTests name sg =
  testGroup
    name
    [properties]
  where
    properties :: TestTree
    properties =
      testGroup
        "Properties"
        [ testProperty
            "At least as many samples are generated as requested"
            (propMinSampleCount sg),
          testProperty
            "All generated samples are contained inside a pixel rectangle"
            (propPixelContainsAllSamples sg)
        ]

propMinSampleCount ::
  SampleGen UV ->
  Property
propMinSampleCount sg = property $ do
  aspect <- forAll genAspect
  n <- forAll genSampleCount
  prnGen <- forAll genKensler
  let sized :: SizedSampleGen UV
      sized = SampleGen.sampleGen sg aspect n
  assert $ SampleGen.sampleCount sized >= n
  let samples :: [UV]
      samples = PRNG.runPRN' prnGen $ SampleGen.samples $ sized
  (fromIntegral . SampleGen.unSampleCount . SampleGen.sampleCount) sized === length samples

propPixelContainsAllSamples ::
  SampleGen UV ->
  Property
propPixelContainsAllSamples sg = property $ do
  aspect <- forAll genAspect
  n <- forAll genSampleCount
  prnGen <- forAll genKensler
  let samples :: [UV]
      samples =
        PRNG.runPRN' prnGen
          $ SampleGen.samples
          $ SampleGen.sampleGen sg aspect n
  --
  annotateShow samples
  assert $ all UV.inUnitSquare samples

genAspect :: Gen Aspect
genAspect = SampleGen.Aspect <$> Gen.float (Range.linearFrac 0.1 10.0)

genSampleCount :: Gen SampleCount
genSampleCount = SampleGen.SampleCount <$> Gen.word16 (Range.linear 1 200)
