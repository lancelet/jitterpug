{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedStrings #-}

module Jitterpug.SampleGen.Tests
  ( tests,
  )
where

import Hedgehog ((===), Gen, Property, annotateShow, assert, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Jitterpug.Image (Pxc (Pxc))
import qualified Jitterpug.Image as Image
import Jitterpug.PRNG (kensler)
import Jitterpug.SampleGen
  ( Aspect (Aspect),
    SampleGen,
    SamplesPerPixel (SamplesPerPixel),
    SizedSampleGen,
    Smc (Smc),
    mkJitter,
    sampleGen,
    samplesForPixel,
    spp,
    unSamplesPerPixel,
    uniform,
  )
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Jitterpug.SampleGen"
    [ sampleGenTests "Uniform" uniformSampleGen
    ]

uniformSampleGen :: SampleGen
uniformSampleGen = uniform kensler (mkJitter 0.99)

sampleGenTests :: TestName -> SampleGen -> TestTree
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

propMinSampleCount :: SampleGen -> Property
propMinSampleCount sg = property $ do
  aspect <- forAll genAspect
  spp' <- forAll genSamplesPerPixel
  let sized :: SizedSampleGen
      sized = sampleGen sg aspect spp'
  assert $ spp sized >= spp'
  pxc <- forAll genPxc
  let samples :: [Smc]
      samples = samplesForPixel sized pxc
  (fromIntegral . unSamplesPerPixel . spp) sized === length samples

propPixelContainsAllSamples :: SampleGen -> Property
propPixelContainsAllSamples sg = property $ do
  aspect <- forAll genAspect
  spp' <- forAll genSamplesPerPixel
  pxc <- forAll genPxc
  let samples :: [Smc]
      samples = samplesForPixel (sampleGen sg aspect spp') pxc
      --
      sampleInPixel :: Smc -> Bool
      sampleInPixel (Smc x y) =
        x >= xf
          && y >= yf
          && x <= (xf + 1)
          && y <= (yf + 1)
        where
          xf, yf :: Float
          xf = fromIntegral (Image.x pxc)
          yf = fromIntegral (Image.y pxc)
  annotateShow samples
  assert $ all sampleInPixel samples

genAspect :: Gen Aspect
genAspect = Aspect <$> Gen.float (Range.linearFrac 0.1 10.0)

genSamplesPerPixel :: Gen SamplesPerPixel
genSamplesPerPixel = SamplesPerPixel <$> Gen.word16 (Range.linear 1 200)

genPxc :: Gen Pxc
genPxc =
  Pxc
    <$> Gen.int (Range.linear -10000 10000)
    <*> Gen.int (Range.linear -10000 10000)
