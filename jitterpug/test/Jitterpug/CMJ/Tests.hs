{-# LANGUAGE OverloadedStrings #-}

module Jitterpug.CMJ.Tests
  ( tests,
  )
where

import Hedgehog
  ( Property,
    annotateShow,
    assert,
    property,
    withTests,
  )
import qualified Jitterpug.CMJ as CMJ
import Jitterpug.Geom (V2)
import qualified Jitterpug.Geom as Geom
import Jitterpug.Test.Util (approxEqListV2)
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  Tasty.testGroup
    "Jitterpug.CMJ"
    [testProperty "unit: cmj matches the C version for some examples" unit_cmj]

---- Unit tests

-- | Compare 'CMJ.cmj' against the C version.
--
-- We have some examples of what the C version of the @cmj@ function produces.
-- This unit test compares those values against the Haskell version.
--
-- The example used here is completely arbitrary.
unit_cmj :: Property
unit_cmj = withTests 1 $ property $ do
  let result :: [V2 Float]
      result = CMJ.cmj 1 15 10 (5.0 / 3.0) <$> [0 .. 14]
      expected :: [V2 Float]
      expected =
        [ Geom.v2 0.407937 0.252455,
          Geom.v2 0.745973 0.696648,
          Geom.v2 0.318336 0.406599,
          Geom.v2 0.396900 0.773642,
          Geom.v2 0.115079 0.467268,
          Geom.v2 0.229177 0.108847,
          Geom.v2 0.937797 0.958067,
          Geom.v2 0.711002 0.387237,
          Geom.v2 0.816166 0.332471,
          Geom.v2 0.646504 0.060028,
          Geom.v2 0.050653 0.194986,
          Geom.v2 0.162821 0.825038,
          Geom.v2 0.590812 0.872327,
          Geom.v2 0.875589 0.662715,
          Geom.v2 0.479629 0.555141
        ]
      eps :: Float
      eps = 1e-6
  annotateShow result
  assert $ approxEqListV2 eps expected result
