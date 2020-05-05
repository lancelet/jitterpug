module Main
  ( main,
  )
where

import qualified Jitterpug.PRNG.Tests
import qualified Jitterpug.SampleGen.Tests
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests =
  Tasty.testGroup
    "Tests"
    [ Jitterpug.PRNG.Tests.tests,
      Jitterpug.SampleGen.Tests.tests
    ]
