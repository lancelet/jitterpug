module Main
  ( main,
  )
where

import qualified Jitterpug.PRNG.Tests
import Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests =
  Tasty.testGroup
    "Tests"
    [Jitterpug.PRNG.Tests.tests]
