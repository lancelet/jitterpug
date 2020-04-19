module Main
    ( main
    )
where

import           Test.Tasty                     ( TestTree )
import qualified Test.Tasty                    as Tasty

import qualified Jitterpug.CMJ.Tests
import qualified Jitterpug.PRNG.Tests

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup
    "Tests"
    [Jitterpug.CMJ.Tests.tests, Jitterpug.PRNG.Tests.tests]
