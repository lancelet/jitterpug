module Main (main) where

import qualified Test.DocTest

main :: IO ()
main =
  Test.DocTest.doctest
    [ "-isrc",
      "src/Jitterpug/Hashable.hs",
      "src/Jitterpug/PRNG.hs",
      "src/Jitterpug/SampleGen.hs"
    ]
