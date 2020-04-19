module Main
    ( main
    )
where

import qualified Criterion
import qualified Criterion.Main

import qualified Jitterpug.PRNG.Bench

main :: IO ()
main = Criterion.Main.defaultMain
    [Criterion.bgroup "Jitterpug" [Jitterpug.PRNG.Bench.benchmarks]]
