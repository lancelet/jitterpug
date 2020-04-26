module Jitterpug.PRNG.Bench
  ( benchmarks,
  )
where

import Criterion (Benchmark)
import qualified Criterion
import qualified Jitterpug.PRNG as PRNG

benchmarks :: Benchmark
benchmarks =
  Criterion.bgroup "Jitterpug.PRNG" [randFloatBench, permuteIndexWord32Bench]

randFloatBench :: Benchmark
randFloatBench =
  Criterion.bench "randFloat 42 19" $ Criterion.nf (PRNG.randFloat 42) 19

permuteIndexWord32Bench :: Benchmark
permuteIndexWord32Bench =
  Criterion.bench "permuteIndexWord32 42 17 11" $
    Criterion.nf (PRNG.unIndex . PRNG.permuteIndex 42 17) 11
