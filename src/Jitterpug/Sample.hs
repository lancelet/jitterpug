{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Jitterpug.Sample
    ( SampleOps(SampleOps, sampleOpsScale, sampleOpsAdd)
    , Jittering(Jittering, unJittering)
    )
where

-- Based on:
--
--   Kensler (2013) Correlated Multi-Jittered Sampling.
--     Pixar Technical Memo 13-01.


-- | A vector space for samples.
data SampleOps s v
  = SampleOps
    { sampleOpsScale :: s -> v -> v
    , sampleOpsAdd   :: v -> v -> v
    }

newtype Jittering = Jittering { unJittering :: Float }
