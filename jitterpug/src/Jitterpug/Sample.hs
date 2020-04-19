module Jitterpug.Sample
    ( Sample(Sample, samplePos, sampleValue)
    )
where

import           Control.DeepSeq                ( NFData
                                                , rnf
                                                )

import           Jitterpug.Geom                 ( V2 )

data Sample e
  = Sample
    { samplePos   :: {-# UNPACK #-} !(V2 Float)
    , sampleValue :: !e
    }

instance NFData (Sample e) where
    rnf = const ()
