module Jitterpug.Filter where

import           Jitterpug.Geom                 ( Size
                                                , V2
                                                )

newtype FilterRelV2 s = FilterRelV2 { unFilterRelV2 :: V2 s }

data Filter s
  = Filter
    { filterSize   :: Size s
    , filterWeight :: FilterRelV2 s -> s
    }
