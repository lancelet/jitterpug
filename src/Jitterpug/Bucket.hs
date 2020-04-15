module Jitterpug.Bucket where

import           Jitterpug.Geom                 ( Size )

newtype BucketSize = BucketSize { unBucketSize :: Size Int }
