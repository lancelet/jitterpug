module Jitterpug where

import           Jitterpug.Bucket               ( BucketSize )
import           Jitterpug.Filter               ( Filter )
import           Jitterpug.Image                ( Image
                                                , ImageSize
                                                )
import           Jitterpug.Sample               ( SampleOps )

reconstruct2D :: SampleOps s v -> ImageSize -> BucketSize -> Filter s -> Image v
reconstruct2D = undefined
