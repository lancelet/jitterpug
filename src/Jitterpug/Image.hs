module Jitterpug.Image where

import           Jitterpug.Geom                 ( Size
                                                , V2
                                                )

newtype ImageSize = ImageSize { unImageSize :: Size Int }

data Image v =
  Image
  { imageSize  :: ImageSize
  , imagePixel :: V2 Int -> v
  }
