module Jitterpug.ImageFn where

newtype ImageFn c = ImageFn {unImageFn :: Float -> Float -> c}
