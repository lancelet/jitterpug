module Main
  ( main,
  )
where

import qualified Criterion
import qualified Criterion.Main

main :: IO ()
main =
  Criterion.Main.defaultMain
    []
