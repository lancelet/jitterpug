module Main where

import Graphics.Jitterpug.Diagrams.CanonicalPixelSamples (renderCanonicalPixelSamplesDiagram)

main :: IO ()
main = do
    putStrLn "Hello, Haskell!"
    renderCanonicalPixelSamplesDiagram "./doc/canonical-pixel-samples.svg"


