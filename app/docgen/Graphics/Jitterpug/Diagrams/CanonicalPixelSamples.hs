{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
module Graphics.Jitterpug.Diagrams.CanonicalPixelSamples
    ( renderCanonicalPixelSamplesDiagram
    ) where

import qualified Graphics.Jitterpug.CMJS     as CMJS

import           Control.Monad.ST            (runST)
import qualified Data.Colour.Names           as C
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import           Diagrams.Backend.SVG        (B, renderSVG)
import           Diagrams.Prelude            (Diagram, ( # ), (^&))
import qualified Diagrams.Prelude            as D
import           Linear.V2                   (V2 (V2))
import qualified System.Random.MWC           as MWC
import qualified Graphics.SVGFonts as SF
import qualified Graphics.SVGFonts.ReadFont as SF (PreparedFont)


-- | IO action to render the canonical N-rooks sampling arrangement.
renderCanonicalPixelSamplesDiagram
    :: FilePath  -- ^ Path of the file to write.
    -> IO ()
renderCanonicalPixelSamplesDiagram filePath = do
    font <- SF.loadFont "resources/KaTeX_Math-Italic.svg"
    let diagram = canonicalPixelSamplesDiagram font
    renderSVG filePath (D.dims (V2 400 400)) diagram


-- | Diagram showing the canonical N-rooks sampling arrangement.
--
--   This diagram is intended to show the same as Figure 1 from:
--     Kensler, A (2013) Correlated Multi-JIttered Sampling,
--       Pixar Technical Memo 13-01.
canonicalPixelSamplesDiagram
    :: SF.PreparedFont Double
    -> Diagram B
canonicalPixelSamplesDiagram font =
    D.frame 0.1
    $ D.scaleY (-1)
    $ mconcat
    [ circles       # D.lw D.none # D.fc thickColor
    , allThickLines # D.lwG thickWidth # D.lc thickColor
    , allThinLines  # D.lwG thinWidth  # D.lc thinColor
    , xAxis         # D.lwG thickWidth
    , yAxis         # D.lwG thickWidth
    , xAxisLabel    # D.lw D.none # D.fc thickColor
    , yAxisLabel    # D.lw D.none # D.fc thickColor
    ]
  where
    thickWidth = 0.004
    thinWidth  = 0.6 * thickWidth
    thickColor = C.black
    thinColor  = C.lightgrey
    n          = 5
    samples    = exampleCPS n
    nf         = fromIntegral n

    -- The sample position circles.
    circleRadius = 0.01
    circle (V2 x y) =
        D.circle circleRadius
        # D.translate (realToFrac x ^& realToFrac y)
    circles = mconcat $ circle <$> U.toList samples

    -- The thick black lines.
    usq =
        D.strokeLoop
        . D.glueLine
        . D.fromVertices
        $ D.p2
        <$> [ (0,0), (0,1), (1,1), (1,0), (0,0) ]
    vThickLine x = D.fromVertices $ D.p2 <$> [ (x / nf, 0), (x / nf, 1) ]
    hThickLine y = D.fromVertices $ D.p2 <$> [ (0, y / nf), (1, y / nf) ]
    allThickLines =
        mconcat
        $ [ usq # D.lineJoin D.LineJoinRound ]
        ++ (vThickLine <$> [ 1 .. n ])
        ++ (hThickLine <$> [ 1 .. n ])

    -- Axis labels.
    axisGap = 0.02
    axisArrowLen = 0.2
    fontSize = 0.08
    xAxis = D.arrowBetween ((1 + axisGap) ^& 0) ((1 + axisArrowLen) ^& 0)
    yAxis = D.arrowBetween (0 ^& (1 + axisGap)) (0 ^& (1 + axisArrowLen))
    xAxisLabel =
        D.strokeP $ SF.textSVG' (SF.TextOpts font SF.INSIDE_H SF.KERN False fontSize fontSize) "x"
          # D.scaleY (-1)
          # D.translate ((1 + axisArrowLen + 2 * axisGap) ^& 0)
    yAxisLabel =
        D.strokeP $ SF.textSVG' (SF.TextOpts font SF.INSIDE_H SF.KERN False fontSize fontSize) "y"
          # D.scaleY (-1)
          # D.translate (0 ^& (1 + axisArrowLen + 2 * axisGap))

    -- The thin grey lines.
    nf2 = nf * nf
    vThinLine x = D.fromVertices $ D.p2 <$> [ (x / nf2, 0), (x / nf2, 1) ]
    hThinLine y = D.fromVertices $ D.p2 <$> [ (0, y / nf2), (1, y / nf2) ]
    allThinLines =
        mconcat
        $  (vThinLine <$> [ 1 .. (n*n) ])
        ++ (hThinLine <$> [ 1 .. (n*n) ])


-- | Example Canonical Pixel Samples.
--
--   This function generates samples within a pixel according to the canonical
--   N-rooks sampling arrangement.
--
--   Samples are jittered over a slightly-reduced domain so that they don't
--   overlap too much with the lines.
exampleCPS
    :: Int
    -> U.Vector (V2 Float)
exampleCPS n = runST $ do
    xs  <- UM.unsafeNew (n * n)
    gen <- MWC.create
    let rand = MWC.uniformR (0.25, 0.75) gen
    CMJS.canonicalPixelSamples rand xs n
    U.unsafeFreeze xs
