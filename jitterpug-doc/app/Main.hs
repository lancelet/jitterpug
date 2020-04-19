module Main
    ( main
    )
where

import qualified Data.Text                     as Text

import qualified Tintin
import qualified Tintin.Capabilities.Filesystem
import qualified Tintin.Capabilities.Logging
import qualified Tintin.Capabilities.Process
import qualified Tintin.Core
import           Tintin.Domain                  ( OutputDirectory
                                                    ( OutputDirectory
                                                    )
                                                )

import qualified Jitterpug.Doc.CMJPixel        as CMJPixel

main :: IO ()
main = do
    let dir :: String
        dir = ".stack-work/tintin/rendered/"

        outputDirectory :: OutputDirectory
        outputDirectory = OutputDirectory (Text.pack dir)

    -- run Tintin to do normal documentation generation
    putStrLn "Running Tintin"
    runTintin outputDirectory

    -- render out all the images
    putStrLn "Rendering images"
    renderImages outputDirectory

    putStrLn $ "Documentation was written to: " <> dir

runTintin :: OutputDirectory -> IO ()
runTintin outputDirectory = do
    let logger :: Tintin.Capabilities.Logging.Capability
        logger = Tintin.Capabilities.Logging.stdOut

        filesystem :: Tintin.Capabilities.Filesystem.Capability
        filesystem = Tintin.Capabilities.Filesystem.local

        process :: Tintin.Capabilities.Process.Capability
        process = Tintin.Capabilities.Process.local

    -- first, run tintin to generate the docs
    Tintin.Core.runEffects (Tintin.runApp False outputDirectory)
                           (logger, filesystem, process)

renderImages :: OutputDirectory -> IO ()
renderImages outputDirectory = do
    let OutputDirectory dirTxt = outputDirectory
        dir                    = Text.unpack dirTxt

    CMJPixel.render5x5Pixel (dir <> "cmj-5x5-pixel.svg")
    CMJPixel.render5x3Pixel (dir <> "cmj-5x3-pixel.svg")
