module Main where

import qualified Data.ByteString.Lazy as BL
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.IO (stderr)

import LiterateBinary (Error, compileIO, compilePlainIO, showError)

main :: IO ()
main = getOpts >>= runCompiler

-- | Data type for command line options.
data Options = Options
    { optInput :: Maybe FilePath
    , optOutput :: Maybe FilePath
    , optPlain :: Bool
    }

-- | Parse command line options.
getOpts :: IO Options
getOpts =
    execParser $ info (helper <*> version <*> options) (fullDesc <> header hdr)
  where
    hdr = "lb - literate binary compiler"
    version = infoOption "1.1.0" (long "version" <> help "Show version number")
    options = Options <$> input <*> output <*> plain
    input =
        optional $
        argument str (metavar "INPUT" <> help "Input file (default STDIN)")
    output =
        optional $
        strOption
            (long "output" <> short 'o' <> metavar "OUTPUT" <>
             help "Output file (default STDOUT)")
    plain =
        switch
            (long "plain" <> short 'p' <> help "Input is just hex, no Markdown")

-- | Compile input based on command line options.
runCompiler :: Options -> IO ()
runCompiler opts =
    readInput opts >>=
    (if optPlain opts
         then compilePlainIO
         else compileIO) >>=
    either writeError (writeOutput opts)

-- | Read input from a file or from STDIN.
readInput :: Options -> IO T.Text
readInput = maybe TIO.getContents TIO.readFile . optInput

-- | Write output to a file or to STDOUT.
writeOutput :: Options -> BL.ByteString -> IO ()
writeOutput = maybe BL.putStr BL.writeFile . optOutput

-- | Write error to STDERR.
writeError :: Error -> IO ()
writeError = TIO.hPutStrLn stderr . showError
