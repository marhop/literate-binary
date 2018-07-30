module Main where

import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import LiterateBinary
import System.Environment

main :: IO ()
main = do
    file <- head <$> getArgs
    text <- TIO.readFile file
    either TIO.putStr BS.putStr $ compile text
