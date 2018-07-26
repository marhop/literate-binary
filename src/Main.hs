module Main where

import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decode)
import Data.Char (isSpace)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment
import qualified Text.Pandoc as P
import Text.Pandoc.Walk (query)

main :: IO ()
main = do
    file <- head <$> getArgs
    text <- TIO.readFile file
    either TIO.putStr BS.putStr $ compile text

-- | Make bit stream from hex string in Markdown code blocks.
compile :: T.Text -> Either T.Text BS.ByteString
compile t = removeComments <$> markdownCode t >>= bytesFromHex

-- | Extract content from code blocks in a Markdown document.
markdownCode :: T.Text -> Either T.Text T.Text
markdownCode =
    bimap (cs . show) (T.unlines . query blocks) . P.readMarkdown P.def . cs
  where
    blocks (P.CodeBlock _ s) = [cs s :: T.Text]
    blocks _ = []

-- | Remove comments and whitespace including line breaks.
--
-- Comments start with '#' and end at end of line.
removeComments :: T.Text -> T.Text
removeComments = remWhitespace . remComments
  where
    remWhitespace = T.filter (not . isSpace)
    remComments = T.unlines . map (T.takeWhile (/= '#')) . T.lines

-- | Expand macros in a hex string. None for now.
expandHexMacros :: T.Text -> Either T.Text T.Text
expandHexMacros = Right

-- | Convert hex string to bit stream.
bytesFromHex :: T.Text -> Either T.Text BS.ByteString
bytesFromHex t =
    let (bytes, err) = decode $ cs t
    in if BS.null err
           then Right bytes
           else Left . cs $ show err
