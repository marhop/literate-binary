module LiterateBinary
    ( compile
    ) where

import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import Data.Char (isSpace)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import LiterateBinary.Macro (bytesFromHexWithMacrosExpanded)
import qualified Text.Pandoc as P
import Text.Pandoc.Walk (query)

-- | Make bit stream from hex string in Markdown code blocks.
compile :: T.Text -> Either T.Text BS.ByteString
compile t = removeComments <$> markdownCode t >>= bytesFromHexWithMacrosExpanded

-- | Extract content from code blocks in a Markdown document.
markdownCode :: T.Text -> Either T.Text T.Text
markdownCode =
    bimap (cs . show) (T.unlines . query blocks) . P.readMarkdown P.def . cs
  where
    blocks (P.CodeBlock (_, classes, _) code)
        | "nobin" `elem` classes = []
        | otherwise = [cs code :: T.Text]
    blocks _ = []

-- | Remove comments and whitespace including line breaks.
--
-- Comments start with '#' and end at end of line.
removeComments :: T.Text -> T.Text
removeComments = remWhitespace . remComments
  where
    remWhitespace = T.filter (not . isSpace)
    remComments = T.unlines . map (T.takeWhile (/= '#')) . T.lines
