module LiterateBinary
    ( markdownCode
    , compile
    ) where

import Control.Applicative (liftA2)
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decode)
import Data.ByteString.Builder
       (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Semigroup (stimes)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Text.Pandoc as P
import Text.Pandoc.Walk (query)
import Text.Parsec

-- | Extract content from code blocks in a Markdown document.
markdownCode :: T.Text -> Either T.Text T.Text
markdownCode =
    bimap (cs . show) (T.unlines . query blocks) . P.readMarkdown P.def . cs
  where
    blocks (P.CodeBlock (_, classes, _) code)
        | "nobin" `elem` classes = []
        | otherwise = [cs code :: T.Text]
    blocks _ = []

-- | Convert hex string to bit stream, with macros expanded.
compile :: T.Text -> Either T.Text BL.ByteString
compile t = parseHex t >>= eval

-- | AST data type for hex string parsing.
data HexString
    = HexLiteral T.Text
    | HexMacro [HexString]
               Int

-- | Parse hex string including macros, creating an AST.
parseHex :: T.Text -> Either T.Text [HexString]
parseHex = first (cs . show) . parse hexStrings "" . removeComments

-- | Remove comments (# ...) and whitespace including line breaks.
removeComments :: T.Text -> T.Text
removeComments = remWhitespace . remComments
  where
    remWhitespace = T.filter (not . isSpace)
    remComments = T.unlines . map (T.takeWhile (/= '#')) . T.lines

-- | Parse a hex string like "ff((01){4}aa){3}e0".
hexStrings :: Parsec T.Text () [HexString]
hexStrings = many hexString <* eof

-- | Parse a hex string like "ff" or "((01){4}aa){3}".
hexString :: Parsec T.Text () HexString
hexString = hexLiteral <|> hexMacro

-- | Parse a hex literal like "ff".
hexLiteral :: Parsec T.Text () HexString
hexLiteral = HexLiteral . cs <$> many1 hexDigit

-- | Parse a hex macro like "((01){4}aa){3}".
hexMacro :: Parsec T.Text () HexString
hexMacro =
    HexMacro <$> (char '(' *> many hexString <* char ')') <*>
    (read <$> (char '{' *> many1 digit <* char '}'))

-- | Synthesize bit stream from AST.
eval :: [HexString] -> Either T.Text BL.ByteString
eval = fmap toLazyByteString . eval'

-- | Create ByteString builder from AST.
eval' :: [HexString] -> Either T.Text Builder
eval' = foldr (liftA2 mappend . e) (Right mempty)
  where
    e (HexLiteral t) = byteString <$> bytesFromHex t
    e (HexMacro hs n) = stimes n <$> eval' hs

-- | Convert hex string to bit stream.
bytesFromHex :: T.Text -> Either T.Text BS.ByteString
bytesFromHex t =
    let (bytes, err) = decode $ cs t
    in if BS.null err
           then Right bytes
           else Left . cs $ show err
