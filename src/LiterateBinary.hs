{-# LANGUAGE OverloadedStrings #-}

module LiterateBinary
    ( markdownCode
    , compile
    , Error
    , showError
    ) where

import Control.Applicative (liftA2)
import Data.Bifunctor (bimap, first)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decode)
import Data.ByteString.Builder
       (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Semigroup ((<>), stimes)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import qualified Text.Pandoc as P
import Text.Pandoc.Walk (query)
import Text.Parsec
import Text.Parsec.Error (errorMessages, showErrorMessages)

-- | Extract content from code blocks in a Markdown document.
markdownCode :: T.Text -> Either Error T.Text
markdownCode =
    bimap MkdParseError (T.unlines . query blocks) . P.readMarkdown P.def . cs
  where
    blocks (P.CodeBlock (_, classes, _) code)
        | "nobin" `elem` classes = []
        | otherwise = [cs code :: T.Text]
    blocks _ = []

-- | Convert hex string to bit stream, with macros expanded.
compile :: T.Text -> Either Error BL.ByteString
compile t = parseHex t >>= eval

-- | AST data type for hex string parsing.
data HexString
    = HexLiteral T.Text
    | HexMacro [HexString]
               Int

-- | Parse hex string including macros, creating an AST.
parseHex :: T.Text -> Either Error [HexString]
parseHex t = first (HexParseError t) . parse hexStrings "" $ removeComments t

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
eval :: [HexString] -> Either Error BL.ByteString
eval = fmap toLazyByteString . eval'

-- | Create ByteString builder from AST.
eval' :: [HexString] -> Either Error Builder
eval' = foldr (liftA2 mappend . e) (Right mempty)
  where
    e (HexLiteral t) = byteString <$> bytesFromHex t
    e (HexMacro hs n) = stimes n <$> eval' hs

-- | Convert hex string to bit stream.
bytesFromHex :: T.Text -> Either Error BS.ByteString
bytesFromHex t
    | odd (T.length t) = Left $ OddCharsError t
    | not (BS.null err) = Left $ ByteConvError t (BS.length bytes * 2)
    | otherwise = Right bytes
  where
    (bytes, err) = decode $ cs t

-- | Data type for error messages.
data Error
    = MkdParseError { pandocErr :: P.PandocError }
    | HexParseError { src :: T.Text
                    , parsecErr :: ParseError }
    | OddCharsError { src :: T.Text }
    | ByteConvError { src :: T.Text
                    , pos :: Int }

-- | Format an error message.
showError :: Error -> T.Text
showError (MkdParseError e) = cs $ show e
showError (HexParseError t e) = label <> src <> mark j <> parsecMsg
  where
    label = "invalid syntax in hex string "
    i = sourceColumn (errorPos e) - 1
    j = T.length label + 1 + min i 5
    src = quote $ T.take 10 (T.drop (i - 5) $ removeComments t)
    parsecMsg =
        cs .
        showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of input" $
        errorMessages e
showError (OddCharsError t) = label <> quote t
  where
    label = "odd number of characters in hex string "
showError (ByteConvError t i) = label <> quote t <> mark j
  where
    label = "invalid character in hex string "
    j = T.length label + 1 + i

-- | Quote a text using double quotes. No escaping!
quote :: T.Text -> T.Text
quote t = "\"" <> t <> "\""

-- | Create a text like "\n    ^" that can be used as a mark.
mark :: Int -> T.Text
mark i
    | i < 1 = "\n^"
    | otherwise = "\n" <> stimes i " " <> "^"
