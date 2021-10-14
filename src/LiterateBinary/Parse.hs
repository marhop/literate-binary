{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module     : LiterateBinary.Parse
-- Copyright  : (c) Martin Hoppenheit 2019-2020
-- License    : MIT
-- Maintainer : martin@hoppenheit.info
--
-- Parser functions for "LiterateBinary".

module LiterateBinary.Parse
    ( parseMarkdown
    , parseHex
    , Error
    , showError
    ) where

import CMark (Node(..), NodeType(CODE_BLOCK), commonmarkToNode)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decodeBase16)
import Data.ByteString.UTF8 (fromString)
import Data.Char (toLower)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Error (errorMessages, showErrorMessages)

import LiterateBinary.HexTree (HexString(..), HexTree)

-- | Extract content from code blocks in a Markdown document.
parseMarkdown :: T.Text -> T.Text
parseMarkdown = T.unlines . blocks . commonmarkToNode []
  where
    blocks :: Node -> [T.Text]
    blocks (Node _ (CODE_BLOCK info code) _)
        | ".nobin" `T.isInfixOf` info = []
        | otherwise = [code]
    blocks (Node _ _ xs) = concatMap blocks xs

-- | Parse hex string including macros, creating an AST.
parseHex :: T.Text -> Either Error HexTree
parseHex t = first (HexParseError t) $ parse hexFile "" t

-- | Parse a (possibly empty) hex string like @ff((01){4}aa|2241){3}e0@,
-- terminated by EOF.
hexFile :: Parsec T.Text () HexTree
hexFile = option [] hexTree <* eof

-- | Parse a hex string like @ff((01){4}aa|2241){3}e0@.
hexTree :: Parsec T.Text () HexTree
hexTree = ignorable *> many1 (hexString <* ignorable)

-- | Skip a (possibly empty) sequence of whitespace and comments (@# ...@).
ignorable :: Parsec T.Text u ()
ignorable = skipMany (space <|> comment)
  where
    comment = char '#' *> many (noneOf "\r\n") *> endOfLine

-- | Parse a hex string like @ff@ or @((01){4}aa|2241){3}@.
hexString :: Parsec T.Text () HexString
hexString = hexLiteral <|> strLiteral <|> parenExpr <|> dot

-- | Parse a hex literal like @ff@.
hexLiteral :: Parsec T.Text () HexString
hexLiteral = Literal . bytes <$> many2 (hexDigit <* ignorable)
  where
    bytes :: String -> BS.ByteString
    bytes s = either (error . (<> ", " <> s) . cs) id $ decodeBase16 (cs s)

-- | Apply a parser an even number of times, at least twice. This is like
-- @Parsec.many1@, but the parser is not applied one or more, but two or four or
-- six or ... times.
many2 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
many2 p = do
    x1 <- p
    x2 <- p
    xs <- option [] (many2 p)
    return (x1 : x2 : xs)

-- | Parse a quoted string literal like @"ASCII string"@ or @'ASCII string'@ and
-- an optional quantifier. Both single and double quotes are allowed. The quote
-- sign itself may appear inside the string escaped by a backslash like @\"@, a
-- literal backslash has to be escaped by another backslash like @\\@. The
-- string will become a UTF-8 encoded ByteString wrapped in a 'Literal'.
strLiteral :: Parsec T.Text () HexString
strLiteral = quantified (pure <$> (quoted '"' <|> quoted '\''))

-- | Parse a string surrounded by a given quote sign and turn it into a UTF-8
-- encoded ByteString wrapped in a 'Literal'. The quote sign itself may appear
-- inside the string escaped by a backslash, a literal backslash has to be
-- escaped by another backslash.
quoted :: Char -> Parsec T.Text () HexString
quoted q =
    Literal . fromString <$>
    (char q *> many (escape q <|> noneOf [q]) <* char q)

-- | Parse a given character preceded (i.e., escaped) by a backslash, or a
-- double backslash (i.e., an escaped backslash).
escape :: Char -> Parsec T.Text () Char
escape c = char '\\' *> (char c <|> char '\\')

-- | Parse an expression in parentheses and an optional quantifier: a repetition
-- like @(ff01){3}@, an alternative like @(aa|ff01)@ or a range like @(00-ff)@.
-- All forms may be mixed and nested.
parenExpr :: Parsec T.Text () HexString
parenExpr = quantified (char '(' *> innerParenExpr <* char ')')

-- | Parse the content of a paren expression: a hex string like @aa@,
-- optionally followed by a tail like @|bb@ or @-cc@, denoting an alternative or
-- a range respectively.
innerParenExpr :: Parsec T.Text () HexTree
innerParenExpr =
    combine <$> hexTree <*> optionMaybe (alternativeTail <|> rangeTail)
  where
    combine :: HexTree -> Maybe HexString -> HexTree
    combine t Nothing = t
    combine t (Just (Alternative ts)) = [Alternative (t : ts)]
    combine t (Just (Range _ t')) = [Range t t']
    combine _ _ = error "Unexpected error parsing paren expression."

-- | Parse the "tail" of an alternative like @|ff01@ or @|ff01|aa@.
alternativeTail :: Parsec T.Text () HexString
alternativeTail = Alternative <$> (char '|' *> hexTree `sepBy1` char '|')

-- | Parse the "tail" of a range like @-ff@. Returns a 'Range' with the first
-- component set to a dummy value, an empty HexTree.
rangeTail :: Parsec T.Text () HexString
rangeTail = Range [] <$> (char '-' *> hexTree)

-- | Parse a single dot and an optional quantifer like @.@ or @.{3}@. This is an
-- alias for the range expression @(00-ff)@ which denotes one random byte.
dot :: Parsec T.Text () HexString
dot = quantified ([Byte] <$ char '.')

-- | Combine a parser with an optional trailing quantifier like @{3}@.
quantified :: Parsec T.Text u HexTree -> Parsec T.Text u HexString
quantified p = combine <$> p <* ignorable <*> option 1 quantifier
  where
    combine :: HexTree -> Int -> HexString
    combine [x] 1 = x
    combine t n = Repetition t n

-- | Parse a quantifier like @{3}@ or @{32K}@, with optional suffix K (factor
-- 2^10), M (factor 2^20) or G (factor 2^30).
quantifier :: Parsec T.Text u Int
quantifier =
    combine <$> (char '{' *> ignorable *> number) <*>
    (suffix <* ignorable <* char '}')
  where
    number :: Parsec T.Text u String
    number = many1 (digit <* ignorable)
    suffix :: Parsec T.Text u (Maybe Char)
    suffix = optionMaybe (oneOf "kmgKMG")
    combine :: String -> Maybe Char -> Int
    combine s Nothing = read s
    combine s (Just c) =
        read s *
        case toLower c of
            'k' -> 1024
            'm' -> 1048576
            'g' -> 1073741824
            _ -> error "Unexpected error parsing quantifier."

-- | Data type for parser error messages (source + error).
data Error = HexParseError T.Text ParseError

-- | Format a parser error message.
showError :: Error -> T.Text
showError (HexParseError t e) = lbl <> src <> parsecMsg
  where
    lbl = "invalid syntax in hex string "
    src = quote $ T.lines t !! (sourceLine (errorPos e) - 1)
    parsecMsg =
        cs .
        showErrorMessages
            "or"
            "unknown parse error"
            "expecting"
            "unexpected"
            "end of input" $
        errorMessages e

-- | Quote a text using single quotes. No escaping!
quote :: T.Text -> T.Text
quote t = "'" <> t <> "'"
