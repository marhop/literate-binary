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
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Semigroup ((<>), stimes)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import System.Random (RandomGen, randomR)
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
compile :: RandomGen g => g -> T.Text -> Either Error BL.ByteString
compile g t = parseHex t >>= eval g

-- | AST data type for hex string parsing.
type HexTree = [HexString]

data HexString
    = Literal T.Text
    | Repetition HexTree
                 Int
    | Alternative [HexTree]

-- | Parse hex string including macros, creating an AST.
parseHex :: T.Text -> Either Error HexTree
parseHex t = first (HexParseError t) . parse hexTree "" $ removeComments t

-- | Remove comments (# ...) and whitespace including line breaks.
removeComments :: T.Text -> T.Text
removeComments = remWhitespace . remComments
  where
    remWhitespace = T.filter (not . isSpace)
    remComments = T.unlines . map (T.takeWhile (/= '#')) . T.lines

-- | Parse a hex string like "ff((01){4}aa|2241){3}e0".
hexTree :: Parsec T.Text () HexTree
hexTree = many hexString <* eof

-- | Parse a hex string like "ff" or "((01){4}aa|2241){3}".
hexString :: Parsec T.Text () HexString
hexString = literal <|> parenExpr

-- | Parse a hex literal like "ff".
literal :: Parsec T.Text () HexString
literal = Literal . cs <$> many1 hexDigit

-- | Parse an expression in parentheses and an optional quantifier: a repetition
-- like "(ff01){3}" or an alternative like "(aa|ff01)". Both forms may be mixed
-- and nested.
parenExpr :: Parsec T.Text () HexString
parenExpr = mkHexString <$> alternative <*> option 1 quantifier
  where
    mkHexString :: [HexTree] -> Int -> HexString
    mkHexString [x] n = Repetition x n
    mkHexString xs 1 = Alternative xs
    mkHexString xs n = Repetition [Alternative xs] n

-- | Parse a sequence of hex strings in parentheses, separated by "|"
-- characters. Note that this may also be a one element sequence i.e., a single
-- hex string in parentheses.
alternative :: Parsec T.Text () [HexTree]
alternative = char '(' *> many1 hexString `sepBy1` char '|' <* char ')'

-- | Parse a quantifier like "{3}".
quantifier :: Parsec T.Text () Int
quantifier = read <$> (char '{' *> many1 digit <* char '}')

-- | Synthesize bit stream from AST.
eval :: RandomGen g => g -> HexTree -> Either Error BL.ByteString
eval g = fmap BSB.toLazyByteString . fst . eval' g

-- | Create ByteString builder from AST.
eval' :: RandomGen g => g -> HexTree -> (Either Error BSB.Builder, g)
eval' g =
    foldr
        (\x (acc, g) -> first (\x -> liftA2 mappend x acc) $ e g x)
        (Right mempty, g)
  where
    e :: RandomGen g => g -> HexString -> (Either Error BSB.Builder, g)
    e g (Literal t) = (BSB.byteString <$> bytesFromHex t, g)
    -- e g (Repetition x n) = first (stimes n) $ eval' g x
    e g (Repetition x n) = fromRep g x n
    e g (Alternative xs) = fromAlt g xs

-- | Create ByteString builder from Repetition.
fromRep ::
       RandomGen g => g -> HexTree -> Int -> (Either Error BSB.Builder, g)
fromRep g = go (Right mempty, g)
  where
    go (acc, g) _ 0 = (acc, g)
    go (acc, g) x n = go (first (liftA2 mappend acc) $ eval' g x) x (n - 1)

-- | Create ByteString builder from Alternative.
fromAlt :: RandomGen g => g -> [HexTree] -> (Either Error BSB.Builder, g)
fromAlt g xs = go $ randomL xs g
  where
    go (Just x, g) = eval' g x
    go (Nothing, g) = (Right mempty, g)

-- | Convert hex string to bit stream.
bytesFromHex :: T.Text -> Either Error BS.ByteString
bytesFromHex t
    | odd (T.length t) = Left $ OddCharsError t
    | not (BS.null err) = Left $ ByteConvError t (BS.length bytes * 2)
    | otherwise = Right bytes
  where
    (bytes, err) = decode $ cs t

-- | Take a random element from a list.
randomL :: RandomGen g => [a] -> g -> (Maybe a, g)
randomL [] g = (Nothing, g)
randomL xs g =
    let (i, g') = randomR (0, length xs - 1) g
    in (Just (xs !! i), g')

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
