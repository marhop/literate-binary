{-# LANGUAGE OverloadedStrings #-}

module LiterateBinary
    ( markdownCode
    , compile
    , Error
    , showError
    ) where

import Control.Monad.State (State, evalState, state)
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
import Text.Parsec hiding (State)
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
compile g t = eval g <$> parseHex t

-- | AST data type for hex string parsing.
type HexTree = [HexString]

data HexString
    = Literal BS.ByteString
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
literal = Literal . bytes <$> many2 hexDigit
  where
    bytes :: String -> BS.ByteString
    bytes s =
        let (bs, err) = decode (cs s)
        in if BS.null err
               then bs
               else error "Unexpected error converting hex to bytes."

-- | Apply a parser an even number of times, at least twice. This is like
-- @Parsec.many1@, but the parser is not applied one or more, but two or four or
-- six or ... times.
many2 :: Stream s m t => ParsecT s u m a -> ParsecT s u m [a]
many2 p = do
    x1 <- p
    x2 <- p
    xs <- option [] (many2 p)
    return (x1 : x2 : xs)

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
eval :: RandomGen g => g -> HexTree -> BL.ByteString
eval g t = BSB.toLazyByteString $ evalState (eval' t) g

-- | Create ByteString builder from AST.
eval' :: RandomGen g => HexTree -> State g BSB.Builder
eval' [Literal x] = return $ BSB.byteString x
eval' [Repetition _ 0] = return mempty
eval' [Repetition t n] = do
    b1 <- eval' t
    b2 <- eval' [Repetition t (n - 1)]
    return (b1 <> b2)
eval' [Alternative ts] = do
    t <- randomL ts
    maybe (return mempty) eval' t
eval' [] = return mempty
eval' (x:xs) = do
    b1 <- eval' [x]
    b2 <- eval' xs
    return (b1 <> b2)

-- | Take a random element from a list.
randomL :: RandomGen g => [a] -> State g (Maybe a)
randomL [] = return Nothing
randomL xs = do
    i <- state $ randomR (0, length xs - 1)
    return $ Just (xs !! i)

-- | Data type for error messages.
data Error
    = MkdParseError { pandocErr :: P.PandocError }
    | HexParseError { src :: T.Text
                    , parsecErr :: ParseError }

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

-- | Quote a text using double quotes. No escaping!
quote :: T.Text -> T.Text
quote t = "\"" <> t <> "\""

-- | Create a text like "\n    ^" that can be used as a mark.
mark :: Int -> T.Text
mark i
    | i < 1 = "\n^"
    | otherwise = "\n" <> stimes i " " <> "^"
