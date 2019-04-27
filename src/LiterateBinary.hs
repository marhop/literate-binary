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
    bimap MkdParseError (T.unlines . query blocks) .
    P.runPure . P.readMarkdown P.def
  where
    blocks :: P.Block -> [T.Text]
    blocks (P.CodeBlock (_, classes, _) code)
        | "nobin" `elem` classes = []
        | otherwise = [cs code]
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
    | Range HexTree
            HexTree
    deriving (Show)

-- | Parse hex string including macros, creating an AST.
parseHex :: T.Text -> Either Error HexTree
parseHex t = first (HexParseError t) . parse hexFile "" $ removeComments t

-- | Remove comments (# ...) and whitespace including line breaks.
removeComments :: T.Text -> T.Text
removeComments = remWhitespace . remComments
  where
    remWhitespace = T.filter (not . isSpace)
    remComments = T.unlines . map (T.takeWhile (/= '#')) . T.lines

-- | Parse a (possibly empty) hex string like "ff((01){4}aa|2241){3}e0",
-- terminated by EOF.
hexFile :: Parsec T.Text () HexTree
hexFile = option [] hexTree <* eof

-- | Parse a hex string like "ff((01){4}aa|2241){3}e0".
hexTree :: Parsec T.Text () HexTree
hexTree = many1 hexString

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
-- like "(ff01){3}", an alternative like "(aa|ff01)" or a range like "(00-ff)".
-- All forms may be mixed and nested.
parenExpr :: Parsec T.Text () HexString
parenExpr =
    combine <$> (char '(' *> innerParenExpr <* char ')') <*> option 1 quantifier
  where
    combine :: HexTree -> Int -> HexString
    combine [x] 1 = x
    combine t n = Repetition t n

-- | Parse the content of a paren expression: a hex string like "aa",
-- optionally followed by a tail like "|bb" or "-cc", denoting an alternative or
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

-- | Parse the "tail" of an alternative like "|ff01" or "|ff01|aa".
alternativeTail :: Parsec T.Text () HexString
alternativeTail = Alternative <$> (char '|' *> hexTree `sepBy1` char '|')

-- | Parse the "tail" of a range like "-ff". Returns a 'Range' with the first
-- component set to a dummy value, an empty HexTree.
rangeTail :: Parsec T.Text () HexString
rangeTail = Range [] <$> (char '-' *> hexTree)

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
eval' [Repetition t n]
    | any isRandom t = mappend <$> eval' t <*> eval' [Repetition t (n - 1)]
    | otherwise = stimes n <$> eval' t
eval' [Alternative ts] = randomL ts >>= maybe (return mempty) eval'
eval' [Range t1 t2] = do
    b1 <- cs . BSB.toLazyByteString <$> eval' t1
    b2 <- cs . BSB.toLazyByteString <$> eval' t2
    maybe mempty BSB.byteString <$> randomL (range b1 b2)
eval' [] = return mempty
eval' (x:xs) = mappend <$> eval' [x] <*> eval' xs

-- | Take a random element from a list.
randomL :: RandomGen g => [a] -> State g (Maybe a)
randomL [] = return Nothing
randomL xs = Just . (xs !!) <$> state (randomR (0, length xs - 1))

-- | Check if a HexString contains (possibly nested) random parts.
isRandom :: HexString -> Bool
isRandom (Literal _) = False
isRandom (Repetition t _) = any isRandom t
isRandom (Alternative _) = True
isRandom (Range _ _) = True

-- | Create a range of ByteStrings based on start and end values.
--
-- > range x y == [x, succBS x, succBS (succBS x), ..., y]
--
-- Note that the order of elements in the resulting range is generally not
-- compatible with the Ord instance for ByteStrings but with the order implied
-- by the 'lteq' and 'succBS' functions.
range :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
range x y = takeWhile (`lteq` y) $ iterate succBS x

-- | Successor function for ByteStrings. Behaves mostly like succ for a
-- ByteString interpreted as a number would do, with the exception that the
-- successor of an empty ByteString by definition is the NULL byte. In
-- particular, this affects carry operations. Examples:
--
-- > succBS 0x00 == 0x01
-- > succBS 0x0000 == 0x0001
-- > succBS 0x0100 == 0x0101
-- > succBS 0x00ff == 0x0100
-- > succBS mempty == 0x00
-- > succBS 0xff == 0x0000 /= 0x0100
succBS :: BS.ByteString -> BS.ByteString
succBS =
    maybe
        (BS.singleton 0x00)
        (\(xs, x) ->
             if x < 0xff
                 then BS.snoc xs (succ x)
                 else BS.snoc (succBS xs) 0x00) .
    BS.unsnoc

-- | Modified "less than or equal" comparison for ByteStrings that takes the
-- length of a ByteString into account. Given two ByteStrings x and y, @x `lteq`
-- y@ iff
--
--   * x is shorter than y or
--   * x and y have equal length and x ≤ y in terms of the Ord instance for
--     ByteStrings.
lteq :: BS.ByteString -> BS.ByteString -> Bool
lteq x y = (compare (BS.length x) (BS.length y) <> compare x y) `elem` [LT, EQ]

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
