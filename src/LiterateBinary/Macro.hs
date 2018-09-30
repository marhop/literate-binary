module LiterateBinary.Macro
    ( bytesFromHexWithMacrosExpanded
    ) where

import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (decode)
import Data.Semigroup (stimes)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Text.Parsec

-- | Convert hex string to bit stream, with macros expanded.
bytesFromHexWithMacrosExpanded :: T.Text -> Either T.Text BS.ByteString
bytesFromHexWithMacrosExpanded t =
    first (cs . show) (parse hexStrings "" t) >>= eval

-- | AST data type for hex string parsing.
data HexString
    = HexLiteral T.Text
    | HexMacro [HexString]
               Int

-- | Synthesize bit stream from AST, with macros expanded.
eval :: [HexString] -> Either T.Text BS.ByteString
eval = foldr (\x acc -> BS.append <$> e x <*> acc) (Right BS.empty)
  where
    e (HexLiteral t) = bytesFromHex t
    e (HexMacro hs n) = stimes n <$> eval hs

-- | Convert hex string to bit stream.
bytesFromHex :: T.Text -> Either T.Text BS.ByteString
bytesFromHex t =
    let (bytes, err) = decode $ cs t
    in if BS.null err
           then Right bytes
           else Left . cs $ show err

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
