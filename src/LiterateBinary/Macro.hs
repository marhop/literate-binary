module LiterateBinary.Macro
    ( expandHexMacros
    ) where

import Data.Bifunctor (bimap)
import Data.String.Conversions (cs)
import qualified Data.Text as T
import Text.Parsec

-- | Expand macros in a hex string.
expandHexMacros :: T.Text -> Either T.Text T.Text
expandHexMacros t = bimap (cs . show) eval $ parse hexStrings "" t

-- | AST data type for hex string parsing.
data HexString
    = HexLiteral T.Text
    | HexMacro [HexString]
               Int

-- | Synthesize hex string from AST, with macros expanded.
eval :: [HexString] -> T.Text
eval = T.concat . map e
  where
    e (HexLiteral t) = t
    e (HexMacro hs n) = T.replicate n $ eval hs

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
