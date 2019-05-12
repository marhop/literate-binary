module LiterateBinary
    ( compile
    , compileIO
    , compilePlain
    , compilePlainIO
    , Error
    , showError
    ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.Random (RandomGen, newStdGen)

import LiterateBinary.Eval (eval)
import LiterateBinary.Parse (Error, parseHex, parseMarkdown, showError)

-- | Convert hex string in Markdown code blocks to bit stream, with macros
-- expanded.
compile :: RandomGen g => g -> T.Text -> Either Error BL.ByteString
compile g t = parseMarkdown t >>= compilePlain g

-- | Convert hex string in Markdown code blocks to bit stream, with macros
-- expanded.
compileIO :: T.Text -> IO (Either Error BL.ByteString)
compileIO t = flip compile t <$> newStdGen

-- | Convert hex string to bit stream, with macros expanded.
compilePlain :: RandomGen g => g -> T.Text -> Either Error BL.ByteString
compilePlain g t = eval g <$> parseHex t

-- | Convert hex string to bit stream, with macros expanded.
compilePlainIO :: T.Text -> IO (Either Error BL.ByteString)
compilePlainIO t = flip compilePlain t <$> newStdGen
