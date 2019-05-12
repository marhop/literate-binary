module LiterateBinary.HexTree
    ( HexTree
    , HexString(..)
    ) where

import qualified Data.ByteString as BS

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
