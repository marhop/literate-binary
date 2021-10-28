-- |
-- Module     : LiterateBinary.HexTree
-- Copyright  : (c) Martin Hoppenheit 2019-2021
-- License    : MIT
-- Maintainer : martin@hoppenheit.info
--
-- AST (Abstract Syntax Tree) data types for "LiterateBinary" hex string
-- parsing.
module LiterateBinary.HexTree (HexTree, HexString (..)) where

import Data.ByteString (ByteString)

-- | The AST is just a list of 'HexString' values.
type HexTree = [HexString]

-- | A single AST element.
data HexString
  = -- | A hex literal like @00ff@.
    Literal ByteString
  | -- | A repetition like @(x){3}@, with HexTree @x@.
    Repetition HexTree Int
  | -- | An alternative like @(x|y|z)@, with HexTrees @x@, @y@, @z@.
    Alternative [HexTree]
  | -- | A range like @(x-y)@, with HexTrees @x@, @y@.
    Range HexTree HexTree
  | -- | A single random byte, represented by the special range @.@. This is
    -- equivalent to @(00-ff)@ but can be evaluated more efficiently than a
    -- general range.
    Byte
  deriving (Show, Eq)
