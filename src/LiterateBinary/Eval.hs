-- |
-- Module     : LiterateBinary.Eval
-- Copyright  : (c) Martin Hoppenheit 2019-2021
-- License    : MIT
-- Maintainer : martin@hoppenheit.info
--
-- Evaluation function for "LiterateBinary" that turns a 'HexTree' AST into a
-- 'ByteString'.
module LiterateBinary.Eval (eval) where

import Control.Monad.State (State, evalState, state)
import Data.ByteString.Builder (Builder, byteString, toLazyByteString, word8)
import Data.ByteString.Enumeration (randomInRange)
import qualified Data.ByteString.Lazy as BL
import Data.Semigroup (stimes)
import Data.String.Conversions (cs)
import LiterateBinary.HexTree (HexString (..), HexTree)
import System.Random (RandomGen, random, randomR)

-- | Synthesize bit stream from AST.
eval :: RandomGen g => g -> HexTree -> BL.ByteString
eval g t = toLazyByteString $ evalState (eval' t) g

-- | Create ByteString builder from AST.
eval' :: RandomGen g => HexTree -> State g Builder
eval' [Literal x] = return $ byteString x
eval' [Repetition _ 0] = return mempty
eval' [Repetition t n]
  | any isRandom t = mappend <$> eval' t <*> eval' [Repetition t (n - 1)]
  | otherwise = stimes n <$> eval' t
eval' [Alternative ts] = randomL ts >>= maybe (return mempty) eval'
eval' [Range t1 t2] = do
  b1 <- cs . toLazyByteString <$> eval' t1
  b2 <- cs . toLazyByteString <$> eval' t2
  byteString <$> state (randomInRange (b1, b2))
eval' [Byte] = word8 <$> state random
eval' [] = return mempty
eval' (x : xs) = mappend <$> eval' [x] <*> eval' xs

-- | Check if a HexString contains (possibly nested) random parts.
isRandom :: HexString -> Bool
isRandom (Literal _) = False
isRandom (Repetition t _) = any isRandom t
isRandom (Alternative _) = True
isRandom (Range _ _) = True
isRandom Byte = True

-- | Take a random element from a list.
randomL :: RandomGen g => [a] -> State g (Maybe a)
randomL [] = return Nothing
randomL xs = Just . (xs !!) <$> state (randomR (0, length xs - 1))
