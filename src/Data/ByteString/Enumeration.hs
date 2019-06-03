-- |
-- Module     : Data.ByteString.Enumeration
-- Copyright  : (c) Martin Hoppenheit 2019
-- License    : MIT
-- Maintainer : martin@hoppenheit.info
--
-- Enumeration for 'ByteString' values. The function application @range x y@
-- enumerates all ByteStrings from x to y, ordered by length (and
-- lexicographically for ByteStrings with equal length). Note that this is not
-- an 'Enum' instance and that the enumeration order is not compatible with the
-- existing 'Ord' instance for ByteString.
--
-- > range 00 ff = [00, 01, ..., fe, ff]
-- > range ff 00 = []
-- > range mempty 0100 = [mempty, 00, 01, ..., ff, 0000, 0001, ..., 00ff, 0100]
--
-- The last example illustrates why the enumeration is not compatible with the
-- Ord instance for ByteString: The enumeration contains the sequence @[..., ff,
-- 0000, ...]@, but @ff > 0000@. See also <https://stackoverflow.com/a/10356655>
-- for a similar example.

module Data.ByteString.Enumeration
    ( range
    , randomInRange
    ) where

import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import System.Random (RandomGen, randomR)

-- | Create a range of ByteStrings based on start and end values.
range :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
range x y
    | x `gt` y = range y x
    | otherwise = takeWhile (not . (`gt` y)) $ iterate (nthSucc 1) x

-- | Create one random ByteString in a range based on start and end values. This
-- function is more efficient than creating a (potentially large) list with
-- 'range' and then taking a random element from it.
randomInRange ::
       RandomGen g => (BS.ByteString, BS.ByteString) -> g -> (BS.ByteString, g)
randomInRange (x, y)
    | x `gt` y = randomInRange (y, x)
    | otherwise = first (`nthSucc` x) . randomR (0, rangeLength x y - 1)

-- | Successor function for ByteStrings. Behaves mostly like a chain of succ
-- applications for a ByteString interpreted as a number would do, with the
-- exception that the successor of an empty ByteString by definition is the NULL
-- byte. In particular, this affects carry operations. Examples:
--
-- > nthSucc 0 00 == 00
-- > nthSucc 1 00 == 01
-- > nthSucc 8 00 == 08
-- > nthSucc 1 0000 == 0001
-- > nthSucc 1 0100 == 0101
-- > nthSucc 1 00ff == 0100
-- > nthSucc 1 mempty == 00
-- > nthSucc 1 ff == 0000 /= 0100
nthSucc :: Integer -> BS.ByteString -> BS.ByteString
nthSucc 0 x = x
nthSucc 1 x =
    maybe
        (BS.singleton 0x00)
        (\(ys, y) ->
             if y < 0xff
                 then BS.snoc ys (succ y)
                 else BS.snoc (nthSucc 1 ys) 0x00) $
    BS.unsnoc x
nthSucc n x
    | n > 1 = nthSucc (n - 1) (nthSucc 1 x)
    | otherwise = mempty

-- | Modified "greater than" comparison for ByteStrings that takes the length of
-- a ByteString into account. Given two ByteStrings x and y, @x `gt` y@ iff
--
--   * x is longer than y or
--   * x and y have equal length and x > y in terms of the Ord instance for
--     ByteStrings.
gt :: BS.ByteString -> BS.ByteString -> Bool
gt x y = (compare (BS.length x) (BS.length y) <> compare x y) == GT

-- | Calculate the length of a range of ByteStrings based on start and end
-- values.
rangeLength :: BS.ByteString -> BS.ByteString -> Integer
rangeLength x y
    | x `gt` y = 0
    | otherwise =
        num y - num x + 1 +
        sum [256 ^ k | k <- [BS.length x .. BS.length y - 1]]

-- | Calculate the numeric value of a ByteString.
num :: BS.ByteString -> Integer
num = BS.foldl' (\acc x -> 256 * acc + fromIntegral x) 0
