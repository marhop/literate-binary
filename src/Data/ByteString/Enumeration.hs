-- |
-- Module     : Data.ByteString.Enumeration
-- Copyright  : (c) Martin Hoppenheit 2019-2021
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
-- > range mempty 0100 = [mempty, 00, 01, ..., ff, 0000, 0001, ..., 00ff, 0100]
--
-- The last example illustrates why the enumeration is not compatible with the
-- Ord instance for ByteString: The enumeration contains the sequence @[..., ff,
-- 0000, ...]@, but @ff > 0000@. See also <https://stackoverflow.com/a/10356655>
-- for a similar example.
module Data.ByteString.Enumeration (range, randomInRange) where

import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import System.Random (RandomGen, randomR)

-- | Create a range of ByteStrings based on start and end values.
range :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
range x y
  | x `gt` y = range y x
  | otherwise = takeWhile (not . (`gt` y)) $ iterate succ' x

-- | Create one random ByteString in a range based on start and end values. This
-- function is more efficient than creating a (potentially large) list with
-- 'range' and then taking a random element from it.
randomInRange ::
  RandomGen g => (BS.ByteString, BS.ByteString) -> g -> (BS.ByteString, g)
randomInRange (x, y)
  | x `gt` y = randomInRange (y, x)
  | otherwise = first toEnum' . randomR (fromEnum' x, fromEnum' y)

-- | Successor function for ByteStrings. Behaves mostly like a chain of succ
-- applications for a ByteString interpreted as a number would do, with the
-- exception that the successor of an empty ByteString by definition is the NULL
-- byte. In particular, this affects carry operations. Examples:
--
-- > succ' 00 == 01
-- > succ' 0000 == 0001
-- > succ' 0100 == 0101
-- > succ' 00ff == 0100
-- > succ' mempty == 00
-- > succ' ff == 0000 /= 0100
succ' :: BS.ByteString -> BS.ByteString
succ' =
  maybe
    (BS.singleton 0x00)
    ( \(xs, x) ->
        if x < 0xff
          then BS.snoc xs (succ x)
          else BS.snoc (succ' xs) 0x00
    )
    . BS.unsnoc

-- | Modified "greater than" comparison for ByteStrings that takes the length of
-- a ByteString into account. Given two ByteStrings x and y, @x `gt` y@ iff
--
--   * x is longer than y or
--   * x and y have equal length and x > y in terms of the Ord instance for
--     ByteStrings.
gt :: BS.ByteString -> BS.ByteString -> Bool
gt x y = (compare (BS.length x) (BS.length y) <> compare x y) == GT

-- | Like the regular 'fromEnum' function but Integer typed. The mapping from
-- ByteString to Integer is a little uncommon because it takes the empty
-- ByteString and leading NULL bytes into account:
--
-- > fromEnum' mempty == 0
-- > fromEnum' 00 == 1
-- > fromEnum' ff == 256
-- > fromEnum' 0000 == 257
-- > fromEnum' 00ff == 512
-- > fromEnum' 0100 == 513
fromEnum' :: BS.ByteString -> Integer
fromEnum' = BS.foldl' (\acc x -> 256 * acc + fromIntegral x + 1) 0

-- | Like the regular 'toEnum' function but Integer typed. This is the inverse
-- of fromEnum'.
toEnum' :: Integer -> BS.ByteString
toEnum' 0 = mempty
toEnum' x
  | x < 0 = error "Unexpected error converting Integer to ByteString."
  | m == 0 = BS.snoc (toEnum' (d - 1)) 255
  | otherwise = BS.snoc (toEnum' d) (fromIntegral m - 1)
  where
    (d, m) = x `divMod` 256
