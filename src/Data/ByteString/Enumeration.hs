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
    ) where

import qualified Data.ByteString as BS

-- | Create a range of ByteStrings based on start and end values.
range :: BS.ByteString -> BS.ByteString -> [BS.ByteString]
range x y = takeWhile (`lteq` y) $ iterate succBS x

-- | Successor function for ByteStrings. Behaves mostly like succ for a
-- ByteString interpreted as a number would do, with the exception that the
-- successor of an empty ByteString by definition is the NULL byte. In
-- particular, this affects carry operations. Examples:
--
-- > succBS 00 == 01
-- > succBS 0000 == 0001
-- > succBS 0100 == 0101
-- > succBS 00ff == 0100
-- > succBS mempty == 00
-- > succBS ff == 0000 /= 0100
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
