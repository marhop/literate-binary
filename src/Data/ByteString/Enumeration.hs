module Data.ByteString.Enumeration
    ( range
    ) where

import qualified Data.ByteString as BS

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
