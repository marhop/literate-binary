-- |
-- Module     : LiterateBinary
-- Copyright  : (c) Martin Hoppenheit 2019
-- License    : MIT
-- Maintainer : martin@hoppenheit.info
--
-- This module provides functions that turn a hex string representing a sequence
-- of bytes into a 'ByteString'. They come in several variants:
--
-- +----------------+---------------------------------------+-----------------+
-- | function       | input                                 | needs RandomGen |
-- +================+=======================================+=================+
-- | compile        | Markdown w/ hex string in code blocks | yes             |
-- +----------------+---------------------------------------+-----------------+
-- | compileIO      | Markdown w/ hex string in code blocks | no              |
-- +----------------+---------------------------------------+-----------------+
-- | compilePlain   | plain hex string w/o Markdown         | yes             |
-- +----------------+---------------------------------------+-----------------+
-- | compilePlainIO | plain hex string w/o Markdown         | no              |
-- +----------------+---------------------------------------+-----------------+
--
-- With Markdown input, the hex string is made up by all code blocks in the
-- Markdown document tied together, except those with the @.nobin@ class. All
-- other content including inline code in backticks is ignored. By the way,
-- embedding a hex string into a Markdown document leads to the notion of
-- Literate Binary (a reminiscence to Literate Programming).
--
-- The set of valid hex strings is defined as follows:
--
-- Each sequence of hex characters (0-9, A-F, upper or lower case) with equal
-- length like @00ff@ is a valid hex string; each pair of characters in this
-- sequence translates to one byte in the usual fashion. Given hex strings x and
-- y and a positive integer n, the following are valid hex strings as well:
--
--   * A /repetition/ of the form @(x){n}@. This translates to the ByteString
--     corresponding to x, repeated n times. The integer n may be followed by a
--     multiplicative suffix @K@ (factor 2^10, KiB), @M@ (factor 2^20, MiB), or
--     @G@ (factor 2^30, GiB).
--   * An /alternative/ of the form @(x|...|y)@. This translates to the
--     ByteString corresponding to either x or ... or y, selected randomly.
--   * A /range/ of the form @(x-y)@. This translates to one random ByteString
--     from the range defined by the ByteStrings corresponding to x and y. The
--     range is defined according to the rules in 'Data.ByteString.Enumeration'.
--   * The special range @.@ (a single dot). This translates to one random byte,
--     so it is equivalent to the range @(00-ff)@.
--   * A /string/ of the form @"..."@ or @'...'@ with arbitrary text content
--     inside the quotes. The quote sign itself may appear inside the string
--     escaped by a backslash, a literal backslash has to be escaped by another
--     backslash. This translates to the UTF-8 encoded ByteString corresponding
--     to the quoted string content. (Note that ASCII is a subset of UTF-8.)
--
-- When combining an alternative, a range or a string with a repetition,
-- redundant parentheses are not required: @(x|y){n}@ is equivalent to
-- @((x|y)){n}@, @(x-y){n}@ is equivalent to @((x-y)){n}@, @.{n}@ is equivalent
-- to @(.){n}@, and @"foo"{3}@ is equivalent to @("foo"){3}@.
--
-- Whitespace including line breaks as well as comments (@# ...@ until end of
-- line) are ignored in a hex string.

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

-- | Convert hex string in Markdown code blocks to ByteString.
compile :: RandomGen g => g -> T.Text -> Either Error BL.ByteString
compile g t = compilePlain g $ parseMarkdown t

-- | Convert hex string in Markdown code blocks to ByteString.
compileIO :: T.Text -> IO (Either Error BL.ByteString)
compileIO t = flip compile t <$> newStdGen

-- | Convert plain hex string to ByteString.
compilePlain :: RandomGen g => g -> T.Text -> Either Error BL.ByteString
compilePlain g t = eval g <$> parseHex t

-- | Convert plain hex string to ByteString.
compilePlainIO :: T.Text -> IO (Either Error BL.ByteString)
compilePlainIO t = flip compilePlain t <$> newStdGen
