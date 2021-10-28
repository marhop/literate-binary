{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (pack)
import Data.Either (isLeft)
import Data.Word (Word8)
import LiterateBinary.HexTree (HexString (..))
import LiterateBinary.Parse (parseHex)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "hex parser (valid syntax)" $ do
    it "accepts an empty string" $
      parseHex "" `shouldBe` Right []
    it "accepts a simple hex string" $
      parseHex "00ff" `shouldBe` Right [mkLiteral [0x00, 0xff]]
    it "accepts a repetition" $
      parseHex "(00){3}" `shouldBe` Right [Repetition [mkLiteral [0x00]] 3]
    it "accepts a repetition with suffix" $
      parseHex "(00){2K}"
        `shouldBe` Right [Repetition [mkLiteral [0x00]] 2048]
    it "accepts an alternative" $
      parseHex "(00|ff|3333)"
        `shouldBe` Right
          [Alternative $ fmap (pure . mkLiteral) [[0x00], [0xff], [0x33, 0x33]]]
    it "accepts a range" $
      parseHex "(00-ff)"
        `shouldBe` Right [Range [mkLiteral [0x00]] [mkLiteral [0xff]]]
    it "accepts a dot range" $
      parseHex "." `shouldBe` Right [Byte]
    it "accepts a string literal in double quotes" $
      parseHex "\"abc\"" `shouldBe` Right [mkLiteral [0x61 .. 0x63]]
    it "accepts a string literal in single quotes" $
      parseHex "'abc'" `shouldBe` Right [mkLiteral [0x61 .. 0x63]]
    it "accepts a string with escape characters" $
      parseHex "'\\''" `shouldBe` Right [mkLiteral [0x27]]
    it "accepts combined macros like alternative + repetition" $
      parseHex "(00|ff){3}"
        `shouldBe` Right
          [Repetition [Alternative [[mkLiteral [0x00]], [mkLiteral [0xff]]]] 3]
    it "accepts comments" $
      parseHex "ff # comment with # inside\n"
        `shouldBe` Right [mkLiteral [0xff]]
  describe "hex parser (invalid syntax)" $ do
    it "rejects non-hex characters" $
      parseHex "7f2x" `shouldSatisfy` isLeft
    it "rejects an odd number of hex characters" $
      parseHex "fff" `shouldSatisfy` isLeft
    it "rejects non-matching parentheses" $
      parseHex "(00" `shouldSatisfy` isLeft
    it "rejects invalid repetition suffixes" $
      parseHex "(00){2Y}" `shouldSatisfy` isLeft
    it "rejects incomplete alternatives" $
      parseHex "(00|ff|)" `shouldSatisfy` isLeft
    it "rejects incomplete ranges" $
      parseHex "(00-)" `shouldSatisfy` isLeft

mkLiteral :: [Word8] -> HexString
mkLiteral = Literal . pack
