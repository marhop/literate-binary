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
      parseHex "00ff" `shouldBe` Right [lit [0x00, 0xff]]
    it "accepts a repetition" $
      parseHex "(00){3}" `shouldBe` Right [Repetition [lit [0x00]] 3]
    it "accepts a repetition with suffix" $
      parseHex "(00){2K}"
        `shouldBe` Right [Repetition [lit [0x00]] 2048]
    it "accepts an alternative" $
      parseHex "(00|ff|3333)"
        `shouldBe` Right
          [Alternative $ fmap (pure . lit) [[0x00], [0xff], [0x33, 0x33]]]
    it "accepts a range" $
      parseHex "(00-ff)" `shouldBe` Right [Range [lit [0x00]] [lit [0xff]]]
    it "accepts a dot range" $
      parseHex "." `shouldBe` Right [Byte]
    it "accepts a string literal in double quotes" $
      parseHex "\"abc\"" `shouldBe` Right [lit [0x61 .. 0x63]]
    it "accepts a string literal in single quotes" $
      parseHex "'abc'" `shouldBe` Right [lit [0x61 .. 0x63]]
    it "accepts a string with escape characters" $
      parseHex "'\\''" `shouldBe` Right [lit [0x27]]
    it "accepts combined macros like alternative + repetition" $
      parseHex "(00|ff){3}"
        `shouldBe` Right
          [Repetition [Alternative [[lit [0x00]], [lit [0xff]]]] 3]
    it "accepts nested macros including whitespace" $
      parseHex "00 (11 | (22|(33-44)|.){5} | 66) 77"
        `shouldBe` Right
          [ lit [0x00],
            Alternative
              [ [lit [0x11]],
                [ Repetition
                    [ Alternative
                        [ [lit [0x22]],
                          [Range [lit [0x33]] [lit [0x44]]],
                          [Byte]
                        ]
                    ]
                    5
                ],
                [lit [0x66]]
              ],
            lit [0x77]
          ]
    it "accepts comments" $
      parseHex "ff # comment with # inside\n"
        `shouldBe` Right [lit [0xff]]
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

lit :: [Word8] -> HexString
lit = Literal . pack
