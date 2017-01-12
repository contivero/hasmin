{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.DimensionSpec where

import Test.Hspec
--import Test.QuickCheck
--import Hasmin.Parser.Internal hiding (property)

--import Test.Hspec.Attoparsec (shouldParse, parseSatisfies, (~>))
import Hasmin.Types.Dimension

spec :: Spec
spec = 
  describe "<length> units equivalences" $ do
    describe "inches conversions" $ do
      it "1in == 2.54cm" $
        (Distance 1 IN) `shouldBe` Distance 2.54 CM
      it "1in == 25.4cm" $
        (Distance 1 IN) `shouldBe` Distance 25.4 MM
      it "1in == 101.6q" $
        (Distance 1 IN) `shouldBe` Distance 101.6 Q
      it "1in == 72pt" $
        (Distance 1 IN) `shouldBe` Distance 72 PT
      it "1in == 6pc" $
        (Distance 1 IN) `shouldBe` Distance 6 PC
      it "1in == 96px" $
        (Distance 1 IN) `shouldBe` Distance 96 PX
    describe "pixel conversions" $ do
      it "48px == .5in" $
        (Distance 48 PX) `shouldBe` Distance 0.5 IN
      it "72px == 1.905cm" $
        (Distance 72 PX) `shouldBe` Distance 1.905 CM
      it "72px == 19.05mm" $
        (Distance 72 PX) `shouldBe` Distance 19.05 MM 
      it "60px == 63.5q" $
        (Distance 60 PX) `shouldBe` Distance 63.5 Q
      it "12px == 9pt" $
        (Distance 12 PX) `shouldBe` Distance 9 PT
      it "16px == 1pc" $
        (Distance 16 PX) `shouldBe` Distance 1 PC
    describe "centimeter conversions" $ do
      it "5.08cm == 2in" $
        (Distance 5.08 CM) `shouldBe` Distance 2 IN
      it "1cm == 10mm" $
        (Distance 1 CM) `shouldBe` Distance 10 MM 
      it "2cm == 80q" $
        (Distance 2 CM) `shouldBe` Distance 80 Q
      it "1.27cm == 36pt" $
        (Distance 1.27 CM) `shouldBe` Distance 36 PT
      it "1.27cm == 3pc" $
        (Distance 1.27 CM) `shouldBe` Distance 3 PC
      it "1.27cm == 48px" $
        (Distance 1.27 CM) `shouldBe` Distance 48 PX

main :: IO ()
main = hspec spec
