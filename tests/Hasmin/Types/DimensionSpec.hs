{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.DimensionSpec where

import Hasmin.Types.Dimension
import Hasmin.TestUtils

dimensionTests :: Spec
dimensionTests =
    describe "Dimension tests with quickcheck" $ do
      it "Minified <length>s are equivalent to the original ones" $
        property (prop_minificationEq :: Length -> Bool)
      it "Minified <angle>s are equivalent to the original ones" $
        property (prop_minificationEq :: Angle -> Bool)
      it "Minified <time>s are equivalent to the original ones" $
        property (prop_minificationEq :: Duration -> Bool)
      it "Minified <frequency>s are equivalent to the original ones" $
        property (prop_minificationEq :: Frequency -> Bool)
      it "Minified <resolution>s are equivalent to the original ones" $
        property (prop_minificationEq :: Resolution -> Bool)

spec :: Spec
spec = do
    dimensionTests
    describe "<length> units equivalences" $ do
      -- inches conversions
      it "1in == 2.54cm" $
        Length 1 IN `shouldBe` Length 2.54 CM
      it "1in == 25.4cm" $
        Length 1 IN `shouldBe` Length 25.4 MM
      it "1in == 101.6q" $
        Length 1 IN `shouldBe` Length 101.6 Q
      it "1in == 72pt" $
        Length 1 IN `shouldBe` Length 72 PT
      it "1in == 6pc" $
        Length 1 IN `shouldBe` Length 6 PC
      it "1in == 96px" $
        Length 1 IN `shouldBe` Length 96 PX
      -- pixel conversions
      it "48px == .5in" $
        Length 48 PX `shouldBe` Length 0.5 IN
      it "72px == 1.905cm" $
        Length 72 PX `shouldBe` Length 1.905 CM
      it "72px == 19.05mm" $
        Length 72 PX `shouldBe` Length 19.05 MM
      it "60px == 63.5q" $
        Length 60 PX `shouldBe` Length 63.5 Q
      it "12px == 9pt" $
        Length 12 PX `shouldBe` Length 9 PT
      it "16px == 1pc" $
        Length 16 PX `shouldBe` Length 1 PC
      -- centimeter conversion
      it "5.08cm == 2in" $
        Length 5.08 CM `shouldBe` Length 2 IN
      it "1cm == 10mm" $
        Length 1 CM `shouldBe` Length 10 MM
      it "2cm == 80q" $
        Length 2 CM `shouldBe` Length 80 Q
      it "1.27cm == 36pt" $
        Length 1.27 CM `shouldBe` Length 36 PT
      it "1.27cm == 3pc" $
        Length 1.27 CM `shouldBe` Length 3 PC
      it "1.27cm == 48px" $
        Length 1.27 CM `shouldBe` Length 48 PX
      it "1in != 1em" $
        Length 1 IN `shouldNotBe` Length 1 EM
    -- describe "<time> conversions" $ do
      -- it "" $ Duration 1 S `shouldBe` Duration 1000 Ms
    -- describe "<frequency> conversions" $ do
      -- it "" $ Frequency 1 Khz `shouldBe` Frequency 1000 Hz
    -- describe "<resolution> conversions" $ do
      -- it "" $ Resolution 96 Dpi `shouldBe` Resolution 1 Dppx

main :: IO ()
main = hspec spec
