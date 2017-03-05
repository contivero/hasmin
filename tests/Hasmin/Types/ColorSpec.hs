{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.ColorSpec where

import Test.Hspec
import Test.QuickCheck
import Hasmin.Parser.Value
import Hasmin.TestUtils

import Test.Hspec.Attoparsec (parseSatisfies, (~>))
import Data.Text (Text)
import Data.Foldable
import Data.Maybe (fromJust)
import Control.Monad
import Control.Applicative (liftA2)
import Hasmin.Types.Color
import Hasmin.Types.Class
import Hasmin.Types.Numeric

instance Arbitrary Color where
  arbitrary = oneof [ fmap Named colorKeyword
                    , liftM3 mkHex3 hexChar hexChar hexChar
                    , liftM3 mkHex6 hexString hexString hexString
                    , liftM4 mkHex4 hexChar hexChar hexChar hexChar
                    , liftM4 mkHex8 hexString hexString hexString hexString
                    , liftM3 mkRGBInt intRange intRange intRange
                    , liftM3 mkRGBPer ratRange ratRange ratRange 
                    , liftM4 mkRGBAInt intRange intRange intRange alphaRange
                    , liftM4 mkRGBAPer ratRange ratRange ratRange alphaRange 
                    , liftM3 mkHSL hueRange ratRange ratRange
                    , liftM4 mkHSLA hueRange ratRange ratRange alphaRange 
                    ]
    where intRange   = choose (0, 255)
          ratRange   = toPercentage <$> (choose (0, 100) :: Gen Float)
          alphaRange = toAlphavalue <$> (choose (0, 1) :: Gen Float)
          hueRange   = choose (0, 360)

-- | Generates color keywords uniformly distributed 
colorKeyword :: Gen Text
colorKeyword = mkGen $ fmap fst keywordColors

-- | Generates a hexadecimal character uniformly distributed 
hexChar :: Gen Char
hexChar = mkGen hexadecimals

hexString :: Gen String
hexString = liftA2 (\x y -> [x,y]) hexChar hexChar

colorTests :: Spec
colorTests =
  describe "Color datatype tests" .
    it "minified color is semantically equivalent" $
      property (prop_minificationEq :: Color -> Bool)

colorParserTests :: Spec
colorParserTests =
  describe "Color Parser tests" .
    it "succeeds in parsing and minifying different red color representations" $
      traverse_ ((`parseSatisfies` (==(fromJust $ mkNamed "red"))) . (~> (minifyColor <$> color))) redColor

colorParserSpacesAndCommentsTests :: Spec
colorParserSpacesAndCommentsTests =
  describe "Color Parser test" .
    it "succeeds in parsing different yellow color representations with spaces and comments in-between" $
      traverse_ ((`parseSatisfies` (==(fromJust $ mkNamed "yellow"))) . (~> color)) commentsAndSpacesInColors

-- | Multiple equivalent red color representations
redColor :: [Text]
redColor = 
  [ "red" 
  , "#f00", "#F00", "#ff0000", "#fF0000", "#Ff0000", "#FF0000"
  , "rgb(255,0,0)" , "rgb(100%,0%,0%)"
  , "rgba(255,0,0,1)", "rgba(255,0,0,1.0)"
  , "rgba(100%,0%,0%,1)", "rgba(100%,0%,0%,1.0)"
  , "hsl(0,100%,50%)", "hsl(360,100%,50%)"
  , "hsla(0,100%,50%,1)", "hsla(0,100%,50%,1.0)"
  , "hsla(360,100%,50%,1)", "hsla(360,100%,50%,1.0)"
  ]

-- | All 4096 (16^3) possible 3 char shorthands
allHex3 :: [String]
allHex3 = replicateM 3 hexadecimals

hexadecimals :: String
hexadecimals = "0123456789abcdef" 

-- Every color is yellow
commentsAndSpacesInColors :: [Text]
commentsAndSpacesInColors = 
  [ "rgb(/**/255,255,0)"
  , "rgb(255,/**/255,0)"
  , "rgb(255,255,/**/0)"
  , "rgb(255/**/,255,0)"
  , "rgb(255,255/**/,0)"
  , "rgb(255,255,0/**/)"
  , "rgb(/* */255/* */,/* */255/* */,/* */0/* */)"
  , "rgb( /* */ 255 /* */ , /* */ 255 /* */ , /* */ 0 /* */ )"
  , "rgba(/**/255,255,0,1)"
  , "rgba(255,/**/255,0,1)"
  , "rgba(255,255,/**/0,1)"
  , "rgba(255,255,0,/**/1)"
  , "rgba(255/**/,255,0,1)"
  , "rgba(255,255/**/,0,1)"
  , "rgba(255,255,0/**/,1)"
  , "rgba(255,255,0,1/**/)"
  , "rgba(/* */255/* */,/* */255/* */,/* */0/* */,/* */1/* */)"
  , "rgba( /* */ 255 /* */ , /* */ 255 /* */ , /* */ 0 /* */ , /* */ 1 /* */)"
  , "hsl(/**/60,100%,50%)"
  , "hsl(60,/**/100%,50%)"
  , "hsl(60,100%,/**/50%)"
  , "hsl(60/**/,100%,50%)"
  , "hsl(60,100%/**/,50%)"
  , "hsl(60,100%,50%/**/)"
  , "hsl(/* */60/* */,/* */100%/* */,/* */50%/* */)"
  , "hsl( /* */ 60 /* */ , /* */ 100% /* */ , /* */ 50% /* */ )"
  , "hsla(/**/60,100%,50%,1)"
  , "hsla(60,/**/100%,50%,1)"
  , "hsla(60,100%,/**/50%,1)"
  , "hsla(60,100%,50%,/**/1)"
  , "hsla(60/**/,100%,50%,1)"
  , "hsla(60,100%/**/,50%,1)"
  , "hsla(60,100%,50%/**/,1)"
  , "hsla(60,100%,50%,1/**/)"
  , "hsla(/* */60/* */,/* */100%/* */,/* */50%/* */,/* */1/* */)"
  , "hsla( /* */ 60 /* */ , /* */ 100% /* */ , /* */ 50% /* */ , /* */ 1 /* */)"
  ]

colorMinificationTests :: Spec
colorMinificationTests = 
    describe "color minification" $
      mapM_ (matchSpec f) colorMinificationTestsInfo
  where f = minify <$> color

colorMinificationTestsInfo :: [(Text, Text)]
colorMinificationTestsInfo =
  [ ("rgba(0,0,0,.4)", "#0006")
  , ("hsla(0,0%,0%,.4)", "#0006")
  , ("#00000066", "#0006")
  ]

spec :: Spec
spec = do colorTests 
          colorParserTests
          colorMinificationTests
          colorParserSpacesAndCommentsTests

main :: IO ()
main = hspec spec
