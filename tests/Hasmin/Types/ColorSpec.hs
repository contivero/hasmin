{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.ColorSpec where

import Control.Applicative (liftA2)
import Control.Monad
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Text (Text)
import Test.Hspec.Attoparsec (parseSatisfies, (~>))

import Hasmin.Parser.Value
import Hasmin.TestUtils
import Hasmin.Types.Class
import Hasmin.Types.Color
import Hasmin.Types.Numeric

colorTests :: Spec
colorTests =
  describe "<color> tests" .
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
  where f = minifyWithTestConfig <$> color

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
