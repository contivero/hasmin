{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.FilterFunctionSpec where

import Test.Hspec

import Data.Text (Text)
import Hasmin.Parser.Value
import Hasmin.Types.Class
import Hasmin.TestUtils

filterTests :: Spec
filterTests = 
    describe "<filter-function> minification tests" $
      mapM_ (matchSpecWithDesc f) filterTestsInfo
  where f = minify <$> value
      
filterTestsInfo :: [(String, Text, Text)]
filterTestsInfo =
  [("Minifies the <color> value in drop-shadow()"
     ,"drop-shadow(1px 1px 1px #ff0000)"
     ,"drop-shadow(1px 1px 1px red)")
  ,("Minifies the <length> values in drop-shadow()"
     ,"drop-shadow(12px 12px 12px red)"
     ,"drop-shadow(9pt 9pt 9pt red)")
  ,("Removes 3rd value in drop-shadow() when it is 0"
     ,"drop-shadow(1px 1px 0 red)"
     ,"drop-shadow(1px 1px red)")
  ,("Minifies <angle> in hue-rotate()"
     ,"hue-rotate(100grad)"
     ,"hue-rotate(90deg)")
  ,("Minifies <length> in blur()"
     ,"blur(12px)"
     ,"blur(9pt)")
  ,("Minifies 100% in contrast()"
     ,"contrast(100%)"
     ,"contrast(1)")
  ,("Minifies 0% in contrast()"
     ,"contrast(0%)"
     ,"contrast(0)")
  ,("Minifies <percentage> multiple of 10 in brightness()"
     ,"brightness(50%)"
     ,"brightness(.5)")
  ,("Normalizes <percentage> to <number> when > 10 in contrast()"
     ,"contrast(11%)"
     ,"contrast(.11)")
  ,("Does not minify <percentage> when > 0 and < 10 in contrast()"
     ,"contrast(8%)"
     ,"contrast(8%)")
  ]

spec :: Spec
spec = filterTests

main :: IO ()
main = hspec spec
