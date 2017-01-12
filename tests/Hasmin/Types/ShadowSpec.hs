{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.ShadowSpec where

import Test.Hspec

import Data.Text (Text)
import Hasmin.Parser.Value
import Hasmin.Types.Class
import Hasmin.TestUtils

shadowTests :: Spec
shadowTests = 
    describe "<shadow> minification tests" $
      mapM_ (matchSpecWithDesc f) shadowTestsInfo
  where f = minify <$> shadowList
      
shadowTestsInfo :: [(String, Text, Text)]
shadowTestsInfo =
  [("Removes 4th <length> value when it is null"
     ,"1px 1px 1px 0px red"
     ,"1px 1px 1px red")
  ,("Removes 3rd <length> value when there is no 4th, and it is zero"
     ,"1px 1px 0px red"
     ,"1px 1px red")
  ,("Minifies permuted value"
     ,"red 1px 1px 0px 0px inset"
     ,"inset 1px 1px red")
  ,("Minifies the <color> value"
     ,"inset 1px 1px #ff0000"
     ,"inset 1px 1px red")
  ,("Minifies the <length> values"
     ,"inset 12px 12px 12px 12px blue"
     ,"inset 9pt 9pt 9pt 9pt blue")
  ,("Minifies list of <shadow> values"
     ,"inset 12px 12px 0 0 blue, 12px 12px 3px 0 red"
     ,"inset 9pt 9pt blue,9pt 9pt 3px red")
  ]

spec :: Spec
spec = shadowTests

main :: IO ()
main = hspec spec

