{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.BgSizeSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Text (Text)
import Hasmin.Parser.Value
import Hasmin.Types.Class
import Hasmin.Types.BgSize
import Hasmin.TestUtils

quickcheckBgSize :: Spec
quickcheckBgSize =
    describe "Quickcheck tests for <bg-size>" $
      it "Minified <bg-size> maintains semantical equivalence" $
        property (prop_minificationEq :: BgSize -> Bool)

bgSizeTests :: Spec
bgSizeTests =
    describe "<bg-size> minification tests" $
      mapM_ (matchSpec f) bgSizeTestsInfo
  where f = minify <$> value

bgSizeTestsInfo :: [(Text, Text)]
bgSizeTestsInfo =
  [("cover" ,"cover")
  ,("contain", "contain")
  ]

spec :: Spec
spec = do
    quickcheckBgSize
    bgSizeTests 

main :: IO ()
main = hspec spec