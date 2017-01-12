{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.RepeatStyleSpec where

import Test.Hspec

import Data.Text (Text)
import Hasmin.Parser.Value
import Hasmin.Types.Class
import Hasmin.TestUtils

repeatStyleTests :: Spec
repeatStyleTests = 
    describe "<repeat-style> minification tests" $
      mapM_ (matchSpec f) repeatStyleTestsInfo
  where f = minify <$> repeatStyle
      
repeatStyleTestsInfo :: [(Text, Text)]
repeatStyleTestsInfo =
  [("repeat no-repeat", "repeat-x")
  ,("no-repeat repeat", "repeat-y")
  ,("no-repeat no-repeat", "no-repeat")
  ,("repeat repeat", "repeat")
  ,("space space", "space")
  ,("round round", "round")
  ]

spec :: Spec
spec = repeatStyleTests

main :: IO ()
main = hspec spec
