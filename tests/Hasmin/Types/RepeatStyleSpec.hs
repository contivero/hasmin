{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.RepeatStyleSpec where

import Data.Text (Text)
import Hasmin.Parser.Value
import Hasmin.Types.RepeatStyle
import Hasmin.TestUtils

repeatStyleTests :: Spec
repeatStyleTests =
    describe "<repeat-style> minification tests" $ do
      it "Minified <repeat-style> maintains semantic equivalence" $
        property (prop_minificationEq :: RepeatStyle -> Bool)
      mapM_ (matchSpec f) repeatStyleTestsInfo
  where f = minifyWithTestConfig <$> repeatStyle

repeatStyleTestsInfo :: [(Text, Text)]
repeatStyleTestsInfo =
  [("repeat no-repeat", "repeat-x")
  ,("no-repeat repeat", "repeat-y")
  ,("no-repeat no-repeat", "no-repeat")
  ,("repeat repeat", "repeat")
  ,("space space", "space")
  ,("round round", "round")
  ,("round  space", "round space")
  ,("repeat-x", "repeat-x")
  ,("repeat-y", "repeat-y")
  ]

spec :: Spec
spec = repeatStyleTests

main :: IO ()
main = hspec spec
