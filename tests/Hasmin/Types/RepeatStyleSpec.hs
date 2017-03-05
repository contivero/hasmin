{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.RepeatStyleSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Text (Text)
import Control.Applicative (liftA2)
import Hasmin.Parser.Value
import Hasmin.Types.Class
import Hasmin.Types.RepeatStyle
import Hasmin.TestUtils

repeatStyleTests :: Spec
repeatStyleTests =
    describe "<repeat-style> minification tests" $ do
      it "Minified <repeat-style> maintains semantic equivalence" $ 
        property (prop_minificationEq :: RepeatStyle -> Bool)
      mapM_ (matchSpec f) repeatStyleTestsInfo
  where f = minify <$> repeatStyle

instance Arbitrary RepeatStyle where
  arbitrary = frequency [(1, pure RepeatX)
                        ,(1, pure RepeatY)
                        ,(6, liftA2 RSPair arbitrary arbitrary)
                        ]

instance Arbitrary RSKeyword where
  arbitrary = mkGen [RsRepeat, RsSpace, RsRound, RsNoRepeat]

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
