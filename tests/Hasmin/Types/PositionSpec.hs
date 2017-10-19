{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.PositionSpec where

import Data.Text (Text)
import Control.Monad (liftM4)

import Hasmin.Parser.Value
import Hasmin.TestUtils
import Hasmin.Types.Class
import Hasmin.Types.Position
import Hasmin.Types.Numeric

positionMinificationTests :: Spec
positionMinificationTests =
    describe "<position> minification" $
        mapM_ (matchSpec f) positionMinificationTestsInfo
      -- it "Minifies <position> properly" $
      -- it "Minified <position> maintains semantical equivalence" $ do
        -- property (prop_minificationEq :: Position -> Bool)
  where f = minifyWithTestConfig <$> position

positionMinificationTestsInfo :: [(Text, Text)]
positionMinificationTestsInfo =
  [("50%",              "50%")
  ,("50% 50%",          "50%")
  ,("center",           "50%")
  ,("center top 50%",   "50%")
  ,("top 50% center",   "50%")
  ,("center left 50%",  "50%")
  ,("top 50% left 50%", "50%")
  ,("left 50% top 50%", "50%")
  ,("left 50% center",  "50%")
  ,("center center",    "50%")

  ,("0% 50%",          "0")
  ,("left",            "0")
  ,("left center",     "0")
  ,("left top 50%",    "0")
  ,("left 0% center",  "0")
  ,("left 0% top 50%", "0")
  ,("top 50% left 0%", "0")
  ,("center left 0%",  "0")
  ,("top 50% left",    "0")
  ,("center left",     "0")

  ,("0% 0%",          "0 0")
  ,("left 0%",        "0 0")
  ,("0% top",         "0 0")
  ,("left top",       "0 0")
  ,("left top 0%",    "0 0")
  ,("left 0% top",    "0 0")
  ,("left 0% top 0%", "0 0")
  ,("top 0% left 0%", "0 0")
  ,("top left 0%",    "0 0")
  ,("top 0% left",    "0 0")
  ,("top left",       "0 0")

  ,("50% 0%",          "top")
  ,("top",             "top")
  ,("top center",      "top")
  ,("top left 50%",    "top")
  ,("top 0% left 50%", "top")
  ,("left 50% top 0%", "top")
  ,("left 50% top",    "top")
  ,("center top",      "top")

  ,("100% 0%",         "100% 0")
  ,("right top",       "100% 0")
  ,("right 0% top",    "100% 0")
  ,("right top 0%",    "100% 0")
  ,("right 0% top 0%", "100% 0")
  ,("top 0% right 0%", "100% 0")
  ,("top 0% right",    "100% 0")
  ,("top right 0%",    "100% 0")
  ,("top right",       "100% 0")

  ,("100%",             "100%")
  ,("100% 50%",         "100%")
  ,("right",            "100%")
  ,("right center",     "100%")
  ,("right top 50%",    "100%")
  ,("right 0% center",  "100%")
  ,("right 0% top 50%", "100%")
  ,("top 50% right 0%", "100%")
  ,("center right 0%",  "100%")
  ,("top 50% right",    "100%")
  ,("center right",     "100%")

  ,("0% 100%",         "0 100%")
  ,("left bottom",     "0 100%")
  ,("left 0 bottom",   "0 100%")
  ,("left bottom 0",   "0 100%")
  ,("left 0 bottom 0", "0 100%")
  ,("bottom 0 left 0", "0 100%")
  ,("bottom left 0",   "0 100%")
  ,("bottom 0 left",   "0 100%")
  ,("bottom left",     "0 100%")

  ,("50% 100%",           "bottom")
  ,("bottom",             "bottom")
  ,("center bottom",      "bottom")
  ,("center bottom 0%",   "bottom")
  ,("left 50% bottom",    "bottom")
  ,("left 50% bottom 0%", "bottom")
  ,("bottom 0% left 50%", "bottom")
  ,("bottom left 50%",    "bottom")
  ,("bottom 0% center",   "bottom")
  ,("bottom center",      "bottom")

  ,("100% 100%",          "100% 100%")
  ,("100% bottom",        "100% 100%")
  ,("right bottom",       "100% 100%")
  ,("right 0% bottom",    "100% 100%")
  ,("right bottom 0%",    "100% 100%")
  ,("right 0% bottom 0%", "100% 100%")
  ,("bottom 0% right 0%", "100% 100%")
  ,("bottom 0% right",    "100% 100%")
  ,("bottom right 0%",    "100% 100%")
  ,("bottom right",       "100% 100%")

  -- Other random tests
  ,("10px 15px",          "10px 15px")
  ,("30% 0",              "30% 0")
  ,("27% 50%",            "27%")
  ,("left 0 top 30px",    "0 30px")
  ,("left 10px top 15px", "10px 15px")
  ,("left 15px",          "0 15px")
  ,("10px top",           "10px 0")
  ,("left top 15px",      "0 15px")
  ,("left 10px top",      "10px 0")

  ,("right 0 top 30px",    "100% 30px")
  ,("20px bottom",         "20px 100%")
  ,("right 20px",          "100% 20px")
  ,("top 30px right 0",    "100% 30px")
  ]

spec :: Spec
spec = positionMinificationTests

main :: IO ()
main = hspec spec
