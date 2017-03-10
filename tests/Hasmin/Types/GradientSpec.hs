{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.GradientSpec where

import Control.Monad.Reader (runReader)
import Data.Text (Text)

import Hasmin.Parser.Value
import Hasmin.Types.Class
import Hasmin.Config
import Hasmin.TestUtils

gradientTests :: Spec
gradientTests = 
    describe "<gradient> minification tests" $
      mapM_ (matchSpecWithDesc f) gradientTestsInfo
  where f = (\x -> runReader (minifyWith x) defaultConfig) <$> value
      
gradientTestsInfo :: [(String, Text, Text)]
gradientTestsInfo =
  [("Converts \"to top\" to 0 (unitless 0 <angle>)"
     ,"linear-gradient(to top, red, blue)"
     ,"linear-gradient(0,red,blue)")
  ,("Converts \"to right\" to 90deg"
     ,"linear-gradient(to right, green, violet)"
     ,"linear-gradient(90deg,green,violet)")
  ,("Converts \"to left\" to 270deg"
     ,"linear-gradient(to left, pink, peru)"
     ,"linear-gradient(270deg,pink,peru)")
  ,("Removes \"to bottom\", since that's the default <side-or-corner> value"
     ,"linear-gradient(to bottom, green, violet)"
     ,"linear-gradient(green,violet)")
  ,("Removes \"180deg\", since that equals \"to bottom\", which is the default value"
     ,"linear-gradient(180deg, green, violet)"
     ,"linear-gradient(green,violet)")
  ,("Should not convert \"to top right\""
     ,"linear-gradient(to top right, pink, peru)"
     ,"linear-gradient(to top right,pink,peru)")
  ,("Should not convert \"to bottom left\""
     ,"linear-gradient(to bottom left, pink, peru)"
     ,"linear-gradient(to bottom left,pink,peru)")
  ,("Reduces a <percentage> value to 0 when it is the same as the previous"
     ,"linear-gradient(72deg, pink 25%, peru 25%)"
     ,"linear-gradient(72deg,pink 25%,peru 0)")
  ,("Reduces a <percentage> value to 0 when it is less than the greatest previous one"
     ,"linear-gradient(72deg, pink 50%, peru 40%)"
     ,"linear-gradient(72deg,pink 50%,peru 0)")
  ,("Reduces a <length> value to 0 when it is the same as the previous"
     ,"linear-gradient(72deg, pink 3em, peru 3em)"
     ,"linear-gradient(72deg,pink 3em,peru 0)")
  ,("Reduces a <length> value to 0 when it is less than the greatest previous one"
     ,"linear-gradient(72deg, pink 6pc, peru 90px)" -- 6pc == 96px
     ,"linear-gradient(72deg,pink 6pc,peru 0)")
  ,("Removes default start and end <percentage> values"
     ,"linear-gradient(pink 0%, peru 100%)"
     ,"linear-gradient(pink,peru)")
  ,("Should not remove trailing zero when it is the last stop"
     ,"linear-gradient(pink,peru 0)"
     ,"linear-gradient(pink,peru 0)")
  ,("Reduce complex combination of linear and non linear color hints"
     ,"linear-gradient(pink 1%, peru 10%, green 20%, red 30%, violet 50%, purple 55%, cyan 60%, #ccc 80%, #000 100%)"
     ,"linear-gradient(pink 1%,peru 10%,green,red 30%,violet 50%,purple,cyan 60%,#ccc,#000)")
  ,("Removes circle in radial-gradient() when using a single <length>"
     ,"radial-gradient(circle 1px, pink, peru)"
     ,"radial-gradient(1px,pink,peru)")
  ,("Removes ellipse in radial-gradient() when using two <length-percentage>"
     ,"radial-gradient(ellipse 1px 1px, pink, peru)"
     ,"radial-gradient(1px 1px,pink,peru)")
  ,("Removes ellipse in radial-gradient() when using an <extent-keyword> in second place"
     ,"radial-gradient(ellipse closest-side, pink, peru)"
     ,"radial-gradient(closest-side,pink,peru)")
  ,("Removes ellipse in radial-gradient() when using an <extent-keyword> first"
     ,"radial-gradient(farthest-side ellipse, pink, peru)"
     ,"radial-gradient(farthest-side,pink,peru)")
  ,("Removes 'ellipse farthest-corner' in radial-gradient()"
     ,"radial-gradient(ellipse farthest-corner, pink, peru)"
     ,"radial-gradient(pink,peru)")
  ]

spec :: Spec
spec = gradientTests

main :: IO ()
main = hspec spec
