{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.BasicShapeSpec where

import Data.Text (Text)
import Data.Foldable (traverse_)
import Data.Attoparsec.Text (Parser)

import Hasmin.Parser.Value
import Hasmin.Types.Value
import Hasmin.TestUtils

basicShapeTests :: Spec
basicShapeTests =
  describe "<basic-shape> tests" $
    traverse_ (matchSpec f) basicShapeTestsInfo
  where f :: Parser Value
        f = minifyWithTestConfig <$> value

basicShapeTestsInfo :: [(Text, Text)]
basicShapeTestsInfo =
  [("inset(1px 2px 1px 2px)",              "inset(1px 2px)")
  ,("inset(1px 1px round 4px / 4px)",      "inset(1px round 4px)")
  ,("inset(1px 1px round 0px 0% / 0 0 0)", "inset(1px)")
  ,("ellipse(12px 12px at left bottom)",   "ellipse(9pt 9pt at 0 100%)")
  ,("ellipse(12px 12px at center)",        "ellipse(9pt 9pt)")
  ,("ellipse(12px closest-side)",          "ellipse(9pt)")
  ,("ellipse(farthest-side closest-side)", "ellipse(farthest-side)")
  ,("ellipse(closest-side closest-side)",  "ellipse()")
  ,("polygon(nonzero, 12px 12px)",         "polygon(9pt 9pt)")
  ,("polygon(evenodd, 12px 12px)",         "polygon(evenodd,9pt 9pt)")
  ,("circle(closest-side at center)",      "circle()")
  ,("circle(closest-side at left bottom)", "circle(at 0 100%)")
  ,("circle(12px 12px at 45%)",            "circle(9pt 9pt at 45%)")
  ]

spec :: Spec
spec = basicShapeTests

main :: IO ()
main = hspec spec
