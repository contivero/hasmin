{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.TransformFunctionSpec where

import Control.Applicative ((<|>))
import Control.Monad.Reader (runReader)
import Data.Text (Text)

import Hasmin.Config
import Hasmin.Parser.Value
import Hasmin.TestUtils
import Hasmin.Types.Class
import Hasmin.Types.TransformFunction
import Hasmin.Types.Value

transformTests :: Spec
transformTests =
    describe "transform function conversion" $
      mapM_ (matchSpec (minify <$> textualvalue)) transformTestsInfo

combinationTests :: Spec
combinationTests =
    describe "transform function combination" $
      mapM_ (matchSpecWithDesc (g <$> (values "transform" <|> valuesFallback))) functionCombinationTestsInfo
  where g = mkValues . fmap TransformV . f . fmap (\(TransformV t) -> t) . valuesToList
        f x = runReader (combine x) defaultConfig

transformTestsInfo :: [(Text, Text)]
transformTestsInfo =
  [("translate(0)",             "skew(0)")
  ,("translateX(0)",            "skew(0)")
  ,("translateY(0)",            "skew(0)")
  ,("translate3d(0,0,0)",       "skew(0)")
  ,("translate3d(0%,0,0)",      "skew(0)")
  ,("translate3d(-100%,0,0)",   "translate(-100%)")
  ,("translate3d(0,-100%,0)",   "translatey(-100%)")
  ,("translate3d(0%,0%,5px)",   "translatez(5px)")
  ,("translate3d(2em,0,0)",     "translate(2em)")
  ,("translate3d(0,3em,0)",     "translatey(3em)")
  ,("translate3d(0,0,4em)",     "translatez(4em)")
  ,("scale3d(4, 1, 1)",         "scale(4)")
  ,("scale3d(1, 5, 1)",         "scaley(5)")
  ,("scale3d(1, 1, 6)",         "scalez(6)")
  ,("matrix(1,0,0,0,0,/**/ 0)", "scaley(0)")
  ,("matrix(7,0,0,8,0, 0)",     "scale(7,8)")
  ]

functionCombinationTestsInfo :: [(String, Text, Text)]
functionCombinationTestsInfo =
  [("Combines consecutive absolute value translate() functions into one",
    "translate(10px) translate(0) translate(10px)", "translate(20px)")
  ,("Don't combine an skew(90deg), since tan(90deg) = ∞",
    "skew(90deg) translate(1px)", "skew(90deg) translate(1px)")
  ,("Remove identity matrices from lists (e.g. translate(0))",
    "skew(90deg) translate(0px)", "skew(90deg)")
  -- ,("Remove identity matrices at the end of a list"
    -- "skew(90deg) translate(100%) skew(0)", "skew(90deg) translate(100%)")
  -- ,("Remove identity matrices at the beginning of a list"
    -- "skew(0) translate(100%) skew(90deg)", "translate(100%) skew(90deg)")
  ]


spec :: Spec
spec = do transformTests
          combinationTests

main :: IO ()
main = hspec spec
