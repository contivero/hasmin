{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.TimingFunctionSpec where

import Test.Hspec

import Data.Text (Text)
import Hasmin.Parser.Value
import Hasmin.Types.TimingFunction
import Hasmin.TestUtils

timingFunctionTests :: Spec
timingFunctionTests =
    describe "<timing-function> minification tests" $
      mapM_ (matchSpec f) timingFunctionTestsInfo
  where f = minifyWithTestConfig <$> timingFunction

quickcheckTimingFunction :: Spec
quickcheckTimingFunction =
    describe "<timing-function> quickcheck tests" .
      it "Minified <timing-function> maintains semantical equivalence" $
        property (prop_minificationEq :: TimingFunction -> Bool)

timingFunctionTestsInfo :: [(Text, Text)]
timingFunctionTestsInfo =
  [("cubic-bezier(0.25, 0.1, 0.25, 1)", "ease")
  ,("cubic-bezier(0.42, 0, 1, 1)",      "ease-in")
  ,("cubic-bezier(0.42, 0, 0.58, 1)",   "ease-in-out")
  ,("cubic-bezier(0, 0, 1, 1)",         "linear")
  ,("cubic-bezier(0, 0, 0.58, 1)",      "ease-out")
  ,("cubic-bezier(0.42, 0, 0.5, 1)",    "cubic-bezier(.42,0,.5,1)")
  ,("cubic-bezier(0, 0, 0.5, 1)",       "cubic-bezier(0,0,.5,1)")
  ,("cubic-bezier(0, 0.3, 0.5, 1)",     "cubic-bezier(0,.3,.5,1)")
  ,("cubic-bezier(0.6, 0, 0.5, 1)",     "cubic-bezier(.6,0,.5,1)")
  ,("steps(1, end)",                    "step-end")
  ,("steps(2, end)",                    "steps(2)")
  ,("steps(1)",                         "step-end")
  ,("steps(1, start)",                  "step-start")
  ,("steps(2)",                         "steps(2)")
  ,("steps(2, start)",                  "steps(2,start)")
  ]

spec :: Spec
spec = do timingFunctionTests
          quickcheckTimingFunction

main :: IO ()
main = hspec spec
