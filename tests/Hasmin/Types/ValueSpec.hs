{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.ValueSpec where

import Data.Monoid ((<>))
import Data.Text (Text)

import Hasmin.Parser.Internal
import Hasmin.Parser.Value
import Hasmin.TestUtils
import Hasmin.Types.Class
import Hasmin.Types.Declaration

valueTests :: Spec
valueTests = do
    describe "value" $
      mapM_ (matchSpec (minify <$> value)) valueTestsInfo
    describe "<bg-layer> value" $
      mapM_ (matchSpec (minify <$> declaration)) bgTests

valueTestsInfo :: [(Text, Text)]
valueTestsInfo =
  [("360deg",                "1turn")
  ,("1000ms",                "1s")
  ,("1000Hz",                "1khz")
  ,("steps(5, end)",         "steps(5)")
  ,("local('DejaVu Serif')", "local(dejavu serif)")
  ]

bgTests :: [(Text, Text)]
bgTests =
  [("background: url('test.jpg') center / 10% auto, no-repeat repeat fixed #f00"
    , "background:url(test.jpg) 50%/10%,repeat-y fixed red")
  ,("background: 0 0, none"
    , "background:none,none") -- replaced because none is more frequent than 0 0 (i.e. for compression)
  ]

spec :: Spec
spec = valueTests

main :: IO ()
main = hspec spec
