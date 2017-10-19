{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Parser.ValueSpec where

import Test.Hspec
-- import Test.QuickCheck
-- import Hasmin.Parser.Internal

-- import Test.Hspec.Attoparsec (shouldParse, parseSatisfies, (~>))
import Data.Text (Text)
-- import Data.Attoparsec.Text (Parser)
import Hasmin.Parser.Value
import Hasmin.TestUtils

valueParserTest :: Spec
valueParserTest = 
  describe "value parser tests" $ do
    describe "dimension parsing  tests" $
      mapM_ (matchSpecWithDesc value) dimensionTestsInfo
    describe "<gradient> parsing  tests" $
      mapM_ (matchSpecWithDesc value) gradientTestsInfo
    describe "<transform-function> parsing  tests" $
      mapM_ (matchSpec value) transformFunctionParserTestsInfo
      
dimensionTestsInfo :: [(String, Text, Text)]
dimensionTestsInfo =
  [("parse px",   "-1px",   "-1px")
  ,("parse em",   "+1em",   "1em")
  ,("parse ex",   ".1e3ex", "100ex")
  ,("parse ch",   "2ch",    "2ch")
  ,("parse vh",   "1.3vh",  "1.3vh")
  ,("parse vw",   "1.98vw", "1.98vw")
  ,("parse vmin", "3vmin",  "3vmin")
  ,("parse vmax", "4vmax",  "4vmax")
  ,("parse rem",  "0.8rem", ".8rem")
  ,("parse q",    "10q",    "10q")
  ,("parse cm",   "7cm",    "7cm")
  ,("parse mm",   "6mm",    "6mm")
  ,("parse in",   "1in",    "1in")
  ,("parse pc",   "1em",    "1em")
  ,("parse pt",   "1pt",    "1pt")
  ,("parse px",   "1px",    "1px")
  ,("parse deg",  "1deg",   "1deg")
  ,("parse grad", "1grad",  "1grad")
  ,("parse rad",  "1rad",   "1rad")
  ,("parse turn", "1turn",  "1turn")
  ,("parse s",    "1s",     "1s")
  ,("parse ms",   "1ms",    "1ms")
  ,("parse hz",   "1hz",    "1hz")
  ,("parse khz",  "1khz",   "1khz")
  ,("parse dpi",  "1dpi",   "1dpi")
  ,("parse dpcm", "1dpcm",  "1dpcm")
  ,("parse dppx", "1dppx",  "1dppx")
  ,("parse %",    "25e-1%", "2.5%")
  ]

numberParserTests :: Spec
numberParserTests =
  describe "number parser tests. Succeeds in parsing:" $
    mapM_ (matchSpecWithDesc number) numberTestsInfo

numberTestsInfo :: [(String, Text, Text)]
numberTestsInfo = 
  [("zero", "0", "0")
  ,("floating point zero",
    "0.0", "0")
  ,("zero with a leading +",
     "+0.0", "0")
  ,("zero with a leading -", -- it is a valid value!
     "-0.0", "0")
  ,("a simple case of scientific notation",
     "10e3", "10000")
  ,("a complex case of scientific notation",
     "-3.4e-2", "-.034")
  ,("number starting with a dot",
     ".60", ".6")
  ,("a positive integer without leading +",
    "12", "12")
  ,("a positive integer with a leading +",
    "+123", "123")
  ]

gradientTestsInfo :: [(String, Text, Text)]
gradientTestsInfo =
  [("linear-gradient() with default <side-or-corner> value",
    "linear-gradient(to bottom, red, green)","linear-gradient(to bottom,red,green)")
  ,("linear-gradient() with default <angle> value", 
    "linear-gradient(180deg, #000, white)", "linear-gradient(180deg,#000,white)")
  ,("linear-gradient() ommiting default value", 
    "linear-gradient(rgb(0,0,0), hsl(300,100%,25%))", "linear-gradient(rgb(0,0,0),hsl(300,100%,25%))")
  ,("linear-gradient() with comments in-between", 
    "linear-gradient(/**/pink/**/,/**/peru/**/)", "linear-gradient(pink,peru)")
  ]

transformFunctionParserTestsInfo :: [(Text, Text)]
transformFunctionParserTestsInfo =
  [("skew( 0deg )",               "skew(0deg)")
  ,("skew(23GRAD /**/,/**/0deg)", "skew(23grad,0deg)")
  ,("sKeWY(/**/1rad)",            "skewy(1rad)")
  ,("SkeWx(0.2turn/**/)",         "skewx(.2turn)")
  ,("SkeWx(0.2turn)",             "skewx(.2turn)")
  ,("translate(0% )",             "translate(0%)")
  ,("translate( 0 )",             "translate(0)")
  ,("translate( 1in )",           "translate(1in)")
  ,("translate(0%, 0%)",          "translate(0%,0%)")
  ,("translate(0, 0%)",           "translate(0,0%)")
  ,("translate(0%, 0)",           "translate(0%,0)")
  ,("translate(0, 0)",            "translate(0,0)")
  ,("translate(12px, 3em)",       "translate(12px,3em)")
  ,("translateX(0%)",             "translatex(0%)")
  ,("translateX(/* 3q */ 4q)",    "translatex(4q)")
  ]

spec :: Spec
spec = do numberParserTests
          valueParserTest

main :: IO ()
main = hspec spec
