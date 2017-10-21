{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.StringSpec where

import Data.Text (Text)

import Hasmin.Parser.Value
import Hasmin.Types.String
import Hasmin.TestUtils

quotesNormalizationTests :: Spec
quotesNormalizationTests =
    describe "Quotes Normalization" $ do
      describe "normalizes <string>s quotes in general" $
        mapM_ (matchSpecWithDesc f) quotesNormalizationTestsInfo
      describe "normalizes <strings>s quotes inside format()" $
        mapM_ (matchSpec g) unquotingFormatTestsInfo
      describe "unquotes url() <string>s" $
        mapM_ (matchSpec g) unquotingUrlsTestsInfo
      describe "Converts escaped characters properly" $
        mapM_ (matchSpec convertEscaped) escapedCharConversionTestsInfo
  where f = minifyWithTestConfig <$> stringvalue
        g = minifyWithTestConfig <$> textualvalue

quotesNormalizationTestsInfo :: [(String, Text, Text)]
quotesNormalizationTestsInfo =
  [("Convert single quotes into double quotes",
     "'x'", "\"x\"")
  ,("Do not convert single quotes when they enclose a double quote",
     "'\"'", "'\"'")
  ,("Convert escaped double quotes into double quotes, but don't convert the enclosing single quotes into double",
     "'\\22'", "'\"'")
  ,("Don't convert escaped double quotes when enclosed in double quotes",
     "\"\\22\"", "\"\\22\"")
  ]

escapedCharConversionTestsInfo :: [(Text, Text)]
escapedCharConversionTestsInfo =
  [("",           "")
  ,("\\",         "\\")
  ,("\\0",        "\NUL")
  ,("\\2a",       "*")
  ,("\\02a",      "*")
  ,("\\002a",     "*")
  ,("\\0002a",    "*")
  ,("\\00002a",   "*")
  ,("\\00002aa",  "*a")
  ,("*\\00002aa", "**a")
  ,("*\\2az",     "**z")
  ]

unquotingUrlsTestsInfo :: [(Text, Text)]
unquotingUrlsTestsInfo =
  [("url(\"validUrl\")", "url(validUrl)")
  ,("url('validUrl')", "url(validUrl)")
  ,("url('a b')", "url('a b')")
  ,("url(\"a'b\")", "url(\"a'b\")")
  ,("url('a\"b')", "url('a\"b')")
  ]

-- TODO rename this, maybe combine it with some of the other tests
unquotingFormatTestsInfo :: [(Text, Text)]
unquotingFormatTestsInfo =
  [("format('woff')", "format(\"woff\")")]

spec :: Spec
spec = quotesNormalizationTests

main :: IO ()
main = hspec spec
