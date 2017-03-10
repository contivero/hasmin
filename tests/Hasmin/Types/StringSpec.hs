{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.StringSpec where

import Data.Text (Text)

import Hasmin.Parser.Value
import Hasmin.TestUtils
import Hasmin.Types.Class

quotesNormalizationTests :: Spec
quotesNormalizationTests =
    describe "Quotes Normalization" $ do
      describe "normalizes <string>s quotes in general" $
        mapM_ (matchSpecWithDesc f) quotesNormalizationTestsInfo
      describe "normalizes <strings>s quotes inside format()" $
        mapM_ (matchSpec g) unquotingFormatTestsInfo
      describe "unquotes url() <string>s" $
        mapM_ (matchSpec g) unquotingUrlsTestsInfo
  where f = minify <$> stringvalue
        g = minify <$> textualvalue

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
