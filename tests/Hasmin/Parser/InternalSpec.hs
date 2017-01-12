{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Parser.InternalSpec where

import Test.Hspec
-- import Test.QuickCheck
import Hasmin.Parser.Internal
import Hasmin.Parser.Value

import Test.Hspec.Attoparsec (shouldSucceedOn, shouldFailOn)
import Data.Text (Text)
import Hasmin.TestUtils

declarationParserTests :: Spec
declarationParserTests =
  describe "declaration parser tests" $
    mapM_ (matchSpecWithDesc declaration) declarationTestInfo

declarationTestInfo :: [(String, Text, Text)]
declarationTestInfo = 
  [("parses declaration with unknown property and value, ending in ';'",
      "a:b;", "a:b")
  ,("parses declaration with unknown property and value, ending in '}'",
      "a:b}", "a:b")
  ,("ignores comments before colon",
      "a/**/:b;", "a:b")
  ,("ignores comments after colon",
      "a:/**/b;", "a:b")
  ,("parses !important and prints it back without space",
      "a:b ! important;", "a:b!important")
  ,("ignores comments before semicolon",
    "a:b/**/;", "a:b")
  ,("parse declaration starting with '+' (IE <= 7)",
    "+property:a", "+property:a")
  ]

declarationParsersTests :: Spec
declarationParsersTests = 
    describe "Declaration parsers tests" $ do
      it "Parses valid font-style values, and fails with invalids" $ do
        fontStyle `shouldSucceedOn` ("normal" :: Text)
        fontStyle `shouldSucceedOn` ("italic" :: Text)
        fontStyle `shouldSucceedOn` ("oblique" :: Text)
        fontStyle `shouldFailOn` ("anything else" :: Text)

{-
styleRuleParser :: Spec
  describe "style rule parser" $ do
    it "parses rule whose final declaration ends in ';'" $ do
      "a { margin : 0;}" ~> (styleRule 
-}

styleRuleTestInfo :: [(String, Text, Text)]
styleRuleTestInfo = 
  [("simple style rule",
    "a { margin : 0 ;}", "a{margin:0}")
  ,("handles unset", 
    "h1 {\n    border: unset\n}", "h1{border:unset}")
  ,("handles initial", 
    "camelCase {\r color: initial;}", "camelCase{color:initial}")
  ,("handles inherit", 
    "h1 {\n\tpadding: inherit\f}", "h1{padding:inherit}")
  ]

selectorParserTests :: Spec
selectorParserTests =
  describe "selector parser tests" $
    mapM_ (matchSpecWithDesc selector) selectorTestInfo

selectorTestInfo :: [(String, Text, Text)]
selectorTestInfo = 
  [("Element", "h6", "h6")
  ,("Element with namespace", "ns|body", "ns|body")
  ,("Element with any namespace", "*|p", "*|p")
  ,("Element with '|' but no namespace", "|html", "html") -- bar shouldn't show
  ,("Class", ".class", ".class")
  ,("Class name with high BMP char", ".無", ".無")
  --,("Class name starting with number or dash", ".\\1\\#-\\.1 .\\--wie.-gehts", ".1#-.1.--wie.-gehts")
  ,("Class name with emoji", ".☺", ".☺")
  ,("Class name with multiple emoji", ".👊✊", ".👊✊")
  ,("Id", "#AnId", "#AnId")
  --,("Id starting with a number", "#\\5\\#-\\.5", "#5#-.5")
  --,("Id starting with a number", "#\\5\\#-\\.5", "#5#-.5")
  ,("Id with latin-1 character", "#ñ", "#ñ")
-- The following four have a pseudo class syntax, but are pseudo elements
  ,(":after pseudo element", ":after", ":after")
  ,(":before pseudo element", ":before", ":before")
  ,(":first-line pseudo element", ":first-line", ":first-line")
  ,(":first-letter pseudo element", ":first-letter", ":first-letter")
-- Since both syntaxes are valid, we print the shorter one (with a single ':')
  ,("::after pseudo element", "::after", ":after")
  ,("::before pseudo element", "::before", ":before")
  ,("::first-line pseudo element", "::first-line", ":first-line")
  ,("::first-letter pseudo element", "::first-letter", ":first-letter")
  ,("Pseudo class", ":link", ":link")
  --,("Pseudo class with escapes", ":na\\me", ":name")
  --,("Pseudo class with content"
  ,("Universal selector", "*", "*")
  ,("Universal selector with namespace", "n|*", "n|*")
  ,("Universal selector with any namespace", "*|*", "*|*")
  ,("Universal selector with '|' but no namespace", "|*", "*") -- bar shouldn't show
  ,(":not() functional pseudo-class of a selector list",
    ":not(:nth-last-child(2n), elem, #id, .class)", ":not(:nth-last-child(2n),elem,#id,.class)")
  ,(":matches() functional pseudo-class of a selector list",
    ".class:matches(:nth-child(2) , element, #AnID, .aClass)", ".class:matches(:nth-child(2),element,#AnID,.aClass)")
  ]
{-
  [("any element","*", "*")
  ,("an element of type E","E","E")
  ,("an E element with a \"foo\" attribute",
    "E[foo]", "E[foo]")
  ,("an E element whose \"foo\" attribute value is exactly equal to \"bar\"",
    "E[foo=\"bar\"]","E[foo=\"bar\"]" )
  ,("an E element whose \"foo\" attribute value is a list of whitespace-separated values, one of which is exactly equal to \"bar\"",
    "E[foo~=\"bar\"]","E[foo~=\"bar\"]")
  ,("an E element whose \"foo\" attribute value begins exactly with the string \"bar\"",
    "E[foo^=\"bar\"]","E[foo^=\"bar\"]")
  ,("an E element whose \"foo\" attribute value ends exactly with the string \"bar\"",
    "E[foo$=\"bar\"]","E[foo$=\"bar\"]")
  ,("an E element whose \"foo\" attribute value contains the substring \"bar\"",
    "E[foo*=\"bar\"]","E[foo*=\"bar\"]")
  ,("an E element whose \"foo\" attribute has a hyphen-separated list of values beginning (from the left) with \"en\"",
    "E[foo|=\"en\"]","E[foo|=\"en\"]")
  ,("an E element, root of the document",
    "E:root","E:root")
  ,("an E element, the n-th child of its parent",
    "E:nth-child(n)","E:nth-child(n)")
  ,("an E element, the n-th child of its parent, counting from the last one",
    "E:nth-last-child(n)","E:nth-last-child(n)")
  ,("an E element, the n-th sibling of its type",
    "E:nth-of-type(n)","E:nth-of-type(n)")
  ,("an E element, the n-th sibling of its type, counting from the last one",
    "E:nth-last-of-type(n)","E:nth-last-of-type(n)")
  ,("an E element, first child of its parent",
    "E:first-child","E:first-child")
  ,("an E element, last child of its parent",
    "E:last-child","E:last-child")
  ,("an E element, first sibling of its type",
    "E:first-of-type","E:first-of-type")
  ,("an E element, last sibling of its type",
    "E:last-of-type","E:last-of-type")
  ,("an E element, only child of its parent",
    "E:only-child","E:only-child")
  ,("an E element, only sibling of its type",
    "E:only-of-type","E:only-of-type")
  ,("an E element that has no children (including text nodes)",
    "E:empty","E:empty")
  ,("An E element being the source anchor of a hyperlink of which the target is not yet visited",
    "E:link", "E:link") 
  ,("An E element being the source anchor of a hyperlink of which the target is already visited",
    "E:visited", "E:visited") 
  ,("an E element during active action",
    "E:active","E:active")
  ,("an E element during hover action",
    "E:hover","E:hover")
  ,("an E element during focus action",
    "E:focus","E:focus")
  ,("an E element being the target of the referring URI",
    "E:target","E:target")
  ,("an element of type E in language \"fr\" (the document language specifies how language is determined)",
    "E:lang(fr)","E:lang(fr)")
  ,("a user interface element E which is enabled",
    "E:enabled", "E:enabled")
  ,("a user interface element E which is disabled",
    "E:disabled","E:disabled")
  ,("a user interface element E which is checked (for instance a radio-button or checkbox)",
    "E:checked","E:checked")
  ,("the first formatted line of an E element",
    "E::first-line","E:first-line")
  ,("the first formatted letter of an E element",
    "E::first-letter","E:first-letter")
  ,("generated content before an E element",
    "E::before","E:before")
  ,("generated content after an E element",
    "E::after","E:after")
  ,("an E element whose class is \"warning\" (the document language specifies how class is determined).",
    "E.warning","E.warning")
  ,("an E element with ID equal to \"myid\".",
    "E#myid","E#myid")
  ,("an E element that does not match simple selector s",
    "E:not(s)","E:not(s)")
  ,("An F element descendant of an E element",
    "E F", "E F")
  ,("An F element child of an E element",
    "E > F", "E>F")
  ,("An F element immediately preceded by an E element",
    "E + F", "E+F")
  ,("An F element preceded by an E element",
    "E ~ F", "E~F")
  ]
-}

--stylesheetParserTests :: Spec
--stylesheetParserTests =
--  describe "stylesheet parser tests" $
--    mapM_ (matchSpecWithDesc stylesheet) stylesheetTestInfo

--stylesheetTestInfo :: [(String, Text, Text)]
--stylesheetTestInfo = 

spec :: Spec
spec = do declarationParserTests
          selectorParserTests
          declarationParsersTests

main :: IO ()
main = hspec spec
