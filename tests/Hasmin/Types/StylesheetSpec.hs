{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.StylesheetSpec where

import Test.Hspec
import Hasmin.Parser.Internal
import Hasmin.TestUtils

import Data.Text (Text)

atRuleTests :: Spec
atRuleTests =
    describe "at rules parsing and printing" $
      mapM_ (matchSpec atRule) atRuleTestsInfo

atRuleTestsInfo :: [(Text, Text)]
atRuleTestsInfo =
  [("@charset \"UTF-8\";",
      "@charset \"UTF-8\";")
  ,("@import/**/ 'custom.css' ;",
      "@import 'custom.css';")
  ,("@import  \"common.css\" screen , projection;",
      "@import \"common.css\" screen,projection;")
  ,("@import  url(\'landscape.css\')  screen  and  (orientation: landscape);",
      "@import url(\'landscape.css\') screen and (orientation:landscape);")
  ,("@namespace /**/ prefix url(XML-namespace-URL);",
      "@namespace prefix url(XML-namespace-URL);")
  ,("@namespace  prefix /**/  \"XML-namespace-URL\";",
      "@namespace prefix \"XML-namespace-URL\";")
  ,("@media screen {s{a:a}}",
      "@media screen{s{a:a}}")
  ,("@media screen and (min-width: 768px){s{a:a}}",
      "@media screen and (min-width:768px){s{a:a}}")
  ,("@keyframes p { from { background-position: 40px 0 } to { background-position: 0 0 } }",
      "@keyframes p{from{background-position:40px 0}to{background-position:0 0}}")
  ,("@font-face  /**/ {a:a;}",
      "@font-face{a:a}")
  -- ,("@supports (--foo: green) { body { color: green; } }",
   --  "@supports (--foo:green){body{color:green}}")
  -- ,("@supports ( transform-style: preserve ) or ( -moz-transform-style: preserve )",
   --  "@supports (transform-style:preserve) or (-moz-transform-style:preserve)")
  -- ,("@supports ( display : table-cell ) and ( not ( display : list-item ) )",
   --  "@supports (display:table-cell) and (not (display:list-item))")
  -- ,("@document url(http://www.w3.org/) , url-prefix(http://www.w3.org/Style/),\
    --            \ domain(mozilla.org), regexp(\"https:.*\")"
  -- ,("@document url(http://www.w3.org/),url-prefix(http://www.w3.org/Style/),\
    --            \domain(mozilla.org),regexp(\"https:.*\")"
  -- ,("@page { margin: 1in }",
    -- "@page{margin:1in}")
  -- ,("@page :left { font-size: 20pt; }",
    -- "@page:left{font-size:20pt}")
  -- ,("@page toc, index { size:8.5in 11in; }",
    -- "@page toc,index{size:8.5in 11in}")
  -- ,("@viewport { min-width: 640px; max-width: 800px; }",
    -- "@viewport{min-width:640px;max-width:800px}")
  -- ,("@counter-style circled-alpha { system: fixed; symbols: Ⓐ Ⓑ Ⓒ; suffix: " "; }",
    -- "@counter-style circled-alpha{system:fixed;symbols:Ⓐ Ⓑ Ⓒ;suffix:" "}")
  -- ,("@font-feature-values Jupiter Sans { @swash { delicate: 1; flowing: 2; } }",
    -- "@font-feature-values Jupiter Sans{@swash{delicate:1;flowing:2}}",
  ] 

spec :: Spec
spec = atRuleTests

main :: IO ()
main = hspec spec
