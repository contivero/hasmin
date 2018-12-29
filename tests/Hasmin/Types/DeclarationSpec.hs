{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Types.DeclarationSpec where

import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import Data.Text (Text)

import Hasmin.Parser.Internal
import Hasmin.TestUtils
import Hasmin.Types.Declaration

declarationTests :: Spec
declarationTests =
    describe "declaration" $
      mapM_ (matchSpec declaration) declarationTestsInfo

propertySpecificTests :: Spec
propertySpecificTests =
    describe "property specific changes" $
      mapM_ (matchSpec f) propertySpecificTestsInfo
  where f = minifyWithTestConfig <$> declaration

propertySpecificTestsInfo :: [(Text, Text)]
propertySpecificTestsInfo =
  [("font-family:Arial Black Long Name", "font-family:arial black long name")
  ,("font:9px Arial Black Long Name", "font:9px arial black long name")
  ,("font-family:\"SANS-SERIF\"", "font-family:\"sans-serif\"")
  ,("font-family:'Helvetica','Arial',sans-serif", "font-family:helvetica,arial,sans-serif")
  -- TODO make this work
  -- should fallback and parse till ';'
  -- ,("font-size:1em\nline-height:1.38;", "font-size:1em\nline-height:1.38;")

  ,("background-size:1px auto, 2px auto", "background-size:1px,2px")

  ,("text-shadow: 1px 1px 1px #ff0000", "text-shadow:1px 1px 1px red")
  ,("text-shadow: 12px 12px 12px", "text-shadow:9pt 9pt 9pt")
  ,("text-shadow: 1px 1px 0 green", "text-shadow:1px 1px green")
  ,("text-shadow: 1px 1px 0 green, 2px 2px 0 blue", "text-shadow:1px 1px green,2px 2px blue")

  ,("font-weight:normal", "font-weight:400")
  ,("font-weight:bold",   "font-weight:700")

  ,("word-spacing:normal",  "word-spacing:0")
  ,("word-spacing:initial", "word-spacing:0")
  ,("word-spacing:unset",   "word-spacing:unset")
  ,("word-spacing:inherit", "word-spacing:unset")

  ,("vertical-align:baseline", "vertical-align:0")
  ,("vertical-align:initial",  "vertical-align:0")
  ,("vertical-align:unset",    "vertical-align:0")

  ,("transform-origin:left top", "transform-origin:0 0")
  ,("transform-origin:left 0",   "transform-origin:0 0")
  ,("transform-origin:0 top",    "transform-origin:0 0")
  ,("transform-origin:0 0",      "transform-origin:0 0")
  ,("transform-origin:top left", "transform-origin:0 0")
  ,("transform-origin:top 0",    "transform-origin:0 0")
  ,("transform-origin:0 left",   "transform-origin:0 0")

  ,("transform-origin:left bottom", "transform-origin:0 100%")
  ,("transform-origin:0 bottom",    "transform-origin:0 100%")
  ,("transform-origin:left 100%",   "transform-origin:0 100%")
  ,("transform-origin:0 100%",      "transform-origin:0 100%")
  ,("transform-origin:bottom left", "transform-origin:0 100%")
  ,("transform-origin:100% left",   "transform-origin:0 100%")
  ,("transform-origin:bottom 0",    "transform-origin:0 100%")

  ,("transform-origin:right top", "transform-origin:100% 0")
  ,("transform-origin:100% top",  "transform-origin:100% 0")
  ,("transform-origin:right 0",   "transform-origin:100% 0")
  ,("transform-origin:top 100%",  "transform-origin:100% 0")
  ,("transform-origin:0% right",  "transform-origin:100% 0")
  ,("transform-origin:top right", "transform-origin:100% 0")

  ,("transform-origin:right bottom", "transform-origin:100% 100%")
  ,("transform-origin:100% bottom",  "transform-origin:100% 100%")
  ,("transform-origin:right 100%",   "transform-origin:100% 100%")
  ,("transform-origin:bottom 100%",  "transform-origin:100% 100%")
  ,("transform-origin:100% right",   "transform-origin:100% 100%")
  ,("transform-origin:bottom right", "transform-origin:100% 100%")

  ,("transform-origin:center bottom", "transform-origin:bottom")
  ,("transform-origin:50% bottom",    "transform-origin:bottom")
  ,("transform-origin:center 100%",   "transform-origin:bottom")
  ,("transform-origin:bottom 50%",    "transform-origin:bottom")
  ,("transform-origin:bottom center", "transform-origin:bottom")

  ,("transform-origin:center top", "transform-origin:top")
  ,("transform-origin:50% top",    "transform-origin:top")
  ,("transform-origin:center 0px", "transform-origin:top")
  ,("transform-origin:top 50%",    "transform-origin:top")
  ,("transform-origin:top center", "transform-origin:top")

  ,("transform-origin:center right", "transform-origin:100%")
  ,("transform-origin:50% right",    "transform-origin:100%")
  ,("transform-origin:right 50%",    "transform-origin:100%")
  ,("transform-origin:100% center",  "transform-origin:100%")
  ,("transform-origin:right center", "transform-origin:100%")

  ,("transform-origin:center left", "transform-origin:0")
  ,("transform-origin:50% left",    "transform-origin:0")
  ,("transform-origin:left 50%",    "transform-origin:0")
  ,("transform-origin:0% center",   "transform-origin:0")
  ,("transform-origin:left center", "transform-origin:0")

  ,("transform-origin:center",            "transform-origin:50%")
  ,("transform-origin:center center",     "transform-origin:50%")
  ,("transform-origin:center 50%",        "transform-origin:50%")
  ,("transform-origin:50% center",        "transform-origin:50%")
  ,("transform-origin:50% 50%",           "transform-origin:50%")
  ,("transform-origin:center center 0",   "transform-origin:50%")
  ,("transform-origin:center center 1px", "transform-origin:50% 50% 1px")

  ,("transform-origin:left 30px",   "transform-origin:0 30px")
  ,("transform-origin:30px left",   "transform-origin:0 30px")
  ,("transform-origin:right 30px",  "transform-origin:100% 30px")
  ,("transform-origin:30px right",  "transform-origin:100% 30px")
  ,("transform-origin:bottom 30px", "transform-origin:30px 100%")
  ,("transform-origin:30px bottom", "transform-origin:30px 100%")
  ,("transform-origin:top 30px",    "transform-origin:30px 0")
  ,("transform-origin:30px top",    "transform-origin:30px 0")
  ,("transform-origin:center 30px", "transform-origin:50% 30px")
  ,("transform-origin:30px center", "transform-origin:30px")

  -- 3 value syntax
  ,("transform-origin:left top 1px",        "transform-origin:0 0 1px")
  ,("transform-origin:top left 1px",        "transform-origin:0 0 1px")
  ,("transform-origin:left bottom 1px",     "transform-origin:0 100% 1px")
  ,("transform-origin:bottom left 1px",     "transform-origin:0 100% 1px")
  ,("transform-origin:right top 1px",       "transform-origin:100% 0 1px")
  ,("transform-origin:top right 1px",       "transform-origin:100% 0 1px")
  ,("transform-origin:right bottom 1px",    "transform-origin:100% 100% 1px")
  ,("transform-origin:bottom right 1px",    "transform-origin:100% 100% 1px")
  ,("transform-origin:center bottom 1px",   "transform-origin:50% 100% 1px")
  ,("transform-origin:bottom center 1px",   "transform-origin:50% 100% 1px")
  ,("transform-origin:center top 1px",      "transform-origin:50% 0 1px")
  ,("transform-origin:top center 1px",      "transform-origin:50% 0 1px")
  ,("transform-origin:center right 1px",    "transform-origin:100% 50% 1px")
  ,("transform-origin:right center 1px",    "transform-origin:100% 50% 1px")
  ,("transform-origin:center left 1px",     "transform-origin:0 50% 1px")
  ,("transform-origin:left center 1px",     "transform-origin:0 50% 1px")

  ,("overflow:unset",           "overflow:unset")
  ,("overflow:initial",         "overflow:unset")
  ,("overflow:visible",         "overflow:unset")
  ,("overflow:visible visible", "overflow:unset")
  ,("overflow:clip visible",    "overflow:clip visible")
  ,("overflow:visible clip",    "overflow:visible clip")
  ,("overflow:clip clip",       "overflow:clip")
  ]


declarationTestsInfo :: [(Text, Text)]
declarationTestsInfo =
  [("box-shadow:0 7px 0 #fefefe, 0 14px 0 #fefefe",
    "box-shadow:0 7px 0 #fefefe,0 14px 0 #fefefe")
  ]

cleaningTests :: Spec
cleaningTests =
    describe "cleaning function" $
      mapM_ (matchSpecWithDesc f) cleaningTestsInfo
  where f = (Declarations . clean) <$> declarations

cleaningTestsInfo :: [(String, Text, Text)]
cleaningTestsInfo =
  [("Merge margin with later margin-top",
        "margin:0;margin-top:5px",
        "margin:5px 0 0")
  ,("Merge margin with later margin-right",
        "margin:2px;margin-right:1in",
        "margin:2px 1in 2px 2px")
  ,("Merge margin with later margin-bottom",
        "margin:auto;margin-bottom:.2em",
        "margin:auto auto .2em")
  ,("Merge margin with later margin-left",
        "margin:fill;margin-left:9px",
        "margin:fill fill fill 9px")
  ,("Merge margin with later margin-top, with declaration in-between",
        "margin:0;color:red;margin-top:5px",
        "margin:5px 0 0;color:red")
  ,("Merge margin with later margin-right, with declaration in-between",
        "margin:2px;color:red;margin-right:1in",
        "margin:2px 1in 2px 2px;color:red")
  ,("Merge margin with later margin-bottom, with declaration in-between",
        "margin:auto;color:red;margin-bottom:.2em",
        "margin:auto auto .2em;color:red")
  ,("Merge margin with later margin-left, with declaration in-between",
        "margin:fill;color:red;margin-left:9px",
        "margin:fill fill fill 9px;color:red")
  ,("Drop a margin-top overwritten by a following margin",
        "margin-top:5px;margin:0",
        "margin:0")
  ,("Drop a margin-right overwritten by a following margin",
        "margin-right:1px;margin:2px",
        "margin:2px")
  ,("Drop a margin-bottom overwritten by a following margin",
        "margin-bottom:.2em;margin:auto",
        "margin:auto")
  ,("Drop a margin-left overwritten by a following margin",
        "margin-left:9px;margin:fill",
        "margin:fill")
  ,("Drop a margin-top overwritten by later margin, with a declaration in-between",
        "margin-top:5px;padding:0;margin:0",
        "padding:0;margin:0")
  ,("Drop a margin-right overwritten by later margin, with a declaration in-between",
        "margin-right:1px;padding:0;margin:2px",
        "padding:0;margin:2px")
  ,("Drop a margin-bottom overwritten by later margin, with a declaration in-between",
        "margin-bottom:.2em;padding:0;margin:auto",
        "padding:0;margin:auto")
  ,("Drop a margin-left overwritten by later margin, with a declaration in-between",
        "margin-left:9px;padding:0;margin:fill",
        "padding:0;margin:fill")
  ,("Drop a margin-top overwritten by a later margin-top, with a declaration in-between",
        "margin-top:99px;color:red;margin-top:0",
        "color:red;margin-top:0")
  ,("Drop a margin-right overwritten by a later margin-right, with a declaration in-between",
        "margin-right:99px;color:red;margin-right:0",
        "color:red;margin-right:0")
  ,("Drop a margin-bottom overwritten by a later margin-bottom, with a declaration in-between",
        "margin-bottom:99px;color:red;margin-bottom:0",
        "color:red;margin-bottom:0")
  ,("Drop a margin-left overwritten by a later margin-left, with a declaration in-between",
        "margin-left:99px;color:red;margin-left:0",
        "color:red;margin-left:0")
  ,("Drop a margin-top overwritten by a previous margin-top with !important",
        "margin-top:99px!important;margin-top:0",
        "margin-top:99px!important")
  ,("Drop a margin-right overwritten by a previous margin-right with !important",
        "margin-right:99px!important;margin-right:0",
        "margin-right:99px!important")
  ,("Drop a margin-bottom overwritten by a previous margin-bottom with !important",
        "margin-bottom:99px!important;margin-bottom:0",
        "margin-bottom:99px!important")
  ,("Drop a margin-left overwritten by a previous margin-left with !important",
        "margin-left:99px!important;margin-left:0",
        "margin-left:99px!important")
  ,("Drop a margin-top overwritten by a previous margin-top with !important, with a declaration in-between",
        "margin-top:99px!important;margin:auto;margin-top:0",
        "margin-top:99px!important;margin:auto")
  ,("Drop a margin-right overwritten by a previous margin-right with !important, with a declaration in-between",
        "margin-right:99px!important;margin:auto;margin-right:0",
        "margin-right:99px!important;margin:auto")
  ,("Drop a margin-bottom overwritten by a previous margin-bottom with !important, with a declaration in-between",
        "margin-bottom:99px!important;margin:auto;margin-bottom:0",
        "margin-bottom:99px!important;margin:auto")
  ,("Drop a margin-left overwritten by a previous margin-left with !important, with a declaration in-between",
        "margin-left:99px!important;margin:auto;margin-left:0",
        "margin-left:99px!important;margin:auto")
  -- ,("When merging, translate initial keyword to the initial value",
        -- "margin:0;margin-top:initial",
        -- "margin:0")
  -- ,("When merging, translate unset keyword of a property that inherits to the initial value",
        -- "margin:0;margin-top:unset",
        -- "margin:0")
  -- ,("Don't merge properties with inherit keyword",
        -- "margin:0;margin-top:inherit",
        -- "margin:0;margin-top:inherit")
  ]

minifyDecTests :: Spec
minifyDecTests =
    describe "minifyDec function" $
      mapM_ (matchSpec (minifyWithTestConfig <$> declaration)) minifyDecTestsInfo

shorthandInitialValuesTests :: Spec
shorthandInitialValuesTests = do
    testMatches "background minification" backgroundTestsInfo
    testMatches "transition minification" transitionTestsInfo
    testMatches "animation minification" animationTestsInfo
    testMatches "font minification" fontTestsInfo
    testMatches "outline minification" outlineTestsInfo
    anyOrderShorthandTests
  where testMatches :: String -> [(String, Text, Text)] -> SpecWith ()
        testMatches desc i = describe desc $
                          mapM_ (matchSpecWithDesc (minifyWithTestConfig <$> declaration)) i

backgroundTestsInfo :: [(String, Text, Text)]
backgroundTestsInfo =
  [("Does not remove <position> from background when the <bg-size> cannot be removed",
    "background: 0 0 / 3px", "background:0 0/3px")
  ,("Removes <position> when it is the default and no <bg-size> is present",
    "background: #fff 0 0", "background:#fff")
  ,("Removes both <position> and <bg-size> when they are the default",
    "background: #fff 0 0/auto", "background:#fff")
  ,("Removes background-clip keyword when it equals the background-origin keyword",
    "background: border-box #fff border-box", "background:border-box #fff")
  ,("Removes the keywords for background-origin and background-clip when both are the initial ones",
    "background: #fff padding-box border-box", "background:#fff")
  ,("Removes the background-attachment keyword when it is the initial one",
    "background: #fff scroll", "background:#fff")
  ,("Removes the background-color value when it is the initial one",
    "background: url(img.png) transparent", "background:url(img.png)")
  ,("Removes background-image when it is the default (i.e. none)",
    "background: none #fff", "background:#fff")
  ,("Removes background-repeat when it is the default (i.e. repeat)",
    "background: repeat #fff", "background:#fff")
  ]

transitionTestsInfo :: [(String, Text, Text)]
transitionTestsInfo =
  [("Removes transition-duration when it is the default",
    "transition: ease-out some-property 0s", "transition:ease-out some-property")
  ,("Removes transition-delay when it is the default",
    "transition: ease-out 1s some-property 0s", "transition:ease-out 1s some-property")
  ,("Leaves an initial (i.e. 0s) transition-duration when the transition-delay is not initial (i.e. also 0s)",
    "transition: all 0s 1s ease", "transition:0s 1s")
  ,("Removes transition-timing-function when it is the default",
    "transition: some-property 1s ease", "transition:1s some-property")
  ,("Removes transition-property when it is the default",
    "transition: ease-out all 1s", "transition:ease-out 1s")
  ,("Leaves only 0s when every present value is initial",
    "transition: all 0s 0s ease", "transition:0s")
  ]

animationTestsInfo :: [(String, Text, Text)]
animationTestsInfo =
  [("Removes default animation-fill-mode when the animation-name doesn't overlap with its keywords",
    "animation:none animName", "animation:animName")
  ,("Leaves default animation-fill-mode when the animation-name overlaps with one of its keywords",
    "animation:none backwards", "animation:none backwards")
  ,("Removes default animation-fill-mode and default animation-name when both are used simultaneously",
    "animation:ease-out none none", "animation:ease-out")
  ,("Removes default animation-timing-function",
    "animation:animName ease", "animation:animName")
  ,("Removes default animation-duration",
    "animation:0s ease-out", "animation:ease-out")
  ,("Removes default animation-delay",
    "animation:1s 0s ease-out", "animation:1s ease-out")
  ,("Leaves default animation-duration when animation-delay is not the default",
    "animation:0s 1s ease-out", "animation:0s 1s ease-out")
  ,("Removes default animation-iteration-count",
    "animation:1 ease-out", "animation:ease-out")
  ,("Removes default animation-direction when the animation-name doesn't overlap with its keywords",
    "animation:normal animName", "animation:animName")
  ,("Leaves default animation-direction when the animation-name overlaps with one of its keywords",
    "animation:normal alternate", "animation:normal alternate")
  ,("Removes default animation-play-state when the animation-name doesn't overlap with its keywords",
    "animation:running animName", "animation:animName")
  ,("Leaves default animation-play-state when the animation-name overlaps with one of its keywords",
    "animation:running paused", "animation:running paused")
  ]

fontTestsInfo :: [(String, Text, Text)]
fontTestsInfo =
  [("Removes default font-style",
    "font:normal condensed bolder small-caps 9px/1 sans", "font:small-caps bolder condensed 9px/1 sans")
  ,("Removes default font-variant",
    "font:normal italic condensed bolder 9px/1 sans", "font:italic bolder condensed 9px/1 sans")
  ,("Removes default font-weight",
    "font:italic normal condensed small-caps 9px/1 sans", "font:italic small-caps condensed 9px/1 sans")
  ,("Converts bold font-weight into 700",
    "font:bold 9px sans", "font:700 9px sans")
  ,("Removes default font-stretch",
    "font:italic normal small-caps bolder 9px/1 sans", "font:italic small-caps bolder 9px/1 sans")
  ,("Removes default line-height",
    "font:9px/normal sans", "font:9px sans")
  ,("Lowercases and removes quotes from font-family values",
    "font:9px 'Arial Black', \"sans-serif\", sans", "font:9px arial black,\"sans-serif\",sans")
  ]

-- Tests for the removal of outline initial values. Almost identical to those
-- shorthands in the anyOrderShorthandTests, except for the invert color.
outlineTestsInfo :: [(String, Text, Text)]
outlineTestsInfo =
  [("Removes default outline-color",
    "outline:invert solid 1px", "outline:solid 1px")
  ,("Removes default outline-style",
    "outline:#fff none 1px", "outline:#fff 1px")
  ,("Removes default outline-width",
    "outline:#fff solid medium", "outline:#fff solid")
  ,("Leaves none as the shortest initial value",
    "outline:invert", "outline:none")
  ]

-- Tests for shorthands whose initial values are "medium none currentColor",
-- and can go on any order.
anyOrderShorthandTests :: Spec
anyOrderShorthandTests =
    describe "Shorthands that accept values in any order (mostly: medium none currentColor)" $
      traverse_ (mapM_ (matchSpec (minifyWithTestConfig <$> declaration))) d
  where d = fmap f shorthandsToTest
        f z = fmap (\(x,y) -> (z <> ":" <> x, z <> ":" <> y)) shorthandEquivalences
        shorthandsToTest = ["border"
                           ,"border-top"
                           ,"border-right"
                           ,"border-bottom"
                           ,"border-left"
                           ,"column-rule"]
        shorthandEquivalences = [("initial",                   "none")
                                ,("unset",                     "none")
                                ,("none",                      "none")
                                ,("medium",                    "none")
                                ,("currentColor",              "none")
                                ,("medium none",               "none")
                                ,("medium currentColor",       "none")
                                ,("none currentColor",         "none")
                                ,("medium none currentColor",  "none")
                                ,("medium none red",           "red")
                                ,("thick none currentColor",   "thick")
                                ,("thick dotted currentColor", "thick dotted")]

minifyDecTestsInfo :: [(Text, Text)]
minifyDecTestsInfo =
  [("BORDER-bottom: initial",                   "border-bottom:none")
  ,("BORDER-bottom: unset",                     "border-bottom:none")
  ,("BORDER-bottom: none",                      "border-bottom:none")
  ,("BORDER-bottom: medium",                    "border-bottom:none")
  ,("BORDER-bottom: currentColor",              "border-bottom:none")
  ,("BORDER-bottom: medium none",               "border-bottom:none")
  ,("BORDER-bottom: medium currentColor",       "border-bottom:none")
  ,("BORDER-bottom: none currentColor",         "border-bottom:none")
  ,("BORDER-bottom: medium none currentColor",  "border-bottom:none")
  ,("BORDER-bottom: medium none red",           "border-bottom:red")
  ,("BORDER-bottom: thick none currentColor",   "border-bottom:thick")
  ,("BORDER-bottom: thick dotted currentColor", "border-bottom:thick dotted")
  ,("BORDER-bottom: 12px red",                  "border-bottom:9pt red")
-- widows inherits, and its initial value is 2
  ,("widows: unset",   "widows:unset")
  ,("widows: inherit", "widows:unset")
  ,("widows: initial", "widows:2")
-- font-synthesis inherits, and its initial value is "weight style",
-- which are two keywords, both needed, in any order.
  ,("font-synthesis: unset",        "font-synthesis:unset")
  ,("font-synthesis: inherit",      "font-synthesis:unset")
  ,("font-synthesis: initial",      "font-synthesis:initial")
  ,("Font-synthesis: weight style", "font-synthesis:initial")
  ,("font-synthesis: style weight", "font-synthesis:initial")
  ,("font-synthesis: weight",       "font-synthesis:weight")
  ,("font-synthesis: style",        "font-synthesis:style")
-- background-size: The first value gives the width of the corresponding image,
-- the second value its height. Its initial value is 'auto', and it doesn't
-- inherit. If only one value is given the second is assumed to be ‘auto’.
  ,("background-size: 96px",      "background-size:6pc")
  ,("background-size: auto",      "background-size:auto")
  ,("background-size: unset",     "background-size:auto")
  ,("background-size: initial",   "background-size:auto")
  ,("background-size: auto auto", "background-size:auto")
  ,("background-size: auto 96px", "background-size:auto 6pc")
  ,("background-size: 96px auto", "background-size:6pc")
  ]

spec :: Spec
spec = do cleaningTests
          minifyDecTests
          shorthandInitialValuesTests
          declarationTests
          propertySpecificTests

main :: IO ()
main = hspec spec
