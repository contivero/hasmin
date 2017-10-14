{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Properties
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Properties
    ( PropertyInfo(..)
    , propertiesTraits
    ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Hasmin.Types.Value
import Hasmin.Parser.Value

data PropertyInfo =
      PropertyInfo { initialValues :: Maybe Values
                   , inherits :: Bool
                   , overwrittenBy :: [Text] -- shorthands that overwrite it
                   , subproperties :: [Text] -- longhands that can be merged into it
                   } deriving (Show)

-- Converts the tuples with CSS properties data into a tuple containing a
-- property's name, and its traits. Allows keeping the table in a more readable
-- and maintainable format.
processTuples :: [(Text, Text, Bool, [Text], [Text])] -> [(Text, PropertyInfo)]
processTuples = foldr (\(p,t,i,a,b) xs -> (p, PropertyInfo (getValues p t) i a b) : xs) []
  where getValues p s = case parseOnly (values p <|> valuesFallback) s of
                          Right initialValues -> Just initialValues
                          Left _              -> Nothing

propertiesTraits :: Map Text PropertyInfo
propertiesTraits = Map.fromList $ processTuples
  [("align-content",                      "stretch", False, mempty, mempty)
  ,("align-items",                        "stretch", False, mempty, mempty)
  ,("align-self",                         "auto", False, mempty, mempty)
  ,("all",                                mempty, False, mempty, mempty) -- No practical initial value
  ,("animation",                          mempty, False, mempty,["animation-name","animation-duration","animation-timing-function","animation-delay","animation-iteration-count","animation-direction","animation-fill-mode","animation-play-state"])
  ,("animation-delay",                    "0s", False, ["animation"], mempty) -- experimental
  ,("animation-direction",                "normal", False, ["animation"], mempty) -- experimental
  ,("animation-duration",                 "0s", False, mempty, mempty) -- experimental
  ,("animation-fill-mode",                "none", False, ["animation"], mempty) -- experimental
  ,("animation-iteration-count",          "1", False, ["animation"], mempty) -- experimental
  ,("animation-name",                     "none", False, ["animation"], mempty) -- experimental
  ,("animation-play-state",               "running", False, ["animation"], mempty) -- experimental
  ,("animation-timing-function",          "ease", False, ["animation"], mempty) -- experimental
  ,("backface-visibility",                "visible", False, mempty, mempty)
  ,("-webkit-backface-visibility",        "visible", False, mempty, mempty)
  ,("backdrop-filter",                    "none", False, mempty, mempty) -- experimental
  ,("-webkit-backdrop-filter",            "none", False, mempty, mempty) -- experimental
  ,("background",                         mempty {-shorthand-}, False, mempty, ["background-image", "background-position", "background-size", "background-repeat", "background-origin", "background-clip", "background-attachment", "background-color"])
  ,("background-attachment",              "scroll", False, ["background"], mempty)
  ,("background-blend-mode",              "normal", False, mempty, mempty) -- TODO find out if these mempty are alright!
  ,("background-clip",                    "border-box", False, ["background"], mempty)
  ,("-webkit-background-clip",            "border-box", False, ["background"], mempty)
  ,("background-color",                   "transparent", False, ["background"], mempty)
  ,("background-image",                   "none", False, ["background"], mempty)
  ,("background-origin",                  "padding-box", False, ["background"], mempty)
  ,("background-position",                mempty, False, ["background"], ["background-position-x", "background-position-y"]) --shorthand
  ,("background-position-x",              "left", False, ["background", "background-position"], mempty)
  ,("background-position-inline",         mempty {-not applicable-}, False, mempty, mempty)
  ,("background-position-block",          mempty {-not applicable-}, False, mempty, mempty)
  ,("background-position-y",              "top", False, ["background", "background-position"], mempty)
  ,("background-repeat",                  "repeat", False, ["background"], mempty)
  ,("background-size",                    "auto", False, ["background"], mempty)
  ,("block-size",                         "auto", False, mempty, mempty) -- experimental
  ,("border",                             "medium none currentcolor", False, mempty, ["border-width", "border-style", "border-color"]) -- shorthand!
  -- border-block-* would go here
  ,("border-bottom",                      "medium none currentcolor", False, ["border"], ["border-bottom-color", "border-bottom-style", "border-bottom-width"]) -- shorthand!
  ,("border-bottom-color",                "currentcolor", False, ["border", "border-color", "border-bottom"], mempty)
  ,("border-bottom-left-radius",          "0", False, ["border-radius"], mempty)
  ,("border-bottom-right-radius",         "0", False, ["border-radius"], mempty)
  ,("border-bottom-style",                "none", False, ["border", "border-style", "border-bottom"], mempty)
  ,("border-bottom-width",                "medium", False, ["border", "border-width", "border-bottom"], mempty)
  ,("border-collapse",                    "separate", True, mempty, mempty)
  ,("border-color",                       mempty, False, ["border"], ["border-top-color", "border-right-color", "border-bottom-color", "border-left-color"]) -- shorthand!
  ,("border-image",                       mempty, False, ["border"], ["border-image-source", "border-image-slice", "border-image-width", "border-image-outset", "border-image-repeat"]) -- shorthand
  ,("border-image-outset",                "0", False, ["border", "border-image"], mempty)
  ,("border-image-repeat",                "stretch", False, ["border", "border-image"], mempty)
  ,("border-image-slice",                 "100%", False, ["border", "border-image"], mempty)
  ,("border-image-source",                "none", False, ["border", "border-image"], mempty)
  ,("border-image-width",                 "1", False, ["border", "border-image"], mempty)
  -- border-inline-* would go here
  ,("border-left",                        "medium none currentcolor", False, ["border"], ["border-left-color", "border-left-style", "border-left-width"]) -- shorthand!
  ,("border-left-color",                  "currentcolor", False, ["border", "border-color", "border-left"], mempty)
  ,("border-left-style",                  "none", False, ["border", "border-style", "border-left"], mempty)
  ,("border-left-width",                  "medium", False, ["border", "border-width", "border-left"], mempty)
  ,("border-radius",                      mempty {-shorthand-}, False, mempty, ["border-top-left-radius","border-top-right-radius","border-bottom-right-radius","border-bottom-left-radius"])
  ,("border-right",                       "medium none currentcolor", False, ["border"], ["border-right-color", "border-right-style", "border-right-width"]) -- shorthand!
  ,("border-right-color",                 "currentcolor", False, ["border", "border-color", "border-right"], mempty)
  ,("border-right-style",                 "none", False, ["border", "border-style", "border-right"], mempty)
  ,("border-right-width",                 "medium", False, ["border", "border-width", "border-right"], mempty)
  ,("border-spacing",                     "0", True, mempty, mempty)
  ,("border-style",                       mempty {-shorthand-}, False, ["border"], ["border-top-style", "border-right-style", "border-bottom-style", "border-left-style"])
  ,("border-top",                         "medium none currentcolor", False, ["border"], ["border-top-color", "border-top-style", "border-top-width"]) -- shorthand
  ,("border-top-color",                   "currentcolor", False, ["border", "border-color", "border-top"], mempty)
  ,("border-top-style",                   "none", False, ["border", "border-style", "border-top"], mempty)
  ,("border-top-width",                   "medium", False, ["border", "border-style", "border-top"], mempty)
  ,("border-top-left-radius",             "0", False, ["border-radius"], mempty)
  ,("border-top-right-radius",            "0", False, ["border-radius"], mempty)
  ,("border-width",                       mempty {-shorthand-}, False, ["border"], ["border-top-width", "border-right-width", "border-bottom-width", "border-left-width"])
  ,("bottom",                             "auto", False, mempty, mempty)
  ,("box-decoration-break",               "slice", False, mempty, mempty) -- experimental
  ,("-webkit-box-decoration-break",       "slice", False, mempty, mempty) -- experimental
  ,("-o-box-decoration-break",            "slice", False, mempty, mempty) -- experimental
  ,("box-shadow",                         "none", False, mempty, mempty)
  ,("-webkit-box-shadow",                 "none", False, mempty, mempty)
  ,("box-sizing",                         "content-box", False, mempty, mempty) -- experimental
  ,("-webkit-box-sizing",                 "content-box", False, mempty, mempty) -- experimental
  ,("-moz-box-sizing",                    "content-box", False, mempty, mempty) -- experimental
  ,("break-after",                        "auto", False, mempty, mempty)
  ,("break-before",                       "auto", False, mempty, mempty)
  ,("break-inside",                       "auto", False, mempty, mempty)
  ,("caption-side",                       "top", True, mempty, mempty)
  ,("clear",                              "none", False, mempty, mempty)
  ,("clip",                               "auto", False, mempty, mempty) -- deprecated!
  ,("color",                              mempty {-UA dependent-}, True, mempty, mempty)
  ,("column-count",                       "auto", False, ["columns"], mempty)
  ,("column-fill",                        "balance", False, mempty, mempty)
  ,("column-gap",                         "normal", False, mempty, mempty)
  ,("column-rule",                        "medium none currentcolor", False, mempty, ["column-rule-color", "column-rule-style", "column-rule-width"]) -- shorthand
  ,("column-rule-color",                  "currentcolor", False, ["column-rule"], mempty)
  ,("column-rule-style",                  "none", False, ["column-rule"], mempty)
  ,("column-rule-width",                  "medium", False, ["column-rule"], mempty)
  ,("column-span",                        "none", False, mempty, mempty)
  ,("column-width",                       "auto", False, ["columns"], mempty)
  ,("columns",                            mempty {-shorthand-}, False, mempty, ["column-width", "column-count"])
  ,("content",                            "normal", False, mempty, mempty)
  ,("counter-increment",                  "none", False, mempty, mempty)
  ,("counter-reset",                      "none", False, mempty, mempty)
  ,("cursor",                             "auto", True, mempty, mempty)
  ,("direction",                          "ltr", True, mempty, mempty)
  ,("display",                            "inline", False, mempty, mempty)
  ,("empty-cells",                        "show", True, mempty, mempty)
  ,("filter",                             "none", False, mempty, mempty) -- experimental
  ,("-webkit-filter",                     "none", False, mempty, mempty) -- experimental
  ,("flex",                               mempty {-shorthand-}, False, mempty, ["flex-grow", "flex-shrink", "flex-basis"])
  ,("flex-basis",                         "auto", False, ["flex"], mempty)
  ,("flex-direction",                     "row", False, ["flex-flow"], mempty)
  ,("flex-flow",                          "row nowrap", False, mempty, ["flex-direction", "flex-wrap"]) -- shorthand
  ,("flex-grow",                          "0", False, ["flex"], mempty)
  ,("flex-shrink",                        "1", False, ["flex"], mempty)
  ,("flex-wrap",                          "nowrap", False, ["flex-flow"], mempty)
  ,("float",                              "none", False, mempty, mempty)
  ,("font",                               mempty {-shorthand-}, True, mempty, ["font-style", "font-variant", "font-weight", "font-stretch", "font-size", "line-height", "font-family"])
  ,("font-family",                        mempty {-UA dependent-}, True, ["font"], mempty)
  ,("font-feature-settings",              "normal", True, mempty, mempty)
  ,("font-kerning",                       "auto", True, ["font"], mempty)
  ,("font-language-override",             "normal", True, ["font"], mempty)
  ,("font-size",                          "medium", True, ["font"], mempty)
  ,("font-size-adjust",                   "none", True, ["font"], mempty)
  ,("font-stretch",                       "normal", True, ["font"], mempty)
  ,("font-style",                         "normal", True, ["font"], mempty)
  ,("font-synthesis",                     "weight style", True, mempty, mempty)
  ,("font-variant",                       "normal", True, ["font"], mempty)
  ,("font-variant-alternates",            "normal", True, mempty, mempty)
  ,("font-variant-caps",                  "normal", True, mempty, mempty)
  ,("font-variant-east-asian",            "normal", True, mempty, mempty)
  ,("font-variant-ligatures",             "normal", True, mempty, mempty)
  ,("font-variant-numeric",               "normal", True, mempty, mempty)
  ,("font-variant-position",              "normal", True, mempty, mempty)
  ,("font-weight",                        "normal", True, ["font"], mempty)
  ,("grid",                               mempty {-shorthand-}, False, mempty, ["grid-template-rows", "grid-template-columns", "grid-template-areas", "grid-auto-rows", "grid-auto-columns", "grid-auto-flow", "grid-column-gap", "grid-row-gap"])-- shorthand, experimental
  ,("grid-area",                          mempty {-shorthand-}, False, mempty, ["grid-row-start", "grid-row-end", "grid-column-start", "grid-column-end"])  -- shorthand, experimental
  ,("grid-auto-columns",                  "auto", False, mempty, mempty) -- experimental
  ,("-ms-grid-auto-columns",              "auto", False, mempty, mempty) -- experimental
  ,("-webkit-grid-auto-columns",          "auto", False, mempty, mempty) -- experimental
  ,("grid-auto-flow",                     "row", False, mempty, mempty) -- experimental
  ,("-webkit-grid-auto-flow",             "row", False, mempty, mempty) -- experimental
  ,("grid-auto-rows",                     "auto", False, mempty, mempty) -- experimental
  ,("-ms-grid-auto-rows",                 "auto", False, mempty, mempty) -- experimental
  ,("-webkit-grid-auto-rows",             "auto", False, mempty, mempty) -- experimental
  ,("grid-column",                        mempty {-shorthand-}, False, mempty, ["grid-column-start", "grid-column-end"])
  ,("grid-column-end",                    "auto", False, ["grid-column", "grid-area"], mempty) -- experimental
  ,("-ms-grid-column-end",                "auto", False, ["grid-column", "grid-area"], mempty) -- experimental
  ,("-webkit-grid-column-end",            "auto", False, ["grid-column", "grid-area"], mempty) -- experimental
  ,("grid-column-gap",                    "0", False, ["grid-gap"], mempty)
  ,("-ms-grid-column-gap",                "0", False, ["grid-gap"], mempty)
  ,("grid-column-start",                  "auto", False, mempty, mempty) -- experimental
  ,("-ms-grid-column-start",              "auto", False, mempty, mempty) -- experimental
  ,("-webkit-grid-column-start",          "auto", False, mempty, mempty) -- experimental
  ,("grid-gap",                           mempty {-shorthand-}, False, mempty, ["grid-row-gap", "grid-column-gap"])
  ,("grid-row",                           mempty {-shorthand-}, False, mempty, ["grid-row-start", "grid-row-end"])
  ,("grid-row-end",                       "auto", False, ["grid-row", "grid-area"], mempty)
  ,("-ms-grid-row-end",                   "auto", False, ["grid-row", "grid-area"], mempty)
  ,("-webkit-grid-row-end",               "auto", False, ["grid-row", "grid-area"], mempty)
  ,("grid-row-gap",                       "0", False, ["grid-gap"], mempty)
  ,("-webkit-grid-row-gap",               "0", False, ["grid-gap"], mempty)
  ,("grid-row-start",                     "auto", False, ["grid-row", "grid-area"], mempty)
  ,("-ms-grid-row-start",                 "auto", False, ["grid-row", "grid-area"], mempty)
  ,("-webkit-grid-row-start",             "auto", False, ["grid-row", "grid-area"], mempty)
  ,("grid-template",                      mempty {-shorthand-}, False, mempty, ["grid-template-columns", "grid-template-rows", "grid-template-areas"])
  ,("grid-template-areas",                "none", False, ["grid-template"], mempty)
  ,("-webkit-grid-template-areas",        "none", False, ["grid-template"], mempty)
  ,("grid-template-columns",              "none", False, ["grid-template"], mempty)
  ,("-ms-grid-template-columns",          "none", False, ["grid-template"], mempty)
  ,("-webkit-grid-template-columns",      "none", False, ["grid-template"], mempty)
  ,("grid-template-rows",                 "none", False, ["grid-template"], mempty)
  ,("-ms-grid-template-rows",             "none", False, ["grid-template"], mempty)
  ,("-webkit-grid-template-rows",         "none", False, ["grid-template"], mempty)
  ,("height",                             "auto", False, mempty, mempty)
  ,("hyphens",                            "manual",      False, mempty, mempty)
  -- ,("image-orientation" --experimental
  -- ,("image-rendering" --experimental
  -- ,("inline-size", -- experimental
  ,("isolation",                          "auto", False, mempty, mempty)
  ,("justify-content",                    "flex-start", False, mempty, mempty)
  ,("left",                               "auto", False, mempty, mempty)
  ,("letter-spacing",                     "normal", True, mempty, mempty)
  ,("line-break",                         "auto", False, mempty, mempty)
  ,("line-height",                        "normal", True, ["font"], mempty)
  ,("list-style",                         "none disc outside", True, mempty, ["list-style-type", "list-style-position", "list-style-image"]) -- none must go first, for entropy reasons.
  ,("list-style-image",                   "none", True, ["list-style"], mempty)
  ,("list-style-position",                "outside", True, ["list-style"], mempty)
  ,("list-style-type",                    "disc", True, ["list-style"], mempty)
  ,("margin",                             mempty {-shorthand-}, False, mempty, ["margin-top", "margin-right", "margin-bottom", "margin-left"]) -- shorthand
  -- ,("margin-block-* -- experimental
  ,("margin-bottom",                      "0", False, ["margin"], mempty)
  -- ,("margin-inline-*" --experimental
  ,("margin-left",                        "0", False, ["margin"], mempty)
  ,("margin-right",                       "0", False, ["margin"], mempty)
  ,("margin-top",                         "0", False, ["margin"], mempty)
  ,("mask",                               mempty {-shorthand-}, False, mempty, ["mask-clip", "mask-origin", "mask-border"])
  ,("mask-border",                        mempty {-shorthand-}, False, ["mask"], ["mask-border-source", "mask-border-slice", "mask-border-width", "mask-border-outset", "mask-border-repeat"])
  ,("mask-border-outset",                 "0", False, ["mask", "mask-border"], mempty)
  ,("mask-border-repeat",                 "stretch", False, ["mask", "mask-border"], mempty)
  ,("mask-border-slice",                  "0", False, ["mask", "mask-border"], mempty)
  ,("mask-border-source",                 "none", False, ["mask", "mask-border"], mempty)
  ,("mask-border-width",                  "auto", False, ["mask", "mask-border"], mempty)
  ,("mask-clip",                          "border-box", False, ["mask"], mempty) -- experimental
  ,("-webkit-mask-clip",                  "border-box", False, ["mask"], mempty) -- experimental
  ,("mask-composite",                     "add", False, ["mask"], mempty) -- experimental
  ,("mask-image",                         "none", False, ["mask"], mempty) -- experimental
  ,("-webkit-mask-image",                 "none", False, ["mask"], mempty) -- experimental
  -- ,("mask-mode",                 "match-source", False) -- experimental, no support
  ,("mask-origin",                        "border-box", False, ["mask"], mempty) -- experimental
  ,("-webkit-mask-origin",                "border-box", False, ["mask"], mempty) -- experimental
  ,("mask-position",                      "center", False, ["mask"], mempty) -- experimental
  ,("-webkit-mask-position",              "center", False, ["mask"], mempty) -- experimental
  ,("mask-repeat",                        "no-repeat", False, ["mask"], mempty) -- experimental
  ,("-webkit-mask-repeat",                "no-repeat", False, ["mask"], mempty) -- experimental
  ,("mask-size",                          "auto", False, ["mask"], mempty) -- experimental
  ,("mask-type",                          "luminance", False, mempty, mempty) -- experimental
  -- ,("max-block-size",         ("0", False)) -- experimental
  ,("max-height",                         "none", False, mempty, mempty)
  -- ,("max-inline-size",        ("0", False)) -- experimental
  ,("max-width",                          "none", False, mempty, mempty)
  -- ,("min-block-size",         ("0", False)) -- experimental
  ,("min-height",                         "0", False, mempty, mempty)
  -- ,("min-inline-size",        ("0", False)) -- experimental
  ,("min-width",                          "0", False, mempty, mempty)
  ,("mix-blend-mode",                     "normal", False, mempty, mempty)
  ,("object-fit",                         "fill", True, mempty, mempty)
  ,("object-position",                    "50% 50%", True, mempty, mempty)
  -- ,("offset-*",                        ("auto", False)) -- experimental
  ,("opacity",                            "1", False, mempty, mempty)
  ,("order",                              "0", False, mempty, mempty)
  ,("orphans",                            "2", True, mempty, mempty)
  ,("outline",                            "medium none invert", False, mempty, ["outline-width", "outline-style", "outline-color"]) -- shorthand
  ,("outline-color",                      "invert" , False, ["outline"], mempty)
  ,("outline-offset",                     "0", False, mempty, mempty)
  ,("outline-style",                      "none", False, ["outline"], mempty)
  ,("outline-width",                      "medium", False, ["outline"], mempty)
  ,("overflow",                           "visible", False, mempty, mempty)
  ,("overflow-wrap",                      "normal", True, mempty, mempty) -- also called word-wrap
  ,("overflow-x",                         "visible", False, mempty, mempty) -- experimental
  ,("overflow-y",                         "visible", False, mempty, mempty) -- experimental
  ,("padding",                            mempty {-shorthand-}, False, mempty, ["padding-top", "padding-right", "padding-bottom", "padding-left"])
  -- ,("padding-block*",          ("0", False)) -- experimental
  ,("padding-bottom",                     "0", False, ["padding"], mempty)
  -- ,("padding-inline-*",                ("0", False)) -- experimental
  ,("padding-left",                       "0", False, ["padding"], mempty)
  ,("padding-right",                      "0", False, ["padding"], mempty)
  ,("padding-top",                        "0", False, ["padding"], mempty)
  ,("page-break-after",                   "auto", False, mempty, mempty)
  ,("page-break-before",                  "auto", False, mempty, mempty)
  ,("page-break-inside",                  "auto", False, mempty, mempty)
  ,("perspective",                        "none", False, mempty, mempty) -- experimental
  ,("perspective-origin",                 "50% 50%", False, mempty, mempty) -- experimental
  ,("pointer-events",                     "auto", True, mempty, mempty)
  ,("position",                           "static", False, mempty, mempty)
  ,("quotes",                             mempty {-UA dependent-}, True, mempty, mempty)
  ,("resize",                             "none", False, mempty, mempty)
  ,("right",                              "auto", False, mempty, mempty)
  ,("right",                              "auto", False, mempty, mempty)
  -- ,("ruby-*", -- experimental
  ,("scroll-behavior",                    "auto", False, mempty, mempty)
  -- ,("scroll-snap-coordinate", ("none", False)) -- experimental
  -- ,("scroll-snap-destination",("0 0", False)) -- experimental, actually 0px 0px
  -- ,("scroll-snap-type", -- experimental
  ,("shape-image-threshold",              "0", False, mempty, mempty)
  ,("shape-margin",                       "0", False, mempty, mempty)
  ,("shape-outside",                      "none", False, mempty, mempty)
  ,("tab-size",                           "8", True, mempty, mempty) -- experimental
  ,("-moz-tab-size",                      "8", True, mempty, mempty) -- experimental
  ,("-o-tab-size",                        "8", True, mempty, mempty) -- experimental
  ,("table-layout",                       "auto", False, mempty, mempty)
  ,("text-align",                         "start", False, mempty, mempty)
  ,("text-align-last",                    "auto", True, mempty, mempty) -- experimental
  ,("text-combine-upright",               "none", True, mempty, mempty) -- experimental
  ,("-webkit-text-combine-upright",       "none", True, mempty, mempty) -- experimental
  ,("text-decoration",                    "currentcolor solid none", False, mempty, ["text-decoration-color", "text-decoration-style", "text-decoration-line"]) -- shorthand
  ,("text-decoration-color",              "currentcolor", False, ["text-decoration"], mempty)
  ,("text-decoration-line",               "none", False, ["text-decoration"], mempty)
  ,("text-decoration-style",              "solid", False, ["text-decoration"], mempty)
  ,("text-emphasis",                      "none currentcolor", False, mempty, ["text-emphasis-color", "text-emphasis-style"]) -- shorthand
  ,("text-emphasis-color",                "currentcolor", False, ["text-emphasis"], mempty)
  ,("text-emphasis-position",             mempty {-"over right"-}, False, mempty, mempty)
  ,("text-emphasis-style",                "none", False, ["text-emphasis"], mempty)
  ,("text-indent",                        "0", True, mempty, mempty)
  ,("text-orientation",                   "mixed", True, mempty, mempty) -- experimental
  ,("-webkit-text-orientation",           "mixed", True, mempty, mempty) -- experimental
  ,("text-overflow",                      "clip", False, mempty, mempty)
  ,("text-rendering",                     "auto", True, mempty, mempty)
  ,("text-shadow",                        "none", True, mempty, mempty)
  ,("text-transform",                     "none", True, mempty, mempty)
  ,("text-underline-position",            "auto", True, mempty, mempty)
  ,("top",                                "auto", False, mempty, mempty)
  ,("touch-action",                       "auto", False, mempty, mempty)
  ,("transform",                          "none", False, mempty, mempty) -- experimental!
  ,("-webkit-transform",                  "none", False, mempty, mempty) -- experimental!
  ,("transform-box",                      "border-box", False, mempty, mempty) -- experimental!
  ,("transform-origin",                   mempty {-"50% 50% 0"-}, False, mempty, mempty) -- experimental!
  ,("-webkit-transform-origin",           mempty {-"50% 50% 0"-}, False, mempty, mempty) -- experimental!
  ,("transform-style",                    "flat", False, mempty, mempty) -- experimental!
  ,("-webkit-transform-style",            "flat", False, mempty, mempty) -- experimental!
  ,("transition",                         mempty {-shorthand-}, False, mempty, ["transition-property", "transition-duration", "transition-timing-function", "transition-delay"])
  ,("transition-delay",                   "0s", False, ["transition"], mempty) -- experimental
  ,("transition-duration",                "0s", False, ["transition"], mempty) -- experimental
  ,("-webkit-transition-duration",        "0s", False, ["transition"], mempty) -- experimental
  ,("-o-transition-duration",             "0s", False, ["transition"], mempty) -- experimental
  ,("-webkit-transition-delay",           "0s", False, ["transition"], mempty) -- experimental
  ,("transition-property",                "all", False, ["transition"], mempty) -- experimental
  ,("-webkit-transition-property",        "all", False, ["transition"], mempty) -- experimental
  ,("-o-transition-property",             "all", False, ["transition"], mempty) -- experimental
  ,("transition-timing-function",         "ease", False, ["transition"], mempty) -- experimental
  ,("-webkit-transition-timing-function", "ease", False, ["transition"], mempty) -- experimental
  ,("unicode-bidi",                       "normal", False, mempty, mempty)
  ,("user-select",                        "none", False, mempty, mempty) -- experimental
  ,("-moz-user-select",                   "none", False, mempty, mempty) -- experimental
  ,("-webkit-user-select",                "none", False, mempty, mempty) -- experimental
  ,("-ms-user-select",                    "none", False, mempty, mempty) -- experimental
  ,("vertical-align",                     "baseline", False, mempty, mempty)
  ,("white-space",                        "normal", True, mempty, mempty)
  ,("widows",                             "2", True, mempty, mempty)
  ,("width",                              "auto", False, mempty, mempty)
  ,("will-change",                        "auto", False, mempty, mempty)
  ,("word-break",                         "normal", True, mempty, mempty)
  ,("word-spacing",                       "normal", True, mempty, mempty)
  ,("writing-mode",                       "horizontal-tb", True, mempty, mempty) -- experimental
  ,("-ms-writing-mode",                   "horizontal-tb", True, mempty, mempty) -- experimental
  ,("-webkit-writing-mode",               "horizontal-tb", True, mempty, mempty) -- experimental
  ,("z-index",                            "auto", False, mempty, mempty)
  ]
