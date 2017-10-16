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
    , Inheritance(..)
    , propertiesTraits
    ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Hasmin.Types.Value
import Hasmin.Parser.Value

data Inheritance = Inherited | NonInherited
  deriving (Eq, Show)

data PropertyInfo =
      PropertyInfo { initialValues :: Maybe Values
                   , inheritance   :: Inheritance
                   , overwrittenBy :: [Text] -- shorthands that overwrite it
                   , subproperties :: [Text] -- longhands that can be merged into it
                   } deriving (Show)

-- Converts the tuples with CSS properties data into a tuple containing a
-- property's name, and its traits. Allows keeping the table in a more readable
-- and maintainable format.
processTuples :: [(Text, Text, Inheritance, [Text], [Text])] -> [(Text, PropertyInfo)]
processTuples = foldr (\(p,t,i,a,b) xs -> (p, PropertyInfo (getValues p t) i a b) : xs) []
  where getValues p s = case parseOnly (values p <|> valuesFallback) s of
                          Right initValues -> Just initValues
                          Left _           -> Nothing

propertiesTraits :: Map Text PropertyInfo
propertiesTraits = Map.fromList $ processTuples
  [("align-content",                      "stretch", NonInherited, mempty, mempty)
  ,("align-items",                        "stretch", NonInherited, mempty, mempty)
  ,("align-self",                         "auto", NonInherited, mempty, mempty)
  ,("all",                                mempty, NonInherited, mempty, mempty) -- No practical initial value
  ,("animation",                          mempty, NonInherited, mempty,["animation-name","animation-duration","animation-timing-function","animation-delay","animation-iteration-count","animation-direction","animation-fill-mode","animation-play-state"])
  ,("animation-delay",                    "0s", NonInherited, ["animation"], mempty) -- experimental
  ,("animation-direction",                "normal", NonInherited, ["animation"], mempty) -- experimental
  ,("animation-duration",                 "0s", NonInherited, mempty, mempty) -- experimental
  ,("animation-fill-mode",                "none", NonInherited, ["animation"], mempty) -- experimental
  ,("animation-iteration-count",          "1", NonInherited, ["animation"], mempty) -- experimental
  ,("animation-name",                     "none", NonInherited, ["animation"], mempty) -- experimental
  ,("animation-play-state",               "running", NonInherited, ["animation"], mempty) -- experimental
  ,("animation-timing-function",          "ease", NonInherited, ["animation"], mempty) -- experimental
  ,("backface-visibility",                "visible", NonInherited, mempty, mempty)
  ,("-webkit-backface-visibility",        "visible", NonInherited, mempty, mempty)
  ,("backdrop-filter",                    "none", NonInherited, mempty, mempty) -- experimental
  ,("-webkit-backdrop-filter",            "none", NonInherited, mempty, mempty) -- experimental
  ,("background",                         mempty {-shorthand-}, NonInherited, mempty, ["background-image", "background-position", "background-size", "background-repeat", "background-origin", "background-clip", "background-attachment", "background-color"])
  ,("background-attachment",              "scroll", NonInherited, ["background"], mempty)
  ,("background-blend-mode",              "normal", NonInherited, mempty, mempty) -- TODO find out if these mempty are alright!
  ,("background-clip",                    "border-box", NonInherited, ["background"], mempty)
  ,("-webkit-background-clip",            "border-box", NonInherited, ["background"], mempty)
  ,("background-color",                   "transparent", NonInherited, ["background"], mempty)
  ,("background-image",                   "none", NonInherited, ["background"], mempty)
  ,("background-origin",                  "padding-box", NonInherited, ["background"], mempty)
  ,("background-position",                mempty, NonInherited, ["background"], ["background-position-x", "background-position-y"]) --shorthand
  ,("background-position-x",              "left", NonInherited, ["background", "background-position"], mempty)
  ,("background-position-inline",         mempty {-not applicable-}, NonInherited, mempty, mempty)
  ,("background-position-block",          mempty {-not applicable-}, NonInherited, mempty, mempty)
  ,("background-position-y",              "top", NonInherited, ["background", "background-position"], mempty)
  ,("background-repeat",                  "repeat", NonInherited, ["background"], mempty)
  ,("background-size",                    "auto", NonInherited, ["background"], mempty)
  ,("block-size",                         "auto", NonInherited, mempty, mempty) -- experimental
  ,("border",                             "medium none currentcolor", NonInherited, mempty, ["border-width", "border-style", "border-color"]) -- shorthand!
  -- border-block-* would go here
  ,("border-bottom",                      "medium none currentcolor", NonInherited, ["border"], ["border-bottom-color", "border-bottom-style", "border-bottom-width"]) -- shorthand!
  ,("border-bottom-color",                "currentcolor", NonInherited, ["border", "border-color", "border-bottom"], mempty)
  ,("border-bottom-left-radius",          "0", NonInherited, ["border-radius"], mempty)
  ,("border-bottom-right-radius",         "0", NonInherited, ["border-radius"], mempty)
  ,("border-bottom-style",                "none", NonInherited, ["border", "border-style", "border-bottom"], mempty)
  ,("border-bottom-width",                "medium", NonInherited, ["border", "border-width", "border-bottom"], mempty)
  ,("border-collapse",                    "separate", Inherited, mempty, mempty)
  ,("border-color",                       mempty, NonInherited, ["border"], ["border-top-color", "border-right-color", "border-bottom-color", "border-left-color"]) -- shorthand!
  ,("border-image",                       mempty, NonInherited, ["border"], ["border-image-source", "border-image-slice", "border-image-width", "border-image-outset", "border-image-repeat"]) -- shorthand
  ,("border-image-outset",                "0", NonInherited, ["border", "border-image"], mempty)
  ,("border-image-repeat",                "stretch", NonInherited, ["border", "border-image"], mempty)
  ,("border-image-slice",                 "100%", NonInherited, ["border", "border-image"], mempty)
  ,("border-image-source",                "none", NonInherited, ["border", "border-image"], mempty)
  ,("border-image-width",                 "1", NonInherited, ["border", "border-image"], mempty)
  -- border-inline-* would go here
  ,("border-left",                        "medium none currentcolor", NonInherited, ["border"], ["border-left-color", "border-left-style", "border-left-width"]) -- shorthand!
  ,("border-left-color",                  "currentcolor", NonInherited, ["border", "border-color", "border-left"], mempty)
  ,("border-left-style",                  "none", NonInherited, ["border", "border-style", "border-left"], mempty)
  ,("border-left-width",                  "medium", NonInherited, ["border", "border-width", "border-left"], mempty)
  ,("border-radius",                      mempty {-shorthand-}, NonInherited, mempty, ["border-top-left-radius","border-top-right-radius","border-bottom-right-radius","border-bottom-left-radius"])
  ,("border-right",                       "medium none currentcolor", NonInherited, ["border"], ["border-right-color", "border-right-style", "border-right-width"]) -- shorthand!
  ,("border-right-color",                 "currentcolor", NonInherited, ["border", "border-color", "border-right"], mempty)
  ,("border-right-style",                 "none", NonInherited, ["border", "border-style", "border-right"], mempty)
  ,("border-right-width",                 "medium", NonInherited, ["border", "border-width", "border-right"], mempty)
  ,("border-spacing",                     "0", Inherited, mempty, mempty)
  ,("border-style",                       mempty {-shorthand-}, NonInherited, ["border"], ["border-top-style", "border-right-style", "border-bottom-style", "border-left-style"])
  ,("border-top",                         "medium none currentcolor", NonInherited, ["border"], ["border-top-color", "border-top-style", "border-top-width"]) -- shorthand
  ,("border-top-color",                   "currentcolor", NonInherited, ["border", "border-color", "border-top"], mempty)
  ,("border-top-style",                   "none", NonInherited, ["border", "border-style", "border-top"], mempty)
  ,("border-top-width",                   "medium", NonInherited, ["border", "border-style", "border-top"], mempty)
  ,("border-top-left-radius",             "0", NonInherited, ["border-radius"], mempty)
  ,("border-top-right-radius",            "0", NonInherited, ["border-radius"], mempty)
  ,("border-width",                       mempty {-shorthand-}, NonInherited, ["border"], ["border-top-width", "border-right-width", "border-bottom-width", "border-left-width"])
  ,("bottom",                             "auto", NonInherited, mempty, mempty)
  ,("box-decoration-break",               "slice", NonInherited, mempty, mempty) -- experimental
  ,("-webkit-box-decoration-break",       "slice", NonInherited, mempty, mempty) -- experimental
  ,("-o-box-decoration-break",            "slice", NonInherited, mempty, mempty) -- experimental
  ,("box-shadow",                         "none", NonInherited, mempty, mempty)
  ,("-webkit-box-shadow",                 "none", NonInherited, mempty, mempty)
  ,("box-sizing",                         "content-box", NonInherited, mempty, mempty) -- experimental
  ,("-webkit-box-sizing",                 "content-box", NonInherited, mempty, mempty) -- experimental
  ,("-moz-box-sizing",                    "content-box", NonInherited, mempty, mempty) -- experimental
  ,("break-after",                        "auto", NonInherited, mempty, mempty)
  ,("break-before",                       "auto", NonInherited, mempty, mempty)
  ,("break-inside",                       "auto", NonInherited, mempty, mempty)
  ,("caption-side",                       "top", Inherited, mempty, mempty)
  ,("clear",                              "none", NonInherited, mempty, mempty)
  ,("clip",                               "auto", NonInherited, mempty, mempty) -- deprecated!
  ,("color",                              mempty {-UA dependent-}, Inherited, mempty, mempty)
  ,("column-count",                       "auto", NonInherited, ["columns"], mempty)
  ,("column-fill",                        "balance", NonInherited, mempty, mempty)
  ,("column-gap",                         "normal", NonInherited, mempty, mempty)
  ,("column-rule",                        "medium none currentcolor", NonInherited, mempty, ["column-rule-color", "column-rule-style", "column-rule-width"]) -- shorthand
  ,("column-rule-color",                  "currentcolor", NonInherited, ["column-rule"], mempty)
  ,("column-rule-style",                  "none", NonInherited, ["column-rule"], mempty)
  ,("column-rule-width",                  "medium", NonInherited, ["column-rule"], mempty)
  ,("column-span",                        "none", NonInherited, mempty, mempty)
  ,("column-width",                       "auto", NonInherited, ["columns"], mempty)
  ,("columns",                            mempty {-shorthand-}, NonInherited, mempty, ["column-width", "column-count"])
  ,("content",                            "normal", NonInherited, mempty, mempty)
  ,("counter-increment",                  "none", NonInherited, mempty, mempty)
  ,("counter-reset",                      "none", NonInherited, mempty, mempty)
  ,("cursor",                             "auto", Inherited, mempty, mempty)
  ,("direction",                          "ltr", Inherited, mempty, mempty)
  ,("display",                            "inline", NonInherited, mempty, mempty)
  ,("empty-cells",                        "show", Inherited, mempty, mempty)
  ,("filter",                             "none", NonInherited, mempty, mempty) -- experimental
  ,("-webkit-filter",                     "none", NonInherited, mempty, mempty) -- experimental
  ,("flex",                               mempty {-shorthand-}, NonInherited, mempty, ["flex-grow", "flex-shrink", "flex-basis"])
  ,("flex-basis",                         "auto", NonInherited, ["flex"], mempty)
  ,("flex-direction",                     "row", NonInherited, ["flex-flow"], mempty)
  ,("flex-flow",                          "row nowrap", NonInherited, mempty, ["flex-direction", "flex-wrap"]) -- shorthand
  ,("flex-grow",                          "0", NonInherited, ["flex"], mempty)
  ,("flex-shrink",                        "1", NonInherited, ["flex"], mempty)
  ,("flex-wrap",                          "nowrap", NonInherited, ["flex-flow"], mempty)
  ,("float",                              "none", NonInherited, mempty, mempty)
  ,("font",                               mempty {-shorthand-}, Inherited, mempty, ["font-style", "font-variant", "font-weight", "font-stretch", "font-size", "line-height", "font-family"])
  ,("font-family",                        mempty {-UA dependent-}, Inherited, ["font"], mempty)
  ,("font-feature-settings",              "normal", Inherited, mempty, mempty)
  ,("font-kerning",                       "auto", Inherited, ["font"], mempty)
  ,("font-language-override",             "normal", Inherited, ["font"], mempty)
  ,("font-size",                          "medium", Inherited, ["font"], mempty)
  ,("font-size-adjust",                   "none", Inherited, ["font"], mempty)
  ,("font-stretch",                       "normal", Inherited, ["font"], mempty)
  ,("font-style",                         "normal", Inherited, ["font"], mempty)
  ,("font-synthesis",                     "weight style", Inherited, mempty, mempty)
  ,("font-variant",                       "normal", Inherited, ["font"], mempty)
  ,("font-variant-alternates",            "normal", Inherited, mempty, mempty)
  ,("font-variant-caps",                  "normal", Inherited, mempty, mempty)
  ,("font-variant-east-asian",            "normal", Inherited, mempty, mempty)
  ,("font-variant-ligatures",             "normal", Inherited, mempty, mempty)
  ,("font-variant-numeric",               "normal", Inherited, mempty, mempty)
  ,("font-variant-position",              "normal", Inherited, mempty, mempty)
  ,("font-weight",                        "normal", Inherited, ["font"], mempty)
  ,("grid",                               mempty {-shorthand-}, NonInherited, mempty, ["grid-template-rows", "grid-template-columns", "grid-template-areas", "grid-auto-rows", "grid-auto-columns", "grid-auto-flow", "grid-column-gap", "grid-row-gap"])-- shorthand, experimental
  ,("grid-area",                          mempty {-shorthand-}, NonInherited, mempty, ["grid-row-start", "grid-row-end", "grid-column-start", "grid-column-end"])  -- shorthand, experimental
  ,("grid-auto-columns",                  "auto", NonInherited, mempty, mempty) -- experimental
  ,("-ms-grid-auto-columns",              "auto", NonInherited, mempty, mempty) -- experimental
  ,("-webkit-grid-auto-columns",          "auto", NonInherited, mempty, mempty) -- experimental
  ,("grid-auto-flow",                     "row", NonInherited, mempty, mempty) -- experimental
  ,("-webkit-grid-auto-flow",             "row", NonInherited, mempty, mempty) -- experimental
  ,("grid-auto-rows",                     "auto", NonInherited, mempty, mempty) -- experimental
  ,("-ms-grid-auto-rows",                 "auto", NonInherited, mempty, mempty) -- experimental
  ,("-webkit-grid-auto-rows",             "auto", NonInherited, mempty, mempty) -- experimental
  ,("grid-column",                        mempty {-shorthand-}, NonInherited, mempty, ["grid-column-start", "grid-column-end"])
  ,("grid-column-end",                    "auto", NonInherited, ["grid-column", "grid-area"], mempty) -- experimental
  ,("-ms-grid-column-end",                "auto", NonInherited, ["grid-column", "grid-area"], mempty) -- experimental
  ,("-webkit-grid-column-end",            "auto", NonInherited, ["grid-column", "grid-area"], mempty) -- experimental
  ,("grid-column-gap",                    "0", NonInherited, ["grid-gap"], mempty)
  ,("-ms-grid-column-gap",                "0", NonInherited, ["grid-gap"], mempty)
  ,("grid-column-start",                  "auto", NonInherited, mempty, mempty) -- experimental
  ,("-ms-grid-column-start",              "auto", NonInherited, mempty, mempty) -- experimental
  ,("-webkit-grid-column-start",          "auto", NonInherited, mempty, mempty) -- experimental
  ,("grid-gap",                           mempty {-shorthand-}, NonInherited, mempty, ["grid-row-gap", "grid-column-gap"])
  ,("grid-row",                           mempty {-shorthand-}, NonInherited, mempty, ["grid-row-start", "grid-row-end"])
  ,("grid-row-end",                       "auto", NonInherited, ["grid-row", "grid-area"], mempty)
  ,("-ms-grid-row-end",                   "auto", NonInherited, ["grid-row", "grid-area"], mempty)
  ,("-webkit-grid-row-end",               "auto", NonInherited, ["grid-row", "grid-area"], mempty)
  ,("grid-row-gap",                       "0", NonInherited, ["grid-gap"], mempty)
  ,("-webkit-grid-row-gap",               "0", NonInherited, ["grid-gap"], mempty)
  ,("grid-row-start",                     "auto", NonInherited, ["grid-row", "grid-area"], mempty)
  ,("-ms-grid-row-start",                 "auto", NonInherited, ["grid-row", "grid-area"], mempty)
  ,("-webkit-grid-row-start",             "auto", NonInherited, ["grid-row", "grid-area"], mempty)
  ,("grid-template",                      mempty {-shorthand-}, NonInherited, mempty, ["grid-template-columns", "grid-template-rows", "grid-template-areas"])
  ,("grid-template-areas",                "none", NonInherited, ["grid-template"], mempty)
  ,("-webkit-grid-template-areas",        "none", NonInherited, ["grid-template"], mempty)
  ,("grid-template-columns",              "none", NonInherited, ["grid-template"], mempty)
  ,("-ms-grid-template-columns",          "none", NonInherited, ["grid-template"], mempty)
  ,("-webkit-grid-template-columns",      "none", NonInherited, ["grid-template"], mempty)
  ,("grid-template-rows",                 "none", NonInherited, ["grid-template"], mempty)
  ,("-ms-grid-template-rows",             "none", NonInherited, ["grid-template"], mempty)
  ,("-webkit-grid-template-rows",         "none", NonInherited, ["grid-template"], mempty)
  ,("height",                             "auto", NonInherited, mempty, mempty)
  ,("hyphens",                            "manual",      NonInherited, mempty, mempty)
  -- ,("image-orientation" --experimental
  -- ,("image-rendering" --experimental
  -- ,("inline-size", -- experimental
  ,("isolation",                          "auto", NonInherited, mempty, mempty)
  ,("justify-content",                    "flex-start", NonInherited, mempty, mempty)
  ,("left",                               "auto", NonInherited, mempty, mempty)
  ,("letter-spacing",                     "normal", Inherited, mempty, mempty)
  ,("line-break",                         "auto", NonInherited, mempty, mempty)
  ,("line-height",                        "normal", Inherited, ["font"], mempty)
  ,("list-style",                         "none disc outside", Inherited, mempty, ["list-style-type", "list-style-position", "list-style-image"]) -- none must go first, for entropy reasons.
  ,("list-style-image",                   "none", Inherited, ["list-style"], mempty)
  ,("list-style-position",                "outside", Inherited, ["list-style"], mempty)
  ,("list-style-type",                    "disc", Inherited, ["list-style"], mempty)
  ,("margin",                             mempty {-shorthand-}, NonInherited, mempty, ["margin-top", "margin-right", "margin-bottom", "margin-left"]) -- shorthand
  -- ,("margin-block-* -- experimental
  ,("margin-bottom",                      "0", NonInherited, ["margin"], mempty)
  -- ,("margin-inline-*" --experimental
  ,("margin-left",                        "0", NonInherited, ["margin"], mempty)
  ,("margin-right",                       "0", NonInherited, ["margin"], mempty)
  ,("margin-top",                         "0", NonInherited, ["margin"], mempty)
  ,("mask",                               mempty {-shorthand-}, NonInherited, mempty, ["mask-clip", "mask-origin", "mask-border"])
  ,("mask-border",                        mempty {-shorthand-}, NonInherited, ["mask"], ["mask-border-source", "mask-border-slice", "mask-border-width", "mask-border-outset", "mask-border-repeat"])
  ,("mask-border-outset",                 "0", NonInherited, ["mask", "mask-border"], mempty)
  ,("mask-border-repeat",                 "stretch", NonInherited, ["mask", "mask-border"], mempty)
  ,("mask-border-slice",                  "0", NonInherited, ["mask", "mask-border"], mempty)
  ,("mask-border-source",                 "none", NonInherited, ["mask", "mask-border"], mempty)
  ,("mask-border-width",                  "auto", NonInherited, ["mask", "mask-border"], mempty)
  ,("mask-clip",                          "border-box", NonInherited, ["mask"], mempty) -- experimental
  ,("-webkit-mask-clip",                  "border-box", NonInherited, ["mask"], mempty) -- experimental
  ,("mask-composite",                     "add", NonInherited, ["mask"], mempty) -- experimental
  ,("mask-image",                         "none", NonInherited, ["mask"], mempty) -- experimental
  ,("-webkit-mask-image",                 "none", NonInherited, ["mask"], mempty) -- experimental
  -- ,("mask-mode",                 "match-source", NonInherited) -- experimental, no support
  ,("mask-origin",                        "border-box", NonInherited, ["mask"], mempty) -- experimental
  ,("-webkit-mask-origin",                "border-box", NonInherited, ["mask"], mempty) -- experimental
  ,("mask-position",                      "center", NonInherited, ["mask"], mempty) -- experimental
  ,("-webkit-mask-position",              "center", NonInherited, ["mask"], mempty) -- experimental
  ,("mask-repeat",                        "no-repeat", NonInherited, ["mask"], mempty) -- experimental
  ,("-webkit-mask-repeat",                "no-repeat", NonInherited, ["mask"], mempty) -- experimental
  ,("mask-size",                          "auto", NonInherited, ["mask"], mempty) -- experimental
  ,("mask-type",                          "luminance", NonInherited, mempty, mempty) -- experimental
  -- ,("max-block-size",         ("0", NonInherited)) -- experimental
  ,("max-height",                         "none", NonInherited, mempty, mempty)
  -- ,("max-inline-size",        ("0", NonInherited)) -- experimental
  ,("max-width",                          "none", NonInherited, mempty, mempty)
  -- ,("min-block-size",         ("0", NonInherited)) -- experimental
  ,("min-height",                         "0", NonInherited, mempty, mempty)
  -- ,("min-inline-size",        ("0", NonInherited)) -- experimental
  ,("min-width",                          "0", NonInherited, mempty, mempty)
  ,("mix-blend-mode",                     "normal", NonInherited, mempty, mempty)
  ,("object-fit",                         "fill", Inherited, mempty, mempty)
  ,("object-position",                    "50% 50%", Inherited, mempty, mempty)
  -- ,("offset-*",                        ("auto", NonInherited)) -- experimental
  ,("opacity",                            "1", NonInherited, mempty, mempty)
  ,("order",                              "0", NonInherited, mempty, mempty)
  ,("orphans",                            "2", Inherited, mempty, mempty)
  ,("outline",                            "medium none invert", NonInherited, mempty, ["outline-width", "outline-style", "outline-color"]) -- shorthand
  ,("outline-color",                      "invert" , NonInherited, ["outline"], mempty)
  ,("outline-offset",                     "0", NonInherited, mempty, mempty)
  ,("outline-style",                      "none", NonInherited, ["outline"], mempty)
  ,("outline-width",                      "medium", NonInherited, ["outline"], mempty)
  ,("overflow",                           "visible", NonInherited, mempty, mempty)
  ,("overflow-wrap",                      "normal", Inherited, mempty, mempty) -- also called word-wrap
  ,("overflow-x",                         "visible", NonInherited, mempty, mempty) -- experimental
  ,("overflow-y",                         "visible", NonInherited, mempty, mempty) -- experimental
  ,("padding",                            mempty {-shorthand-}, NonInherited, mempty, ["padding-top", "padding-right", "padding-bottom", "padding-left"])
  -- ,("padding-block*",          ("0", NonInherited)) -- experimental
  ,("padding-bottom",                     "0", NonInherited, ["padding"], mempty)
  -- ,("padding-inline-*",                ("0", NonInherited)) -- experimental
  ,("padding-left",                       "0", NonInherited, ["padding"], mempty)
  ,("padding-right",                      "0", NonInherited, ["padding"], mempty)
  ,("padding-top",                        "0", NonInherited, ["padding"], mempty)
  ,("page-break-after",                   "auto", NonInherited, mempty, mempty)
  ,("page-break-before",                  "auto", NonInherited, mempty, mempty)
  ,("page-break-inside",                  "auto", NonInherited, mempty, mempty)
  ,("perspective",                        "none", NonInherited, mempty, mempty) -- experimental
  ,("perspective-origin",                 "50% 50%", NonInherited, mempty, mempty) -- experimental
  ,("pointer-events",                     "auto", Inherited, mempty, mempty)
  ,("position",                           "static", NonInherited, mempty, mempty)
  ,("quotes",                             mempty {-UA dependent-}, Inherited, mempty, mempty)
  ,("resize",                             "none", NonInherited, mempty, mempty)
  ,("right",                              "auto", NonInherited, mempty, mempty)
  ,("right",                              "auto", NonInherited, mempty, mempty)
  -- ,("ruby-*", -- experimental
  ,("scroll-behavior",                    "auto", NonInherited, mempty, mempty)
  -- ,("scroll-snap-coordinate", ("none", NonInherited)) -- experimental
  -- ,("scroll-snap-destination",("0 0", NonInherited)) -- experimental, actually 0px 0px
  -- ,("scroll-snap-type", -- experimental
  ,("shape-image-threshold",              "0", NonInherited, mempty, mempty)
  ,("shape-margin",                       "0", NonInherited, mempty, mempty)
  ,("shape-outside",                      "none", NonInherited, mempty, mempty)
  ,("tab-size",                           "8", Inherited, mempty, mempty) -- experimental
  ,("-moz-tab-size",                      "8", Inherited, mempty, mempty) -- experimental
  ,("-o-tab-size",                        "8", Inherited, mempty, mempty) -- experimental
  ,("table-layout",                       "auto", NonInherited, mempty, mempty)
  ,("text-align",                         "start", NonInherited, mempty, mempty)
  ,("text-align-last",                    "auto", Inherited, mempty, mempty) -- experimental
  ,("text-combine-upright",               "none", Inherited, mempty, mempty) -- experimental
  ,("-webkit-text-combine-upright",       "none", Inherited, mempty, mempty) -- experimental
  ,("text-decoration",                    "currentcolor solid none", NonInherited, mempty, ["text-decoration-color", "text-decoration-style", "text-decoration-line"]) -- shorthand
  ,("text-decoration-color",              "currentcolor", NonInherited, ["text-decoration"], mempty)
  ,("text-decoration-line",               "none", NonInherited, ["text-decoration"], mempty)
  ,("text-decoration-style",              "solid", NonInherited, ["text-decoration"], mempty)
  ,("text-emphasis",                      "none currentcolor", NonInherited, mempty, ["text-emphasis-color", "text-emphasis-style"]) -- shorthand
  ,("text-emphasis-color",                "currentcolor", NonInherited, ["text-emphasis"], mempty)
  ,("text-emphasis-position",             mempty {-"over right"-}, NonInherited, mempty, mempty)
  ,("text-emphasis-style",                "none", NonInherited, ["text-emphasis"], mempty)
  ,("text-indent",                        "0", Inherited, mempty, mempty)
  ,("text-orientation",                   "mixed", Inherited, mempty, mempty) -- experimental
  ,("-webkit-text-orientation",           "mixed", Inherited, mempty, mempty) -- experimental
  ,("text-overflow",                      "clip", NonInherited, mempty, mempty)
  ,("text-rendering",                     "auto", Inherited, mempty, mempty)
  ,("text-shadow",                        "none", Inherited, mempty, mempty)
  ,("text-transform",                     "none", Inherited, mempty, mempty)
  ,("text-underline-position",            "auto", Inherited, mempty, mempty)
  ,("top",                                "auto", NonInherited, mempty, mempty)
  ,("touch-action",                       "auto", NonInherited, mempty, mempty)
  ,("transform",                          "none", NonInherited, mempty, mempty) -- experimental!
  ,("-webkit-transform",                  "none", NonInherited, mempty, mempty) -- experimental!
  ,("transform-box",                      "border-box", NonInherited, mempty, mempty) -- experimental!
  ,("transform-origin",                   mempty {-"50% 50% 0"-}, NonInherited, mempty, mempty) -- experimental!
  ,("-webkit-transform-origin",           mempty {-"50% 50% 0"-}, NonInherited, mempty, mempty) -- experimental!
  ,("transform-style",                    "flat", NonInherited, mempty, mempty) -- experimental!
  ,("-webkit-transform-style",            "flat", NonInherited, mempty, mempty) -- experimental!
  ,("transition",                         mempty {-shorthand-}, NonInherited, mempty, ["transition-property", "transition-duration", "transition-timing-function", "transition-delay"])
  ,("transition-delay",                   "0s", NonInherited, ["transition"], mempty) -- experimental
  ,("transition-duration",                "0s", NonInherited, ["transition"], mempty) -- experimental
  ,("-webkit-transition-duration",        "0s", NonInherited, ["transition"], mempty) -- experimental
  ,("-o-transition-duration",             "0s", NonInherited, ["transition"], mempty) -- experimental
  ,("-webkit-transition-delay",           "0s", NonInherited, ["transition"], mempty) -- experimental
  ,("transition-property",                "all", NonInherited, ["transition"], mempty) -- experimental
  ,("-webkit-transition-property",        "all", NonInherited, ["transition"], mempty) -- experimental
  ,("-o-transition-property",             "all", NonInherited, ["transition"], mempty) -- experimental
  ,("transition-timing-function",         "ease", NonInherited, ["transition"], mempty) -- experimental
  ,("-webkit-transition-timing-function", "ease", NonInherited, ["transition"], mempty) -- experimental
  ,("unicode-bidi",                       "normal", NonInherited, mempty, mempty)
  ,("user-select",                        "none", NonInherited, mempty, mempty) -- experimental
  ,("-moz-user-select",                   "none", NonInherited, mempty, mempty) -- experimental
  ,("-webkit-user-select",                "none", NonInherited, mempty, mempty) -- experimental
  ,("-ms-user-select",                    "none", NonInherited, mempty, mempty) -- experimental
  ,("vertical-align",                     "baseline", NonInherited, mempty, mempty)
  ,("white-space",                        "normal", Inherited, mempty, mempty)
  ,("widows",                             "2", Inherited, mempty, mempty)
  ,("width",                              "auto", NonInherited, mempty, mempty)
  ,("will-change",                        "auto", NonInherited, mempty, mempty)
  ,("word-break",                         "normal", Inherited, mempty, mempty)
  ,("word-spacing",                       "normal", Inherited, mempty, mempty)
  ,("writing-mode",                       "horizontal-tb", Inherited, mempty, mempty) -- experimental
  ,("-ms-writing-mode",                   "horizontal-tb", Inherited, mempty, mempty) -- experimental
  ,("-webkit-writing-mode",               "horizontal-tb", Inherited, mempty, mempty) -- experimental
  ,("z-index",                            "auto", NonInherited, mempty, mempty)
  ]
