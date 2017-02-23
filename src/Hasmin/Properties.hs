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
module Hasmin.Properties (
       PropertyInfo(..)
     , shorthandAndLonghandsMap
     , propertiesTraits
    ) where

import Data.Attoparsec.Text (parseOnly)
import Data.Text (Text, unpack)
import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Hasmin.Types.Value
import Hasmin.Parser.Value

-- TODO see if this can be extended to contain all the information from a
-- property (such as initial value). Otherwise, delete it.
data PropertyInfo =
     PropertyInfo { overwrittenBy :: [Text] -- shorthands that overwrite it
                  , subproperties :: [Text] -- longhands that can be merged into it
                  } deriving (Show)

shorthandAndLonghandsMap :: Map Text PropertyInfo
shorthandAndLonghandsMap = Map.fromList properties

properties :: [(Text,PropertyInfo)]
properties =
  [("animation", PropertyInfo mempty ["animation-name","animation-duration","animation-timing-function","animation-delay","animation-iteration-count","animation-direction","animation-fill-mode","animation-play-state"])
  ,("animation-delay",           PropertyInfo ["animation"] mempty)
  ,("animation-direction",       PropertyInfo ["animation"] mempty)
  ,("animation-duration",        PropertyInfo ["animation"] mempty)
  ,("animation-fill-mode",       PropertyInfo ["animation"] mempty)
  ,("animation-iteration-count", PropertyInfo ["animation"] mempty)
  ,("animation-name",            PropertyInfo ["animation"] mempty)
  ,("animation-play-state",      PropertyInfo ["animation"] mempty)
  ,("animation-timing-function", PropertyInfo ["animation"] mempty)
  ,("background",            PropertyInfo mempty ["background-image", "background-position", "background-size", "background-repeat", "background-origin", "background-clip", "background-attachment", "background-color"])
  ,("background-attachment", PropertyInfo ["background"] mempty)
  ,("background-clip",       PropertyInfo ["background"] mempty)
  ,("background-color",      PropertyInfo ["background"] mempty)
  ,("background-image",      PropertyInfo ["background"] mempty)
  ,("background-origin",     PropertyInfo ["background"] mempty)
  ,("background-position",   PropertyInfo ["background"] ["background-position-x", "background-position-y"])
  ,("background-position-x", PropertyInfo ["background", "background-position"] mempty)
  ,("background-position-y", PropertyInfo ["background", "background-position"] mempty)
  ,("background-repeat",     PropertyInfo ["background"] mempty)
  ,("background-size",       PropertyInfo ["background"] mempty)
  ,("border",              PropertyInfo mempty ["border-width", "border-style", "border-color"])
  ,("border-bottom",       PropertyInfo ["border"] ["border-bottom-color", "border-bottom-style", "border-bottom-width"])
  ,("border-bottom-color", PropertyInfo ["border", "border-color", "border-bottom"] mempty)
  ,("border-bottom-left-radius",  PropertyInfo ["border-radius"] mempty)
  ,("border-bottom-right-radius", PropertyInfo ["border-radius"] mempty)
  ,("border-bottom-style", PropertyInfo ["border", "border-style", "border-bottom"] mempty)
  ,("border-bottom-width", PropertyInfo ["border", "border-width", "border-bottom"] mempty)
  ,("border-color",        PropertyInfo ["border"] ["border-top-color", "border-right-color", "border-bottom-color", "border-left-color"])
  ,("border-image",        PropertyInfo ["border"] ["border-image-source", "border-image-slice", "border-image-width", "border-image-outset", "border-image-repeat"])
  ,("border-image-outset", PropertyInfo ["border", "border-image"] mempty)
  ,("border-image-repeat", PropertyInfo ["border", "border-image"] mempty)
  ,("border-image-slice",  PropertyInfo ["border", "border-image"] mempty)
  ,("border-image-source", PropertyInfo ["border", "border-image"] mempty)
  ,("border-image-width",  PropertyInfo ["border", "border-image"] mempty)
  ,("border-left",       PropertyInfo ["border"] ["border-left-color", "border-left-style", "border-left-width"])
  ,("border-left-color", PropertyInfo ["border", "border-color", "border-left"] mempty)
  ,("border-left-style", PropertyInfo ["border", "border-style", "border-left"] mempty)
  ,("border-left-width", PropertyInfo ["border", "border-width", "border-left"] mempty)
  ,("border-radius", PropertyInfo mempty ["border-top-left-radius","border-top-right-radius","border-bottom-right-radius","border-bottom-left-radius"])
  ,("border-right",       PropertyInfo ["border"] ["border-right-color", "border-right-style", "border-right-width"])
  ,("border-right-color", PropertyInfo ["border", "border-color", "border-right"] mempty)
  ,("border-right-style", PropertyInfo ["border", "border-style", "border-right"] mempty)
  ,("border-right-width", PropertyInfo ["border", "border-width", "border-right"] mempty)
  ,("border-style", PropertyInfo ["border"] ["border-top-style", "border-right-style", "border-bottom-style", "border-left-style"])
  ,("border-top",       PropertyInfo ["border"] ["border-top-color", "border-top-style", "border-top-width"])
  ,("border-top-color", PropertyInfo ["border", "border-color", "border-top"] mempty)
  ,("border-top-left-radius",     PropertyInfo ["border-radius"] mempty)
  ,("border-top-right-radius",    PropertyInfo ["border-radius"] mempty)
  ,("border-top-style", PropertyInfo ["border", "border-style", "border-top"] mempty)
  ,("border-top-width", PropertyInfo ["border", "border-width", "border-top"] mempty)
  ,("border-width",        PropertyInfo ["border"] ["border-top-width", "border-right-width", "border-bottom-width", "border-left-width"])
  ,("column-count", PropertyInfo ["columns"] mempty)
  ,("column-rule", PropertyInfo mempty ["column-rule-color", "column-rule-style", "column-rule-width"])
  ,("column-rule-color", PropertyInfo ["column-rule"] mempty)
  ,("column-rule-style", PropertyInfo ["column-rule"] mempty)
  ,("column-rule-width", PropertyInfo ["column-rule"] mempty)
  ,("columns", PropertyInfo mempty ["column-width", "column-count"])
  ,("column-width", PropertyInfo ["columns"] mempty)
  ,("flex",        PropertyInfo mempty ["flex-grow", "flex-shrink", "flex-basis"])
  ,("flex-basis",  PropertyInfo ["flex"] mempty)
  ,("flex-direction", PropertyInfo ["flex-flow"] mempty)
  ,("flex-flow",      PropertyInfo mempty ["flex-direction", "flex-wrap"])
  ,("flex-grow",   PropertyInfo ["flex"] mempty)
  ,("flex-shrink", PropertyInfo ["flex"] mempty)
  ,("flex-wrap",      PropertyInfo ["flex-flow"] mempty)
  ,("font", PropertyInfo mempty ["font-style", "font-variant", "font-weight", "font-stretch", "font-size", "line-height", "font-family"])
  ,("font-family",            PropertyInfo ["font"] mempty)
  ,("font-kerning",           PropertyInfo ["font"] mempty)
  ,("font-language-override", PropertyInfo ["font"] mempty)
  ,("font-size",              PropertyInfo ["font"] mempty)
  ,("font-size-adjust",       PropertyInfo ["font"] mempty)
  ,("font-stretch",           PropertyInfo ["font"] mempty)
  ,("font-style",             PropertyInfo ["font"] mempty)
  ,("font-variant",           PropertyInfo ["font"] mempty)
  ,("font-weight",            PropertyInfo ["font"] mempty)
  ,("grid", PropertyInfo mempty ["grid-template-rows", "grid-template-columns", "grid-template-areas", "grid-auto-rows", "grid-auto-columns", "grid-auto-flow", "grid-column-gap", "grid-row-gap"])
  ,("grid-area",     PropertyInfo mempty ["grid-row-start", "grid-row-end", "grid-column-start", "grid-column-end"])
  ,("grid-column",   PropertyInfo mempty ["grid-column-start", "grid-column-end"])
  ,("grid-column-end", PropertyInfo ["grid-column", "grid-area"] mempty)
  ,("grid-column-end", PropertyInfo ["grid-column", "grid-area"] mempty)
  ,("grid-column-gap", PropertyInfo ["grid-gap"] mempty)
  ,("grid-gap",      PropertyInfo mempty ["grid-row-gap", "grid-column-gap"])
  ,("grid-row",      PropertyInfo mempty ["grid-row-start", "grid-row-end"])
  ,("grid-row-end", PropertyInfo ["grid-row", "grid-area"] mempty)
  ,("grid-row-gap",    PropertyInfo ["grid-gap"] mempty)
  ,("grid-row-start", PropertyInfo ["grid-row", "grid-area"] mempty)
  ,("grid-template", PropertyInfo mempty ["grid-template-columns", "grid-template-rows", "grid-template-areas"])
  ,("grid-template-areas",   PropertyInfo ["grid-template"] mempty)
  ,("grid-template-columns", PropertyInfo ["grid-template"] mempty)
  ,("grid-template-rows",    PropertyInfo ["grid-template"] mempty)
  ,("line-height",            PropertyInfo ["font"] mempty)
  ,("list-style",          PropertyInfo mempty ["list-style-type", "list-style-position", "list-style-image"])
  ,("list-style-image",    PropertyInfo ["list-style"] mempty)
  ,("list-style-position", PropertyInfo ["list-style"] mempty)
  ,("list-style-type",     PropertyInfo ["list-style"] mempty)
  ,("margin",        PropertyInfo mempty ["margin-top", "margin-right", "margin-bottom", "margin-left"])
  ,("margin-bottom", PropertyInfo ["margin"] mempty)
  ,("margin-left",   PropertyInfo ["margin"] mempty)
  ,("margin-right",  PropertyInfo ["margin"] mempty)
  ,("margin-top",    PropertyInfo ["margin"] mempty)
  ,("mask",        PropertyInfo mempty ["mask-clip", "mask-origin", "mask-border"])
  ,("mask-border", PropertyInfo ["mask"] ["mask-border-source", "mask-border-slice", "mask-border-width", "mask-border-outset", "mask-border-repeat"])
  ,("mask-border-outset", PropertyInfo ["mask", "mask-border"] mempty)
  ,("mask-border-repeat", PropertyInfo ["mask", "mask-border"] mempty)
  ,("mask-border-slice",  PropertyInfo ["mask", "mask-border"] mempty)
  ,("mask-border-source", PropertyInfo ["mask", "mask-border"] mempty)
  ,("mask-border-width",  PropertyInfo ["mask", "mask-border"] mempty)
  ,("mask-clip",      PropertyInfo ["mask"] mempty)
  ,("mask-composite", PropertyInfo ["mask"] mempty)
  ,("mask-image",     PropertyInfo ["mask"] mempty)
  ,("mask-mode",      PropertyInfo ["mask"] mempty)
  ,("mask-origin",    PropertyInfo ["mask"] mempty)
  ,("mask-position",  PropertyInfo ["mask"] mempty)
  ,("mask-repeat",    PropertyInfo ["mask"] mempty)
  ,("mask-size",      PropertyInfo ["mask"] mempty)
  ,("outline",       PropertyInfo mempty ["outline-width", "outline-style", "outline-color"])
  ,("outline-color", PropertyInfo ["outline"] mempty)
  ,("outline-style", PropertyInfo ["outline"] mempty)
  ,("outline-width", PropertyInfo ["outline"] mempty)
  ,("padding",        PropertyInfo mempty ["padding-top", "padding-right", "padding-bottom", "padding-left"])
  ,("padding-bottom", PropertyInfo ["padding"] mempty)
  ,("padding-left",   PropertyInfo ["padding"] mempty)
  ,("padding-right",  PropertyInfo ["padding"] mempty)
  ,("padding-top",    PropertyInfo ["padding"] mempty)
  ,("text-decoration",       PropertyInfo mempty ["text-decoration-color", "text-decoration-style", "text-decoration-line"])
  ,("text-decoration-color", PropertyInfo ["text-decoration"] mempty)
  ,("text-decoration-line",  PropertyInfo ["text-decoration"] mempty)
  ,("text-decoration-style", PropertyInfo ["text-decoration"] mempty)
  ,("text-emphasis",       PropertyInfo mempty ["text-emphasis-color", "text-emphasis-style"])
  ,("text-emphasis-color", PropertyInfo ["text-emphasis"] mempty)
  ,("text-emphasis-style", PropertyInfo ["text-emphasis"] mempty)
  ,("transition", PropertyInfo mempty ["transition-property", "transition-duration", "transition-timing-function", "transition-delay"])
  ,("transition-delay",           PropertyInfo ["transition"] mempty)
  ,("transition-duration",        PropertyInfo ["transition"] mempty)
  ,("transition-property",        PropertyInfo ["transition"] mempty)
  ,("transition-timing-function", PropertyInfo ["transition"] mempty)
  ]

-- mempty is used when there is no initial value, or for properties that need
-- special treatment, such as many of the shorthands
propertiesTraits :: Map Text (Maybe Values, Bool)
propertiesTraits = Map.fromList $ replaceTextWithValues
  [("align-content",              ("stretch",                  False))
  ,("align-items",                ("stretch",                  False))
  ,("align-self",                 ("auto",                     False))
  ,("all",                        (mempty,                     False)) -- No practical initial value
  ,("animation",                  (mempty,                     False)) -- shorthand
  ,("animation-delay",            ("0s",                       False)) -- experimental
  ,("animation-direction",        ("normal",                   False)) -- experimental
  ,("animation-duration",         ("0s",                       False)) -- experimental
  ,("animation-fill-mode",        ("none",                     False)) -- experimental
  ,("animation-iteration-count",  ("1",                        False)) -- experimental
  ,("animation-name",             ("none",                     False)) -- experimental
  ,("animation-play-state",       ("running",                  False)) -- experimental
  ,("animation-timing-function",  ("ease",                     False)) -- experimental
  ,("backface-visibility",        ("visible",                  False))
  ,("-webkit-backface-visibility", ("visible",                 False))
  ,("backdrop-filter",            ("none",                     False)) -- experimental
  ,("-webkit-backdrop-filter",    ("none",                     False)) -- experimental
  ,("background",                 (mempty {-shorthand-},       False))
  ,("background-attachment",      ("scroll",                   False))
  ,("background-blend-mode",      ("normal",                   False))
  ,("background-clip",            ("border-box",               False))
  ,("-webkit-background-clip",    ("border-box",               False))
  ,("background-color",           ("transparent",              False))
  ,("background-image",           ("none",                     False))
  ,("background-origin",          ("padding-box",              False))
  ,("background-position",        (mempty,                     False)) --shorthand
  ,("background-position-x",      ("left",                     False))
  ,("background-position-inline", (mempty {-not applicable-},  False))
  ,("background-position-block",  (mempty {-not applicable-},  False))
  ,("background-position-y",      ("top",                      False))
  ,("background-repeat",          ("repeat",                   False))
  ,("background-size",            ("auto",                     False))
  ,("block-size",                 ("auto",                     False)) -- experimental
  ,("border",                     ("medium none currentcolor", False)) -- shorthand!
  -- border-block-* would go here
  ,("border-bottom",              ("medium none currentcolor", False)) -- shorthand!
  ,("border-bottom-color",        ("currentcolor",             False))
  ,("border-bottom-left-radius",  ("0",                        False))
  ,("border-bottom-right-radius", ("0",                        False))
  ,("border-bottom-style",        ("none",                     False))
  ,("border-bottom-width",        ("medium",                   False))
  ,("border-collapse",            ("separate",                 True))
  ,("border-color",               (mempty,                     False)) -- shorthand!
  ,("border-image",               (mempty,                     False)) -- shorthand
  ,("border-image-outset",        ("0",                        False))
  ,("border-image-repeat",        ("stretch",                  False))
  ,("border-image-slice",         ("100%",                     False))
  ,("border-image-source",        ("none",                     False))
  ,("border-image-width",         ("1",                        False))
  -- border-inline-* would go here
  ,("border-left",                ("medium none currentcolor", False)) -- shorthand!
  ,("border-left-color",          ("currentcolor",             False))
  ,("border-left-style",          ("none",                     False))
  ,("border-left-width",          ("medium",                   False))
  -- ,("border-radius" -- shorthand
  ,("border-right",               ("medium none currentcolor", False)) -- shorthand!
  ,("border-right-color",         ("currentcolor",             False))
  ,("border-right-style",         ("none",                     False))
  ,("border-right-width",         ("medium",                   False))
  ,("border-spacing",             ("0",                        True))
  -- ,("border-style" -- shorthand
  ,("border-top",                 ("medium none currentcolor", False)) -- shorthand
  ,("border-top-color",           ("currentcolor",             False))
  ,("border-top-style",           ("none",                     False))
  ,("border-top-width",           ("medium",                   False))
  ,("border-top-left-radius",     ("0",                        False))
  ,("border-top-right-radius",    ("0",                        False))
  -- ,("border-width" -- shorthand
  ,("bottom",                     ("auto",                     False))
  ,("box-decoration-break",       ("slice",                    False)) -- experimental
  ,("-webkit-box-decoration-break", ("slice",                  False)) -- experimental
  ,("-o-box-decoration-break",    ("slice",                    False)) -- experimental
  ,("box-shadow",                 ("none",                     False))
  ,("-webkit-box-shadow",         ("none",                     False))
  ,("box-sizing",                 ("content-box",              False)) -- experimental
  ,("-webkit-box-sizing",         ("content-box",              False)) -- experimental
  ,("-moz-box-sizing",            ("content-box",              False)) -- experimental
  ,("break-after",                ("auto",                     False))
  ,("break-before",               ("auto",                     False))
  ,("break-inside",               ("auto",                     False))
  ,("caption-side",               ("top",                      True))
  ,("clear",                      ("none",                     False))
  ,("clip",                       ("auto",                     False)) -- deprecated!
  ,("color",                      (mempty {-UA dependent-},    True))
  ,("column-count",               ("auto",                     False))
  ,("column-fill",                ("balance",                  False))
  ,("column-gap",                 ("normal",                   False))
  ,("column-rule",                ("medium none currentcolor", False)) -- shorthand
  ,("column-rule-color",          ("currentcolor",             False))
  ,("column-rule-style",          ("none",                     False))
  ,("column-rule-width",          ("medium",                   False))
  ,("column-span",                ("none",                     False))
  ,("column-width",               ("auto",                     False))
  -- ,("columns", -- shorthand
  ,("content",                    ("normal",                   False))
  ,("counter-increment",          ("none",                     False))
  ,("counter-reset",              ("none",                     False))
  ,("cursor",                     ("auto",                     True))
  ,("direction",                  ("ltr",                      True))
  ,("display",                    ("inline",                   False))
  ,("empty-cells",                ("show",                     True))
  ,("filter",                     ("none",                     False)) -- experimental
  ,("-webkit-filter",             ("none",                     False)) -- experimental
  -- ,("flex",                    ("0",                     False)) -- shorthand
  ,("flex-basis",                 ("auto",                     False))
  ,("flex-direction",             ("row",                      False))
  ,("flex-flow",                  ("row nowrap",               False)) -- shorthand
  ,("flex-grow",                  ("0",                        False))
  ,("flex-shrink",                ("1",                        False))
  ,("flex-wrap",                  ("nowrap",                   False))
  ,("float",                      ("none",                     False))
  ,("float",                      ("none",                     False))
  ,("font",                       (mempty {-shorthand-},       True))
  ,("font-family",                (mempty {-UA dependent-},    True))
  ,("font-feature-settings",      ("normal",      True))
  ,("font-kerning",               ("auto",        True))
  ,("font-language-override",     ("normal",      True))
  ,("font-size",                  ("medium",      True))
  ,("font-size-adjust",           ("none",        True))
  ,("font-stretch",               ("normal",      True))
  ,("font-style",                 ("normal",      True))
  ,("font-synthesis",             ("weight style",      True))
  ,("font-variant",               ("normal",      True))
  ,("font-variant-alternates",    ("normal",      True))
  ,("font-variant-caps",          ("normal",      True))
  ,("font-variant-east-asian",    ("normal",      True))
  ,("font-variant-ligatures",     ("normal",      True))
  ,("font-variant-numeric",       ("normal",      True))
  ,("font-variant-position",      ("normal",      True))
  ,("font-weight",                ("normal",      True))
  -- grid -- shorthand, experimental
  -- grid-area -- shorthand, experimental
  ,("grid-auto-columns",          ("auto", False)) -- experimental
  ,("-ms-grid-auto-columns",      ("auto", False)) -- experimental
  ,("-webkit-grid-auto-columns",  ("auto", False)) -- experimental
  ,("grid-auto-flow",             ("row", False)) -- experimental
  ,("-webkit-grid-auto-flow",     ("row", False)) -- experimental
  ,("grid-auto-rows",             ("auto", False)) -- experimental
  ,("-ms-grid-auto-rows",         ("auto", False)) -- experimental
  ,("-webkit-grid-auto-rows",     ("auto", False)) -- experimental
  -- grid-column -- shorthand, experimental
  ,("grid-column-end",            ("auto", False)) -- experimental
  ,("-ms-grid-column-end",        ("auto", False)) -- experimental
  ,("-webkit-grid-column-end",    ("auto", False)) -- experimental
  ,("grid-column-gap",            ("0", False)) -- experimental
  ,("-ms-grid-column-gap",        ("0", False)) -- experimental
  ,("grid-column-start",          ("auto", False)) -- experimental
  ,("-ms-grid-column-start",      ("auto", False)) -- experimental
  ,("-webkit-grid-column-start",  ("auto", False)) -- experimental
  -- ,("grid-gap", -- shorthand, experimental
  -- ,("grid-row", -- shorthand, experimental
  ,("grid-row-end",               ("auto", False)) -- experimental
  ,("-ms-grid-row-end",           ("auto", False)) -- experimental
  ,("-webkit-grid-row-end",       ("auto", False)) -- experimental
  ,("grid-row-gap",               ("0", False)) -- experimental
  ,("-webkit-grid-row-gap",       ("0", False)) -- experimental
  ,("grid-row-start",             ("auto", False)) -- experimental
  ,("-ms-grid-row-start",         ("auto", False)) -- experimental
  ,("-webkit-grid-row-start",     ("auto", False)) -- experimental
  -- ,("grid-template", -- shorthand, experimental
  ,("grid-template-areas",        ("none", False)) -- experimental
  ,("-webkit-grid-template-areas", ("none", False)) -- experimental
  ,("grid-template-columns",      ("none", False)) -- experimental
  ,("-ms-grid-template-columns",  ("none", False)) -- experimental
  ,("-webkit-grid-template-columns", ("none", False)) -- experimental
  ,("grid-template-rows",         ("none", False)) -- experimental
  ,("-ms-grid-template-rows",     ("none", False)) -- experimental
  ,("-webkit-grid-template-rows", ("none", False)) -- experimental
  ,("height",                     ("auto",      False))
  ,("hyphens",                    ("manual",      False))
  -- ,("image-orientation" --experimental
  -- ,("image-rendering" --experimental
  -- ,("inline-size", -- experimental
  ,("isolation",                  ("auto",      False))
  ,("justify-content",            ("flex-start",      False))
  ,("left",                       ("auto",      False))
  ,("letter-spacing",             ("normal",      True))
  ,("line-break",                 ("auto",      False))
  ,("line-height",                ("normal",      True))
  ,("list-style",                 ("none disc outside", True)) -- none must go first, for entropy reasons.
  ,("list-style-image",           ("none", True))
  ,("list-style-position",        ("outside", True))
  ,("list-style-type",            ("disc", True))
  ,("margin",                     (mempty {-shorthand-}, False)) -- shorthand
  -- ,("margin-block-* -- experimental
  ,("margin-bottom",                      ("0", False))
  -- ,("margin-inline-*" --experimental
  ,("margin-left",                        ("0", False))
  ,("margin-right",                       ("0", False))
  ,("margin-top",                         ("0", False))
  -- ,("mask", -- shorthand
  ,("mask-clip",                          ("border-box", False)) -- experimental
  ,("-webkit-mask-clip",                  ("border-box", False)) -- experimental
  ,("mask-composite",                     ("add", False)) -- experimental
  ,("mask-image",                         ("none", False)) -- experimental
  ,("-webkit-mask-image",                 ("none", False)) -- experimental
  -- ,("mask-mode",                 ("match-source", False)) -- experimental, no support
  ,("mask-origin",                        ("border-box", False)) -- experimental
  ,("-webkit-mask-origin",                ("border-box", False)) -- experimental
  ,("mask-position",                      ("center", False)) -- experimental
  ,("-webkit-mask-position",              ("center", False)) -- experimental
  ,("mask-repeat",                        ("no-repeat", False)) -- experimental
  ,("-webkit-mask-repeat",                ("no-repeat", False)) -- experimental
  ,("mask-size",                          ("auto", False)) -- experimental
  ,("mask-type",                          ("luminance", False)) -- experimental
  -- ,("max-block-size",         ("0", False)) -- experimental
  ,("max-height",                         ("none", False))
  -- ,("max-inline-size",        ("0", False)) -- experimental
  ,("max-width",                          ("none", False))
  -- ,("min-block-size",         ("0", False)) -- experimental
  ,("min-height",                         ("0", False))
  -- ,("min-inline-size",        ("0", False)) -- experimental
  ,("min-width",                          ("0", False))
  ,("mix-blend-mode",                     ("normal", False))
  ,("object-fit",                         ("fill", True))
  ,("object-position",                    ("50% 50%", True))
  -- ,("offset-*",                        ("auto", False)) -- experimental
  ,("opacity",                            ("1", False))
  ,("order",                              ("0", False))
  ,("orphans",                            ("2", True))
  ,("outline",                            ("medium none invert", False)) -- shorthand
  ,("outline-color",                      ("invert" , False))
  ,("outline-offset",                     ("0", False))
  ,("outline-style",                      ("none", False))
  ,("outline-width",                      ("medium", False))
  ,("overflow",                           ("visible", False))
  ,("overflow-wrap",                      ("normal", True)) -- also called word-wrap
  ,("overflow-x",                         ("visible", False)) -- experimental
  ,("overflow-y",                         ("visible", False)) -- experimental
  ,("padding",                            (mempty, False)) -- shorthand
  -- ,("padding-block*",          ("0", False)) -- experimental
  ,("padding-bottom",                     ("0", False))
  -- ,("padding-inline-*",                ("0", False)) -- experimental
  ,("padding-left",                       ("0", False))
  ,("padding-right",                      ("0", False))
  ,("padding-top",                        ("0", False))
  ,("page-break-after",                   ("auto", False))
  ,("page-break-before",                  ("auto", False))
  ,("page-break-inside",                  ("auto", False))
  ,("perspective",                        ("none", False)) -- experimental
  ,("perspective-origin",                 ("50% 50%", False)) -- experimental
  ,("pointer-events",                     ("auto", True))
  ,("position",                           ("static", False))
  ,("quotes",                             (mempty {-UA dependent-}, True))
  ,("resize",                             ("none", False))
  ,("right",                              ("auto", False))
  ,("right",                              ("auto", False))
  -- ,("ruby-*", -- experimental
  ,("scroll-behavior",                    ("auto", False))
  -- ,("scroll-snap-coordinate", ("none", False)) -- experimental
  -- ,("scroll-snap-destination",("0 0", False)) -- experimental, actually 0px 0px
  -- ,("scroll-snap-type", -- experimental
  ,("shape-image-threshold",              ("0", False))
  ,("shape-margin",                       ("0", False))
  ,("shape-outside",                      ("none", False))
  ,("tab-size",                           ("8", True)) -- experimental
  ,("-moz-tab-size",                      ("8", True)) -- experimental
  ,("-o-tab-size",                        ("8", True)) -- experimental
  ,("table-layout",                       ("auto", False))
  ,("text-align",                         ("start", False))
  ,("text-align-last",                    ("auto", True)) -- experimental
  ,("text-combine-upright",               ("none", True)) -- experimental
  ,("-webkit-text-combine-upright",       ("none", True)) -- experimental
  ,("text-decoration",                    ("currentcolor solid none", False)) -- shorthand
  ,("text-decoration-color",              ("currentcolor", False))
  ,("text-decoration-line",               ("none", False))
  ,("text-decoration-style",              ("solid", False))
  ,("text-emphasis",                      ("none currentcolor", False)) -- shorthand
  ,("text-emphasis-color",                ("currentcolor", False))
  ,("text-emphasis-position",             (mempty {-"over right"-}, False))
  ,("text-emphasis-style",                ("none", False))
  ,("text-indent",                        ("0", True))
  ,("text-orientation",                   ("mixed", True)) -- experimental
  ,("-webkit-text-orientation",           ("mixed", True)) -- experimental
  ,("text-overflow",                      ("clip", False))
  ,("text-rendering",                     ("auto", True))
  ,("text-shadow",                        ("none", True))
  ,("text-transform",                     ("none", True))
  ,("text-underline-position",            ("auto", True))
  ,("top",                                ("auto", False))
  ,("touch-action",                       ("auto", False))
  ,("transform",                          ("none", False)) -- experimental!
  ,("-webkit-transform",                  ("none", False)) -- experimental!
  ,("transform-box",                      ("border-box", False)) -- experimental!
  ,("transform-origin",                   (mempty {-"50% 50% 0"-}, False)) -- experimental!
  ,("-webkit-transform-origin",           (mempty {-"50% 50% 0"-}, False)) -- experimental!
  ,("transform-style",                    ("flat", False)) -- experimental!
  ,("-webkit-transform-style",            ("flat", False)) -- experimental!
  ,("transition",                         (mempty, False))-- shorthand and experimental
  ,("transition-delay",                   ("0s", False)) -- experimental
  ,("transition-duration",                ("0s", False)) -- experimental
  ,("-webkit-transition-duration",        ("0s", False)) -- experimental
  ,("-o-transition-duration",             ("0s", False)) -- experimental
  ,("-webkit-transition-delay",           ("0s", False)) -- experimental
  ,("transition-property",                ("all", False)) -- experimental
  ,("-webkit-transition-property",        ("all", False)) -- experimental
  ,("-o-transition-property",             ("all", False)) -- experimental
  ,("transition-timing-function",         ("ease", False)) -- experimental
  ,("-webkit-transition-timing-function", ("ease", False)) -- experimental
  ,("unicode-bidi",                       ("normal", False))
  ,("user-select",                        ("none", False)) -- experimental
  ,("-moz-user-select",                   ("none", False)) -- experimental
  ,("-webkit-user-select",                ("none", False)) -- experimental
  ,("-ms-user-select",                    ("none", False)) -- experimental
  ,("vertical-align",                     ("baseline", False))
  ,("white-space",                        ("normal", True))
  ,("widows",                             ("2", True))
  ,("width",                              ("auto", False))
  ,("will-change",                        ("auto", False))
  ,("word-break",                         ("normal", True))
  ,("word-spacing",                       ("normal", True))
  ,("writing-mode",                       ("horizontal-tb", True)) -- experimental
  ,("-ms-writing-mode",                   ("horizontal-tb", True)) -- experimental
  ,("-webkit-writing-mode",               ("horizontal-tb", True)) -- experimental
  ,("z-index",                            ("auto", False))
  ]

-- Parses text and replaces it with an array of values, to makes the
-- propertiesTraits table more readable and maintainable in general.
replaceTextWithValues :: [(Text, (Text, Bool))] -> [(Text, (Maybe Values, Bool))]
replaceTextWithValues = foldr (\(p,(t,i)) xs -> (p, (getValues p t,i)) : xs) []
  where getValues p s = case parseOnly (values p <|> valuesFallback) s of
                          Right initialValues -> Just initialValues
                          Left _              -> Nothing
