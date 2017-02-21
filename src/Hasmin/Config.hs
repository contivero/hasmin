-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Config
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Hasmin.Config (
      Config(..)
    , ColorSettings(..)
    , DimensionSettings(..)
    , GradientSettings(..)
    , FontWeightSettings(..)
    , LetterCase(..)
    , SortingMethod(..)
    , defaultConfig
    , Instructions
    , Commands(..)
    ) where

type Instructions = (Commands, Config)

data Commands = Commands { shouldBeautify :: Bool
                         , shouldCompress :: Bool
                         , file :: FilePath
                         } deriving (Show)

data ColorSettings = ColorMinOff | ColorMinOn
  deriving (Show, Eq)
data DimensionSettings = DimMinOff | DimMinOn
  deriving (Show, Eq)
data GradientSettings = GradientMinOff | GradientMinOn
  deriving (Show, Eq)
data LetterCase = Original  -- ^ Leave letter casing as is.
                | Lowercase -- ^ Lowercase whatever possible to improve gzip compression.
  deriving (Show, Eq)
data SortingMethod = NoSorting | Lexicographical
  deriving (Show, Eq)
data FontWeightSettings = FontWeightMinOff | FontWeightMinOn
  deriving (Show, Eq)

-- TODO: * avoid boolean blindness
--       * Use a more declarative style for the names, e.g. shouldLowercase vs.
--       preferredCasing, shouldSortSelectors vs. selectorSorting, etc.
-- | The configuration used for minifying.
data Config = Config { colorSettings :: ColorSettings
                     , dimensionSettings :: DimensionSettings
                     , gradientSettings :: GradientSettings
                     , shouldUsePropertyTraits :: Bool
                     , shouldCleanRules :: Bool
                     , shouldMinifyTimingFunctions :: Bool
                     , shouldMinifyFilterFunctions :: Bool
                     , shouldRemoveQuotes :: Bool
                     , fontweightSettings :: FontWeightSettings
                     , shouldMinifyTransformOrigin :: Bool
                     , shouldMinifyMicrosyntax :: Bool
                     , shouldMinifyKeyframeSelectors :: Bool
                     , shouldMinifyTransformFunction :: Bool
                     , shouldConvertEscaped :: Bool
                     , shouldConvertNullPercentages :: Bool
                     , shouldRemoveEmptyBlocks :: Bool
                     , shouldRemoveDuplicateSelectors :: Bool
                     , shouldNormalizeQuotes :: Bool
                     , letterCase :: LetterCase
                     , selectorSorting :: SortingMethod
                     , declarationSorting :: SortingMethod
                     } deriving (Show)

-- | A default config with most settings enabled. Used by the minify function,
-- mainly for testing purposes.
defaultConfig :: Config
defaultConfig = Config { colorSettings                  = ColorMinOn
                       , dimensionSettings              = DimMinOn
                       , gradientSettings               = GradientMinOn
                       , shouldUsePropertyTraits        = True
                       , shouldCleanRules               = True
                       , shouldMinifyTimingFunctions    = True
                       , shouldMinifyFilterFunctions    = True
                       , shouldRemoveQuotes             = True
                       , fontweightSettings             = FontWeightMinOn
                       , shouldMinifyTransformOrigin    = True
                       , shouldMinifyMicrosyntax        = True
                       , shouldMinifyKeyframeSelectors  = True
                       , shouldMinifyTransformFunction  = True
                       , shouldConvertEscaped           = True
                       , shouldConvertNullPercentages   = True
                       , shouldRemoveEmptyBlocks        = True
                       , shouldRemoveDuplicateSelectors = True
                       , shouldNormalizeQuotes          = True
                       , letterCase                     = Lowercase
                       , selectorSorting                = NoSorting
                       , declarationSorting             = NoSorting
                       }
