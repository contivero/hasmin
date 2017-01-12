-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Config
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Hasmin.Config where

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

-- TODO: avoid boolean blindness?
data Config = Config { colorSettings :: ColorSettings
                     , dimensionSettings :: DimensionSettings
                     , gradientSettings :: GradientSettings
                     , shouldUsePropertyTraits :: Bool
                     , shouldCleanRules :: Bool
                     , shouldMinifyTimingFunctions :: Bool
                     , shouldMinifyFilterFunctions :: Bool
                     , shouldRemoveQuotes :: Bool
                     , shouldMinifyFontWeight :: Bool
                     , shouldMinifyTransformOrigin :: Bool
                     , shouldMinifyMicrosyntax :: Bool
                     , shouldMinifyKeyframeSelectors :: Bool
                     , shouldMinifyTransformFunction :: Bool
                     , shouldConvertEscaped :: Bool
                     , shouldConvertNullPercentages :: Bool
                     , shouldRemoveEmptyBlocks :: Bool
                     , shouldRemoveDuplicateSelectors :: Bool
                     , shouldNormalizeQuotes :: Bool
                     , shouldLowercase :: Bool
                     , shouldSortSelectors :: Bool
                     , shouldSortProperties :: Bool
                     } deriving (Show)

-- Used for the minify function
defaultConfig :: Config
defaultConfig = Config { colorSettings                 = ColorMinOn
                       , dimensionSettings             = DimMinOn
                       , gradientSettings              = GradientMinOn
                       , shouldUsePropertyTraits       = True
                       , shouldCleanRules              = True
                       , shouldMinifyTimingFunctions   = True
                       , shouldMinifyFilterFunctions   = True
                       , shouldRemoveQuotes            = True
                       , shouldMinifyFontWeight        = True
                       , shouldMinifyTransformOrigin   = True
                       , shouldMinifyMicrosyntax       = True
                       , shouldMinifyKeyframeSelectors = True
                       , shouldMinifyTransformFunction = True
                       , shouldConvertEscaped          = True
                       , shouldConvertNullPercentages  = True
                       , shouldRemoveEmptyBlocks       = True
                       , shouldRemoveDuplicateSelectors = True
                       , shouldNormalizeQuotes         = True
                       , shouldLowercase               = True
                       , shouldSortSelectors           = False
                       , shouldSortProperties          = False
                       } 
