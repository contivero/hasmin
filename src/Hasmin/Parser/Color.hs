{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.Color
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Parsers for CSS \<color> values.
--
-----------------------------------------------------------------------------
module Hasmin.Parser.Color where

import Control.Applicative ((<|>),  optional)
import Control.Monad (mzero)
import Data.Attoparsec.Text (Parser)
import Data.Maybe (fromMaybe)
import qualified Data.Attoparsec.Text as A
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Hasmin.Parser.Utils
import Hasmin.Parser.Numeric
import Hasmin.Types.Color

hex :: Parser Color
hex = do
    _ <- A.char '#'
    a <- hexadecimal
    b <- hexadecimal
    c <- hexadecimal
    x <- optional hexadecimal
    case x of
      Nothing -> pure $ mkHex3 a b c
      Just d  -> do y <- optional hexadecimal
                    case y of
                      Nothing -> pure $ mkHex4 a b c d
                      Just e  -> do f <- hexadecimal
                                    z <- optional hexadecimal
                                    case z of
                                      Nothing -> pure $ mkHex6 [a,b] [c,d] [e,f]
                                      Just g  -> do h <- hexadecimal
                                                    pure $ mkHex8 [a,b] [c,d] [e,f] [g,h]

-- Assumes "rgb(" has already been read
rgb :: Parser Color
rgb = functionParser (rgbInt <|> rgbPer)
  where rgbInt = mkRGBInt <$> word8 <* comma <*> word8 <* comma <*> word8
        rgbPer = mkRGBPer <$> percentage <* comma
                          <*> percentage <* comma <*> percentage

-- Assumes "rgba(" has already been read
rgba :: Parser Color
rgba = functionParser (rgbaInt <|> rgbaPer)
  where rgbaInt = mkRGBAInt <$> word8 <* comma <*> word8 <* comma
                            <*> word8 <* comma <*> alphavalue
        rgbaPer = mkRGBAPer <$> percentage <* comma <*> percentage <* comma
                            <*> percentage <* comma <*> alphavalue

-- Assumes "hsl(" has already been read
hsl :: Parser Color
hsl = functionParser p
  where p = mkHSL <$> int <* comma <*> percentage <* comma <*> percentage

-- Assumes "hsla(" has already been read
hsla :: Parser Color
hsla = functionParser p
  where p = mkHSLA <$> int <* comma <*> percentage <* comma
                   <*> percentage <* comma <*> alphavalue

-- | Parser for <https://drafts.csswg.org/css-color-3/#colorunits \<color\>>.
color :: Parser Color
color = hex <|> othercolor
  where
    othercolor = do
      i <- ident
      let t = T.toLower i
      fromMaybe (colorFunctionParser t) (Map.lookup t namedColorsParsersMap)

namedColorsParsersMap :: Map Text (Parser Color)
namedColorsParsersMap = Map.fromList $ foldr f [] keywordColors
  where f x xs = let a = fst x
                 in (a, pure $ Named a) : xs

colorFunctionsParsers :: [(Text, Parser Color)]
colorFunctionsParsers =
          [("rgb",  rgb)
          ,("rgba", rgba)
          ,("hsl",  hsl)
          ,("hsla", hsla)
          ]

functionPar :: Map Text (Parser a) -> Text -> Parser a
functionPar m i = A.char '(' *> fromMaybe mzero (Map.lookup t m)
  where t = T.toLower i

colorFunctionParser :: Text -> Parser Color
colorFunctionParser = functionPar (Map.fromList colorFunctionsParsers)
