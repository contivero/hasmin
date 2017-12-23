{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.Value
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Parsers for CSS \<gradient> values.
--
-----------------------------------------------------------------------------

module Hasmin.Parser.Gradient where

import Control.Applicative ((<|>), many, optional)
import Data.Functor (($>))
import Text.Parser.Permutation ((<|?>), (<$$>), (<$?>), (<||>), permute)
import Data.Maybe (isNothing)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import Hasmin.Parser.Utils
import Hasmin.Parser.Color
import Hasmin.Parser.Position
import Hasmin.Parser.Dimension
import Hasmin.Parser.PercentageLength
import Hasmin.Types.Gradient
import Hasmin.Utils

radialgradient :: Parser Gradient
radialgradient = functionParser $ do
    (def, c) <- A.option (True, RadialGradient Nothing Nothing) ((False,) <$> endingShapeAndSize <* skipComments)
    p  <- optional (A.asciiCI "at" *> skipComments *> position)
    _  <- if def && isNothing p
             then pure '*' -- do nothing
             else comma
    cs <- colorStopList
    pure $ c p cs
  where circle = A.asciiCI "circle" $> Just Circle <* skipComments
        ellipse = A.asciiCI "ellipse" $> Just Ellipse <* skipComments
        endingShapeAndSize = r1 <|> r2 <|> r3
          where r1 = permute (RadialGradient <$?> (Nothing, ellipse) <||> (Just <$> (PL <$> percentageLength <*> lexeme percentageLength)))
                r2 = permute (RadialGradient <$?> (Nothing, circle) <||> ((Just . SL) <$> distance <* skipComments))
                r3 = permute (RadialGradient <$?> (Nothing, circle <|> ellipse) <||> extentKeyword)
                   <|> permute (RadialGradient <$$> (circle <|> ellipse) <|?> (Nothing, extentKeyword))
                extentKeyword = Just <$>
                    parserFromPairs [("closest-corner",  pure ClosestCorner)
                                    ,("closest-side",    pure ClosestSide)
                                    ,("farthest-corner", pure FarthestCorner)
                                    ,("farthest-side",   pure FarthestSide)] <* skipComments

-- | Assumes "linear-gradient(", or one of its prefixed equivalents, has been parsed.
-- : [<angle>|to <side-or-corner> ,]? <color-stop> [, <color-stop>]+
lineargradient :: Parser Gradient
lineargradient = functionParser (lg <|> oldLg)
  where lg = LinearGradient <$> optional angleOrSide <*> colorStopList
        oldLg = OldLinearGradient <$> optional ((ga <|> sc) <* comma)
                                  <*> colorStopList
        angleOrSide = (ga <|> gs) <* comma
        ga = Left <$> angle
        gs = A.asciiCI "to" *> skipComments *> sc
        sc = Right <$> sideOrCorner

-- <side-or-corner> = [left | right] || [top | bottom]
sideOrCorner :: Parser (Side, Maybe Side)
sideOrCorner = orderOne <|> orderTwo
  where orderOne = mzip (leftright <* skipComments) (optional topbottom)
        orderTwo = mzip (topbottom <* skipComments) (optional leftright)

        leftright :: Parser Side
        leftright =  parserFromPairs [("left", pure LeftSide), ("right", pure RightSide)]

        topbottom :: Parser Side
        topbottom = parserFromPairs [("top", pure TopSide), ("bottom", pure BottomSide)]

colorStopList :: Parser [ColorStop]
colorStopList = do
    c1 <- colorStop
    _  <- A.char ',' <* skipComments
    c2 <- colorStop
    cs <- many (A.char ',' *> skipComments *> colorStop)
    pure $ c1:c2:cs

colorStop :: Parser ColorStop
colorStop = ColorStop <$> color <* skipComments
        <*> optional (percentageLength <* skipComments)
