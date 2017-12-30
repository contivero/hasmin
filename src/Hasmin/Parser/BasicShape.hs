{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.BasicShape
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Parsers for CSS <https://www.w3.org/TR/css-shapes/#typedef-basic-shape \<basic-shape>> values.
--
-----------------------------------------------------------------------------
module Hasmin.Parser.BasicShape where

import Control.Applicative ((<|>),  optional, many)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.List.NonEmpty (NonEmpty((:|)))

import Hasmin.Parser.Utils
import Hasmin.Parser.PercentageLength
import Hasmin.Parser.Position
import Hasmin.Parser.BorderRadius
import Hasmin.Types.BasicShape
import Hasmin.Utils

-- inset( <shape-arg>{1,4} [round <border-radius>]? )
inset :: Parser BasicShape
inset = do
  sa  <- percentageLength <* skipComments
  sas <- atMost 3 (percentageLength <* skipComments)
  br  <- optional (A.asciiCI "round" *> skipComments *> borderRadius)
  pure $ Inset (sa:|sas) br

       -- circle( [<shape-radius>]? [at <position>]? )
circle :: Parser BasicShape
circle =
    Circle <$> optional (shapeRadius <* skipComments)
           <*> optional (A.asciiCI "at" *> skipComments *> position)

-- ellipse( [<shape-radius>{2}]? [at <position>]? )
ellipse :: Parser BasicShape
ellipse =
    Ellipse <$> optional (pair shapeRadius)
            <*> optional (lexeme position)

-- polygon( [<fill-rule>,]? [<shape-arg> <shape-arg>]# )
polygon :: Parser BasicShape
polygon = Polygon <$> optional fillrule <*> many (pair percentageLength)

pair :: Parser a -> Parser (a, a)
pair p = mzip (lexeme p) p

shapeRadius :: Parser ShapeRadius
shapeRadius = either SRPercentage SRLength <$> percentageLength
           <|> parserFromPairs [("closest-side", pure SRClosestSide)
                               ,("farthest-side", pure SRFarthestSide)]

fillrule :: Parser FillRule
fillrule = parserFromPairs [("nonzero", pure NonZero)
                           ,("evenodd", pure EvenOdd)]
