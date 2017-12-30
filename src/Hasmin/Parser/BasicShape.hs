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
ellipse = Ellipse <$> twoSR <*> optional atPosition
  where atPosition = lexeme (A.asciiCI "at") *> position
        twoSR      = A.option None $ do
            sr1 <- shapeRadius
            (Two sr1 <$> (skipComments *> shapeRadius)) <|> pure (One sr1)

-- polygon( [<fill-rule>,]? [<shape-arg> <shape-arg>]# )
polygon :: Parser BasicShape
polygon = Polygon <$> optional fillrule <*> shapeargs
  where shapeargs = (:|) <$> pairOf percentageLength
                         <*> many (comma *> pairOf percentageLength)
        fillrule  = parserFromPairs
                      [("nonzero", pure NonZero)
                      ,("evenodd", pure EvenOdd)] <* comma
        pairOf p = mzip (p <* skipComments) p

shapeRadius :: Parser ShapeRadius
shapeRadius = either SRPercentage SRLength <$> percentageLength
           <|> parserFromPairs [("closest-side", pure SRClosestSide)
                               ,("farthest-side", pure SRFarthestSide)]

