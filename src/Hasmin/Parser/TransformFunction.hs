-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.TransformFunction
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Parsers for CSS values.
--
-----------------------------------------------------------------------------
module Hasmin.Parser.TransformFunction
    ( matrix
    , matrix3d
    , rotate3d
    , scale
    , scale3d
    , skew
    , translate
    , translate3d
    ) where

import Control.Applicative (liftA3, optional)
import Data.Attoparsec.Text (count, Parser)

import Hasmin.Parser.Numeric
import Hasmin.Parser.Dimension
import Hasmin.Parser.PercentageLength
import Hasmin.Parser.Utils
import Hasmin.Types.TransformFunction

matrix :: Parser TransformFunction
matrix = functionParser $ do
    n  <-  number
    ns <-  count 5 (comma *> number)
    pure $ mkMat (n:ns)

matrix3d :: Parser TransformFunction
matrix3d = functionParser $ do
    n  <-  number
    ns <-  count 15 (comma *> number)
    pure $ mkMat3d (n:ns)

rotate3d :: Parser TransformFunction
rotate3d = functionParser $ do
    x  <-  number <* comma
    y  <-  number <* comma
    z  <-  number <* comma
    Rotate3d x y z <$> angle

-- | Parser of scale() function. Assumes "scale(" has been already parsed
scale :: Parser TransformFunction
scale = functionParser $ do
    n  <- number
    mn <- optional (comma *> number)
    pure $ Scale n mn

scale3d :: Parser TransformFunction
scale3d = functionParser $ liftA3 Scale3d n n number
  where n = number <* comma

skew :: Parser TransformFunction
skew = functionParser $ do
    a  <- angle
    ma <- optional (comma *> angle)
    pure $ Skew a ma

-- | Assumes "translate(" has been already parsed
translate :: Parser TransformFunction
translate = functionParser $ do
    pl  <- percentageLength
    mpl <- optional (comma *> percentageLength)
    pure $ Translate pl mpl

translate3d :: Parser TransformFunction
translate3d = functionParser $
    Translate3d <$> percentageLength <* comma
                <*> percentageLength <* comma
                <*> distance
