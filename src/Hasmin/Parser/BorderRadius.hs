-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.BorderRadius
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Parsers for CSS values.
--
-----------------------------------------------------------------------------
module Hasmin.Parser.BorderRadius
    ( borderRadius
    ) where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.List.NonEmpty (NonEmpty((:|)))

import Hasmin.Parser.Utils
import Hasmin.Parser.PercentageLength
import Hasmin.Types.BorderRadius

-- <length-percentage>{1,4} [ / <length-percentage>{1,4} ]?
borderRadius :: Parser BorderRadius
borderRadius = do
    x  <- percentageLength <* skipComments
    xs <- atMost 3 (percentageLength <* skipComments)
    ys <- A.option [] $ A.char '/' *> atMost 4 (skipComments *> percentageLength)
    pure $ BorderRadius (x:|xs) ys
