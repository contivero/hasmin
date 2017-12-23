{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.PercentageLength
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Parser.PercentageLength where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map.Strict as Map
import qualified Data.Char as C
import qualified Data.Text as T
import Control.Monad (mzero)

import Hasmin.Parser.Utils
import Hasmin.Parser.Numeric
import Hasmin.Parser.Dimension
import Hasmin.Types.Numeric
import Hasmin.Types.Dimension
import Hasmin.Types.PercentageLength

percentageLength :: Parser PercentageLength
percentageLength = do
    n    <- number
    rest <- opt (A.string "%" <|> A.takeWhile1 C.isAlpha)
    if T.null rest  -- if true, then it was just a <number> value
       then if n == 0
               then pure $ Right NullLength
               else mzero
       else let r = T.toLower rest
            in case Map.lookup r plMap of
                 Just f  -> pure $ f n
                 Nothing -> mzero
  where plMap = Map.fromList $
            ("%", Left . toPercentage):((fmap . fmap) (Right .) distanceConstructorsList)
