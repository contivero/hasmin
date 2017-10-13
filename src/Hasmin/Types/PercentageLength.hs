{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.PercentageLength
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Hasmin.Types.PercentageLength
    ( PercentageLength
    , isZero
    , isNonZeroPercentage
    ) where

import Hasmin.Types.Dimension
import Hasmin.Types.Numeric
import Hasmin.Types.Class

-- | CSS <length-percentage> data type, i.e.: [length | percentage]
-- Though because of the name it would be more intuitive to define:
-- type LengthPercentage = Either Distance Percentage,
-- they are inverted here to make use of Either's Functor instance, because it
-- makes no sense to minify a Percentage.
type PercentageLength = Either Percentage Distance

-- TODO see if this instance can be deleted altogether.
instance Minifiable PercentageLength where
  minifyWith x@(Right _) = mapM minifyWith x
  minifyWith x@(Left p) | p == 0    = pure $ Right (Distance 0 Q) -- minifies 0% to 0
                        | otherwise = pure x

isNonZeroPercentage :: PercentageLength -> Bool
isNonZeroPercentage (Left p) = p /= 0
isNonZeroPercentage _        = False

isZero :: (Num a, Eq a) => Either a Distance -> Bool
isZero = either (== 0) (== Distance 0 Q)


