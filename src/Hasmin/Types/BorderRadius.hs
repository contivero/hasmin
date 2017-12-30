{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.BorderRadius
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Types.BorderRadius
    ( BorderRadius(..)
    ) where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Monoid ((<>))
import qualified Data.Text.Lazy.Builder as B

import Hasmin.Class
import Hasmin.Types.PercentageLength
import Hasmin.Utils
                                -- <length-percentage>{1,4} [ / <length-percentage>{1,4} ]?
data BorderRadius = BorderRadius (NonEmpty PercentageLength) [PercentageLength]
  deriving Show

instance Eq BorderRadius where
  BorderRadius xs1 ys1 == BorderRadius xs2 ys2 =
      minifyBorderRadius xs1 ys1 `eq` minifyBorderRadius xs2 ys2
    where (BorderRadius as1 bs1) `eq` (BorderRadius as2 bs2) = as1 == as2 && bs1 == bs2

instance Minifiable BorderRadius where
  minify (BorderRadius xs ys) =
      minifyBorderRadius <$> traverse minifyPL xs <*> traverse minifyPL ys

minifyBorderRadius :: NonEmpty PercentageLength -> [PercentageLength] -> BorderRadius
minifyBorderRadius as [] = BorderRadius (reduceTRBL as) []
minifyBorderRadius as bs
    | l1 == l2  = BorderRadius l1 []
    | otherwise = BorderRadius l1 (NE.toList l2)
  where l1 = reduceTRBL as
        l2 = reduceTRBL (NE.fromList bs)

instance ToText BorderRadius where
  toBuilder (BorderRadius (h:|ts) xs) = toBuilder h <> f ' ' ts <> f '/' xs
    where f _ [] = mempty
          f i ys = B.singleton i <> mconcatIntersperse toBuilder " " ys
