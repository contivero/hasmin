{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.FilterFunction
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Types.FilterFunction
    ( FilterFunction(..)
    , minifyPseudoShadow
    ) where

import Control.Monad.Reader (Reader, ask)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton, Builder)

import Hasmin.Config
import Hasmin.Class
import Hasmin.Types.Dimension
import Hasmin.Types.Color
import Hasmin.Types.Numeric

-- | CSS <https://drafts.fxtf.org/filter-effects/#typedef-filter-function \<filter-function\>>
-- data type.
data FilterFunction = Blur Length
                    | Brightness (Either Number Percentage)
                    | Contrast (Either Number Percentage)
                    | Grayscale (Either Number Percentage)
                    | Invert (Either Number Percentage)
                    | Opacity (Either Number Percentage)
                    | Saturate (Either Number Percentage)
                    | Sepia (Either Number Percentage)
                    | HueRotate Angle
                    | DropShadow Length Length (Maybe Length) (Maybe Color)
  deriving (Show)
instance Eq FilterFunction where
  Blur a == Blur b                         = a == b
  Brightness a == Brightness b             = filterFunctionEquality a b
  Contrast a == Contrast b                 = filterFunctionEquality a b
  Grayscale a == Grayscale b               = filterFunctionEquality a b
  Invert a == Invert b                     = filterFunctionEquality a b
  Opacity a == Opacity b                   = filterFunctionEquality a b
  Saturate a == Saturate b                 = filterFunctionEquality a b
  Sepia a == Sepia b                       = filterFunctionEquality a b
  HueRotate a == HueRotate b               = a == b
  DropShadow a b c d == DropShadow e f g h =
      a == e && b == f && d == h && c `thirdValueEq` g
    where thirdValueEq Nothing (Just n) | isZeroLen n = True
          thirdValueEq (Just n) Nothing | isZeroLen n = True
          thirdValueEq x y = x == y
  _ == _                                   = False
instance ToText FilterFunction where
  toBuilder (Blur d)        = "blur("       <> toBuilder d  <> singleton ')'
  toBuilder (Brightness np) = "brightness(" <> toBuilder np <> singleton ')'
  toBuilder (HueRotate a)   = "hue-rotate(" <> toBuilder a  <> singleton ')'
  toBuilder (Contrast np)   = "contrast("   <> toBuilder np <> singleton ')'
  toBuilder (Grayscale np)  = "grayscale("  <> toBuilder np <> singleton ')'
  toBuilder (Invert np)     = "invert("     <> toBuilder np <> singleton ')'
  toBuilder (Opacity np)    = "opacity("    <> toBuilder np <> singleton ')'
  toBuilder (Saturate np)   = "saturate("   <> toBuilder np <> singleton ')'
  toBuilder (Sepia np)      = "sepia("      <> toBuilder np <> singleton ')'
  toBuilder (DropShadow l1 l2 ml mc) =
      let maybeToBuilder :: ToText a => Maybe a -> Builder
          maybeToBuilder = maybe mempty (\x -> singleton ' ' <> toBuilder x)
      in "drop-shadow(" <> toBuilder l1 <> singleton ' ' <> toBuilder l2
       <> maybeToBuilder ml <> maybeToBuilder mc <> singleton ')'
instance Minifiable FilterFunction where
  minify (Blur a)       = Blur <$> minify a
  minify (HueRotate a)  = HueRotate <$> minify a
  minify (Contrast x)   = Contrast <$> minifyNumberPercentage x
  minify (Brightness x) = Brightness <$> minifyNumberPercentage x
  minify (Grayscale x)  = Grayscale <$> minifyNumberPercentage x
  minify (Invert x)     = Invert <$> minifyNumberPercentage x
  minify (Opacity x)    = Opacity <$> minifyNumberPercentage x
  minify (Saturate x)   = Saturate <$> minifyNumberPercentage x
  minify (Sepia x)      = Sepia <$> minifyNumberPercentage x
  minify s@(DropShadow a b c d) = do
      conf <- ask
      if shouldMinifyFilterFunctions conf
         then minifyPseudoShadow DropShadow a b c d
         else pure s

minifyPseudoShadow :: (Minifiable b, Minifiable t1, Minifiable t2, Traversable t)
                   => (t2 -> t1 -> Maybe Length -> t b -> b1)
                   -> t2 -> t1 -> Maybe Length -> t b -> Reader Config b1
minifyPseudoShadow constr a b c d = do
              x  <- minify a
              y  <- minify b
              z  <- case c of
                      Just r -> if isZeroLen r
                                   then pure Nothing
                                   else traverse minify c
                      Nothing -> pure Nothing
              c2 <- traverse minify d
              pure $ constr x y z c2

minifyNumberPercentage :: Either Number Percentage
                       -> Reader Config (Either Number Percentage)
minifyNumberPercentage x = do
    conf <- ask
    pure $ if shouldMinifyFilterFunctions conf
              then either convertNumber convertPercentage x
              else x
  where convertNumber n
            | 0 < n && n < 0.1 = Right $ toPercentage (n * 100)
            | otherwise        = Left n
        convertPercentage p
            | p == 0          = Left 0
            | 0 < p && p < 10 = Right p
            | otherwise       = Left $ toNumber (p / 100)

filterFunctionEquality :: Either Number Percentage
                       -> Either Number Percentage -> Bool
filterFunctionEquality (Left a) (Left b)   = toRational a == toRational b
filterFunctionEquality (Right a) (Right b) = toRational a == toRational b
filterFunctionEquality (Left a) (Right b)  = toRational a == toRational b/100
filterFunctionEquality (Right a) (Left b)  = toRational a/100 == toRational b
