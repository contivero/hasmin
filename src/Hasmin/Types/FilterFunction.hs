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
module Hasmin.Types.FilterFunction (
      FilterFunction(..)
    , minifyPseudoShadow
    ) where

import Control.Monad.Reader (Reader, ask)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton, Builder)
import Hasmin.Config
import Hasmin.Types.Class
import Hasmin.Types.Dimension
import Hasmin.Types.Color
import Hasmin.Types.Numeric

-- | CSS <https://drafts.fxtf.org/filter-effects/#typedef-filter-function \<filter-function\>>
-- data type.
data FilterFunction = Blur Distance
                    | Brightness (Either Number Percentage)
                    | Contrast (Either Number Percentage)
                    | Grayscale (Either Number Percentage)
                    | Invert (Either Number Percentage)
                    | Opacity (Either Number Percentage)
                    | Saturate (Either Number Percentage)
                    | Sepia (Either Number Percentage)
                    | HueRotate Angle
                    | DropShadow Distance Distance (Maybe Distance) (Maybe Color)
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
    where thirdValueEq Nothing (Just (Distance 0 _)) = True
          thirdValueEq (Just (Distance 0 _)) Nothing = True
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
  minifyWith (Blur a)       = Blur <$> minifyWith a
  minifyWith (HueRotate a)  = HueRotate <$> minifyWith a
  minifyWith (Contrast x)   = Contrast <$> minifyNumberPercentage x
  minifyWith (Brightness x) = Brightness <$> minifyNumberPercentage x
  minifyWith (Grayscale x)  = Grayscale <$> minifyNumberPercentage x
  minifyWith (Invert x)     = Invert <$> minifyNumberPercentage x
  minifyWith (Opacity x)    = Opacity <$> minifyNumberPercentage x
  minifyWith (Saturate x)   = Saturate <$> minifyNumberPercentage x
  minifyWith (Sepia x)      = Sepia <$> minifyNumberPercentage x
  minifyWith s@(DropShadow a b c d) = do
      conf <- ask
      if shouldMinifyFilterFunctions conf
         then minifyPseudoShadow DropShadow a b c d
         else pure s

minifyPseudoShadow :: (Minifiable b, Minifiable t1, Minifiable t2, Traversable t)
                   => (t2 -> t1 -> Maybe Distance -> t b -> b1)
                   -> t2 -> t1 -> Maybe Distance -> t b -> Reader Config b1
minifyPseudoShadow constr a b c d = do
              x  <- minifyWith a
              y  <- minifyWith b
              z  <- case c of
                      Just r -> if r == Distance 0 Q
                                   then pure Nothing
                                   else mapM minifyWith c
                      Nothing -> pure Nothing
              c2 <- mapM minifyWith d
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

