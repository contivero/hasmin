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

-- Note: In a previous specification, translate3d() took two
-- <https://www.w3.org/TR/css-transforms-1/#typedef-translation-value \<translation-value\>>,
-- however in the <https://drafts.csswg.org/css-transforms/ latest draft>, it
-- takes two \<length-percentage\> (which makes sense since translateX() and
-- translateY() also do).

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
  deriving (Eq, Show)

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

convertNumber :: Number -> Either Number Percentage
convertNumber x
    | 0 < x && x < 0.1 = Right $ toPercentage (x * 100)
    | otherwise        = Left x

convertPercentage :: Percentage -> Either Number Percentage
convertPercentage p
    | p == 0            = Left 0
    | 0 < p && p < 10   = Right p
    | otherwise         = Left $ toNumber (p / 100)
