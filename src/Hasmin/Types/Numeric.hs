{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.Numeric
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- CSS Numeric data types: \<number\>, \<percentage\>, and \<alphavalue\>.
-- All Rational newtypes to ensure dimension conversion precision.
--
-----------------------------------------------------------------------------
module Hasmin.Types.Numeric (
    Percentage(..), toPercentage,
    Number(..), toNumber, fromNumber,
    Alphavalue(..), toAlphavalue, mkAlphavalue
    ) where

import Data.Text (pack)
import Hasmin.Types.Class
import Hasmin.Utils
import Text.PrettyPrint.Mainland (strictText, text, Pretty, ppr)
import Text.Printf (printf)

-- | The \<number\> data type. Real numbers, possibly with a fractional component.
-- When written literally, a number is either an integer, or zero or more
-- decimal digits followed by a dot (.) followed by one or more decimal digits
-- and optionally an exponent composed of "e" or "E" and an integer. It
-- corresponds to the \<number-token\> production in the CSS Syntax Module
-- [CSS3SYN]. As with integers, the first character of a number may be
-- immediately preceded by - or + to indicate the number’s sign.
-- Specifications:
--
-- 1. <https://drafts.csswg.org/css-values-3/#numbers CSS Values and Units Module Level 3 (§4.2)>
-- 2. <https://www.w3.org/TR/CSS2/syndata.html#numbers CSS2.1 (§4.3.1)>
-- 3. <https://www.w3.org/TR/CSS1/#units CSS1 (6 Units)>
newtype Number = Number { getRational :: Rational }
  deriving (Eq, Show, Ord, Num, Fractional, Real, RealFrac)

instance ToText Number where
  toText = pack . trimLeadingZeros . showRat . toRational -- check if scientific notation is shorter!
instance Pretty Number where
  ppr = strictText . toText

toNumber :: Real a => a -> Number
toNumber = Number . toRational

fromNumber :: Fractional a => Number -> a
fromNumber = fromRational . toRational
-- | The \<alphavalue\> data type. Syntactically a \<number\>. It is the
-- uniform opacity setting to be applied across an entire object. Any values
-- outside the range 0.0 (fully transparent) to 1.0 (fully opaque) are clamped
-- to this range. Specification:
--
-- 1. <https://www.w3.org/TR/css3-color/#transparency CSS Color Module Level 3 (§3.2)
newtype Alphavalue = Alphavalue Rational
  deriving (Eq, Show, Ord, Real, RealFrac)

instance Num Alphavalue where
  a + b = mkAlphavalue $ toRational a + toRational b
  a * b = mkAlphavalue $ toRational a * toRational b
  abs = id -- numbers are never negative, so abs doesn't do anything
  signum a | toRational a == 0 = 0
           | otherwise         = 1
  fromInteger = toAlphavalue
  a - b = mkAlphavalue $ toRational a - toRational b

instance ToText Alphavalue where
  toText = pack .  trimLeadingZeros . showRat . toRational
instance Pretty Alphavalue where
  ppr = text . showRat . toRational
instance Bounded Alphavalue where
  minBound = 0
  maxBound = 1
instance Fractional Alphavalue where
  fromRational = mkAlphavalue
  (Alphavalue a) / (Alphavalue b) = mkAlphavalue (toRational a / toRational b)
toAlphavalue :: Real a => a -> Alphavalue
toAlphavalue = mkAlphavalue . toRational

mkAlphavalue :: Rational -> Alphavalue
mkAlphavalue = Alphavalue . restrict 0 1

-- | The \<percentage\> data type. Many CSS properties can take percentage
-- values, often to define sizes in terms of parent objects. Percentages are
-- formed by a \<number\> immediately followed by the percentage sign %.
-- There is no space between the '%' and the number. Specification:
--
-- 1. <https://drafts.csswg.org/css-values-3/#percentages CSS Value and Units Module Level 3 (§4.3)
-- 2. <https://www.w3.org/TR/CSS2/syndata.html#percentage-units CSS2.1 (§4.3.3)
-- 3. <https://www.w3.org/TR/CSS1/#percentage-units CSS1 (§6.2)
newtype Percentage = Percentage Rational
  deriving (Eq, Show, Ord, Num, Fractional, Real, RealFrac)

instance Pretty Percentage where
  ppr = strictText . toText
instance ToText Percentage where
  toText = pack . (++ "%") . trimLeadingZeros . showRat . toRational

toPercentage :: Real a => a -> Percentage
toPercentage = Percentage . toRational

-- Note: printf used instead of show to avoid scientific notation
-- | Show a Rational in decimal notation, removing leading zeros,
-- and not displaying fractional part if the number is an integer.
showRat :: Rational -> String
showRat r | abs (r - fromInteger x) < eps = printf "%d" x
          | otherwise                     = printf "%f" d
  where x = round r
        d = fromRational r :: Double

trimLeadingZeros :: String -> String
trimLeadingZeros l@(x:xs) | x == '-'  = x : go xs
                          | otherwise = go l
  where go ('0':y:ys) = go (y:ys)
        go z = z
trimLeadingZeros []  = ""
