{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.TimingFunction
-- Copyright   : (c) 2016 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Hasmin.Types.TimingFunction (
    TimingFunction(..), StepsSecondParam(..)
    ) where

import Control.Monad.Reader (ask)
import Data.Monoid ((<>))
import Prelude hiding (sin, cos, acos, tan, atan)
import Data.Maybe (isNothing, fromJust)
import Data.Text.Lazy.Builder (singleton)
import Hasmin.Config
import Hasmin.Types.Class
import Hasmin.Utils
import Hasmin.Types.Numeric
--import Text.PrettyPrint.Mainland (Pretty, ppr, strictText)

-- | CSS \<single-transition-timing-function\> data type. Specifications:
--
-- 1. <https://drafts.csswg.org/css-transitions/#propdef-transition-timing-function CSS Transitions>
-- 2. <https://drafts.csswg.org/css-animations/#animation-timing-function CSS Animations Level 1>
-- 3. <https://developer.mozilla.org/en-US/docs/Web/CSS/timing-function Mozilla summary>
data TimingFunction = CubicBezier Number Number Number Number
                    | Steps Int (Maybe StepsSecondParam)
                    | Ease | EaseIn | EaseInOut | EaseOut
                    | Linear | StepEnd | StepStart
  deriving (Show)

instance Eq TimingFunction where
  Steps i1 ms1 == Steps i2 ms2 = i1 == i2 && (ms1 == ms2
                                    || (isNothing ms1 && fromJust ms2 == End)
                                    || (fromJust ms1 == End && isNothing ms2))
  s@Steps{} == x         = maybe False (== s) (toSteps x)
  x == s@Steps{}         = s == x
  CubicBezier x1 x2 x3 x4 == CubicBezier y1 y2 y3 y4 =
      x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4
  c@CubicBezier{} == x   = maybe False (== c) (toCubicBezier x)
  x == c@CubicBezier{}   = c == x
  StepEnd == StepEnd     = True
  StepStart == StepStart = True
  Ease == Ease           = True
  Linear == Linear       = True
  EaseIn == EaseIn       = True
  EaseOut == EaseOut     = True
  EaseInOut == EaseInOut = True
  _ == _                 = False

toCubicBezier :: TimingFunction -> Maybe TimingFunction
toCubicBezier Ease      = Just $ CubicBezier 0.25 0.1 0.25 1
toCubicBezier EaseIn    = Just $ CubicBezier 0.42 0   1    1
toCubicBezier EaseInOut = Just $ CubicBezier 0.42 0   0.58 1
toCubicBezier Linear    = Just $ CubicBezier 0    0   1    1
toCubicBezier EaseOut   = Just $ CubicBezier 0    0   0.58 1
toCubicBezier _         = Nothing

toSteps :: TimingFunction -> Maybe TimingFunction
toSteps StepEnd   = Just $ Steps 1 (Just End)
toSteps StepStart = Just $ Steps 1 (Just Start)
toSteps _         = Nothing


data StepsSecondParam = Start | End -- End is the default value
  deriving (Eq, Show)
instance ToText StepsSecondParam where
  toBuilder Start = "start"
  toBuilder End   = "end"

instance ToText TimingFunction where
  toBuilder (CubicBezier a b c d) = "cubic-bezier("
      <> mconcatIntersperse toBuilder (singleton ',') [a,b,c,d]
      <> singleton ')'
  toBuilder (Steps i ms) = "steps(" <> toBuilder i <> sp <> singleton ')'
    where sp = maybe mempty (\x -> singleton ',' <> toBuilder x) ms
  toBuilder Ease      = "ease"
  toBuilder Linear    = "linear"
  toBuilder EaseIn    = "ease-in"
  toBuilder EaseOut   = "ease-out"
  toBuilder EaseInOut = "ease-in-out"
  toBuilder StepStart = "step-start"
  toBuilder StepEnd   = "step-end"

instance Minifiable TimingFunction where
  minifyWith x = do
      conf <- ask
      if shouldMinifyTimingFunctions conf
         then pure $ minifyTimingFunction x
         else pure x

minifyTimingFunction :: TimingFunction -> TimingFunction
minifyTimingFunction x@(CubicBezier a b c d)
    | a == 0.25 && b == 0.1 && c == 0.25 && d == 1 = Ease
    | a == 0.42 && b == 0 && d == 1 = if c == 1
                                         then EaseIn
                                         else if c == 0.58
                                                 then EaseInOut
                                                 else x
    | a == 0 && b == 0 && d == 1 = if c == 1
                                      then Linear
                                      else if c == 0.58
                                              then EaseOut
                                              else x
    | otherwise = x
minifyTimingFunction x@(Steps i ms)
    | (isNothing ms || (fromJust ms == End)) && i == 1 = StepEnd
    | i == 1 && (fromJust ms == Start) = StepStart
    | otherwise = x
minifyTimingFunction x = x
