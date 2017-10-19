{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.TimingFunction
-- Copyright   : © 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Types.TimingFunction
    ( TimingFunction(..)
    , StepsSecondParam(..)
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

-- | CSS <https://drafts.csswg.org/css-timing-1/#typedef-single-timing-function \<single-transition-timing-function\>> data type.
data TimingFunction = CubicBezier Number Number Number Number
                    | Steps Int (Maybe StepsSecondParam)
                    | Ease
                    | EaseIn
                    | EaseInOut
                    | EaseOut
                    | Linear
                    | StepEnd
                    | StepStart
  deriving (Show)

instance Eq TimingFunction where
  Steps i1 ms1 == Steps i2 ms2 = i1 == i2 && (ms1 == ms2
                                    || (isNothing ms1 && fromJust ms2 == End)
                                    || (fromJust ms1 == End && isNothing ms2))
  s@Steps{} == x         = toSteps x == Just s
    where toSteps :: TimingFunction -> Maybe TimingFunction
          toSteps StepEnd   = Just $ Steps 1 (Just End)
          toSteps StepStart = Just $ Steps 1 (Just Start)
          toSteps _         = Nothing
  x == s@Steps{} = s == x
  CubicBezier x1 x2 x3 x4 == CubicBezier y1 y2 y3 y4 =
      x1 == y1 && x2 == y2 && x3 == y3 && x4 == y4
  c@CubicBezier{} == x   = toCubicBezier x == Just c
    where toCubicBezier :: TimingFunction -> Maybe TimingFunction
          toCubicBezier Ease      = Just $ CubicBezier 0.25 0.1 0.25 1
          toCubicBezier EaseIn    = Just $ CubicBezier 0.42 0   1    1
          toCubicBezier EaseInOut = Just $ CubicBezier 0.42 0   0.58 1
          toCubicBezier Linear    = Just $ CubicBezier 0    0   1    1
          toCubicBezier EaseOut   = Just $ CubicBezier 0    0   0.58 1
          toCubicBezier _         = Nothing
  x == c@CubicBezier{}   = c == x
  StepEnd == StepEnd     = True
  StepStart == StepStart = True
  Ease == Ease           = True
  Linear == Linear       = True
  EaseIn == EaseIn       = True
  EaseOut == EaseOut     = True
  EaseInOut == EaseInOut = True
  _ == _                 = False

-- TODO rename to StepPosition
-- | A step position, as used by the
-- <https://drafts.csswg.org/css-timing-1/#step-timing-functions step timing function>.
data StepsSecondParam = Start
                      | End -- End is the default value
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
  minify tf = do
      conf <- ask
      pure $ if shouldMinifyTimingFunctions conf
                then minifyTimingFunction tf
                else tf
    where minifyTimingFunction :: TimingFunction -> TimingFunction
          minifyTimingFunction x@(CubicBezier a b c 1)
              | a == 0.25 && b == 0.1 && c == 0.25 = Ease
              | b == 0 = if a == 0.42
                            then if c == 1
                                    then EaseIn
                                    else if c == 0.58
                                            then EaseInOut
                                            else x
                            else if a == 0
                                    then if c == 1
                                            then Linear
                                            else if c == 0.58
                                                    then EaseOut
                                                    else x
                                    else x
          minifyTimingFunction x@CubicBezier{} = x
          minifyTimingFunction (Steps 1 ms) =
              case ms of
                Just Start -> StepStart
                _          -> StepEnd
          minifyTimingFunction (Steps i (Just End)) = Steps i Nothing
          minifyTimingFunction x = x
