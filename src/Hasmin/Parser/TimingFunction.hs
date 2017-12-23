{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.TimingFunction
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Parsers for \<single-timing-function> values.
--
-----------------------------------------------------------------------------
module Hasmin.Parser.TimingFunction
    ( timingFunction
    , cubicbezier
    , steps
    ) where

import Control.Applicative (optional)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A

import Hasmin.Parser.Primitives
import Hasmin.Parser.Numeric
import Hasmin.Parser.Utils
import Hasmin.Types.TimingFunction

-- | Parser for <https://drafts.csswg.org/css-timing-1/#single-timing-function-production \<single-timing-function\>>.
timingFunction :: Parser TimingFunction
timingFunction = parserFromPairs [("ease",         pure Ease)
                                 ,("ease-in",      pure EaseIn)
                                 ,("ease-in-out",  pure EaseInOut)
                                 ,("ease-out",     pure EaseOut)
                                 ,("linear",       pure Linear)
                                 ,("step-end",     pure StepEnd)
                                 ,("step-start",   pure StepStart)
                                 ,("steps",        A.char '(' *> steps)
                                 ,("cubic-bezier", A.char '(' *> cubicbezier)]

-- | Parses what's between parenthesis in the "cubic-bezier()" function.
cubicbezier :: Parser TimingFunction
cubicbezier = functionParser $
    CubicBezier <$> number <* comma <*> number <* comma
                <*> number <* comma <*> number

-- | Parses what's between parenthesis in the "steps()" function.
steps :: Parser TimingFunction
steps = functionParser $ Steps <$> int <*> optional startOrEnd
  where startOrEnd = comma *> parserFromPairs [("end",   pure End)
                                              ,("start", pure Start)]
