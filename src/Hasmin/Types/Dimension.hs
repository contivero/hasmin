{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.Dimension
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- CSS Dimension data types: \<length\> (distance), \<angle\>, \<duration\>,
-- \<frequency\>, and \<resolution\>. Provides conversion of absolute
-- dimensions into other equivalent dimensions.
--
-----------------------------------------------------------------------------
module Hasmin.Types.Dimension (
      Distance(..)
    , DistanceUnit(..)
    , Angle(..)
    , AngleUnit(..)
    , Duration(..)
    , DurationUnit(..)
    , Frequency(..)
    , FrequencyUnit(..)
    , Resolution(..)
    , ResolutionUnit(..)
    , toInches
    , toPixels
    , toRadians
    , isRelative
    ) where

import Control.Monad.Reader (asks)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton, fromText)
import Data.Typeable (Typeable)
import Hasmin.Types.Class
import Hasmin.Types.Numeric
import Hasmin.Config
import Hasmin.Utils

-- | The \<length\> CSS data type
data Distance = Distance Number DistanceUnit
  deriving (Show)
instance Eq Distance where
  (Distance r1 u1) == (Distance r2 u2)
    | u1 == u2  = r1 == r2
    | otherwise = toInches r1 u1 == toInches r2 u2
instance Minifiable Distance where
  minifyWith d@(Distance r u) = do
      shouldMinifyUnits <- asks ((DimMinOn ==) . dimensionSettings)
      pure $ if (not . isRelative) u && shouldMinifyUnits
                then minDim Distance r u [Q, CM, MM, IN, PC, PT, PX]
                else d

isRelative :: DistanceUnit -> Bool
isRelative x = x == EM || x == EX || x == CH || x == VH
            || x == VW || x == VMIN || x == VMAX || x == REM

instance ToText Distance where
  toBuilder (Distance 0 _) = singleton '0'
  toBuilder (Distance r u) = (fromText . toText) r <> (fromText . toText) u

-- | The \<angle\> CSS data type
data Angle = Angle Number AngleUnit
  deriving (Show)
instance Eq Angle where
  (Angle r1 u1) == (Angle r2 u2)
    | u1 == u2  = r1 == r2
    | otherwise = toDegrees r1 u1 == toDegrees r2 u2
instance Minifiable Angle where
  minifyWith a = do
      shouldMinifyUnits <- asks ((DimMinOn ==) . dimensionSettings)
      pure $ if shouldMinifyUnits
                then minifyAngle a
                else a

minifyAngle :: Angle -> Angle
minifyAngle (Angle r u) = minDim Angle r u [Turn, Grad, Rad, Deg]

instance ToText Angle where
  toBuilder (Angle r u)
      | abs r < toNumber eps = singleton '0'
      | otherwise            = toBuilder r <> toBuilder u

-- | The \<duration\> CSS data type
data Duration = Duration Number DurationUnit
  deriving (Show)
instance Eq Duration where
  (Duration r1 u1) == (Duration r2 u2)
    | u1 == u2  = r1 == r2
    | otherwise = toSeconds r1 u1 == toSeconds r2 u2
instance Minifiable Duration where
  minifyWith d@(Duration r u) = do
      shouldMinifyUnits <- asks ((DimMinOn ==) . dimensionSettings)
      pure $ if shouldMinifyUnits
                then minDim Duration r u [S, Ms]
                else d
instance ToText Duration where
  toBuilder (Duration r u) = toBuilder r <> toBuilder u

-- | The \<frequency\> CSS data type
data Frequency = Frequency Number FrequencyUnit
  deriving (Show)
instance Eq Frequency where
  (Frequency r1 u1) == (Frequency r2 u2)
    | u1 == u2  = r1 == r2
    | otherwise = toHertz r1 u1 == toHertz r2 u2
instance Minifiable Frequency where
  minifyWith f@(Frequency r u) = do
      dimSettings <- asks dimensionSettings
      pure $ case dimSettings of
                DimMinOn  -> minDim Frequency r u [Khz, Hz]
                DimMinOff -> f
instance ToText Frequency where
  toBuilder (Frequency r u) = toBuilder r <> toBuilder u

-- | The \<resolution\> CSS data type
data Resolution = Resolution Number ResolutionUnit
  deriving (Show)
instance Eq Resolution where
  (Resolution r1 u1) == (Resolution r2 u2)
    | u1 == u2  = r1 == r2
    | otherwise = toDpi r1 u1 == toDpi r2 u2
instance Minifiable Resolution where
  minifyWith x@(Resolution r u) = do
      shouldMinifyUnits <- asks ((DimMinOn ==) . dimensionSettings)
      pure $ if shouldMinifyUnits
                then minDim Resolution r u [Dpcm, Dppx, Dpi]
                else x
instance ToText Resolution where
  toBuilder (Resolution r u) = toBuilder r <> toBuilder u

-- | Given a constructor, a number, and a unit, returns
-- the shortest equivalent representation. If there is more than one, returns
-- the latest found to "normalize" values, hopefully improving gzip compression.
minDim :: (Unit a, ToText a) => (Number -> a -> b) -> Number -> a -> [a] -> b
minDim constructor r u (x:xs)
    | currentLength < newLength = minDim constructor r u xs
    | otherwise                 = minDim constructor equivValue x xs
  where equivValue    = convertTo x r u
        currentLength = textualLength r + textualLength u
        newLength     = textualLength equivValue + textualLength x
minDim constructor r u [] = constructor r u

class Unit a where
  convertTo :: a -> Number -> a -> Number

data DistanceUnit = IN | CM | MM | Q | PC | PT | PX            -- absolute
                  | EM | EX | CH | VH | VW | VMIN | VMAX | REM -- relative
  deriving (Show, Eq)
instance ToText DistanceUnit where
  toBuilder IN   = "in"
  toBuilder CM   = "cm"
  toBuilder MM   = "mm"
  toBuilder Q    = "q"
  toBuilder PC   = "pc"
  toBuilder PT   = "pt"
  toBuilder PX   = "px"
  toBuilder EM   = "em"
  toBuilder EX   = "ex"
  toBuilder CH   = "ch"
  toBuilder VH   = "vh"
  toBuilder VW   = "vw"
  toBuilder VMIN = "vmin"
  toBuilder VMAX = "vmax"
  toBuilder REM  = "rem"
instance  Unit DistanceUnit where
  convertTo IN = toInches
  convertTo CM = toCentimeters
  convertTo MM = toMilimeters
  convertTo Q  = toQuarterMilimeter
  convertTo PT = toPoints
  convertTo PC = toPica
  convertTo PX = toPixels
  convertTo _  = const

data AngleUnit = Deg | Grad | Rad | Turn
  deriving (Show, Eq, Typeable)
instance ToText AngleUnit where
  toBuilder Deg  = "deg"
  toBuilder Grad = "grad"
  toBuilder Rad  = "rad"
  toBuilder Turn = "turn"
instance Unit AngleUnit where
  convertTo Deg  = toDegrees
  convertTo Grad = toGradians
  convertTo Rad  = toRadians
  convertTo Turn = toTurns

data DurationUnit = S -- seconds
                  | Ms -- miliseconds
  deriving (Show, Eq)
instance ToText DurationUnit where
  toBuilder S  = "s"
  toBuilder Ms = "ms"
instance Unit DurationUnit where
  convertTo S  = toSeconds
  convertTo Ms = toMiliseconds

data FrequencyUnit = Hz | Khz
  deriving (Show, Eq)
instance ToText FrequencyUnit where
  toBuilder Hz  = "hz"
  toBuilder Khz = "khz"
instance Unit FrequencyUnit where
  convertTo Hz  = toHertz
  convertTo Khz = toKilohertz

data ResolutionUnit = Dpi | Dpcm | Dppx
  deriving (Show, Eq)
instance ToText ResolutionUnit where
  toBuilder Dpi  = "dpi"
  toBuilder Dpcm = "dpcm"
  toBuilder Dppx = "dppx"
instance Unit ResolutionUnit where
  convertTo Dpi  = toDpi
  convertTo Dpcm = toDpcm
  convertTo Dppx = toDppx

toInches :: Number -> DistanceUnit -> Number
toInches d CM = d / 2.54
toInches d MM = d / 25.4
toInches d Q  = d / 101.6
toInches d PT = d / 72
toInches d PC = d / 6
toInches d PX = d / 96
toInches d _  = d -- IN, or any relative value

toCentimeters :: Number -> DistanceUnit -> Number
toCentimeters d IN = d * 2.54
toCentimeters d MM = d / 10
toCentimeters d Q  = d / 40
toCentimeters d PT = d * (2.54 / 72)
toCentimeters d PC = d * (2.54 / 6)
toCentimeters d PX = d * (2.54 / 96)
toCentimeters d _  = d -- CM, or any relative value

toMilimeters :: Number -> DistanceUnit -> Number
toMilimeters d IN = d * 25.4
toMilimeters d CM = d * 10
toMilimeters d Q  = d / 4
toMilimeters d PT = d * (25.4 / 72)
toMilimeters d PC = d * (25.4 / 6)
toMilimeters d PX = d * (25.4 / 96)
toMilimeters d _  = d

toQuarterMilimeter :: Number -> DistanceUnit -> Number
toQuarterMilimeter d IN = d * 101.6
toQuarterMilimeter d CM = d * 40
toQuarterMilimeter d MM = d * 4
toQuarterMilimeter d PT = d * (101.6 / 72)
toQuarterMilimeter d PC = d * (101.6 / 6)
toQuarterMilimeter d PX = d * (101.6 / 96)
toQuarterMilimeter d _  = d

toPoints :: Number -> DistanceUnit -> Number
toPoints d IN = d / 72
toPoints d CM = d * (72 / 2.54)
toPoints d MM = d * (72 / 25.4)
toPoints d Q  = d * (72 / 101.6)
toPoints d PC = d * 12
toPoints d PX = d * (3 / 4)
toPoints d _  = d

toPica :: Number -> DistanceUnit -> Number
toPica d IN = d / 6
toPica d CM = d * (6 / 2.54)
toPica d MM = d * (6 / 25.4)
toPica d Q  = d * (6 / 101.6)
toPica d PT = d / 12
toPica d PX = d / 16
toPica d _  = d

toPixels :: Number -> DistanceUnit -> Number
toPixels d IN = d * 96
toPixels d CM = d * (96 / 2.54)
toPixels d MM = d * (96 / 25.4)
toPixels d Q  = d * (96 / 101.6)
toPixels d PT = d * (4 / 3)
toPixels d PC = d / 6
toPixels d _  = d
------------------------------------------------------------------------------
rationalPi :: Number
rationalPi = Number $ toRational (pi :: Double)

toDegrees :: Number -> AngleUnit -> Number
toDegrees d Deg  = d
toDegrees d Grad = d * (9 / 10)
toDegrees d Rad  = d * (180 / rationalPi)
toDegrees d Turn = d * 360

toGradians :: Number -> AngleUnit -> Number
toGradians d Deg  = d * (10 / 9)
toGradians d Grad = d
toGradians d Rad  = d * (200 / rationalPi)
toGradians d Turn = d * 400

toRadians :: Number -> AngleUnit -> Number
toRadians d Deg  = d * (rationalPi / 180)
toRadians d Grad = d * (rationalPi / 200)
toRadians d Rad  = d
toRadians d Turn = d * 2 * rationalPi

toTurns :: Number -> AngleUnit -> Number
toTurns d Deg  = d / 360
toTurns d Grad = d / 400
toTurns d Rad  = d / (2 * rationalPi)
toTurns d Turn = d
------------------------------------------------------------------------------
toSeconds :: Number -> DurationUnit -> Number
toSeconds d S     = d
toSeconds d Ms = d / 1000

toMiliseconds :: Number -> DurationUnit -> Number
toMiliseconds d S     = d * 1000
toMiliseconds d Ms = d
------------------------------------------------------------------------------
toHertz :: Number -> FrequencyUnit -> Number
toHertz d Hz  = d
toHertz d Khz = d * 1000

toKilohertz :: Number -> FrequencyUnit -> Number
toKilohertz d Hz  = d / 1000
toKilohertz d Khz = d
------------------------------------------------------------------------------
toDpi :: Number -> ResolutionUnit -> Number
toDpi d Dpi  = d
toDpi d Dpcm = d * 2.54
toDpi d Dppx = d * 96

toDpcm :: Number -> ResolutionUnit -> Number
toDpcm d Dpi  = d / 2.54
toDpcm d Dpcm = d
toDpcm d Dppx = d * (2.54 / 96)

toDppx :: Number -> ResolutionUnit -> Number
toDppx d Dpi  = d / 96
toDppx d Dpcm = d * (96 / 2.54)
toDppx d Dppx = d
