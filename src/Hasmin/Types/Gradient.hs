{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.Gradient
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Hasmin.Types.Gradient
    ( Gradient(..)
    , Side(..)
    , ColorStop(..)
    , Size(..)
    , Shape(..)
    ) where

import Control.Monad.Reader (Reader, ask)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton)
import Data.Maybe (catMaybes, fromJust, isNothing, isJust)
import Data.Either (isLeft)
import Hasmin.Config
import Hasmin.Class
import Hasmin.Types.Color
import Hasmin.Types.Dimension
import Hasmin.Types.Numeric
import Hasmin.Types.PercentageLength
import Hasmin.Types.Position
import Hasmin.Utils

-- | CSS <side-or-corner> data type
data Side = LeftSide | RightSide | TopSide | BottomSide
  deriving (Show, Eq)
instance ToText Side where
  toBuilder LeftSide   = "left"
  toBuilder RightSide  = "right"
  toBuilder TopSide    = "top"
  toBuilder BottomSide = "bottom"

-- Possible pair of values, as expected by linear-gradient()
type SideOrCorner = (Side, Maybe Side)

-- | CSS <color-stop> data type
data ColorStop = ColorStop { csColor   :: Color
                           , colorHint :: Maybe PercentageLength
                           } deriving (Show, Eq)
instance ToText ColorStop where
  toBuilder (ColorStop c mpl) = toBuilder c <> maybe mempty f mpl
    where f (Left p)  = singleton ' ' <> toBuilder p
          f (Right l) = singleton ' ' <> toBuilder l
instance Minifiable ColorStop where
  minify (ColorStop c mlp) = do
    newC   <- minify c
    newMlp <- (mapM . mapM) minify mlp
    pure $ ColorStop newC newMlp

-- minifies color hints in a \<color-stops\> list
minifyColorHints :: [ColorStop] -> [ColorStop]
minifyColorHints [c1,c2] = [newC1, newC2]
  where ch1 = colorHint c1
        ch2 = colorHint c2
        newC1
            | isJust ch1 && isZero (fromJust ch1) = c1 {colorHint = Nothing}
            | otherwise = c1
        newC2
            | ch2 == Just (Left (Percentage 100)) = c2 {colorHint = Nothing}
            | otherwise = if ch2 `notGreaterThan` ch1
                             then c2 {colorHint = Just $ Right NullLength}
                             else c2
minifyColorHints (c@(ColorStop a x):xs) = case x of
                       Nothing -> c : analyzeList (Left $ Percentage 0) 1 (c:xs) xs
                       Just y  -> if isZero y
                                     then ColorStop a Nothing : analyzeList y 1 (c:xs) xs
                                     else c: analyzeList y 1 (c:xs) xs
minifyColorHints xs = error ("invalid <color-stop> list: " ++ show xs)

-- Returns True if the first value is equal or less than the second one, and False
-- otherwise, or if a comparison isn't possible.
notGreaterThan :: Maybe PercentageLength -> Maybe PercentageLength -> Bool
y `notGreaterThan` x
    | isNothing x || isZero (fromJust x) = notPositive y
    | otherwise = case fromJust x of
                    Left p  -> maybe False (either (<= p) (const False)) y
                    Right d -> maybe False (either (const False) (notGreaterThanLength d)) y
  where notPositive  = maybe False (either (<= 0) notPositiveLength)
  
        notPositiveLength (Length d _) = d <= 0
        notPositiveLength NullLength   = True

        notGreaterThanLength NullLength NullLength   = True
        notGreaterThanLength NullLength (Length r _) = r <= 0
        notGreaterThanLength (Length r _) NullLength = 0 <= r
        notGreaterThanLength (Length r1 u1) (Length r2 u2)
            | u1 == u2 = r2 <= r1
            | isRelative u1 || isRelative u2 = False
            | otherwise = toInches r2 u2 <= toInches r1 u1

-- Gathers at least three color stops to interpolate between the first and
-- last, and see if the middle one can be removed. As long as the color hints
-- are Nothing, keeps accumulating until it finds a Just it can use to interpolate.
analyzeList :: PercentageLength -> Int -> [ColorStop]
            -> [ColorStop] -> [ColorStop]
analyzeList start n list (ColorStop _ mpl:xs)
    | n < 2 = analyzeList start (n+1) list xs
    | otherwise =
        case mpl of
          Just y  -> let (newList, remainingList, startVal) = minifySegment start y n list
                     in newList ++ analyzeList startVal 2 remainingList xs
          Nothing -> analyzeList start (n+1) list xs
analyzeList start n list [] =
    case mpl of
      Just (Left (Percentage 100)) -> [ColorStop x Nothing]
      Nothing  -> let end = Left $ Percentage 100
                      (newList, _, _) = minifySegment start end (n-1) list
                  in newList ++ [(last list) {colorHint = Nothing}]
      _        -> [c]
  where c@(ColorStop x mpl) = last list

-- Given two values and a count of values, uses them to create a list of
-- interpolated values, and based on the list decides if it should remove a
-- color hint or not.
minifySegment :: PercentageLength -> PercentageLength -> Int -> [ColorStop]
              -> ([ColorStop], [ColorStop], PercentageLength)
minifySegment start end n list
    | all isPercentage segment = handlePercentages (fromLeft' start) (fromLeft' end) n remainingList
    -- add here support for dimension interpolation
    | otherwise = (take (n-1) remainingList, remainingList, fromJust $ colorHint (head remainingList))
  where segment = take (n+1) list
        (_, remainingList) = splitAt (n-1) list
        isPercentage x = maybe True isLeft (colorHint x)

-- Handles the minification of color hint values between percentages
handlePercentages :: Percentage -> Percentage -> Int
                  -> [ColorStop] -> ([ColorStop], [ColorStop], PercentageLength)
handlePercentages start end n remainingList =
    let newList = zipWith simplifyValue remainingList interpolation
    in (newList, remainingList, Left newStartVal)
  where newStartVal = maybe (last interpolation) fromLeft' (colorHint $ head remainingList)
        step = (end - start) / toPercentage n
        interpolation = [start + toPercentage x * step | x <- [1..n-1]]
        simplifyValue (ColorStop x mpl) y = ColorStop x $ mpl >>= \v ->
            if fromLeft' v == y
               then Nothing
               else if fromLeft' v <= start
                       then Just $ Right NullLength
                       else Just v

-- | CSS <https://drafts.csswg.org/css-images-3/#typedef-gradient \<gradient\>>
-- data type.
data Gradient = OldLinearGradient (Maybe (Either Angle SideOrCorner)) [ColorStop]
              -- OldLinearGradient is for the old syntax. It should eventually be deleted.
              | LinearGradient (Maybe (Either Angle SideOrCorner)) [ColorStop]
              | RadialGradient (Maybe Shape) (Maybe Size) (Maybe Position) [ColorStop]
                -- TODO: replace with Maybe (These Shape Size)
  deriving (Show)
              -- ,| RepeatingLinearGradient
              -- ,| RepeatingRadialGradient


{-
  radial-gradient() = radial-gradient(
    [ <ending-shape> || <size> ]? [ at <position> ]? ,
    <color-stop-list>
  )

  radial-gradient(
    [ [ circle || <length> ]                         [ at <position> ]? , |
      [ ellipse || [ <length> | <percentage> ]{2} ]  [ at <position> ]? , |
      [ [ circle | ellipse ] || <extent-keyword> ]   [ at <position> ]? , |
      at <position> ,
    ]?
    <color-stop> [ , <color-stop> ]+
  )
  where <extent-keyword> = closest-corner | closest-side | farthest-corner | farthest-side
    and <color-stop>     = <color> [ <percentage> | <length> ]?
-}

-- | CSS <https://drafts.csswg.org/css-images-3/#typedef-size \<size\>> data
-- type, used by @radial-gradient()@.
data Size = ClosestCorner | ClosestSide | FarthestCorner | FarthestSide
          | SL Length
          | PL PercentageLength PercentageLength
  deriving (Eq, Show)

instance ToText Size where
  toBuilder ClosestCorner  = "closest-corner"
  toBuilder ClosestSide    = "closest-side"
  toBuilder FarthestCorner = "farthest-corner"
  toBuilder FarthestSide   = "farthest-side"
  toBuilder (SL d)         = toBuilder d
  toBuilder (PL pl1 pl2)   = toBuilder pl1 <> singleton ' ' <> toBuilder pl2

-- TODO rename to EndingShape
-- | CSS <https://drafts.csswg.org/css-images-3/#valdef-radial-gradient-ending-shape \<ending-shape\>> data type, used by @radial-gradient()@.
data Shape = Circle | Ellipse
  deriving (Eq, Show)

instance ToText Shape where
  toBuilder Circle  = "circle"
  toBuilder Ellipse = "ellipse"

-- If the argument is to top, to right, to bottom, or to left, the angle of
-- the gradient line is 0deg, 90deg, 180deg, or 270deg, respectively.
instance Minifiable Gradient where
  minify g@(OldLinearGradient x cs) = do
      conf <- ask
      case gradientSettings conf of
        GradientMinOn  -> do css  <- mapM minify cs
                             pure $ OldLinearGradient x (minifyColorHints css)
        GradientMinOff -> pure g
  minify g@(LinearGradient x cs) = do
      conf <- ask
      case gradientSettings conf of
        GradientMinOn  -> do css  <- mapM minify cs
                             newX <- minifyAngleOrSide x
                             pure $ LinearGradient newX (minifyColorHints css)
        GradientMinOff -> pure g
  minify g@(RadialGradient sh sz p cs) = do
      conf <- ask
      case gradientSettings conf of
        GradientMinOn  -> do css  <- mapM minify cs
                             let np = minifyRadialPosition True {-shouldMinifyPosition conf-} p
                             pure $ minShapeAndSize sh sz np (minifyColorHints css)
        GradientMinOff -> pure g

-- If a single length was used, the default shape is circle, otherwise ellipse.
-- circle farthest-corner == circle
-- ellipse farthest-corner == ellipse == farthest-corner
minShapeAndSize :: Maybe Shape -> Maybe Size -> Maybe Position -> [ColorStop] -> Gradient
minShapeAndSize (Just Circle) sz@(Just (SL _))       = RadialGradient Nothing sz
minShapeAndSize (Just Circle) (Just FarthestCorner)  = RadialGradient (Just Circle) Nothing
minShapeAndSize (Just Ellipse) sz@(Just (PL _ _))    = RadialGradient Nothing sz
minShapeAndSize (Just Ellipse) (Just FarthestCorner) = RadialGradient Nothing Nothing
minShapeAndSize (Just Ellipse) sz@(Just _)           = RadialGradient Nothing sz
minShapeAndSize (Just Ellipse) Nothing               = RadialGradient Nothing Nothing
minShapeAndSize Nothing (Just FarthestCorner)        = RadialGradient Nothing Nothing
minShapeAndSize x sz                                 = RadialGradient x sz

-- Minifies the position in the radial gradient based on the position
-- minification settings. If positions should be minified, and if it is
-- equivalent to 'center', it is removed. If positions should not be minified,
-- it still removes it if it is equivalent to 'center', but leaves it untouched
-- otherwise.
minifyRadialPosition :: Bool -> Maybe Position -> Maybe Position
minifyRadialPosition _ Nothing = Nothing
minifyRadialPosition cond (Just p)
    | minifiedPos == centerPos = Nothing
    | cond                     = Just minifiedPos
    | otherwise                = Just p
  where centerPos = Position Nothing p50 Nothing Nothing
        minifiedPos = minifyPosition p

minifyAngleOrSide :: Maybe (Either Angle SideOrCorner)
                  -> Reader Config (Maybe (Either Angle SideOrCorner))
minifyAngleOrSide mas =
    case mas of
      Nothing -> pure Nothing
      Just y -> case y of
                  Left a  -> if a == defaultGradientAngle
                                then pure Nothing
                                else minify a >>= pure . Just . Left
                  Right b -> if b == defaultGradientSideOrCorner
                                then pure Nothing
                                else pure $ Just (minifySide b)
  where minifySide (TopSide, Nothing)    = Left NullAngle
        minifySide (RightSide, Nothing)  = Left (Angle 90 Deg)
        minifySide (BottomSide, Nothing) = Left (Angle 180 Deg)
        minifySide (LeftSide, Nothing)   = Left (Angle 270 Deg)
        minifySide z                     = Right z

        defaultGradientAngle = Angle 180 Deg
        defaultGradientSideOrCorner = (BottomSide, Nothing)

instance ToText Gradient where
  toBuilder (OldLinearGradient mas csl) = maybe mempty f mas
      <> mconcatIntersperse id (singleton ',') (fmap toBuilder csl)
    where f         = either ((<> singleton ',') . toBuilder) g
          g (s, ms) = toBuilder s
                   <> maybe mempty (\x -> singleton ' ' <> toBuilder x) ms
                   <> singleton ','
  toBuilder (LinearGradient mas csl) = maybe mempty f mas
      <> mconcatIntersperse id (singleton ',') (fmap toBuilder csl)
    where f         = either ((<> singleton ',') . toBuilder) g
          g (s, ms) = "to " <> toBuilder s
                   <> maybe mempty (\x -> singleton ' ' <> toBuilder x) ms
                   <> singleton ','
  toBuilder (RadialGradient sh sz p cs) = firstPart
      <> mconcatIntersperse id (singleton ',') (fmap toBuilder cs)
    where l = catMaybes [fmap toBuilder sh, fmap toBuilder sz, fmap (\x -> "at " <> toBuilder x) p]
          firstPart = if null l
                         then mempty
                         else mconcatIntersperse id (singleton ' ') l <> singleton ','

instance Eq Gradient where
  LinearGradient x1 csl1 == LinearGradient x2 csl2 =
      handleMaybe x1 x2 && csl1 == csl2
    where handleMaybe Nothing Nothing      = True
          handleMaybe (Just x) (Just y)    = handleEither x y
          handleMaybe _ _                  = False
          handleEither (Left a1) (Left a2) = a1 == a2
          handleEither (Left a) (Right s)  = angleSideEq a s
          handleEither (Right s) (Left a)  = angleSideEq a s
          handleEither s1 s2               = s1 == s2
  LinearGradient{} == _ = False
  _ == LinearGradient{} = False
  _ == _ = False -- TODO implement other comparisons

angleSideEq :: Angle -> SideOrCorner -> Bool
angleSideEq (Angle 90 Deg) (RightSide, Nothing)   = True
angleSideEq (Angle 180 Deg) (BottomSide, Nothing) = True
angleSideEq (Angle 270 Deg) (LeftSide, Nothing)   = True
angleSideEq a (TopSide, Nothing)
    | isZeroAngle a = True
    | otherwise     = False
angleSideEq _ _                                   = False
