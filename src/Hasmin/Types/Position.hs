{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.Position
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Hasmin.Types.Position (
      Position(..)
    , PosKeyword(..)
    , minifyPosition
    , p50
    , l0
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (singleton, fromText)
import Data.Maybe (isJust)
import Hasmin.Class
import Hasmin.Types.Dimension
import Hasmin.Types.PercentageLength
import Hasmin.Utils

data PosKeyword = PosCenter
                | PosLeft
                | PosRight
                | PosTop
                | PosBottom
  deriving (Show, Eq, Enum, Bounded)
instance ToText PosKeyword where
  toBuilder PosCenter = "center"
  toBuilder PosTop    = "top"
  toBuilder PosRight  = "right"
  toBuilder PosBottom = "bottom"
  toBuilder PosLeft   = "left"

-- TODO turn this into a proper algebraic type with all the cases, avoding Maybe
-- and making invalid Position values impossible.
data Position = Position { origin1 :: Maybe PosKeyword
                         , offset1 :: Maybe PercentageLength
                         , origin2 :: Maybe PosKeyword
                         , offset2 :: Maybe PercentageLength
                         } deriving (Show)
instance Minifiable Position where
  minify p = pure $ minifyPosition p

instance ToText Position where
  toBuilder (Position a b c d) = mconcatIntersperse fromText (singleton ' ') $ filter (not . T.null) [f a, f b, f c, f d]
    where f :: ToText a => Maybe a -> Text
          f = maybe mempty toText

minifyPosition :: Position -> Position
-- Single keyword
minifyPosition p@(Position (Just x) Nothing Nothing Nothing) = f x
  where mkPos1 y    = Position Nothing y Nothing Nothing
        f PosCenter = mkPos1 p50
        f PosRight  = mkPos1 p100
        f PosLeft   = mkPos1 l0
        f _         = p
-- Keyword and <lenght-percentage>
minifyPosition p@(Position (Just x) Nothing Nothing (Just y)) =
    minifyPos2 x y
  where mkPos2 i j = if isZero j
                        then Position Nothing i Nothing l0
                        else Position Nothing i Nothing (Just j)
        minifyPos2 PosLeft a   = mkPos2 l0 a
        minifyPos2 PosRight a  = mkPos2 p100 a
        minifyPos2 PosCenter a = mkPos2 p50 a
        minifyPos2 _ _         = p
minifyPosition p@(Position Nothing (Just x) (Just y) Nothing) =
    minifyPos2 x y
  where mkPos2 i j = if isZero i
                        then Position Nothing l0 Nothing j
                        else Position Nothing (Just i) Nothing j
        minifyPos2 a PosTop    = mkPos2 a l0
        minifyPos2 a PosBottom = mkPos2 a p100
        minifyPos2 a PosCenter = mkPos2 a p50
        minifyPos2 _ _         = p
-- Two <lenght-percentage>
minifyPosition p@(Position Nothing (Just x) Nothing (Just y)) = f x y
  where f :: PercentageLength -> PercentageLength -> Position
        f (Left 50) (Left 50)   = Position Nothing p50 Nothing Nothing
        f (Left 50) (Left 100)  = Position (Just PosBottom) Nothing Nothing Nothing
        f (Left 100) (Left 100) = Position Nothing p100 Nothing p100
        f (Left 100) (Left 50)  = Position Nothing p100 Nothing Nothing
        f a b
          | isZero a = if isZero b
                          then Position Nothing l0 Nothing l0
                          else case b of
                                 Left 50  -> Position Nothing l0 Nothing Nothing
                                 Left 100 -> Position Nothing l0 Nothing p100
                                 _        -> p { offset1 = l0 }
          | isZero b = case a of
                         Left 50  -> Position (Just PosTop) Nothing Nothing Nothing
                         Left 100 -> Position Nothing p100 Nothing l0
                         _        -> p { offset2 = l0 }
          | b == Left 50 = Position Nothing (Just a) Nothing Nothing
          | otherwise    = p
minifyPosition (Position (Just x) (Just y) Nothing Nothing) =
  uncurry (\a b -> Position a b Nothing Nothing) (minAxis x y)
-- 2 keywords
minifyPosition p@(Position (Just x) Nothing (Just y) Nothing) = f x y
  where f :: PosKeyword -> PosKeyword -> Position
        -- 'center', 'center center' == '50% 50%'.
        f PosCenter PosCenter =  Position Nothing p50 Nothing Nothing
        -- 'left', 'left center', 'center left' == '0% 50%'.
        f PosLeft PosCenter   = Position Nothing l0 Nothing Nothing
        f PosCenter PosLeft   = Position Nothing l0 Nothing Nothing
        -- 'top left', 'left top' == '0% 0%'.
        f PosTop PosLeft      = Position Nothing l0 Nothing l0
        f PosLeft PosTop      = Position Nothing l0 Nothing l0
        -- 'top', 'top center', 'center top' == '50% 0%'.
        f PosTop PosCenter    = p { origin2 = Nothing }
        f PosCenter PosTop    = p { origin1 = Just PosTop, origin2 = Nothing }
        -- 'right top' and 'top right' == '100% 0%'.
        f PosRight PosTop     = Position Nothing p100 Nothing l0
        f PosTop PosRight     = Position Nothing p100 Nothing l0
        -- 'right', 'right center', 'center right' == '100% 50%'.
        f PosRight PosCenter  = Position Nothing p100 Nothing Nothing
        f PosCenter PosRight  = Position Nothing p100 Nothing Nothing
        -- 'bottom left' and 'left bottom' == '0% 100%'.
        f PosBottom PosLeft   = Position Nothing l0 Nothing p100
        f PosLeft PosBottom   = Position Nothing l0 Nothing p100
        -- 'bottom', 'bottom center', 'center bottom' == '50% 100%'.
        f PosBottom PosCenter = p { origin2 = Nothing }
        f PosCenter PosBottom = p { origin1 = Just PosBottom, origin2 = Nothing }
        -- 'bottom right', 'right bottom'             == '100% 100%'.
        f PosBottom PosRight = Position Nothing p100 Nothing p100
        f PosRight PosBottom = Position Nothing p100 Nothing p100
        f _ _ = p
-- keyword pl keyword syntax
minifyPosition p@(Position (Just x) (Just y) (Just z) Nothing)
    | x == PosTop || x == PosBottom || z == PosLeft || z == PosRight =
        minifyPosition $ Position (Just z) Nothing (Just x) (Just y)
    | otherwise = minifyPos3 x y z
  where minifyPos3 PosLeft b PosBottom
          | isZero b     = Position Nothing l0 Nothing p100
          | b == Left 50 = Position (Just PosBottom) Nothing Nothing Nothing
          | otherwise    = Position Nothing (Just b) Nothing p100
        minifyPos3 PosLeft b PosTop
          | isZero b     = Position Nothing l0 Nothing l0
          | b == Left 50 = Position (Just PosTop) Nothing Nothing Nothing
          | otherwise    = Position Nothing (Just b) Nothing l0
        minifyPos3 PosLeft b PosCenter
          | isZero b     = Position Nothing l0 Nothing Nothing
          | b == Left 50 = Position Nothing p50 Nothing Nothing
          | otherwise    = Position Nothing (Just b) Nothing p50
        minifyPos3 PosRight b PosTop
          | isZero b     = Position Nothing p100 Nothing l0
          | otherwise    = Position (Just PosRight) (Just b) Nothing l0
        minifyPos3 PosRight b PosBottom
          | isZero b     = Position Nothing p100 Nothing p100
          | otherwise    = Position (Just PosRight) (Just b) Nothing p100
        minifyPos3 PosRight b PosCenter
          | isZero b     = Position Nothing p100 Nothing Nothing
          | otherwise    = Position (Just PosRight) (Just b) Nothing p50
        minifyPos3 _ _ _ = p
minifyPosition p@(Position (Just c) Nothing (Just a) (Just b)) = f $ minAxis a b
  where f (x, y)
          | c == PosLeft && x == Just PosTop && isJust y = minifyPosition $ Position Nothing l0 Nothing y
          | otherwise = if Just a == x && Just b == y
                           then p
                           else minifyPosition $ p {origin2 = x, offset2 = y }
-- 4 value syntax
minifyPosition p@(Position (Just PosLeft) (Just _) (Just PosTop) (Just _)) =
    minifyPosition p { origin1 = Nothing, origin2 = Nothing }
minifyPosition (Position (Just a) (Just b) (Just c) (Just d)) =
    minifyPos4 a b c d
minifyPosition p = p

-- Sort values so that the Xs are first, then the Ys, and later minify.
minifyPos4 :: PosKeyword -> PercentageLength -> PosKeyword -> PercentageLength -> Position
minifyPos4 v1 v2 v3 v4
    | v1 == PosTop || v1 == PosBottom || v3 == PosLeft || v3 == PosRight = minifyPos4' v3 v4 v1 v2
    | otherwise = minifyPos4' v1 v2 v3 v4
  where minifyPos4' PosLeft a PosTop b  = minifyPosition $ Position Nothing (Just a) Nothing (Just b)
        minifyPos4' PosRight a PosTop b
            | isZero a  = minifyPosition $ Position Nothing p100 Nothing (Just b)
            | isZero b  = minifyPosition $ Position (Just PosRight) (Just a) (Just PosTop) Nothing
            | otherwise = Position (Just PosRight) (Just a) (Just PosTop) (Just b)
        minifyPos4' PosLeft a PosBottom b
            | isZero b  = minifyPosition $ Position Nothing (Just a) Nothing p100
            | isZero a  = minifyPosition $ Position (Just PosLeft) Nothing (Just PosTop) (Just b)
            | otherwise = minifyPosition $ Position (Just PosLeft) (Just a) (Just PosBottom) (Just b)
        minifyPos4' PosRight a PosBottom b
            | isZero a && isZero b = Position Nothing p100 Nothing p100
            | otherwise            = Position (Just PosRight) (Just a) (Just PosBottom) (Just b)
        minifyPos4' a b c d = Position (Just a) (Just b) (Just c) (Just d)

minAxis :: PosKeyword -> PercentageLength -> (Maybe PosKeyword, Maybe PercentageLength)
minAxis PosTop x =
    case x of
      Left 50 -> (Just PosCenter, Nothing)
      b       -> if isZero b
                    then (Just PosTop, Nothing)
                    else (Just PosTop, Just x)
minAxis PosLeft x =
    case x of
      Left 50 -> (Just PosCenter, Nothing)
      smth    -> if isZero smth
                    then (Just PosLeft, Nothing)
                    else (Just PosLeft, Just x)
minAxis PosRight x
    | isZero x  = (Just PosRight, Nothing)
    | otherwise = (Just PosRight, Just x)
minAxis PosBottom x
    | isZero x  = (Just PosBottom, Nothing)
    | otherwise = (Just PosBottom, Just x)
minAxis PosCenter x = (Just PosCenter, Just x)

instance Eq Position where
  x == y = let (Position a b c d) = minifyPosition x
               (Position e f g h) = minifyPosition y
           in a == e && b == f && c == g && d == h

l0 :: Maybe PercentageLength
l0 = Just $ Right NullLength

p100 :: Maybe PercentageLength
p100 = Just $ Left 100

p50 :: Maybe PercentageLength
p50 = Just $ Left 50
