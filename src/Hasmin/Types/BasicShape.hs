{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts,
             StandaloneDeriving, DeriveFunctor, DeriveFoldable,
             DeriveTraversable #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.BasicShape
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Types.BasicShape
    ( BasicShape(..)
    , ShapeRadius(..)
    , AtMost2(..)
    , FillRule(..)
    ) where

import Control.Monad.Reader (Reader)
import Data.Monoid ((<>), mempty)
import Data.Bitraversable (bitraverse)
import Data.Maybe (isJust)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Text.Lazy.Builder (Builder)

import Hasmin.Types.Position
import Hasmin.Types.BorderRadius
import Hasmin.Types.Dimension
import Hasmin.Types.PercentageLength
import Hasmin.Types.Numeric
import Hasmin.Config
import Hasmin.Class
import Hasmin.Utils

type ShapeArg = PercentageLength

-- | CSS <https://drafts.csswg.org/css-shapes/#basic-shape-functions \<basic-shape\>> data type.
data BasicShape
       -- inset( <shape-arg>{1,4} [round <border-radius>]? )
        = Inset (NonEmpty ShapeArg) (Maybe BorderRadius)
       -- circle( [<shape-radius>]? [at <position>]? )
        | Circle (Maybe ShapeRadius) (Maybe Position)
       -- ellipse( [<shape-radius>{2}]? [at <position>]? )
        | Ellipse (AtMost2 ShapeRadius) (Maybe Position)
       -- polygon( [<fill-rule>,]? [<shape-arg> <shape-arg>]# )
        | Polygon (Maybe FillRule) (NonEmpty (ShapeArg, ShapeArg))
  deriving Show

instance Eq BasicShape where
    Inset sas1 mbr1 == Inset sas2 mbr2     = eqUsing sasEq sas1 sas2 && mbrEq mbr1 mbr2
    Circle msr1 mp1 == Circle msr2 mp2     = msrEq msr1 msr2 && mpEq mp1 mp2
    Ellipse sr2 mp1 == Ellipse sr2' mp2    = sr2Eq sr2 sr2' && mpEq mp1 mp2
    Polygon mfr1 sas1 == Polygon mfr2 sas2 = mfrEq mfr1 mfr2 && eqUsing pairEq sas1 sas2
    _ == _                                 = False

eqUsing :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty a -> Bool
eqUsing f (x:|xs) (y:|ys) = f x y && go xs ys
  where go [] []    = True
        go (_:_) [] = False
        go [] (_:_) = False
        go (c:cs) (d:ds) = f c d && go cs ds

pairEq :: (Num a, Eq a) => (Either a Length, Either a Length)
                        -> (Either a Length, Either a Length) -> Bool
pairEq (a1, a2) (b1, b2) = a1 `sasEq` b1 && a2 `sasEq` b2

sasEq :: (Num a, Eq a) => Either a Length -> Either a Length -> Bool
sasEq a b = isZero a && isZero b || a == b

mfrEq :: Maybe FillRule -> Maybe FillRule -> Bool
mfrEq Nothing (Just NonZero) = True
mfrEq (Just NonZero) Nothing = True
mfrEq x y                    = x == y

sr2Eq :: AtMost2 ShapeRadius -> AtMost2 ShapeRadius -> Bool
sr2Eq None x =
    case x of
      One SRClosestSide               -> True
      Two SRClosestSide SRClosestSide -> True
      None                            -> True
      _                               -> False
sr2Eq (One SRClosestSide) None               = True
sr2Eq (One x) (Two y SRClosestSide)          = x == y
sr2Eq (One x) (One y)                        = x == y
sr2Eq One{} _                                = False
sr2Eq (Two x SRClosestSide) (One y)          = x == y
sr2Eq (Two SRClosestSide SRClosestSide) None = True
sr2Eq (Two a b) (Two c d)                    = a == c && b == d
sr2Eq Two{} _                                = False

msrEq :: Maybe ShapeRadius -> Maybe ShapeRadius -> Bool
msrEq Nothing (Just SRClosestSide) = True
msrEq (Just SRClosestSide) Nothing = True
msrEq x y                          = x == y

mbrEq :: Maybe BorderRadius -> Maybe BorderRadius -> Bool
mbrEq Nothing y = maybe True isZeroBR y
mbrEq x Nothing = mbrEq Nothing x
mbrEq x y       = x == y

mpEq :: Maybe Position -> Maybe Position -> Bool
mpEq Nothing (Just x) = x == centerpos
mpEq (Just x) Nothing = x == centerpos
mpEq x y              = x == y

data ShapeRadius = SRLength Length
                 | SRPercentage Percentage
                 | SRClosestSide
                 | SRFarthestSide
  deriving (Show, Eq)

instance ToText ShapeRadius where
  toBuilder (SRLength l)     = toBuilder l
  toBuilder (SRPercentage p) = toBuilder p
  toBuilder SRClosestSide    = "closest-side"
  toBuilder SRFarthestSide   = "farthest-side"

minifySR :: ShapeRadius -> Reader Config ShapeRadius
minifySR (SRLength l) = SRLength <$> minify l
minifySR sr           = pure sr

data FillRule = NonZero | EvenOdd
  deriving (Show, Eq)

data AtMost2 a = None | One a | Two a a
  deriving (Functor, Foldable, Traversable)

deriving instance Show a => Show (AtMost2 a)
deriving instance Eq a => Eq (AtMost2 a)

instance ToText FillRule where
  toBuilder NonZero = "nonzero"
  toBuilder EvenOdd = "evenodd"

instance Minifiable BasicShape where
  minify (Inset xs Nothing) = pure $ Inset (reduceTRBL xs) Nothing
  minify (Inset xs (Just br)) = Inset (reduceTRBL xs) <$> br'
    where br' = do
              x <- minify br
              pure $ if isZeroBR x
                        then Nothing
                        else Just x
  minify (Circle msr mp) = do
      mp' <- traverse minify mp
      let newPos = if mp' == Just centerpos then Nothing else mp'
      Circle <$> minifyMSR msr <*> pure newPos
    where minifyMSR :: Maybe ShapeRadius -> Reader Config (Maybe ShapeRadius)
          minifyMSR Nothing   = pure Nothing
          minifyMSR (Just sr) =
              case sr of
                SRLength l    -> Just . SRLength <$> minify l
                SRClosestSide -> pure Nothing
                _             -> pure (Just sr)
  minify (Ellipse sr2 mp) = do
      sr' <- minifySR2 sr2
      mp' <- traverse minify mp
      let newPos = if mp' == Just centerpos
                      then Nothing
                      else mp'
      pure $ Ellipse sr' newPos
    where minifySR2 (One x) =
              case x of
                SRClosestSide -> pure None
                SRLength l    -> One . SRLength <$> minify l
                _             -> pure (One x)
          minifySR2 (Two x SRClosestSide) = minifySR2 (One x)
          minifySR2 t@Two{}               = traverse minifySR t
          minifySR2 None                  = pure None
  minify (Polygon mfr mp) =
      case mfr of
        Just NonZero -> Polygon Nothing <$> mp'
        _            -> Polygon mfr <$> mp'
    where mp' = traverse (bitraverse minifyPL minifyPL) mp

instance ToText BasicShape where
  toBuilder (Inset xs mys) = surround "inset" $ mconcatIntersperse toBuilder " " (NE.toList xs) <> mys'
    where mys' = maybe mempty (\x -> " round " <> toBuilder x) mys
  toBuilder (Circle msr mp) = surround "circle" $ msr' <> ms <> mp'
    where msr' = maybe mempty toBuilder msr
          mp'  = maybe mempty (\x -> "at " <> toBuilder x) mp
          ms   = if isJust msr && isJust mp then " " else mempty
  toBuilder (Ellipse m2sr mp) = surround "ellipse" $ bsr2 <> ms <> mp'
    where ms   = if bsr2 == mempty || mp' == mempty then mempty else " "
          mp'  = maybe mempty (\x -> "at " <> toBuilder x) mp
          bsr2 =
              case m2sr of
                One rx    -> toBuilder rx
                Two rx ry -> toBuilder rx <> " " <> toBuilder ry
                None      -> mempty
  toBuilder (Polygon mfr xys) = surround "polygon" $ f mfr xys
    where f Nothing xys'   = mconcatIntersperse g "," (NE.toList xys')
          f (Just fr) xys' = toBuilder fr <> "," <> mconcatIntersperse g "," (NE.toList xys')
          g (x, y) = toBuilder x <> " " <> toBuilder y

surround :: Builder -> Builder -> Builder
surround func x = func <> "(" <> x <> ")"
