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
module Hasmin.Types.BasicShape where

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
  deriving (Show, Eq)

type ShapeArg = PercentageLength

data ShapeRadius
        = SRLength Length
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
          isZeroBR (BorderRadius (a:|[]) []) = isZero a
          isZeroBR _                         = False
  minify (Circle msr mp) = do
      mp' <- traverse minify mp
      let newPos = if mp' == Just centerpos then Nothing else mp'
      Circle <$> minifyMSR msr <*> pure newPos
    where minifyMSR :: Maybe ShapeRadius -> Reader Config (Maybe ShapeRadius)
          minifyMSR Nothing   = pure Nothing
          minifyMSR (Just sr) =
              case sr of
                SRLength l    -> (Just . SRLength) <$> minify l
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
                SRLength l    -> (One . SRLength) <$> minify l
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
  toBuilder (Ellipse m2sr mp) = surround "ellipse" $ f m2sr mp
    where f :: AtMost2 ShapeRadius -> Maybe Position -> Builder
          f sr2 mp' = bsr2 <> maybe mempty (\x -> " at " <> toBuilder x) mp'
            where bsr2 =
                      case sr2 of
                        One rx    -> toBuilder rx
                        Two rx ry -> toBuilder rx <> " " <> toBuilder ry
                        None      -> mempty
  toBuilder (Polygon mfr xys) = surround "polygon" $ f mfr xys
    where f Nothing xys'   = mconcatIntersperse g "," (NE.toList xys')
          f (Just fr) xys' = toBuilder fr <> "," <> mconcatIntersperse g "," (NE.toList xys')
          g (x, y) = toBuilder x <> " " <> toBuilder y

surround :: Builder -> Builder -> Builder
surround func x = func <> "(" <> x <> ")"
