{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (isJust)
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
        | Ellipse (Maybe (ShapeRadius, ShapeRadius)) (Maybe Position)
       -- polygon( [<fill-rule>,]? [<shape-arg> <shape-arg>]# )
        | Polygon (Maybe FillRule) [(ShapeArg, ShapeArg)]
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

minifyMSR :: Maybe ShapeRadius -> Reader Config (Maybe ShapeRadius)
minifyMSR Nothing   = pure Nothing
minifyMSR (Just sr) =
    case sr of
      SRLength l    -> (Just . SRLength) <$> minify l
      SRClosestSide -> pure Nothing
      _             -> pure (Just sr)

data FillRule = NonZero | EvenOdd
  deriving (Show, Eq)

instance ToText FillRule where
  toBuilder NonZero = "nonzero"
  toBuilder EvenOdd = "evenodd"

instance Minifiable BasicShape where
  minify (Inset xs mys)  = Inset (reduceTRBL xs) <$> traverse minify mys -- TODO see what's the default value of border-radius here.
  minify (Circle msr mp) = do
    mp' <- traverse minify mp
    let newPos = if mp' == Just centerpos then Nothing else mp'
    Circle <$> minifyMSR msr <*> pure newPos
  minify (Ellipse m2sr mp) = do
    mp' <- traverse minify mp
    let newPos = if mp' == Just centerpos then Nothing else mp'
    Ellipse <$> pure m2sr <*> pure newPos -- TODO minify m2sr
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
          mp'  = maybe mempty toBuilder mp
          ms   = if isJust msr && isJust mp then " " else mempty
  toBuilder (Ellipse m2sr mp) = surround "ellipse" $ f m2sr mp
    where f :: Maybe (ShapeRadius, ShapeRadius) -> Maybe Position -> Builder
          f Nothing mp'         = maybe mempty toBuilder mp'
          f (Just (rx, ry)) mp' = toBuilder rx <> " " <> toBuilder ry <> maybe mempty (\x -> " " <> toBuilder x) mp'
  toBuilder (Polygon mfr xys) = surround "polygon" $ f mfr xys
    where f Nothing xys'   = mconcatIntersperse g "," xys'
          f (Just fr) xys' = toBuilder fr <> " " <> mconcatIntersperse g "," xys' -- FIXME unnecessary space might pop up in the middle
          g (x, y) = toBuilder x <> " " <> toBuilder y

surround :: Builder -> Builder -> Builder
surround func x = func <> "(" <> x <> ")"
