{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.BgSize
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Hasmin.Types.BgSize
    ( BgSize(..)
    , Auto(..)
    ) where

import Control.Monad.Reader (Reader)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton)

import Hasmin.Class
import Hasmin.Config
import Hasmin.Types.PercentageLength

-- | The CSS @auto@ keyword.
data Auto = Auto
  deriving (Eq, Show)

instance ToText Auto where
  toBuilder Auto = "auto"

-- | CSS <https://drafts.csswg.org/css-backgrounds-3/#typedef-bg-size \<bg-size\>>
-- data type, used by the @background-size@ and @background@ properties.
data BgSize = Cover
            | Contain
            | BgSize1 (Either PercentageLength Auto)
            | BgSize2 (Either PercentageLength Auto) (Either PercentageLength Auto)
  deriving Show

instance Eq BgSize where
  BgSize1 x1 == BgSize1 x2       = x1 `bgsizeArgEq` x2
  BgSize2 x1 y == BgSize1 x2     = x1 `bgsizeArgEq` x2 && y == Right Auto
  x@BgSize1{} == y@BgSize2{}     = y == x
  BgSize2 x1 y1 == BgSize2 x2 y2 = x1 `bgsizeArgEq` x2 && y1 `bgsizeArgEq` y2
  Cover == Cover                 = True
  Contain == Contain             = True
  _ == _ = False

bgsizeArgEq :: Either PercentageLength Auto -> Either PercentageLength Auto -> Bool
bgsizeArgEq (Left x) (Left y) = isZero x && isZero y || x == y
bgsizeArgEq x y = x == y

instance ToText BgSize where
  toBuilder Cover         = "cover"
  toBuilder Contain       = "contain"
  toBuilder (BgSize1 x)   = toBuilder x
  toBuilder (BgSize2 x y) = toBuilder x <> singleton ' ' <> toBuilder y

-- | Minifying a @\<bg-size\>@ value entails, apart from minifying the
-- individual values, removing any @auto@ value in the second position (if
-- present).
instance Minifiable BgSize where
  minify (BgSize1 x)   = BgSize1 <$> minifyBgSizeArg x
  minify (BgSize2 x y) = do
      nx   <- minifyBgSizeArg x
      ny   <- minifyBgSizeArg y
      let b = BgSize2 nx ny
      pure $ if True {- shouldMinifyBgSize conf -}
                then minifyBgSize b
                else b
    where minifyBgSize (BgSize2 l (Right Auto)) = BgSize1 l
          minifyBgSize z = z
  minify x = pure x

minifyBgSizeArg :: Either PercentageLength Auto
                -> Reader Config (Either PercentageLength Auto)
minifyBgSizeArg (Left a)     = Left <$> minifyPL a
minifyBgSizeArg (Right Auto) = pure $ Right Auto
