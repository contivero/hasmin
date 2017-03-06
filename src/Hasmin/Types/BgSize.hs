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

import Control.Monad.Reader (ask)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton)

import Hasmin.Types.Class
import Hasmin.Types.PercentageLength

data Auto = Auto
  deriving (Eq, Show)

instance ToText Auto where
  toBuilder Auto = "auto"

data BgSize = Cover
            | Contain
            | BgSize (Either PercentageLength Auto) (Maybe (Either PercentageLength Auto))
  deriving Show

instance Eq BgSize where
  Cover == Cover = True
  Contain == Contain = True
  BgSize a b == BgSize c d = fstParamEquality a c && b `equals` d
    where equals (Just (Right Auto)) Nothing = True
          equals Nothing (Just (Right Auto)) = True
          equals x y = x == y
          fstParamEquality (Left x) (Left y) = isZero x && isZero y || x == y
          fstParamEquality x y = x == y

instance ToText BgSize where
  toBuilder Cover = "cover"
  toBuilder Contain = "contain"
  toBuilder (BgSize x y) = toBuilder x <> maybe mempty (\a -> singleton ' ' <> toBuilder a) y

instance Minifiable BgSize where
  minifyWith (BgSize x y) = do
      conf <- ask
      nx   <- minFirst x
      ny   <- mapM minFirst y
      let b = BgSize nx ny
      pure $ if True {- shouldMinifyBgSize conf -}
                then minifyBgSize b
                else b
    where minFirst (Left a) = Left <$> minifyWith a
          minFirst (Right Auto) = pure (Right Auto)
  minifyWith x = pure x

minifyBgSize :: BgSize -> BgSize
minifyBgSize (BgSize l (Just (Right Auto))) = BgSize l Nothing
minifyBgSize x = x

