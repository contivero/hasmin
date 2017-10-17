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

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton)

import Hasmin.Types.Class
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
            | BgSize (Either PercentageLength Auto) (Maybe (Either PercentageLength Auto))
            -- TODO Avoid Maybe by adding another constructor
  deriving Show

instance Eq BgSize where
  Cover == Cover           = True
  Contain == Contain       = True
  BgSize a b == BgSize c d = ftsArgEq a c && b `equals` d
    where equals (Just (Right Auto)) Nothing     = True
          equals Nothing (Just (Right Auto))     = True
          equals (Just (Left x)) (Just (Left y)) = isZero x && isZero y || x == y
          equals x y                             = x == y
          ftsArgEq (Left x) (Left y) = isZero x && isZero y || x == y
          ftsArgEq x y = x == y
  _ == _ = False

instance ToText BgSize where
  toBuilder Cover        = "cover"
  toBuilder Contain      = "contain"
  toBuilder (BgSize x y) = toBuilder x <> maybe mempty (\a -> singleton ' ' <> toBuilder a) y

-- | Minifying a @\<bg-size\>@ value entails, apart from minifying the
-- individual values, removing any @auto@ value in the second position (if
-- present).
instance Minifiable BgSize where
  minifyWith (BgSize x y) = do
      -- conf <- ask
      nx   <- minFirst x
      ny   <- mapM minFirst y
      let b = BgSize nx ny
      pure $ if True {- shouldMinifyBgSize conf -}
                then minifyBgSize b
                else b
    where minFirst (Left a)     = Left <$> minifyWith a
          minFirst (Right Auto) = pure (Right Auto)

          minifyBgSize (BgSize l (Just (Right Auto))) = BgSize l Nothing
          minifyBgSize z = z
  minifyWith x = pure x
