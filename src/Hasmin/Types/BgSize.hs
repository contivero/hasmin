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
module Hasmin.Types.BgSize (
    BgSize(..), Auto(..)
    ) where
import Hasmin.Types.Class
import Hasmin.Types.PercentageLength
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton)
import Control.Monad.Reader (ask)

data Auto = Auto
  deriving (Eq, Show)

instance ToText Auto where
  toBuilder Auto = "auto"

data BgSize = Cover 
            | Contain
            | BgSize (Either PercentageLength Auto) (Maybe (Either PercentageLength Auto))
  deriving (Eq, Show)

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

