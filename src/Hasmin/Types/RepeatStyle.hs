{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.RepeatStyle
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- \<repeat-style> data type used in background-repeat. Specification:
-- <https://drafts.csswg.org/css-backgrounds-3/#the-background-repeat CSS Backgrounds and Borders Module Level 3 (§3.4)>
-- 
-----------------------------------------------------------------------------
module Hasmin.Types.RepeatStyle (
    RepeatStyle(..), RSKeyword(..)
    ) where

import Control.Monad.Reader (ask)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton)
import Hasmin.Types.Class

data RepeatStyle = RepeatX 
                 | RepeatY
                 | RSPair RSKeyword (Maybe RSKeyword)
  deriving (Show)

data RSKeyword = RsRepeat | RsSpace | RsRound | RsNoRepeat
  deriving (Eq, Show)

instance Eq RepeatStyle where
  RepeatX == RepeatX = True
  a@RepeatX == b@RSPair{} = b == a
  RepeatY == RepeatY = True
  a@RepeatY == b@RSPair{} = b == a
  RSPair RsNoRepeat (Just RsRepeat) == RepeatY = True
  RSPair RsRepeat (Just RsNoRepeat) == RepeatX = True
  RSPair RsNoRepeat (Just RsRepeat) == RSPair RsNoRepeat (Just RsRepeat) = True
  RSPair RsRepeat (Just RsNoRepeat) == RSPair RsRepeat (Just RsNoRepeat) = True
  RSPair RsSpace (Just RsSpace) == RSPair RsSpace Nothing = True
  RSPair RsSpace (Just RsSpace) == RSPair RsSpace (Just RsSpace) = True
  RSPair RsRound (Just RsRound) == RSPair RsRound Nothing = True
  RSPair RsRound (Just RsRound) == RSPair RsRound (Just RsRound) = True
  RSPair RsNoRepeat (Just RsNoRepeat) == RSPair RsNoRepeat Nothing = True
  RSPair RsNoRepeat (Just RsNoRepeat) == RSPair RsNoRepeat (Just RsNoRepeat) = True
  RSPair RsRepeat (Just RsRepeat) == RSPair RsRepeat Nothing = True
  RSPair RsRepeat (Just RsRepeat) == RSPair RsRepeat (Just RsRepeat) = True
  RSPair x Nothing == RSPair y Nothing = x == y
  a@(RSPair _ Nothing) == b@(RSPair _ _) = b == a
  _ == _ = False

instance ToText RSKeyword where
  toBuilder RsRepeat   = "repeat"
  toBuilder RsSpace    = "space"
  toBuilder RsRound    = "round"
  toBuilder RsNoRepeat = "no-repeat"

instance ToText RepeatStyle where
  toBuilder RepeatX = "repeat-x"
  toBuilder RepeatY = "repeat-y"
  toBuilder (RSPair r1 r2 ) = toBuilder r1 <> maybe mempty (\x -> singleton ' ' <> toBuilder x) r2

instance Minifiable RepeatStyle where
  minifyWith r = do
    conf <- ask
    pure $ if True {- shouldMinifyRepeatStyle conf -}
              then minifyRepeatStyle r
              else r

minifyRepeatStyle :: RepeatStyle -> RepeatStyle
minifyRepeatStyle (RSPair RsRepeat (Just RsNoRepeat)) = RepeatX
minifyRepeatStyle (RSPair RsNoRepeat (Just RsRepeat)) = RepeatY
minifyRepeatStyle (RSPair x (Just y)) = if x == y
                                           then RSPair x Nothing
                                           else RSPair x (Just y)
minifyRepeatStyle x = x
