{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.RepeatStyle
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Types.RepeatStyle
    ( RepeatStyle(..)
    , RSKeyword(..)
    ) where

import Control.Monad.Reader (ask)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton)
import Hasmin.Types.Class

-- | CSS <https://drafts.csswg.org/css-backgrounds-3/#typedef-repeat-style \<repeat-style\>>
-- data type, used in the properties @background-repeat@ and @background@.
data RepeatStyle = RepeatX
                 | RepeatY
                 | RepeatStyle1 RSKeyword
                 | RepeatStyle2 RSKeyword RSKeyword
  deriving Show
instance ToText RepeatStyle where
  toBuilder RepeatX            = "repeat-x"
  toBuilder RepeatY            = "repeat-y"
  toBuilder (RepeatStyle1 x)   = toBuilder x
  toBuilder (RepeatStyle2 x y) = toBuilder x <> singleton ' ' <> toBuilder y
instance Minifiable RepeatStyle where
  minifyWith r = do
      conf <- ask
      pure $ if True {- shouldMinifyRepeatStyle conf -}
                then minifyRepeatStyle r
                else r
    where minifyRepeatStyle :: RepeatStyle -> RepeatStyle
          minifyRepeatStyle (RepeatStyle2 RsRepeat RsNoRepeat) = RepeatX
          minifyRepeatStyle (RepeatStyle2 RsNoRepeat RsRepeat) = RepeatY
          minifyRepeatStyle rs2@(RepeatStyle2 x y)
              | x == y    = RepeatStyle1 x
              | otherwise = rs2
          minifyRepeatStyle x = x

instance Eq RepeatStyle where
  RepeatX == RepeatX = True
  a@RepeatX == b@RepeatStyle2{} = b == a
  RepeatStyle2 RsRepeat RsNoRepeat == RepeatX = True
  RepeatY == RepeatY = True
  a@RepeatY == b@RepeatStyle2{} = b == a
  RepeatStyle2 RsNoRepeat RsRepeat == RepeatY = True
  RepeatStyle2 RsNoRepeat RsRepeat == RepeatStyle2 RsNoRepeat RsRepeat = True
  RepeatStyle2 RsRepeat RsNoRepeat == RepeatStyle2 RsRepeat RsNoRepeat = True
  RepeatStyle2 RsSpace RsSpace == RepeatStyle2 RsSpace RsSpace = True
  RepeatStyle2 RsSpace RsSpace == RepeatStyle1 RsSpace = True
  RepeatStyle2 RsRound RsRound == RepeatStyle2 RsRound RsRound = True
  RepeatStyle2 RsRound RsRound == RepeatStyle1 RsRound = True
  RepeatStyle2 RsNoRepeat RsNoRepeat == RepeatStyle2 RsNoRepeat RsNoRepeat = True
  RepeatStyle2 RsNoRepeat RsNoRepeat == RepeatStyle1 RsNoRepeat = True
  RepeatStyle2 RsRepeat RsRepeat == RepeatStyle2 RsRepeat RsRepeat = True
  RepeatStyle2 RsRepeat RsRepeat == RepeatStyle1 RsRepeat = True
  RepeatStyle1 x == RepeatStyle1 y = x == y
  a@RepeatStyle1{} == b@RepeatStyle2{} = b == a
  RepeatStyle2 x1 y1 == RepeatStyle2 x2 y2 = x1 == x2 && y1 == y2
  _ == _ = False

data RSKeyword = RsRepeat
               | RsSpace
               | RsRound
               | RsNoRepeat
  deriving (Eq, Show)
instance ToText RSKeyword where
  toBuilder RsRepeat   = "repeat"
  toBuilder RsSpace    = "space"
  toBuilder RsRound    = "round"
  toBuilder RsNoRepeat = "no-repeat"
