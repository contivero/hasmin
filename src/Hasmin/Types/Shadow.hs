{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.Shadow
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Types.Shadow
    ( Shadow(..)
    ) where

import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton)
import Data.Bool (bool)

import Hasmin.Class
import Hasmin.Types.Color
import Hasmin.Types.Dimension

-- | CSS <https://drafts.csswg.org/css-backgrounds-3/#typedef-shadow \<shadow\>>
-- data type, used by the @box-shadow@ property.
data Shadow = Shadow { _inset        :: Bool
                     , _sOffsetX     :: Length
                     , _sOffsetY     :: Length
                     , _blurRadius   :: Maybe Length
                     , _spreadRadius :: Maybe Length
                     , _sColor       :: Maybe Color
                     } deriving (Eq, Show)

instance ToText Shadow where
  toBuilder (Shadow i ox oy br sr c) =
      bool mempty "inset " i
      <> toBuilder ox <> singleton ' '
      <> toBuilder oy <> prependSpace br
      <> prependSpace sr <> prependSpace c
    where prependSpace x = maybe mempty (\y -> singleton ' ' <> toBuilder y) x -- don't eta reduce this!

instance Minifiable Shadow where
  minify (Shadow i ox oy br sr c) = do
      -- conf <- ask
      x  <- minify ox
      y  <- minify oy
      nb <- mapM minify br
      ns <- mapM minify sr
      c2 <- mapM minify c
      pure $ if True {- shouldMinifyShadows conf -}
                then let (a, b) = minifyBlurAndSpread nb ns
                     in Shadow i x y a b c2
                else Shadow i x y nb ns c2
    where minifyBlurAndSpread :: Maybe Length -> Maybe Length -> (Maybe Length, Maybe Length)
          minifyBlurAndSpread (Just br') Nothing
              | isZeroLen br' = (Nothing, Nothing)
              | otherwise     = (Just br', Nothing)
          minifyBlurAndSpread (Just br') (Just sr')
              | isZeroLen sr' = minifyBlurAndSpread (Just br') Nothing
              | otherwise     = (Just br', Just sr')
          minifyBlurAndSpread x y = (x, y)
