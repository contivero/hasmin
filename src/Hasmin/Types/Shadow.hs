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

import Control.Monad.Reader (ask)
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (singleton)
import Data.Bool (bool)

import Hasmin.Types.Class
import Hasmin.Types.Color
import Hasmin.Types.Dimension

-- | CSS <https://drafts.csswg.org/css-backgrounds-3/#typedef-shadow \<shadow\>>
-- data type, used by the @box-shadow@ property.
data Shadow = Shadow { inset        :: Bool
                     , sOffsetX     :: Length
                     , sOffsetY     :: Length
                     , blurRadius   :: Maybe Length
                     , spreadRadius :: Maybe Length
                     , sColor       :: Maybe Color
                     } deriving (Eq, Show)

instance ToText Shadow where
  toBuilder (Shadow i ox oy br sr c) =
      bool mempty "inset " i
      <> toBuilder ox <> singleton ' '
      <> toBuilder oy <> prependSpace br
      <> prependSpace sr <> prependSpace c
    where prependSpace x = maybe mempty (\y -> singleton ' ' <> toBuilder y) x -- don't eta reduce this!

instance Minifiable Shadow where
  minifyWith (Shadow i ox oy br sr c) = do
      conf <- ask
      x  <- minifyWith ox
      y  <- minifyWith oy
      nb <- mapM minifyWith br
      ns <- mapM minifyWith sr
      c2 <- mapM minifyWith c
      pure $ if True {- shouldMinifyShadows conf -}
                then let (a, b) = minifyBlurAndSpread nb ns
                     in Shadow i x y a b c2
                else Shadow i x y nb ns c2
    where minifyBlurAndSpread :: Maybe Length -> Maybe Length -> (Maybe Length, Maybe Length)
          minifyBlurAndSpread (Just br') Nothing
              | br' == Length 0 Q = (Nothing, Nothing)
              | otherwise           = (Just br', Nothing)
          minifyBlurAndSpread (Just br') (Just sr')
              | sr' == Length 0 Q = minifyBlurAndSpread (Just br') Nothing
              | otherwise           = (Just br', Just sr')
          minifyBlurAndSpread x y = (x, y)
