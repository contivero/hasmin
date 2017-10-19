{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Class
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Hasmin.Class
    ( ToText(..)
    , Minifiable(..)
    ) where

import Control.Monad.Reader (Reader)
import Data.Word (Word8)
import Data.Text (pack, Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, toLazyText)

import Hasmin.Config

-- | Class for types that can be minified
class Minifiable a where
  {-# MINIMAL minify #-}
  minify :: a -> Reader Config a

-- | Class for types that can be converted to Text. Used for printing the
-- minified results.
class ToText a where
  {-# MINIMAL toText | toBuilder #-}
  toText :: a -> Text
  toBuilder :: a -> Builder
  toText    = toStrict . toLazyText . toBuilder
  toBuilder = fromText . toText
instance ToText Word8 where
  toText = pack . show
instance ToText Int where
  toText = pack . show
instance ToText Text where
  toText = id
instance (ToText a, ToText b) => ToText (Either a b) where
  toText    = either toText toText
  toBuilder = either toBuilder toBuilder
