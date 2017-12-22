{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.String
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Types.String
  ( removeQuotes
  , unquoteUrl
  , unquoteFontFamily
  , convertEscaped
  , mapString
  , StringType(..)
  ) where

import Control.Monad.Reader (ask, Reader)
import Data.Attoparsec.Text (Parser, parse, IResult(Done, Partial, Fail), maybeResult, feed)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

import Hasmin.Config
import Hasmin.Class
import Hasmin.Parser.String
import Hasmin.Parser.Primitives

-- | The <https://drafts.csswg.org/css-values-3/#strings \<string\>> data type.
-- It represents a string, formed by Unicode characters, delimited by either
-- double (\") or single (\') quotes.
data StringType = DoubleQuotes Text
                | SingleQuotes Text
  deriving (Show, Eq)
instance ToText StringType where
  toBuilder (DoubleQuotes t) = B.singleton '\"' <> B.fromText t <> B.singleton '\"'
  toBuilder (SingleQuotes t) = B.singleton '\'' <> B.fromText t <> B.singleton '\''
instance Minifiable StringType where
  minify (DoubleQuotes t) = do
    conf <- ask
    convertedText <- convertEscapedText t
    pure $ if T.any ('\"' ==) convertedText
              then DoubleQuotes t
              else if shouldNormalizeQuotes conf
              -- TODO make it so that the best quotes are used for the file
                      then DoubleQuotes convertedText
                      else DoubleQuotes convertedText
  minify (SingleQuotes t) = do
    conf <- ask
    convertedText <- convertEscapedText t
    pure $ if T.any ('\'' ==) convertedText
              then SingleQuotes t
              else if shouldNormalizeQuotes conf && T.all ('\"' /=) convertedText
                      then DoubleQuotes convertedText
                      else SingleQuotes convertedText

convertEscapedText :: Text -> Reader Config Text
convertEscapedText t = do
    conf <- ask
    pure $ if shouldConvertEscaped conf
              then either (const t) id (A.parseOnly convertEscaped t)
              else t

mapString :: (Text -> Reader Config Text) -> StringType -> Reader Config StringType
mapString f (DoubleQuotes t) = DoubleQuotes <$> f t
mapString f (SingleQuotes t) = SingleQuotes <$> f t

unquoteStringType :: (Text -> Maybe Text) -> StringType -> Either Text StringType
unquoteStringType g x@(DoubleQuotes s) = maybe (Right x) Left (g s)
unquoteStringType g x@(SingleQuotes s) = maybe (Right x) Left (g s)

removeQuotes :: StringType -> Either Text StringType
removeQuotes = unquoteStringType toIdent

unquoteUrl :: StringType -> Either Text StringType
unquoteUrl = unquoteStringType toUnquotedURL

unquoteFontFamily :: StringType -> Either Text StringType
unquoteFontFamily = unquoteStringType toUnquotedFontFamily

unquote :: Parser Text -> Text -> Maybe Text
unquote p s = case parse p s of
                Done i r      -> if T.null i
                                    then Just r
                                    else Nothing
                par@Partial{} -> maybeResult (feed par mempty)
                Fail{}        -> Nothing

toIdent :: Text -> Maybe Text
toIdent = unquote ident

toUnquotedURL :: Text -> Maybe Text
toUnquotedURL = unquote unquotedURL

toUnquotedFontFamily :: Text -> Maybe Text
toUnquotedFontFamily = unquote fontfamilyname

