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
module Hasmin.Types.String (
    removeQuotes
  , unquoteUrl
  , unquoteFontFamily
  , mapString
  , StringType(..)
  ) where

import Control.Applicative (liftA2)
import Control.Monad.Reader (ask, Reader)
import Data.Attoparsec.Text (Parser, parse, IResult(Done, Partial, Fail), maybeResult, feed)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy.Builder (singleton, fromText, toLazyText)
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Hasmin.Config
import Hasmin.Parser.Utils
import Hasmin.Types.Class

-- | The \<string\> data type represents a string, formed by Unicode
-- characters, delimited by either double (") or single (') quotes.
-- Specification:
--
-- 1. <https://drafts.csswg.org/css-values-3/#strings CSS Values and Units Module Level 3 (§3.3)
-- 2. <https://www.w3.org/TR/CSS2/syndata.html#strings CSS2.1 (§4.3.7)
data StringType = DoubleQuotes Text
                | SingleQuotes Text
  deriving (Show, Eq)
instance ToText StringType where
  toBuilder (DoubleQuotes t) = singleton '\"' <> fromText t <> singleton '\"'
  toBuilder (SingleQuotes t) = singleton '\'' <> fromText t <> singleton '\''
instance Minifiable StringType where
  minifyWith (DoubleQuotes t) = do
    conf <- ask
    convertedText <- convertEscapedText t
    pure $ if T.any ('\"' ==) convertedText
              then DoubleQuotes t
              else if shouldNormalizeQuotes conf
              -- TODO make it so that the best quotes are used for the file
                      then DoubleQuotes convertedText
                      else DoubleQuotes convertedText
  minifyWith (SingleQuotes t) = do
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
mapString f (DoubleQuotes t) = f t >>= pure . DoubleQuotes
mapString f (SingleQuotes t) = f t >>= pure . SingleQuotes

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

convertEscaped :: Parser Text
convertEscaped = (TL.toStrict . toLazyText) <$> go
  where go = do
            t <- fromText <$> A.takeWhile (/= '\\')
            c <- A.peekChar
            case c of
              Just '\\' -> A.char '\\' *> liftA2 (mappend . mappend t . singleton) utf8 go
              _         -> pure t
        utf8 = C.chr <$> A.hexadecimal
