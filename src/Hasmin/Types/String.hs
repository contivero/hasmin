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
import Control.Monad (mzero)
import Control.Applicative (liftA2, (<|>), many)
import Data.Attoparsec.Text (Parser, parse, IResult(Done, Partial, Fail), maybeResult, feed)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Char as C
import Data.Bits ((.|.), shiftL)
import Data.Foldable (foldl')

import Hasmin.Config
import Hasmin.Class
import Hasmin.Parser.Primitives
import Hasmin.Parser.Utils

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
removeQuotes = unquoteStringType (unquote ident)

unquoteUrl :: StringType -> Either Text StringType
unquoteUrl = unquoteStringType (unquote unquotedURL)

unquoteFontFamily :: StringType -> Either Text StringType
unquoteFontFamily = unquoteStringType (unquote fontfamilyname)

unquote :: Parser Text -> Text -> Maybe Text
unquote p s = case parse p s of
                Done i r      -> if T.null i
                                    then Just r
                                    else Nothing
                par@Partial{} -> maybeResult (feed par mempty)
                Fail{}        -> Nothing

-- TODO can the Parser be avoided by a fold, or one of the provided library
-- functions? Apart from being cleaner, doing so would simplify other functions.
-- | Parse and convert any escaped unicode to its underlying Char.
convertEscaped :: Parser Text
convertEscaped = (TL.toStrict . B.toLazyText) <$> go
  where
    go = do
        nonescapedText <- B.fromText <$> A.takeWhile (/= '\\')
        cont nonescapedText <|> pure nonescapedText
    cont b = do
        _ <- A.char '\\'
        c <- A.peekChar
        case c of
          Just _  -> parseEscapedAndContinue b
          Nothing -> pure (b <> B.singleton '\\')

    parseEscapedAndContinue :: Builder -> Parser Builder
    parseEscapedAndContinue b = do
        u8 <- utf8
        ((b <> u8) <>) <$>  go

    utf8 :: Parser Builder
    utf8 = do
        mch <- atMost 6 hexadecimal
        pure $ maybe ("\\" <> B.fromString mch) B.singleton (hexToChar mch)

    -- Interpret a hexadecimal string as a decimal Int, and convert it into the
    -- corresponding Char.
    hexToChar :: [Char] -> Maybe Char
    hexToChar xs
        | i > maxChar = Nothing
        | otherwise   = Just (C.chr i)
      where i = foldl' step 0 xs
            maxChar = fromEnum (maxBound :: Char)
            step a c
                | w - 48 < 10 = (a `shiftL` 4) .|. fromIntegral (w - 48)
                | w >= 97     = (a `shiftL` 4) .|. fromIntegral (w - 87)
                | otherwise   = (a `shiftL` 4) .|. fromIntegral (w - 55)
              where w = C.ord c

    atMost :: Int -> Parser a -> Parser [a]
    atMost 0 _ = pure []
    atMost n p = A.option [] $ liftA2 (:) p (atMost (n-1) p)

fontfamilyname :: Parser Text
fontfamilyname = do
    i  <- ident
    is <- many (skipComments *> ident)
    if T.toLower i `elem` invalidNames
       then mzero
       else pure $ i <> foldMap (" "<>) is
  where invalidNames = ["serif", "sans-serif", "monospace", "cursive",
                        "fantasy", "inherit", "initial", "unset", "default"]

-- | Parse a URL without enclosing quotes.
-- See <https://drafts.csswg.org/css-syntax-3/#consume-a-url-token §4.3.6. Consume a url token>
unquotedURL :: Parser Text
unquotedURL = do
    t <- many (escape <|> (B.singleton <$> A.satisfy validChar))
    pure $ TL.toStrict (B.toLazyText (mconcat t))
  where validChar x = x /= '\"' && x /= '\'' && x /= '(' && x /= ')'
                   && x /= '\\' && notWhitespace x && notNonprintable x
        notWhitespace x = x /= '\n' &&  x /= '\t' && x /= ' '
        notNonprintable x = not (C.chr 0 <= x && x <= C.chr 8)
                         && x /= '\t'
                         && not ('\SO' <= x && x <= C.chr 31)
                         && x /= '\DEL'
