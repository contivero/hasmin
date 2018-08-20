{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.Primitives
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Parsers for very basic CSS grammar tokens.
--
-----------------------------------------------------------------------------
module Hasmin.Parser.Primitives
    ( ident
    , escape
    , unicode
    , nmstart
    , nmchar
    , int
    , int'
    , digits
    ) where

import Control.Applicative ((<|>), some, many)
import Data.Attoparsec.Text (Parser, (<?>))
import Data.Monoid ((<>))
import Data.Text.Lazy.Builder (Builder)
import Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B

-- TODO combine with unicode to make it more efficient
-- escape: {unicode}|\\[^\n\r\f0-9a-f]
escape :: Parser Builder
escape =  unicode
      <|> (mappend <$> (B.singleton <$> A.char '\\') <*> (B.singleton <$> A.satisfy cond))
      <?> "not an escape token: {unicode}|\\\\[^\\n\\r\\f0-9a-f]"
  where cond c = c /= '\n'
              && c /= '\r'
              && c /= '\f'
              && (not . C.isHexDigit) c

-- unicode        \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
unicode :: Parser Builder
unicode = do
    backslash <- A.char '\\'
    hexChars  <- A.takeWhile1 C.isHexDigit
    _         <- A.option mempty (A.string "\r\n" <|> (T.singleton <$> A.satisfy ws))
    if T.length hexChars <= 6
       then pure $ B.singleton backslash <> B.fromText hexChars
       else fail "unicode escaped character with length greater than 6"
  where ws x = x == ' ' || x == '\n' || x == '\r' || x == '\t' || x == '\f'

-- ident: -?-?{nmstart}{nmchar}*
ident :: Parser Text
ident = do
    dash <- (B.fromText <$> A.string "--") <|> (B.singleton <$> A.char '-') <|> pure mempty
    ns   <- nmstart
    nc   <- mconcat <$> many nmchar
    pure $ TL.toStrict (B.toLazyText (dash <> ns <> nc))

-- nmstart: [_a-z]|{nonascii}|{escape}
nmstart :: Parser Builder
nmstart = B.singleton <$> A.satisfy (\c -> C.isAlpha c || (not . C.isAscii) c || c == '_')
       <|> escape
       <?> "not an nmstart token: [_a-z]|{nonascii}|{escape}"

-- nmchar: [_a-z0-9-]|{nonascii}|{escape}
nmchar :: Parser Builder
nmchar = B.singleton <$> A.satisfy cond <|> escape
  where cond x = C.isAlphaNum x || x == '_' || x == '-'
              || (not . C.isAscii) x

-- | \<integer\> data type parser, but into a String instead of an Int, for other
-- parsers to use (e.g.: see the parsers int, or rational)
int' :: Parser String
int' = do
  sign <- A.char '-' <|> pure '+'
  d    <- digits
  case sign of
    '+' -> pure d
    '-' -> pure (sign:d)
    _   -> error "int': parsed a number starting with other than [+|-]"

-- | Parser for \<integer\>: [+|-][0-9]+
int :: Parser Int
int = read <$> int'

-- | Parser one or more digits.
digits :: Parser String
digits = some A.digit
