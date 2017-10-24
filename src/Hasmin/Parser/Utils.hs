{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.Utils
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Parser.Utils
    ( ident
    , fontfamilyname
    , unquotedURL
    , skipComments
    , lexeme
    , functionParser
    , digits
    , comma
    , colon
    , slash
    , opt
    , nmchar
    , hexadecimal
    ) where

import Control.Applicative ((<|>), many)
import Control.Monad (void, mzero)
import Data.Attoparsec.Text (char, many1, digit,
  option, Parser, satisfy, skipSpace, string, takeWhile1, (<?>))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy.Builder as LB
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- | Skip whatever comments and whitespaces are found.
skipComments :: Parser ()
skipComments = void $ many (skipSpace *> comment) <* skipSpace

-- | Parse a comment, i.e. a string starting with \"\/\*\" and ending with \"\*\/\"
comment :: Parser Text
comment = mappend <$> string "/*" <*> (string "*/" <|> untilAsterisk)
  where untilAsterisk = mappend <$> A.takeWhile (/= '*') <*> checkAsterisk
        checkAsterisk = mappend <$> string "*" <*> (string "/" <|> untilAsterisk)

comma :: Parser Char
comma = lexeme $ char ','

colon :: Parser Char
colon = lexeme $ char ':'

slash :: Parser Char
slash = lexeme $ char '/'

lexeme :: Parser a -> Parser a
lexeme p = skipComments *> p <* skipComments

-- | Given a parser, makes it optional, defaulting to whatever value its 'Monoid'
-- instance defines for 'mempty'.
opt :: Monoid m => Parser m -> Parser m
opt = option mempty

-- | Parse a URL without enclosing quotes.
-- See <https://drafts.csswg.org/css-syntax-3/#consume-a-url-token §4.3.6. Consume a url token>
unquotedURL :: Parser Text
unquotedURL = do
    t <- many (escape <|> (LB.singleton <$> satisfy validChar))
    pure $ TL.toStrict (toLazyText (mconcat t))
  where validChar x = x /= '\"' && x /= '\'' && x /= '(' && x /= ')'
                   && x /= '\\' && notWhitespace x && notNonprintable x
        notWhitespace x = x /= '\n' &&  x /= '\t' && x /= ' '
        notNonprintable x = not (C.chr 0 <= x && x <= C.chr 8)
                         && x /= '\t'
                         && not ('\SO' <= x && x <= C.chr 31)
                         && x /= '\DEL'

fontfamilyname :: Parser Text
fontfamilyname = do
    i  <- ident
    is <- many (skipComments *> ident)
    if T.toLower i `elem` invalidNames
       then mzero
       else pure $ i <> foldMap (" "<>) is
  where invalidNames = ["serif", "sans-serif", "monospace", "cursive",
                        "fantasy", "inherit", "initial", "unset", "default"]

-- ident: -?{nmstart}{nmchar}*
ident :: Parser Text
ident = do
    dash <- option mempty (LB.singleton <$> char '-')
    ns   <- nmstart
    nc   <- mconcat <$> many nmchar
    pure $ TL.toStrict (toLazyText (dash <> ns <> nc))

-- nmstart: [_a-z]|{nonascii}|{escape}
nmstart :: Parser Builder
nmstart = LB.singleton <$> satisfy (\c -> C.isAlpha c || (not . C.isAscii) c || c == '_')
       <|> escape
       <?> "not an nmstart token: [_a-z]|{nonascii}|{escape}"

-- nmchar: [_a-z0-9-]|{nonascii}|{escape}
nmchar :: Parser Builder
nmchar = LB.singleton <$> satisfy cond <|> escape
  where cond x = C.isAlphaNum x || x == '_' || x == '-'
              || (not . C.isAscii) x

-- TODO combine with unicode to make it more efficient
-- escape: {unicode}|\\[^\n\r\f0-9a-f]
escape :: Parser Builder
escape =  unicode
      <|> (mappend <$> (LB.singleton <$> char '\\') <*> (LB.singleton <$> satisfy cond))
      <?> "not an escape token: {unicode}|\\\\[^\\n\\r\\f0-9a-f]"
  where cond c = c /= '\n'
              && c /= '\r'
              && c /= '\f'
              && (not . C.isHexDigit) c

-- unicode        \\[0-9a-f]{1,6}(\r\n|[ \n\r\t\f])?
unicode :: Parser Builder
unicode = do
    backslash <- char '\\'
    hexChars  <- takeWhile1 C.isHexDigit
    _         <- opt (string "\r\n" <|> (T.singleton <$> satisfy ws))
    if T.length hexChars <= 6
       then pure $ LB.singleton backslash <> LB.fromText hexChars
       else fail "unicode escaped character with length greater than 6"
  where ws x = x == ' ' || x == '\n' || x == '\r' || x == '\t' || x == '\f'

-- | Assumes the identifier and the left parenthesis have been parsed
-- Parses p, ignoring surrounding whitespace and comments, and consumes the
-- final right parenthesis.
functionParser :: Parser a -> Parser a
functionParser p = lexeme p <* char ')'

-- | Parser one or more digits.
digits :: Parser String
digits = many1 digit

hexadecimal :: Parser Char
hexadecimal = satisfy C.isHexDigit
