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
    , word8
    ) where

import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Attoparsec.Text (char, many1, digit,
  option, Parser, satisfy, skipSpace, string)
import Data.Text (Text)
import Data.Word (Word8)
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C

import Hasmin.Parser.Primitives

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

word8 :: Parser Word8
word8 = read <$> digits
