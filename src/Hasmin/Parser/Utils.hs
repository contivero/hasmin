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
    , parserFromPairs
    , atMost
    ) where

import Control.Applicative (liftA2, (<|>), many)
import Control.Monad (void, mzero)
import Data.Attoparsec.Text (char,
  option, Parser, satisfy, skipSpace, string)
import Data.Text (Text)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word8)
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import qualified Data.Map.Strict as Map

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

hexadecimal :: Parser Char
hexadecimal = satisfy C.isHexDigit

word8 :: Parser Word8
word8 = read <$> digits

parserFromPairs :: [(Text, Parser a)] -> Parser a
parserFromPairs ls = do
    i <- ident
    let t = T.toLower i
    fromMaybe mzero (Map.lookup t m)
  where m = Map.fromList ls

atMost :: Int -> Parser a -> Parser [a]
atMost 0 _ = pure []
atMost n p = A.option [] $ liftA2 (:) p (atMost (n-1) p)
