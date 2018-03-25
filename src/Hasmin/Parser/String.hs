{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Parser.String
    ( convertEscaped
    , stringtype
    ) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser)
import Data.Functor (($>))
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T

import Hasmin.Types.String

-- <string> data type parser
stringtype :: Parser StringType
stringtype = doubleQuotes <|> singleQuotes
  where
    doubleQuotes :: Parser StringType
    doubleQuotes =  A.char '\"' *> (DoubleQuotes <$> untilDoubleQuotes)
      where untilDoubleQuotes = mappend <$> A.takeWhile (\c -> c /= '\\' && c /= '\"') <*> checkCharacter
            checkCharacter = (A.string "\"" $> mempty)
                          <|> (T.cons <$> A.char '\\' <*> untilDoubleQuotes)

    singleQuotes :: Parser StringType
    singleQuotes = A.char '\'' *> (SingleQuotes <$> untilSingleQuotes)
      where untilSingleQuotes = mappend <$> A.takeWhile (\c -> c /= '\\' && c /= '\'') <*> checkCharacter
            checkCharacter = (A.string "\'" $> mempty)
                          <|> (T.cons <$> A.char '\\' <*> untilSingleQuotes)
