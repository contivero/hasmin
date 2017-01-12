{-# LANGUAGE OverloadedStrings #-}

module Hasmin.TestUtils where

import Test.Hspec

import Data.Text (Text, unpack, singleton)
import Test.Hspec.Attoparsec (parseSatisfies, (~>))
import Data.Attoparsec.Text (Parser)
import Hasmin.Types.Class
import Hasmin.Types.Declaration
import Hasmin.Utils

-- Given a parser and a 3-tuple, prints a test description,
-- applies the parser, and compares its result with the expected result
matchSpecWithDesc :: ToText a => Parser a -> (String, Text, Text) -> Spec
matchSpecWithDesc parser (description, textToParse, expectedResult) = 
  it description $
    (toText <$> (textToParse ~> parser)) `parseSatisfies` (== expectedResult)

matchSpec :: ToText a => Parser a -> (Text, Text) -> Spec
matchSpec parser (textToParse, expectedResult) =
  it (unpack textToParse) $ (toText <$> (textToParse ~> parser)) `parseSatisfies` (== expectedResult)

data Declarations = Declarations [Declaration]
instance ToText Declarations where
  toText (Declarations ds) = mconcatIntersperse toText (singleton ';') ds

