module Hasmin.Parser.Numeric
    ( alphavalue
    , rational
    , int
    , percentage
    , number
    ) where

import Control.Applicative ((<|>))
import Numeric (readSigned, readFloat)
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Hasmin.Types.Numeric
import Hasmin.Parser.Utils


-- | Parser for <https://www.w3.org/TR/css-values-3/#numbers \<number\>>.
number :: Parser Number
number = Number <$> rational

-- | Parser for <https://drafts.csswg.org/css-values-3/#percentages \<percentage\>>.
percentage :: Parser Percentage
percentage = Percentage <$> rational <* A.char '%'

alphavalue :: Parser Alphavalue
alphavalue = mkAlphavalue <$> rational

-- Note that many properties that allow an integer or real number as a value
-- actually restrict the value to some range, often to a non-negative value.
--
-- | Real number parser. \<number\>: <'int' integer> | [0-9]*.[0-9]+
rational :: Parser Rational
rational = do
    sign <- A.option [] (wrapMinus <$> (A.char '-' <|> A.char '+'))
    dgts <- ((++) <$> digits <*> A.option "" fractionalPart)
           <|> ("0"++) <$> fractionalPart -- append a zero for read not to fail
    e <- A.option [] expo
    pure . fst . head $ readSigned readFloat (sign ++ dgts ++ e)
  where fractionalPart = (:) <$> A.char '.' <*> digits
        expo = (:) <$> A.satisfy (\c -> c == 'e' || c == 'E') <*> int'
        wrapMinus x = [x | x == '-'] -- we use this since read fails with leading '+'

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
