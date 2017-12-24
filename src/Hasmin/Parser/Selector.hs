{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.Selector
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Parser.Selector
    ( selectors
    , selector
    ) where

import Control.Applicative ((<|>), many, some, empty, optional)
import Data.Attoparsec.Combinator (sepBy)
import Data.Attoparsec.Text (asciiCI, char, Parser, satisfy, string)
import qualified Data.Attoparsec.Text as A
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Functor (($>))
import qualified Data.Text.Lazy.Builder as LB
import Data.List.NonEmpty (NonEmpty( (:|) ))

import Hasmin.Parser.Utils
import Hasmin.Parser.String
import Hasmin.Utils

import Hasmin.Types.Selector
import Hasmin.Types.String

-- | Parser for CSS complex selectors (see 'Selector' for more details).
selector :: Parser Selector
selector = Selector <$> compoundSelector <*> combinatorsAndSelectors
  where combinatorsAndSelectors = many $ mzip (combinator <* skipComments) compoundSelector

-- First tries with '>>' (descendant), '>' (child), '+' (adjacent sibling), and
-- '~' (general sibling) combinators. If those fail, it tries with the
-- descendant (whitespace) combinator. This is done to allow comments in-between.
--
-- | Parser for selector combinators, i.e. ">>" (descendant), '>' (child), '+'
-- (adjacent sibling), '~' (general sibling), and ' ' (descendant) combinators.
combinator :: Parser Combinator
combinator =  (skipComments *> ((string ">>" $> DescendantBrackets)
          <|> (char '>' $> Child)
          <|> (char '+' $> AdjacentSibling)
          <|> (char '~' $> GeneralSibling)))
          <|> (satisfy ws $> DescendantSpace)
  where ws c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'

compoundSelector :: Parser CompoundSelector
compoundSelector = cs1 <|> cs2
  where cs1 = do
            sel  <- typeSelector <|> universal
            sels <- many p
            pure $ sel:|sels
        cs2 = (Universal mempty :|) <$> some p
        p = idSel <|> classSel <|> attributeSel <|> pseudo

-- | Parses a \"number sign\" (U+0023, \#) immediately followed by the ID value,
-- which must be a CSS identifier.
idSel :: Parser SimpleSelector
idSel = do
    _    <- char '#'
    name <- mconcat <$> some nmchar
    pure . IdSel . TL.toStrict $ LB.toLazyText name

-- class: '.' IDENT
classSel :: Parser SimpleSelector
classSel = char '.' *> (ClassSel <$> ident)

{-
attrib
  : '[' S* [ namespace_prefix ]? IDENT S*
        [ [ PREFIXMATCH |
            SUFFIXMATCH |
            SUBSTRINGMATCH |
            '=' |
            INCLUDES |
            DASHMATCH ] S* [ IDENT | STRING ] S*
        ]? ']'
  ;
-}
-- FIXME namespace prefixes aren't allowed inside attribute selectors,
-- but they should be.
attributeSel :: Parser SimpleSelector
attributeSel = do
    _     <- char '['
    attId <- lexeme ident
    g     <- A.option Attribute attValue
    _     <- char ']'
    pure $ AttributeSel (g attId)
  where attValue = do
          f <- ((string "^=" $> (:^=:)) <|>
                (string "$=" $> (:$=:)) <|>
                (string "*=" $> (:*=:)) <|>
                (string "="  $> (:=:))  <|>
                (string "~=" $> (:~=:)) <|>
                (string "|=" $> (:|=:))) <* skipComments
          attval <- identOrString <* skipComments
          pure (`f` attval)

-- type_selector: [namespace_prefix]? element_name
typeSelector :: Parser SimpleSelector
typeSelector = Type <$> opt namespacePrefix <*> ident

-- universal: [ namespace_prefix ]? '*'
universal :: Parser SimpleSelector
universal = Universal <$> opt namespacePrefix <* char '*'

-- namespace_prefix: [ IDENT | '*' ]? '|'
namespacePrefix :: Parser Text
namespacePrefix = opt (ident <|> string "*") <* char '|'

{- '::' starts a pseudo-element, ':' a pseudo-class
   Exceptions: :first-line, :first-letter, :before and :after.
   Note that pseudo-elements are restricted to one per selector and
   occur only in the last simple_selector_sequence.

   <pseudo-class-selector> = ':' <ident-token> |
                             ':' <function-token> <any-value> ')'
   <pseudo-element-selector> = ':' <pseudo-class-selector>
-}
-- pseudo: ':' ':'? [ IDENT | functional_pseudo ]
pseudo :: Parser SimpleSelector
pseudo = char ':' *> (pseudoElementSelector <|> pseudoClassSelector)
  where pseudoClassSelector = do
            i <- ident
            c <- A.peekChar
            case c of
              Just '(' -> char '(' *> case Map.lookup (T.toLower i) fpcMap of
                            Just p  -> functionParser p
                            Nothing -> functionParser (FunctionalPseudoClass i <$> A.takeWhile (/= ')'))
              _        -> pure $ PseudoClass i
        pseudoElementSelector =
            (char ':' *> (PseudoElem <$> ident)) <|> (ident >>= handleSpecialCase)
          where
            handleSpecialCase :: Text -> Parser SimpleSelector
            handleSpecialCase t
                | isSpecialPseudoElement = pure $ PseudoElem t
                | otherwise              = empty
              where isSpecialPseudoElement = T.toLower t `elem` specialPseudoElements

-- \<An+B> microsyntax parser.
anplusb :: Parser AnPlusB
anplusb = (asciiCI "even" $> Even)
      <|> (asciiCI "odd" $> Odd)
      <|> do
        s    <- optional parseSign
        dgts <- A.option mempty digits
        case dgts of
          [] -> ciN *> skipComments *> A.option (A s Nothing) (AB s Nothing <$> bValue)
          _  -> let n = read dgts :: Int
                in (ciN *> skipComments *> A.option (A s $ Just n) (AB s (Just n) <$> bValue))
                    <|> (pure . B $ getSign s * n)
  where ciN       = satisfy (\c -> c == 'N' || c == 'n')
        parseSign = (char '-' $> Minus) <|> (char '+' $> Plus)
        getSign (Just Minus) = -1
        getSign _            = 1
        bValue    = do
            readPlus <- (char '-' $> False) <|> (char '+' $> True)
            d        <- skipComments *> digits
            if readPlus
               then pure $ read d
               else pure $ read ('-':d)

-- Functional pseudo classes parsers map
fpcMap :: Map Text (Parser SimpleSelector)
fpcMap = Map.fromList
    [buildTuple "nth-of-type"      (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "nth-last-of-type" (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "nth-column"       (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "nth-last-column"  (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "not"              (\x -> FunctionalPseudoClass1 x <$> compoundSelectorList)
    ,buildTuple "matches"          (\x -> FunctionalPseudoClass1 x <$> compoundSelectorList)
    ,buildTuple "nth-child"        (anbAndSelectors . FunctionalPseudoClass3)
    ,buildTuple "nth-last-child"   (anbAndSelectors . FunctionalPseudoClass3)
    ,buildTuple "lang"             (const (Lang <$> identOrString))
    --
    -- :drop( [ active || valid || invalid ]? )
    -- The :drop() functional pseudo-class is identical to :drop
    -- ,("drop", anplusb)
    --
    -- It accepts a comma-separated list of one or more language ranges as its
    -- argument. Each language range in :lang() must be a valid CSS <ident> or
    -- <string>.
    -- ,("lang", anplusb)
    --
    -- ,("dir", text)
    -- ,("has", relative selectors)
    ]
  where buildTuple t c = (t, c t)
        compoundSelectorList = (:) <$> compoundSelector <*> many (comma *> compoundSelector)
        anbAndSelectors constructor = do
            a <- anplusb <* skipComments
            o <- A.option [] (asciiCI "of" *> skipComments *> compoundSelectorList)
            pure $ constructor a o

-- | Parse a list of comma-separated selectors, ignoring whitespace and
-- comments.
selectors :: Parser [Selector]
selectors = lexeme selector `sepBy` char ','

identOrString :: Parser (Either Text StringType)
identOrString = (Left <$> ident) <|> (Right <$> stringtype)
