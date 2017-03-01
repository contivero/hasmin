{-# LANGUAGE OverloadedStrings
           , FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.Selector
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Types.Selector (
      Selector(..)
    , SimpleSelector(..)
    , CompoundSelector
    , Combinator(..)
    , Sign(..)
    , AnPlusB(..)
    , AValue(..)
    , Att(..)
    , specialPseudoElements
    ) where

import Control.Applicative (liftA2)
import Control.Monad.Reader (ask)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromText, singleton, Builder)
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as N
import Text.PrettyPrint.Mainland (Pretty, char, ppr, strictText)

import Hasmin.Config
import Hasmin.Types.Class
import Hasmin.Types.String
import Hasmin.Utils

{-
class Specificity a where
  specificity :: a -> (Int, Int, Int, Int)

addSpecificity :: (Int, Int, Int, Int)
               -> (Int, Int, Int, Int)
               -> (Int, Int, Int, Int)
addSpecificity (a1,b1,c1,d1) (a2,b2,c2,d2) = (a1 + a2, b1 + b2, c1 + c2, d1 + d2)
-}

-- | Combinators are: whitespace, "greater-than sign" (U+003E, >), "plus sign"
-- (U+002B, +) and "tilde" (U+007E, ~). White space may appear between a
-- combinator and the simple selectors around it. Only the characters "space"
-- (U+0020), "tab" (U+0009), "line feed" (U+000A), "carriage return" (U+000D),
-- and "form feed" (U+000C) can occur in whitespace. Other space-like
-- characters, such as "em-space" (U+2003) and "ideographic space" (U+3000), are
-- never part of whitespace.
data Combinator = Descendant      -- ^ ' '
                | Child           -- ^ '>'
                | AdjacentSibling -- ^ '+'
                | GeneralSibling  -- ^ '~'
  deriving (Eq, Show)

instance ToText Combinator where
  toBuilder Descendant      = " "
  toBuilder Child           = ">"
  toBuilder AdjacentSibling = "+"
  toBuilder GeneralSibling  = "~"
instance Pretty Combinator where
  ppr = strictText . toText

-- An empty selector, containing no sequence of simple selectors and no
-- pseudo-element, is an invalid selector.
data Selector = Selector CompoundSelector [(Combinator, CompoundSelector)]
  deriving (Eq, Show)

instance Ord Selector where
  -- Lexicographical order
  s1 <= s2 = toText s1 <= toText s2

instance Pretty Selector where
  ppr (Selector cs ccss) = ppr cs
                        <> mconcat (fmap toDocument ccss)
    where toDocument (comb, compSel) = ppr comb <> ppr compSel
instance ToText Selector where
  toBuilder (Selector cs ccss) = toBuilder cs
                              <> mconcat (fmap build ccss)
    where build (comb, compSel) = toBuilder comb <> toBuilder compSel
instance Minifiable Selector where
  minifyWith (Selector c xs) = do
      newC  <- minifyWith c
      newCs <- (mapM . mapM) minifyWith xs
      pure $ Selector newC newCs

{-
instance Specificity Selector where
  specificity (Selector (x :| xs) ss) =
      case x of
        (Type _ _) -> f (0,0,0,1) xs `addSpecificity` g (0,0,0,0) ss
        (Universal _) -> f (0,0,0,0) xs `addSpecificity` g (0,0,0,0) ss
    where f = foldr (addSpecificity . specificity)
          g = foldr (addSpecificity . specificity . snd)
-}

-- | Called <https://www.w3.org/TR/css3-selectors/#grammar simple_sequence_selector>
-- in CSS2.1, but <https://drafts.csswg.org/selectors-4/#typedef-compound-selector
-- compound-selector> in CSS Syntax Module Level 3.
type CompoundSelector = NonEmpty SimpleSelector

-- instance ToText a => ToText (NonEmpty a) where
instance ToText CompoundSelector where
  toBuilder ns@(Universal{} :| xs)
      | length ns > 1 = mconcat $ fmap toBuilder xs
  toBuilder ns = mconcat $ N.toList (fmap toBuilder ns)

instance Pretty CompoundSelector where
  ppr ns@(Universal{} :| xs)
      | length ns > 1 = mconcat $ fmap ppr xs
  ppr ns = mconcat $ N.toList (fmap ppr ns)

instance Minifiable CompoundSelector where
  minifyWith (a :| xs) = liftA2 (:|) (minifyWith a) (mapM minifyWith xs)

{-
instance Specificity a => Specificity (NonEmpty a) where
  specificity = foldr (\x y -> specificity x `addSpecificity` y) (0,0,0,0)
-}

-- | Certain selectors support namespace prefixes. Namespace prefixes are
-- declared with the @namespace rule. A type selector containing a namespace
-- prefix that has not been previously declared for namespaced selectors is an
-- invalid selector.
type Namespace = Text
type Element = Text -- e.g.: h1, em, body, ...
type Identifier = Text

-- Characters in Selectors can be escaped with a backslash according to the same
-- escaping rules as CSS. [CSS21].

-- If a universal selector represented by * (i.e. without a namespace prefix)
-- is not the only component of a sequence of simple selectors selectors or is
-- immediately followed by a pseudo-element, then the * may be omitted and the
-- universal selector's presence implied.

-- | A simple selector is either a type selector, universal selector, attribute
-- selector, class selector, ID selector, or pseudo-class.  The first selector
-- must be a type or universal selector.  When none is specified, the universal
-- selector is implied, i.e.: #myId is the same as *#myId
-- mempty is used to denote an empty namespace
data SimpleSelector = Type Namespace Element -- ^ e.g.: *, ns|*, h1, em, body
                    | Universal Namespace    -- ^ '*'
                    | AttributeSel Att
                    | ClassSel Identifier
                    | IdSel Identifier
                    | PseudoElem Identifier
                    | PseudoClass Identifier
                    | Lang (Either Text StringType)
                    -- generic functional pseudo class
                    | FunctionalPseudoClass Identifier Text
                    -- :not() and :matches() fpc
                    | FunctionalPseudoClass1 Identifier [CompoundSelector] -- :not( <selector># )
                    -- :nth-of-type(), :nth-last-of-type(), :nth-column(), and
                    -- :nth-of-last-column() fpc
                    | FunctionalPseudoClass2 Identifier AnPlusB
                    -- :nth-child(), nth-last-child()
                    | FunctionalPseudoClass3 Identifier AnPlusB [CompoundSelector]
  deriving (Eq, Show)

instance Pretty SimpleSelector where
  ppr (Type n e)
      | T.null n  = strictText e
      | otherwise = strictText n <> char '|' <> strictText e
  ppr (Universal n)
      | T.null n  = char '*'
      | otherwise = strictText n <> strictText "|*"
  ppr (AttributeSel att) = char '[' <> ppr att <> char ']'
  ppr (ClassSel t)       = char '.' <> strictText t
  ppr (IdSel t)          = char '#' <> strictText t
  ppr (PseudoClass t)    = char ':' <> strictText t
  ppr (PseudoElem t)     = if T.toCaseFold t `elem` specialPseudoElements
                              then strictText ":" <> strictText t
                              else strictText "::" <> strictText t
  ppr (FunctionalPseudoClass i t)   = strictText i <> char '(' <> ppr t <> char ')'
  -- ppr (FunctionalPseudoClass1 i cs) = strictText i <>

instance ToText SimpleSelector where
  toBuilder (Type n e)
      | T.null n  = fromText e
      | otherwise = fromText n <> singleton '|' <> fromText e
  toBuilder (Universal n)
      | T.null n  = singleton '*'
      | otherwise = fromText n <> fromText "|*"
  toBuilder (AttributeSel att) = singleton '[' <> toBuilder att <> singleton ']'
  toBuilder (ClassSel t)       = singleton '.' <> fromText t
  toBuilder (IdSel t)          = singleton '#' <> fromText t
  toBuilder (PseudoClass t)    = singleton ':' <> fromText t
  toBuilder (PseudoElem t)
      | T.toCaseFold t `elem` specialPseudoElements = fromText ":" <> fromText t
      | otherwise                                   = fromText "::" <> fromText t
  toBuilder (Lang x) = ":lang" <> singleton '(' <> toBuilder x <> singleton ')'
  toBuilder (FunctionalPseudoClass t x) = fromText t <> singleton '(' <> fromText x <> singleton ')'
  toBuilder (FunctionalPseudoClass1 t ss) = singleton ':' <> fromText t <> singleton '('
      <> mconcatIntersperse toBuilder (singleton ',') ss
      <> singleton ')'
  toBuilder (FunctionalPseudoClass2 t x) = singleton ':' <> fromText t
      <> singleton '(' <> toBuilder x <> singleton ')'
  toBuilder (FunctionalPseudoClass3 t a xs) = singleton ':' <> fromText t
      <> singleton '(' <> toBuilder a <> f xs <> singleton ')'
    where f [] = mempty
          f (y:ys) = " of " <> toBuilder y
            <> mconcat (fmap (\z -> singleton ',' <> toBuilder z) ys)

-- Pseudo-elements that support the old pseudo-element syntax of a single
-- semicolon, as well as the new one of two semicolons.
specialPseudoElements :: [Text]
specialPseudoElements = fmap T.toCaseFold
    ["after", "before", "first-line", "first-letter"]

instance Minifiable SimpleSelector where
  minifyWith a@(AttributeSel att) = do
      conf <- ask
      pure $ if shouldRemoveQuotes conf
                then AttributeSel (removeAttributeQuotes att)
                else a
  minifyWith a@(Lang x) = do
      conf <- ask
      pure $ if shouldRemoveQuotes conf
                then case x of
                       Left _  -> a
                       Right s -> Lang (removeQuotes s)
                else a
  minifyWith a@(FunctionalPseudoClass2 i n) = do
      conf <- ask
      pure $ if shouldMinifyMicrosyntax conf
                then FunctionalPseudoClass2 i (minifyAnPlusB n)
                else a
  minifyWith a@(FunctionalPseudoClass3 i n cs) = do
      conf <- ask
      pure $ if shouldMinifyMicrosyntax conf
                then FunctionalPseudoClass3 i (minifyAnPlusB n) cs
                else a
  minifyWith (FunctionalPseudoClass1 i cs) = do
      newcs <- mapM minifyWith cs
      pure $ FunctionalPseudoClass1 i newcs
  minifyWith x = pure x

data Sign = Plus | Minus
  deriving (Eq, Show)

isPositive :: Maybe Sign -> Bool
isPositive Nothing      = True
isPositive (Just Plus)  = True
isPositive (Just Minus) = False

instance ToText Sign where
  toBuilder Plus  = singleton '+'
  toBuilder Minus = singleton '-'

data AValue = Nwith (Maybe Sign) (Maybe Int) -- at least a lone 'n'
            | NoValue -- The "An" part is omitted.
  deriving (Eq, Show)
instance ToText AValue where
  toBuilder NoValue     = mempty
  toBuilder (Nwith s i) = maybeToBuilder s <> maybeToBuilder i <> singleton 'n'
    where maybeToBuilder :: ToText a => Maybe a -> Builder
          maybeToBuilder = maybe mempty toBuilder

minifyAValue :: AValue -> AValue
minifyAValue (Nwith _ (Just 0)) = NoValue
minifyAValue (Nwith s a)
    | isPositive s = Nwith Nothing (maybe Nothing droppedOne a)
    | otherwise    = Nwith s (maybe Nothing droppedOne a)
  where droppedOne x = if x == 1
                         then Nothing
                         else Just x
minifyAValue NoValue = NoValue

-- We could maybe model the AB constructor with an Either,
-- to make sure AB NoValue Nothing isn't possible (which is invalid).
-- Also, modelling a BValue would cover all remaining cases,
-- for example +6 vs 6, -0 vs 0 vs +0.
data AnPlusB = Even
             | Odd
             | AB AValue (Maybe Int)
  deriving (Eq, Show)
instance ToText AnPlusB where
  toBuilder Even     = "even"
  toBuilder Odd      = "odd"
  toBuilder (AB a b) = toBuilder a <> bToBuilder b
    where bToBuilder
              | a == NoValue = maybe (singleton '0') toBuilder
              | otherwise    = maybe mempty (\x -> bSign x <> toBuilder x)
          bSign x
              | x < 0     = mempty
              | otherwise = singleton '+'

minifyAnPlusB :: AnPlusB -> AnPlusB
minifyAnPlusB Even = AB (Nwith Nothing (Just 2)) Nothing
minifyAnPlusB (AB n@(Nwith s a) (Just b))
    | isPositive s && a == Just 2 =
        if b == 1 || odd b && b < 0
           then Odd
           else if even b && b <= 0
                then minifyAnPlusB Even
                else AB (minifyAValue n) (Just b)
    | otherwise = AB (minifyAValue n) $ if b == 0
                                           then Nothing
                                           else Just b
minifyAnPlusB (AB n@Nwith{} Nothing) = AB (minifyAValue n) Nothing
minifyAnPlusB x = x
-- instance Specificity SimpleSelector where
  -- specificity (IdSel _)    = (0,1,0,0)
  -- specificity (ClassSel _) = (0,0,1,0)

type AttId = Text
type AttValue = Either Text StringType

data Att = Attribute AttId
         | AttId :=: AttValue    -- ^ \'=\'
         | AttId :~=: AttValue   -- ^ \'~=\'
         | AttId :|=: AttValue   -- ^ \'|=\'
         | AttId :^=: AttValue   -- ^ \'^=\'
         | AttId :$=: AttValue   -- ^ \'$=\'
         | AttId :*=: AttValue   -- ^ \'*=\'
  deriving (Eq, Show)

removeAttributeQuotes :: Att -> Att
removeAttributeQuotes (attId :=: val)  = attId :=: either Left removeQuotes val
removeAttributeQuotes (attId :~=: val) = attId :~=: either Left removeQuotes val
removeAttributeQuotes (attId :|=: val) = attId :|=: either Left removeQuotes val
removeAttributeQuotes (attId :^=: val) = attId :^=: either Left removeQuotes val
removeAttributeQuotes (attId :$=: val) = attId :$=: either Left removeQuotes val
removeAttributeQuotes (attId :*=: val) = attId :*=: either Left removeQuotes val
removeAttributeQuotes a@Attribute{}    = a

instance Pretty Att where
  ppr (Attribute t) = strictText t
  ppr (attid :=: attval)  = strictText attid <> char '=' <> ppr attval
  ppr (attid :~=: attval) = strictText attid <> strictText "~=" <> ppr attval
  ppr (attid :|=: attval) = strictText attid <> strictText "|=" <> ppr attval
  ppr (attid :^=: attval) = strictText attid <> strictText "^=" <> ppr attval
  ppr (attid :$=: attval) = strictText attid <> strictText "$=" <> ppr attval
  ppr (attid :*=: attval) = strictText attid <> strictText "*=" <> ppr attval
instance ToText Att where
  toBuilder (Attribute t) = fromText t
  toBuilder (attid :=: attval)  = fromText attid <> singleton '=' <> toBuilder attval
  toBuilder (attid :~=: attval) = fromText attid <> fromText "~=" <> toBuilder attval
  toBuilder (attid :|=: attval) = fromText attid <> fromText "|=" <> toBuilder attval
  toBuilder (attid :^=: attval) = fromText attid <> fromText "^=" <> toBuilder attval
  toBuilder (attid :$=: attval) = fromText attid <> fromText "$=" <> toBuilder attval
  toBuilder (attid :*=: attval) = fromText attid <> fromText "*=" <> toBuilder attval
