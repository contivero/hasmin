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

-- An empty selector, containing no sequence of simple selectors and no
-- pseudo-element, is an invalid selector.
data Selector = Selector CompoundSelector [(Combinator, CompoundSelector)]
  deriving (Eq, Show)

instance Ord Selector where
  -- Lexicographical order
  s1 <= s2 = toText s1 <= toText s2

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
  minifyWith (FunctionalPseudoClass1 i cs)   = FunctionalPseudoClass1 i <$> mapM minifyWith cs
  minifyWith (FunctionalPseudoClass2 i n)    = FunctionalPseudoClass2 i <$> minifyWith n
  minifyWith (FunctionalPseudoClass3 i n cs) = FunctionalPseudoClass3 i <$> minifyWith n <*> pure cs
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

{- data AValue = Nwith (Maybe Sign) (Maybe Int) -- at least a lone 'n'
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
-}

-- We could maybe model the AB constructor with an Either,
-- to make sure AB NoValue Nothing isn't possible (which is invalid).
-- Also, modelling a BValue would cover all remaining cases,
-- for example +6 vs 6, -0 vs 0 vs +0.
data AnPlusB = Even
             | Odd
             | A (Maybe Sign) (Maybe Int) -- "sign n number", e.g. +3n, -2n, 1n.
             | B Int                      -- "sign number", e.g. +1, +2, 3.
             | AB (Maybe Sign) (Maybe Int) Int -- "sign n number sign number", e.g. 2n+1
  deriving (Eq, Show)
instance ToText AnPlusB where
  toBuilder Even         = "even"
  toBuilder Odd          = "odd"
  toBuilder (B b)        = toBuilder b
  toBuilder (A ms mi)    = an2Builder ms mi
  toBuilder (AB ms mi b) = an2Builder ms mi <> bSign b <> toBuilder b
    where bSign x
              | x < 0     = mempty
              | otherwise = singleton '+'

an2Builder :: Maybe Sign -> Maybe Int -> Builder
an2Builder ms mi = maybeToBuilder ms <> maybeToBuilder mi <> singleton 'n'
  where maybeToBuilder :: ToText a => Maybe a -> Builder
        maybeToBuilder = maybe mempty toBuilder

instance Minifiable AnPlusB where
  minifyWith x = do
    conf <- ask
    pure $ if shouldMinifyMicrosyntax conf
              then minifyAnPlusB x
              else x

minifyAnPlusB :: AnPlusB -> AnPlusB
minifyAnPlusB Even = A Nothing (Just 2)
minifyAnPlusB (A ms mi) =
    case mi of
      Just 0 -> B 0
      _      -> uncurry A (minifyAN ms mi)
minifyAnPlusB (AB _ (Just 0) b) = B b
minifyAnPlusB (AB ms mi b)
    | isPositive ms && mi == Just 2 =
        if b == 1 || b < 0 && odd b
           then Odd
           else if even b && b <= 0
                then minifyAnPlusB Even
                else AB ms' mi' b
    | otherwise = if b == 0
                     then A ms' mi'
                     else AB ms' mi' b
  where (ms', mi') = minifyAN ms mi
minifyAnPlusB x = x

minifyAN :: Maybe Sign -> Maybe Int -> (Maybe Sign, Maybe Int)
minifyAN (Just Plus) i = minifyAN Nothing i
minifyAN s (Just 1)    = minifyAN s Nothing
minifyAN s i           = (s, i)

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
instance ToText Att where
  toBuilder (Attribute t) = fromText t
  toBuilder (attid :=: attval)  = fromText attid <> singleton '=' <> toBuilder attval
  toBuilder (attid :~=: attval) = fromText attid <> fromText "~=" <> toBuilder attval
  toBuilder (attid :|=: attval) = fromText attid <> fromText "|=" <> toBuilder attval
  toBuilder (attid :^=: attval) = fromText attid <> fromText "^=" <> toBuilder attval
  toBuilder (attid :$=: attval) = fromText attid <> fromText "$=" <> toBuilder attval
  toBuilder (attid :*=: attval) = fromText attid <> fromText "*=" <> toBuilder attval

removeAttributeQuotes :: Att -> Att
removeAttributeQuotes (attId :=: val)  = attId :=: either Left removeQuotes val
removeAttributeQuotes (attId :~=: val) = attId :~=: either Left removeQuotes val
removeAttributeQuotes (attId :|=: val) = attId :|=: either Left removeQuotes val
removeAttributeQuotes (attId :^=: val) = attId :^=: either Left removeQuotes val
removeAttributeQuotes (attId :$=: val) = attId :$=: either Left removeQuotes val
removeAttributeQuotes (attId :*=: val) = attId :*=: either Left removeQuotes val
removeAttributeQuotes a@Attribute{}    = a
