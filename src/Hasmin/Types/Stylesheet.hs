{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.Stylesheet
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Types.Stylesheet (
      Expression(..)
    , MediaQuery(..)
    , Rule(..)
    , KeyframeSelector(..)
    , KeyframeBlock(..)
    , SupportsCondition(..)
    , SupportsCondInParens(..)
    , isEmpty
    ) where

import Control.Monad.Reader (Reader, ask)
import Control.Applicative (liftA2)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy.Builder (singleton, fromText, Builder)
import Data.List (sortBy, (\\))
import Data.Map.Strict (Map)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import Text.PrettyPrint.Mainland (Pretty, ppr, strictText,
  lbrace, rbrace, (</>), (<+>), comma, folddoc, nest, semi, stack, char)

import Hasmin.Config
import Hasmin.Selector
import Hasmin.Types.Class
import Hasmin.Types.Value
import Hasmin.Types.Declaration
import Hasmin.Types.String
import Hasmin.Types.Numeric
import Hasmin.Types.Dimension
import Hasmin.Utils

-- | Data type for media queries. For the syntax, see
-- <https://www.w3.org/TR/css3-mediaqueries/#syntax media query syntax>.
data MediaQuery = MediaQuery1 Text Text [Expression]  -- ^ First possibility in the grammar
                | MediaQuery2 [Expression] -- ^ Second possibility in the grammar
  deriving (Show, Eq)
instance Minifiable MediaQuery where
  minifyWith (MediaQuery1 t1 t2 es) = MediaQuery1 t1 t2 <$> mapM minifyWith es
  minifyWith (MediaQuery2 es)       = MediaQuery2 <$> mapM minifyWith es

instance ToText MediaQuery where
  toBuilder (MediaQuery1 t1 t2 es) = notOrOnly <> fromText t2 <> expressions
    where notOrOnly   | T.null t1 = mempty
                      | otherwise = fromText t1 <> singleton ' '
          expressions = foldr (\x xs -> " and " <> toBuilder x <> xs) mempty es
  toBuilder (MediaQuery2 es) = mconcatIntersperse toBuilder " and " es

data Expression = Expression Text (Maybe Value)
                | InvalidExpression Text
  deriving (Show, Eq)
instance Minifiable Expression where
  minifyWith (Expression t mv) = Expression t <$> mapM minifyWith mv
  minifyWith x = pure x
instance ToText Expression where
  toBuilder (Expression t mv) =
      singleton '(' <> fromText t <> v <> singleton ')'
    where v = maybe mempty (\x -> singleton ':' <> toBuilder x) mv
  toBuilder (InvalidExpression t) =
      singleton '(' <> fromText t <> singleton ')'

data KeyframeSelector = From | To | KFPercentage Percentage
  deriving (Eq, Show)
instance ToText KeyframeSelector where
  toText From             = "from"
  toText To               = "to"
  toText (KFPercentage p) = toText p
instance Pretty KeyframeSelector where
  ppr = strictText . toText
instance Minifiable KeyframeSelector where
  minifyWith x = do
      conf <- ask
      pure $ if shouldMinifyKeyframeSelectors conf
                then minifyKFS x
                else x

minifyKFS :: KeyframeSelector -> KeyframeSelector
minifyKFS From                            = KFPercentage $ Percentage 0
minifyKFS (KFPercentage (Percentage 100)) = To
minifyKFS x = x

data KeyframeBlock = KeyframeBlock [KeyframeSelector] [Declaration]
  deriving (Eq, Show)
instance ToText KeyframeBlock where
  toBuilder (KeyframeBlock ss ds) =
      mconcatIntersperse toBuilder (singleton ',') ss
      <> singleton '{'
      <> mconcatIntersperse toBuilder (singleton ';') ds
      <> singleton '}'
instance Minifiable KeyframeBlock where
  minifyWith (KeyframeBlock ss ds) = do
      decs <- mapM minifyWith ds
      sels <- mapM minifyWith ss
      pure $ KeyframeBlock sels decs

type VendorPrefix = Text

data Rule = AtCharset StringType
          | AtImport (Either StringType Url) [MediaQuery]
          | AtNamespace Text (Either StringType Url)
          | AtMedia [MediaQuery] [Rule]
          | AtKeyframes VendorPrefix Text [KeyframeBlock]
          | AtSupports SupportsCondition [Rule]
          | AtBlockWithRules Text [Rule]
          | AtBlockWithDec Text [Declaration]
          | StyleRule [Selector] [Declaration]
 deriving (Show)
instance Pretty Rule where
  ppr (StyleRule ss ds) = folddoc (\x y -> x <> comma <+> y) (fmap ppr ss)
                     <+> nest 4 (lbrace </> stack (fmap ((<> semi) . ppr) ds))
                     </> rbrace
  ppr (AtBlockWithRules t rs) = char '@' <> strictText t
      <+> nest 4 (lbrace </> stack (fmap ppr rs)) </> rbrace
  ppr (AtBlockWithDec t ds) = char '@' <> strictText t
      <+> nest 4 (lbrace </> stack (fmap ppr ds)) </> rbrace
  ppr _ = error "not implemented (TODO)"

instance ToText Rule where
  toBuilder (AtMedia mqs rs) = "@media " <> mconcatIntersperse toBuilder (singleton ',') mqs
      <> singleton '{' <> mconcat (fmap toBuilder rs) <> singleton '}'
  toBuilder (AtSupports sc rs) = "@supports " <> toBuilder sc
      <> singleton '{' <> mconcat (fmap toBuilder rs) <> singleton '}'
  toBuilder (AtImport esu mqs) = "@import " <> toBuilder esu <> mediaqueries
      <> singleton ';'
    where mediaqueries =
            case mqs of
              [] -> mempty
              _  -> singleton ' ' <> mconcatIntersperse toBuilder (singleton ',') mqs
  toBuilder (AtCharset s) = "@charset " <> toBuilder s <> singleton ';'
  toBuilder (AtNamespace t esu) = "@namespace "
      <> prefix <> toBuilder esu <> singleton ';'
    where prefix = if T.null t
                      then mempty
                      else toBuilder t <> singleton ' '
  toBuilder (StyleRule ss ds) =
    mconcat [mconcatIntersperse toBuilder (singleton ',') ss
            ,singleton '{'
            ,mconcatIntersperse toBuilder (singleton ';') ds
            ,singleton '}']
  toBuilder (AtBlockWithRules t rs) =
    mconcat [singleton '@', fromText t, singleton '{'
            , mconcat (fmap toBuilder rs), singleton '}']
  toBuilder (AtBlockWithDec t ds)   =
    mconcat [singleton '@', fromText t, singleton '{'
            ,mconcatIntersperse id (singleton ';') (fmap toBuilder ds)
            ,singleton '}']
  toBuilder (AtKeyframes vp n bs) = singleton '@' <> fromText vp <> "keyframes"
            <> singleton ' ' <> fromText n <> singleton '{'
            <> mconcat (fmap toBuilder bs) <> singleton '}'

instance Minifiable Rule where
  minifyWith (AtMedia mqs rs) = liftA2 AtMedia (mapM minifyWith mqs) (mapM minifyWith rs)
  minifyWith (AtSupports sc rs) = liftA2 AtSupports (minifyWith sc) (mapM minifyWith rs)
  minifyWith (AtKeyframes vp n bs) = AtKeyframes vp n <$> mapM minifyWith bs
  minifyWith (AtBlockWithRules t rs) = AtBlockWithRules t <$> mapM minifyWith rs
  minifyWith (AtBlockWithDec t ds) = do
      decs <- cleanRule ds >>= compactLonghands >>= mapM minifyWith
      pure $ AtBlockWithDec t decs
  minifyWith (StyleRule ss ds) = do
      decs <- cleanRule ds >>= compactLonghands >>= mapM minifyWith >>= sortDeclarations
      sels <- mapM minifyWith ss >>= removeDuplicateSelectors >>= sortSelectors
      pure $ StyleRule sels decs
  minifyWith (AtImport esu mqs) = AtImport esu <$> mapM minifyWith mqs
  minifyWith (AtCharset s) = AtCharset <$> mapString lowercaseText s
  minifyWith x = pure x

cleanRule :: [Declaration] -> Reader Config [Declaration]
cleanRule ds = do
    conf <- ask
    pure $ if shouldCleanRules conf
              then clean ds
              else ds

sortSelectors :: [Selector] -> Reader Config [Selector]
sortSelectors sls = do
    conf <- ask
    pure $ case selectorSorting conf of
                   Lexicographical -> sortBy lexico sls
                   NoSorting       -> sls
sortDeclarations :: [Declaration] -> Reader Config [Declaration]
sortDeclarations ds = do
    conf <- ask
    pure $ case declarationSorting conf of
             Lexicographical -> sortBy lexico ds
             NoSorting       -> ds

removeDuplicateSelectors :: [Selector] -> Reader Config [Selector]
removeDuplicateSelectors sls = do
    conf <- ask
    pure $ if shouldRemoveDuplicateSelectors conf
              then nub' sls
              else sls

gatherLonghands :: [Declaration] -> Map (Text, Bool) Declaration
gatherLonghands = go Map.empty
  where go m [] = m
        go m (d@(Declaration p _ i _):ds)
            | S.member p longhands = go (Map.insert (p,i) d m) ds
            | otherwise            = go m ds
        longhands = S.fromList (marginLonghands ++ paddingLonghands)

-- TODO delete this.
marginLonghands = ["margin-top", "margin-right", "margin-bottom", "margin-left"]
paddingLonghands = ["padding-top", "padding-right", "padding-bottom", "padding-left"]

compactTRBL :: Text -> [Text] -> Map (Text, Bool) Declaration
            -> (Maybe Declaration, [Declaration])
compactTRBL name lhs m =
    case sequenceA (map getDeclaration lhs) of
      Just l -> (Just (Declaration name (shValues l) False False), l)
      Nothing -> (Nothing, [])
  where getDeclaration x = Map.lookup (x, False) m
        shValues = mkValues . map (head . valuesToList . valueList)

compactMargin  = compactTRBL "margin" marginLonghands
compactPadding = compactTRBL "padding" paddingLonghands
-- ,("border-color", mergeIntoTRBL)
-- ,("border-width", mergeIntoTRBL)
-- ,("border-style", mergeIntoTRBL)

compacter m ds = compacter' [compactMargin, compactPadding]
  where compacter' [] = ds
        compacter' (f:fs) = case f m of
                              (Just sh, l) -> (compacter' fs \\ l) ++ [sh]
                              (Nothing, _) -> compacter' fs

compactLonghands :: [Declaration] -> Reader Config [Declaration]
compactLonghands ds = do
    conf <- ask
    pure $ if True {- shouldGatherLonghands conf -}
              then compacter (gatherLonghands ds) ds
              else ds

-- Used for sorting selectors
-- TODO: move to the correct place, and maybe rename.
lexico :: ToText a => a -> a -> Ordering
lexico s1 s2 = compare (toText s1) (toText s2)

isEmpty :: Rule -> Bool
isEmpty (StyleRule _ ds)        = null ds
isEmpty (AtMedia _ rs)          = null rs || all isEmpty rs
isEmpty (AtKeyframes _ _ kfbs)  = null kfbs
isEmpty (AtBlockWithDec _ ds)   = null ds
isEmpty (AtBlockWithRules _ rs) = null rs || all isEmpty rs
isEmpty _                       = False

-- O(n log n) implementation, vs. O(n^2) which is the normal nub.
-- Note that nub has only an Eq constraint, while this one has an Ord one.
-- taken from: http://buffered.io/posts/a-better-nub/
nub' :: (Ord a) => [a] -> [a]
nub' = go S.empty
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs


data SupportsCondition = Not SupportsCondInParens
                       | And SupportsCondInParens (NonEmpty SupportsCondInParens)
                       | Or SupportsCondInParens (NonEmpty SupportsCondInParens)
                       | Parens SupportsCondInParens
  deriving (Show)
instance ToText SupportsCondition where
  toBuilder (Not x)    = "not " <> toBuilder x
  toBuilder (And x y)  = appendWith " and " x y
  toBuilder (Or x y)   = appendWith " or " x y
  toBuilder (Parens x) = toBuilder x
instance Minifiable SupportsCondition where
  minifyWith (And x y)   = And <$> pure x <*> mapM pure y
  minifyWith (Or x y)    = Or <$> pure x <*> mapM pure y
  minifyWith (Parens x)  = Parens <$> pure x
  minifyWith (Not x) =
    case x of
      ParensCond (Not y) -> case y of
                              ParensCond a@And{}    -> pure a
                              ParensCond o@Or{}     -> pure o
                              ParensCond n@Not{}    -> pure n
                              ParensCond (Parens c) -> Parens <$> pure c
                              ParensDec d           -> (Parens . ParensDec) <$> pure d
      ParensCond y       -> (Not . ParensCond) <$> pure y
      ParensDec y        -> (Not . ParensDec) <$> pure y

appendWith :: Builder -> SupportsCondInParens -> NonEmpty SupportsCondInParens -> Builder
appendWith s x y = toBuilder x <> s <> mconcatIntersperse toBuilder s (NE.toList y)

-- Note that "general_enclosed" is not included, because, per the spec:
--
-- The result is always false. Additionally, style sheets must
-- not write @supports rules that match this grammar production. (In other
-- words, this production exists only for future extensibility, and is not part
-- of the description of a valid style sheet in this level of the
-- specification.) Note that future levels may define functions or other
-- parenthesized expressions that can evaluate to true.
data SupportsCondInParens = ParensCond SupportsCondition
                          | ParensDec Declaration
  deriving (Show)
instance ToText SupportsCondInParens where
  toBuilder (ParensDec x)  = "(" <> toBuilder x <> ")"
  toBuilder (ParensCond x) = "(" <> toBuilder x <> ")"
instance Minifiable SupportsCondInParens where
  minifyWith (ParensDec x) = ParensDec <$> minifyWith x
  minifyWith  (ParensCond x)  = ParensCond <$> minifyWith x

data GeneralEnclosed = GEFunction
  deriving (Show)

{-
supports_rule
  : SUPPORTS_SYM S* supports_condition group_rule_body
  ;

supports_condition
  : supports_negation | supports_conjunction | supports_disjunction |
    supports_condition_in_parens
  ;

supports_condition_in_parens
  : ( '(' S* supports_condition ')' S* ) | supports_declaration_condition |

supports_negation
  : NOT S* supports_condition_in_parens
  ;

supports_conjunction
  : supports_condition_in_parens ( AND S* supports_condition_in_parens )+
  ;

supports_disjunction
  : supports_condition_in_parens ( OR S* supports_condition_in_parens )+
  ;

supports_declaration_condition
  : '(' S* declaration ')' S*
  ;
  -}
