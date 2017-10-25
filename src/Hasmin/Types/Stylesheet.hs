{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.Stylesheet
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Types.Stylesheet
    ( Expression(..)
    , MediaQuery(..)
    , Rule(..)
    , KeyframeSelector(..)
    , KeyframeBlock(..)
    , SupportsCondition(..)
    , SupportsCondInParens(..)
    , minifyRules
    , collapse
    , mergeRules
    ) where

import Control.Applicative (liftA2)
import Control.Monad ((>=>))
import Control.Monad.Reader (Reader, ask, asks)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Lazy.Builder (singleton, fromText, Builder)
import Data.List (union, sort, sortBy, (\\))
import Data.Map.Strict (Map)
import Data.List.NonEmpty (NonEmpty)
import Data.Foldable (toList)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Hasmin.Config
import Hasmin.Types.Selector
import Hasmin.Class
import Hasmin.Types.Value
import Hasmin.Types.Declaration
import Hasmin.Types.String
import Hasmin.Types.Numeric
import Hasmin.Utils
import Hasmin.Properties

-- | Data type for media queries. For the syntax, see
-- <https://www.w3.org/TR/css3-mediaqueries/#syntax media query syntax>.
data MediaQuery = MediaQuery1 Text Text [Expression]  -- ^ First possibility in the grammar
                | MediaQuery2 [Expression] -- ^ Second possibility in the grammar
  deriving (Show, Eq)
instance Minifiable MediaQuery where
  minify (MediaQuery1 t1 t2 es) = MediaQuery1 t1 t2 <$> traverse minify es
  minify (MediaQuery2 es)       = MediaQuery2 <$> traverse minify es

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
  minify (Expression t mv) = Expression t <$> traverse minify mv
  minify x = pure x
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
instance Minifiable KeyframeSelector where
  minify kfs = do
      conf <- ask
      pure $ if shouldMinifyKeyframeSelectors conf
                then minifyKFS kfs
                else kfs
    where minifyKFS :: KeyframeSelector -> KeyframeSelector
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
  minify (KeyframeBlock ss ds) =
    KeyframeBlock <$> traverse minify ss <*> traverse minify ds

type VendorPrefix = Text

-- | A CSS rule, either a normal style rule, or one of the many possible
-- at-rules.
data Rule = AtCharset StringType
          | AtImport (Either StringType Url) [MediaQuery]
          | AtNamespace Text (Either StringType Url)
          | AtMedia [MediaQuery] [Rule]
          | AtKeyframes VendorPrefix Text [KeyframeBlock]
          | AtSupports SupportsCondition [Rule]
          | AtBlockWithRules Text [Rule]
          | AtBlockWithDec Text [Declaration]
          | StyleRule [Selector] [Declaration]
 deriving (Eq, Show)
instance ToText Rule where
  toBuilder (AtMedia mqs rs) = "@media " <> mconcatIntersperse toBuilder (singleton ',') mqs
      <> singleton '{' <> foldMap toBuilder rs <> singleton '}'
  toBuilder (AtSupports sc rs) = "@supports " <> toBuilder sc
      <> singleton '{' <> foldMap toBuilder rs <> singleton '}'
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
            , foldMap toBuilder rs, singleton '}']
  toBuilder (AtBlockWithDec t ds)   =
    mconcat [singleton '@', fromText t, singleton '{'
            ,mconcatIntersperse id (singleton ';') (fmap toBuilder ds)
            ,singleton '}']
  toBuilder (AtKeyframes vp n bs) = singleton '@' <> fromText vp <> "keyframes"
            <> singleton ' ' <> fromText n <> singleton '{'
            <> foldMap toBuilder bs <> singleton '}'
instance Minifiable Rule where
  -- @media all {..} == @media {..}
  minify (AtMedia [MediaQuery1 t1 t2 xs] rs)
      | T.null t1 && T.toLower t2 == "all" = do
          xs' <- traverse minify xs
          AtMedia [MediaQuery2 xs'] <$> traverse minify rs
  minify (AtMedia mqs rs)        = liftA2 AtMedia (traverse minify mqs) (traverse minify rs)
  minify (AtSupports sc rs)      = liftA2 AtSupports (minify sc) (traverse minify rs)
  minify (AtKeyframes vp n bs)   = AtKeyframes vp n <$> traverse minify bs
  minify (AtBlockWithRules t rs) = AtBlockWithRules t <$> traverse minify rs
  minify (AtBlockWithDec t ds)   = do
      decs <- cleanRule ds >>= collapseLonghands >>= traverse minify
      pure $ AtBlockWithDec t decs
  minify (StyleRule ss ds) = do
      decs <- cleanRule ds >>= collapseLonghands >>= traverse minify >>= sortDeclarations
      sels <- traverse minify ss >>= removeDuplicateSelectors >>= sortSelectors
      pure $ StyleRule sels decs
    where sortSelectors :: [Selector] -> Reader Config [Selector]
          sortSelectors sls = do
              conf <- ask
              pure $ case selectorSorting conf of
                            -- Selector's Ord instance implements lexicographical order
                            Lexicographical -> sort sls
                            NoSorting       -> sls

          removeDuplicateSelectors :: [Selector] -> Reader Config [Selector]
          removeDuplicateSelectors sls = do
              conf <- ask
              pure $ if shouldRemoveDuplicateSelectors conf
                        then nub' sls
                        else sls
  -- convert url() to " "
  minify (AtImport esu mqs) =
      case esu of
        Left _          -> AtImport esu <$> traverse minify mqs
        Right (Url ets) -> do a <- traverse minify ets
                              let na = either DoubleQuotes id a
                              b <- traverse minify mqs
                              pure $ AtImport (Left na) b
  minify (AtCharset s) = AtCharset <$> mapString lowercaseText s
  minify x = pure x

sortDeclarations :: [Declaration] -> Reader Config [Declaration]
sortDeclarations ds = do
    conf <- ask
    pure $ case declarationSorting conf of
             Lexicographical -> sortBy lexico ds
             NoSorting       -> ds
  where lexico :: ToText a => a -> a -> Ordering
        lexico s1 s2 = compare (toText s1) (toText s2)

cleanRule :: [Declaration] -> Reader Config [Declaration]
cleanRule ds = do
    conf <- ask
    pure $ if shouldCleanRules conf
              then clean ds
              else ds

collapseLonghands :: [Declaration] -> Reader Config [Declaration]
collapseLonghands decs = do
    -- conf <- ask
    pure $ if True {- shouldGatherLonghands conf -}
              then collapse decs
              else decs

-- | Given a list of declarations, gathers the longhands, and if every longhand
-- of a given shorthand is present, \"collapses\" them into the shorthand
-- (i.e. replaces all the declarations for an equivalent shorthand).
collapse :: [Declaration] -> [Declaration]
collapse decs = collapse' $ zipWith collapseTRBL trbls longhands
  where collapse' []     = decs
        collapse' (f:fs) = case f (gatherLonghands decs) of
                             (Just sh, l) -> (collapse' fs \\ l) ++ [sh]
                             (Nothing, _) -> collapse' fs

        gatherLonghands :: [Declaration] -> Map (Text, Bool) Declaration
        gatherLonghands = go Map.empty
          where go m [] = m
                go m (d@(Declaration p _ i _):ds)
                    | p `Set.member` longhandsSet = go (Map.insert (p,i) d m) ds
                    | otherwise              = go m ds
                longhandsSet = Set.fromList (concat longhands)

        trbls :: [Text]
        trbls = ["margin", "padding", "border-color", "border-width", "border-style"]

        longhands :: [[Text]]
        longhands = map subpropertiesOf trbls

        subpropertiesOf :: Text -> [Text]
        subpropertiesOf p = subproperties . fromJust $ Map.lookup p propertiesTraits

        collapseTRBL :: Text -> [Text] -> Map (Text, Bool) Declaration
                     -> (Maybe Declaration, [Declaration])
        collapseTRBL name lnhs m =
            case traverse getDeclaration lnhs of
              -- If every longhand is present, return the collapsed ones
              Just l  -> (Just (Declaration name (shValues l) False False), l)
              -- If a longhand was missing, don't combine because it's unsafe
              Nothing -> (Nothing, [])
          where getDeclaration x = Map.lookup (x, False) m
                shValues = mkValues . map (head . valuesToList . valueList)

-- O(n log n) implementation, vs. O(n^2) which is the normal nub.
-- Note that nub has only an Eq constraint, while this one has an Ord one.
-- taken from: http://buffered.io/posts/a-better-nub/
nub' :: (Ord a) => [a] -> [a]
nub' = go Set.empty
  where go _ [] = []
        go s (x:xs) | Set.member x s = go s xs
                    | otherwise      = x : go (Set.insert x s) xs

data SupportsCondition = Not SupportsCondInParens
                       | And SupportsCondInParens (NonEmpty SupportsCondInParens)
                       | Or SupportsCondInParens (NonEmpty SupportsCondInParens)
                       | Parens SupportsCondInParens
  deriving (Eq, Show)
instance ToText SupportsCondition where
  toBuilder (Not x)    = "not " <> toBuilder x
  toBuilder (And x y)  = appendWith " and " x y
  toBuilder (Or x y)   = appendWith " or " x y
  toBuilder (Parens x) = toBuilder x
instance Minifiable SupportsCondition where
  minify x@And{}    = pure x
  minify x@Or{}     = pure x
  minify x@Parens{} = pure x
  minify (Not x) =
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
appendWith s x y = toBuilder x <> s <> mconcatIntersperse toBuilder s (toList y)

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
  deriving (Eq, Show)
instance ToText SupportsCondInParens where
  toBuilder (ParensDec x)  = "(" <> toBuilder x <> ")"
  toBuilder (ParensCond x) = "(" <> toBuilder x <> ")"
instance Minifiable SupportsCondInParens where
  minify (ParensDec x)  = ParensDec <$> minify x
  minify (ParensCond x) = ParensCond <$> minify x

instance Minifiable [Rule] where
  minify = minifyRules

minifyRules :: [Rule] -> Reader Config [Rule]
minifyRules = handleAdjacentMediaQueries
          >=> handleEmptyBlocks
          >=> mergeStyleRules
          >=> traverse minify -- minify rules individually
  where handleEmptyBlocks :: [Rule] -> Reader Config [Rule]
        handleEmptyBlocks rs = do
          conf <- ask
          pure $ if shouldRemoveEmptyBlocks conf
                    then filter (not . isEmpty) rs
                    else rs

        isEmpty :: Rule -> Bool
        isEmpty (StyleRule _ ds)        = null ds
        isEmpty (AtMedia _ rs)          = null rs || all isEmpty rs
        isEmpty (AtKeyframes _ _ kfbs)  = null kfbs
        isEmpty (AtBlockWithDec _ ds)   = null ds
        isEmpty (AtBlockWithRules _ rs) = null rs || all isEmpty rs
        isEmpty _                       = False

        handleAdjacentMediaQueries :: [Rule] -> Reader Config [Rule]
        handleAdjacentMediaQueries rs =
            pure $ combineAdjacentMediaQueries rs
          where combineAdjacentMediaQueries :: [Rule] -> [Rule]
                combineAdjacentMediaQueries (a@(AtMedia mqs es) : b@(AtMedia mqs2 es2) : xs)
                    | mqs == mqs2 = combineAdjacentMediaQueries (AtMedia mqs (es ++ es2) : xs)
                    | otherwise   = a : combineAdjacentMediaQueries (b:xs)
                combineAdjacentMediaQueries (x:xs) = x : combineAdjacentMediaQueries xs
                combineAdjacentMediaQueries [] = []

mergeStyleRules :: [Rule] -> Reader Config [Rule]
mergeStyleRules xs = do
    mergeSettings <- asks rulesMergeSettings
    pure $ case mergeSettings of
             MergeRulesOn  -> mergeRules xs
             MergeRulesOff -> xs

-- Imperative pseudocode:
--
-- for rule r1 in positions [0..n-2]
--    for rule r2 in positions [r1 pos..n-1]
--      if r1 and r2 have the same declarations:
--          merge (i.e. combine both into r1, remove r2 from map)
--      else if r1 and r2 have the same specificity, and some declaration overlaps
--          continue with next r1
--      else
--          continue with the next r2
mergeRules :: [Rule] -> [Rule]
mergeRules zs = Map.elems $ mergeRules' 0 1 rulesInMap
  where rulesInMap = Map.fromList $ zip [0..] zs
        mapSize    = Map.size rulesInMap

        mergeRules' :: Int -> Int -> Map Int Rule -> Map Int Rule
        mergeRules' i j m
            | i == mapSize - 1 = m
            | j == mapSize     = mergeRules' (i+1) (i+2) m
            | otherwise        =
                case Map.lookupGE i m of
                  Nothing -> m
                  Just (key, r) ->
                    let index = if key >= j then key + 1 else j
                    in case Map.lookupGE index m of
                         Nothing -> m
                         Just (key2, r2) ->
                           let newIndex = key2 + 1
                           in case mergeAndRemove r r2 key key2 newIndex m of
                                Nothing -> if shouldSkip r r2
                                              then mergeRules' (key+1) (key+2) m
                                              else mergeRules' key newIndex m
                                Just m' -> m'

        mergeAndRemove :: Rule -> Rule -> Int -> Int -> Int -> Map Int Rule -> Maybe (Map Int Rule)
        mergeAndRemove (StyleRule ss ds) (StyleRule ss2 ds2) key key2 newIndex m
            | Set.fromList ds2 == Set.fromList ds =
                let ruleMergedBySelectors = StyleRule (ss `union` ss2) ds
                    newMap = Map.delete key2 (Map.insert key ruleMergedBySelectors m)
                in Just $ mergeRules' key newIndex newMap
            | Set.fromList ss == Set.fromList ss2 =
                let ruleMergedByDeclarations = StyleRule ss (ds `union` ds2)
                    newMap = Map.delete key2 (Map.insert key ruleMergedByDeclarations m)
                in Just $ mergeRules' key newIndex newMap
            | otherwise = Nothing
        mergeAndRemove _ _ _ _ _ _ = Nothing

        overlaps :: Declaration -> Declaration -> Bool
        overlaps (Declaration p1 _ _ _) (Declaration p2 _ _ _) =
            p1 == p2 || overlaps'
          where overlaps' =
                  case Map.lookup p2 propertiesTraits of
                    Nothing    -> True -- Be safe, in the absence of information, assume the worst
                    Just pinfo -> p1 `elem` subproperties pinfo || p1 `elem` overwrittenBy pinfo

        shouldSkip :: Rule -> Rule -> Bool
        shouldSkip (StyleRule ss ds) (StyleRule ss2 ds2) =
            thereIsAPairOfSelectorsWithTheSameSpecificity && twoDeclarationsClash
          where thereIsAPairOfSelectorsWithTheSameSpecificity = any (\x -> any (\y -> specificity x == specificity y) ss) ss2
                twoDeclarationsClash = any (\x -> any (`overlaps` x) ds) ds2
        shouldSkip StyleRule{} AtKeyframes{} = False
        shouldSkip StyleRule{} (AtBlockWithDec t _)
            | t == "font-face" = False
            | otherwise        = True
         -- TODO see better what needs to be skipped and what doesn't need to
         -- be, e.g. what should be done between a style rule and a at-media
         -- rule?
         --
         -- Skip whatever isn't a pair of style rules
        shouldSkip _ _ = True


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
