{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.Stylesheet
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Hasmin.Types.Stylesheet (
    Expression(..), MediaQuery(..), Rule(..), KeyframeSelector(..),
    KeyframeBlock(..), isEmpty
    ) where

import Control.Monad.Reader
import Data.Monoid ((<>))
import Data.Text (Text) 
import Data.Text.Lazy.Builder (singleton, fromText)
import Data.List (sortBy, (\\))
import Hasmin.Config
import Hasmin.Selector
import Hasmin.Types.Class
import Hasmin.Types.Value
import Hasmin.Types.Declaration
import Hasmin.Types.String
import Hasmin.Types.Numeric
import Hasmin.Utils
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import Text.PrettyPrint.Mainland (Pretty, ppr, strictText,
  lbrace, rbrace, (</>), (<+>), comma, folddoc, nest, semi, stack, char)

data MediaQuery = MediaQuery1 Text Text [Expression]  -- ^ First possibility in the grammar
                | MediaQuery2 [Expression] -- ^ Second possibility in the grammar
  deriving (Show, Eq)
instance Minifiable MediaQuery where
  minifyWith (MediaQuery1 t1 t2 es) = fmap (MediaQuery1 t1 t2) (mapM minifyWith es)
  minifyWith (MediaQuery2 es)       = fmap MediaQuery2 (mapM minifyWith es)

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
  minifyWith (Expression t mv) = fmap (Expression t) (mapM minifyWith mv)
  minifyWith x = pure x
instance ToText Expression where
  toBuilder (Expression t mv) = 
      singleton '(' <> fromText t <> v <> singleton ')'
    where v = maybe mempty (\x -> singleton ':' <> toBuilder x) mv
  toBuilder (InvalidExpression t) = 
      singleton '(' <> fromText t <> singleton ')'
  
type Comment = Maybe Text
data Stylesheet = Stylesheet Comment [Rule]
  deriving (Show)

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
minifyKFS From = KFPercentage $ Percentage 0
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
  minifyWith (AtMedia mqs rs) = do
      newMqs <- mapM minifyWith mqs
      newRs  <- mapM minifyWith rs
      pure $ AtMedia newMqs newRs
  minifyWith (AtKeyframes vp n bs) = do
      blocks <- mapM minifyWith bs
      pure $ AtKeyframes vp n blocks
  minifyWith (AtBlockWithRules t rs) = do
      rules <- mapM minifyWith rs
      pure $ AtBlockWithRules t rules 
  minifyWith (AtBlockWithDec t ds) = do
      conf <- ask
      decs <- cleanRule ds >>= compactLonghands >>= mapM minifyWith
      pure $ AtBlockWithDec t decs 
  minifyWith (StyleRule ss ds) = do
      conf <- ask
      dcs <- cleanRule ds >>= compactLonghands >>= mapM minifyWith
      let decs = if shouldSortProperties conf
                    then sortBy lexico dcs
                    else dcs
      sels <- mapM minifyWith ss >>= removeDuplicateSelectors >>= sortSelectors
      pure $ StyleRule sels decs
  minifyWith (AtImport esu mqs) = fmap (AtImport esu) (mapM minifyWith mqs)
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
    pure $ if shouldSortSelectors conf
              then sortBy lexico sls
              else sls

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
