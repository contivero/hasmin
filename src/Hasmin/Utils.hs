{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Utils
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Utils
    ( epsilon
    , eps
    , fromLeft'
    , fromRight'
    , mconcatIntersperse
    , restrict
    , textualLength
    , replaceAt
    ) where

import Data.Monoid ((<>))
import qualified Data.Text as T

import Hasmin.Class

textualLength :: ToText a => a -> Int
textualLength = T.length . toText

restrict :: Ord a => a -> a -> a -> a
restrict minv maxv val | val >= maxv = maxv
                       | val < minv  = minv
                       | otherwise   = val

mconcatIntersperse :: Monoid b => (a -> b) -> b -> [a] -> b
mconcatIntersperse _ _ []            = mempty
mconcatIntersperse toMonoid y (x:xs) = toMonoid x <> rest
  where rest | null xs   = mempty
             | otherwise = y <> mconcatIntersperse toMonoid y xs

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index val ls = case splitAt index ls of
                           (xs, _:vs) -> xs ++ (val:vs)
                           (xs, [])   -> xs

fromRight' :: Either a b -> b
fromRight' (Right x) = x
fromRight' _         = error "fromRight'"

fromLeft' :: Either a b -> a
fromLeft' (Left x) = x
fromLeft' _        = error "fromLeft'"

-- TODO: Find out how precise is enough.
-- Can we use double and round when printing?
epsilon :: Rational
epsilon = 2.2204460492503131e-30 :: Rational

eps :: Rational
eps = 0.000001 -- See https://bug-30341-attachments.webkit.org/attachment.cgi?id=45276
