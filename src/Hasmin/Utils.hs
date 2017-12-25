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
    , mzip
    , reduceTRBL
    ) where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty((:|)))

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

-- Why does Attoparsec have no instance of MonadZip? :«
mzip :: Applicative f => f a -> f b -> f (a, b)
mzip f g = (,) <$> f <*> g

reduceTRBL :: Eq a => NonEmpty a -> NonEmpty a
reduceTRBL xs =
    case xs of
      t:|[r,b,l] -> reduce4 t r b l
      t:|[r,b]   -> reduce3 t r b
      t:|[r]     -> reduce2 t r
      _          -> xs
  where reduce4 tv rv bv lv
            | lv == rv  = reduce3 tv rv bv
            | otherwise = xs
        reduce3 tv rv bv
            | tv == bv  = reduce2 tv rv
            | otherwise = tv:|[rv, bv]
        reduce2 tv rv
            | tv == rv  = tv:|[]
            | otherwise = tv:|[rv]
