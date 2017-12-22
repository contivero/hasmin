{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Hasmin.Parser.Position where

import Control.Applicative ((<|>), optional)
import Data.Functor (($>))
import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Control.Monad (mzero)

import Hasmin.Parser.Utils
import Hasmin.Parser.PercentageLength
import Hasmin.Types.Position
import Hasmin.Types.PercentageLength

-- | Parser for <https://drafts.csswg.org/css-values-3/#position \<position\>>.
position :: Parser Position
position = perLen <|> kword
  where
    perLen = percentageLength >>= startsWithPL
    kword = do
        i <- ident
        case Map.lookup (T.toCaseFold i) keywords of
          Just x  -> skipComments *> x
          Nothing -> mzero
    keywords = Map.fromList
        [("left",   startsWith (Just PosLeft)   tb)
        ,("right",  startsWith (Just PosRight)  tb)
        ,("top",    startsWith (Just PosTop)    lr)
        ,("bottom", startsWith (Just PosBottom) lr)
        ,("center", startsWithCenter)]
    tb = (A.asciiCI "top"  $> Just PosTop,  A.asciiCI "bottom" $> Just PosBottom)
    lr = (A.asciiCI "left" $> Just PosLeft, A.asciiCI "right"  $> Just PosRight)

    startsWithPL :: PercentageLength -> Parser Position
    startsWithPL x = skipComments *>
        (followsWithPL <|> someKeyword <|> wasASinglePL)
      where
        pl = Just x
        followsWithPL = Position Nothing pl Nothing <$> (Just <$> percentageLength)
        wasASinglePL  = pure $ Position Nothing pl Nothing Nothing
        someKeyword   = do
            i <- ident
            case T.toCaseFold i of
              "center" -> pure $ Position Nothing pl (Just PosCenter) Nothing
              "top"    -> pure $ Position Nothing pl (Just PosTop) Nothing
              "bottom" -> pure $ Position Nothing pl (Just PosBottom) Nothing
              _        -> mzero

    maybePL :: Parser (Maybe PercentageLength)
    maybePL = optional percentageLength

    startsWithCenter :: Parser Position
    startsWithCenter =  followsWithPL
                    <|> followsWithAKeyword
                    <|> pure (posTillNow Nothing Nothing)
      where
        followsWithPL = (posTillNow Nothing . Just) <$> percentageLength
        followsWithAKeyword = do
            i <- ident <* skipComments
            let f x = posTillNow (Just x) <$> maybePL
            case T.toCaseFold i of
              "left"   -> f PosLeft
              "right"  -> f PosRight
              "top"    -> f PosTop
              "bottom" -> f PosBottom
              "center" -> pure $ posTillNow (Just PosCenter) Nothing
              _        -> mzero
        posTillNow = Position (Just PosCenter) Nothing

    -- Used for the cases when a position starts with the X axis (left and right
    -- keywords) or Y axis (top and bottom)
    startsWith :: Maybe PosKeyword
               -> (Parser (Maybe PosKeyword), Parser (Maybe PosKeyword))
               -> Parser Position
    startsWith x (p1, p2) = do
        pl <- optional (percentageLength <* skipComments)
        let endsWithCenter = Position x pl <$> center <*> pure Nothing
            endsWithKeywordAndMaybePL = Position x pl <$> posKeyword <*> maybePL
            endsWithPL = pure $ Position x Nothing Nothing pl
        endsWithCenter <|> endsWithKeywordAndMaybePL <|> endsWithPL
      where
        posKeyword = (p1 <|> p2)  <* skipComments
        center = A.asciiCI "center" $> Just PosCenter

