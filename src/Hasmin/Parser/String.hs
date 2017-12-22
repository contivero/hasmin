{-# LANGUAGE OverloadedStrings #-}

module Hasmin.Parser.String
    ( convertEscaped
    , unquotedURL
    , fontfamilyname
    ) where

import Data.Attoparsec.Text (Parser)
import Control.Applicative (liftA2, (<|>), many)
import Data.Monoid ((<>))
import Control.Monad (mzero)
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Bits ((.|.), shiftL)
import Data.Foldable (foldl')

import Hasmin.Parser.Primitives
import Hasmin.Parser.Utils

-- | Parse a URL without enclosing quotes.
-- See <https://drafts.csswg.org/css-syntax-3/#consume-a-url-token §4.3.6. Consume a url token>
unquotedURL :: Parser Text
unquotedURL = do
    t <- many (escape <|> (B.singleton <$> A.satisfy validChar))
    pure $ TL.toStrict (B.toLazyText (mconcat t))
  where validChar x = x /= '\"' && x /= '\'' && x /= '(' && x /= ')'
                   && x /= '\\' && notWhitespace x && notNonprintable x
        notWhitespace x = x /= '\n' &&  x /= '\t' && x /= ' '
        notNonprintable x = not (C.chr 0 <= x && x <= C.chr 8)
                         && x /= '\t'
                         && not ('\SO' <= x && x <= C.chr 31)
                         && x /= '\DEL'

-- TODO can the Parser be avoided by a fold, or one of the provided library
-- functions? Apart from being cleaner, doing so would simplify other functions.
-- | Parse and convert any escaped unicode to its underlying Char.
convertEscaped :: Parser Text
convertEscaped = (TL.toStrict . B.toLazyText) <$> go
  where
    go = do
        nonescapedText <- B.fromText <$> A.takeWhile (/= '\\')
        cont nonescapedText <|> pure nonescapedText
    cont b = do
        _ <- A.char '\\'
        c <- A.peekChar
        case c of
          Just _  -> parseEscapedAndContinue b
          Nothing -> pure (b <> B.singleton '\\')

    parseEscapedAndContinue :: Builder -> Parser Builder
    parseEscapedAndContinue b = do
        u8 <- utf8
        ((b <> u8) <>) <$>  go

    utf8 :: Parser Builder
    utf8 = do
        mch <- atMost 6 hexadecimal
        pure $ maybe ("\\" <> B.fromString mch) B.singleton (hexToChar mch)

    -- Interpret a hexadecimal string as a decimal Int, and convert it into the
    -- corresponding Char.
    hexToChar :: [Char] -> Maybe Char
    hexToChar xs
        | i > maxChar = Nothing
        | otherwise   = Just (C.chr i)
      where i = foldl' step 0 xs
            maxChar = fromEnum (maxBound :: Char)
            step a c
                | w - 48 < 10 = (a `shiftL` 4) .|. fromIntegral (w - 48)
                | w >= 97     = (a `shiftL` 4) .|. fromIntegral (w - 87)
                | otherwise   = (a `shiftL` 4) .|. fromIntegral (w - 55)
              where w = C.ord c

    atMost :: Int -> Parser a -> Parser [a]
    atMost 0 _ = pure []
    atMost n p = A.option [] $ liftA2 (:) p (atMost (n-1) p)

fontfamilyname :: Parser Text
fontfamilyname = do
    i  <- ident
    is <- many (skipComments *> ident)
    if T.toLower i `elem` invalidNames
       then mzero
       else pure $ i <> foldMap (" "<>) is
  where invalidNames = ["serif", "sans-serif", "monospace", "cursive",
                        "fantasy", "inherit", "initial", "unset", "default"]

