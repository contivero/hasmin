{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.Color
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-- \<color> data type.
--
-----------------------------------------------------------------------------
module Hasmin.Types.Color
  ( Color(Named)
  , mkHex3, mkHex4, mkHex6, mkHex8, mkNamed
  , mkHSL, mkHSLA, mkRGBInt, mkRGBPer, mkRGBAInt, mkRGBAPer
  , keywordColors, minifyColor
  ) where

import Control.Arrow (first)
import Control.Monad.Reader (ask)
import Data.Char (isHexDigit, digitToInt, intToDigit, toLower)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Data.Text (pack, Text)
import Data.Text.Lazy.Builder (Builder, singleton, fromText)
import Data.Word (Word8)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Hasmin.Config
import Hasmin.Types.Class
import Hasmin.Types.Numeric
import Hasmin.Utils
import Text.PrettyPrint.Mainland (Pretty, ppr, strictText, string, char, (<>), (<+>), comma, rparen)

-- | The \<color\> CSS data type. Specifications:
--
-- 4. <https://drafts.csswg.org/css-color/#colorunits  CSS Color Module Level 4>
-- 3. <https://drafts.csswg.org/css-color-3/#colorunits CSS Color Module Level 3>
-- 2. <https://www.w3.org/TR/CSS2/syndata.html#value-def-color CSS2.1>
-- 1. <https://www.w3.org/TR/CSS1/#color-units CSS1>
data Color = Hex3     Char Char Char
           | Hex4     Char Char Char Char
           | Hex6     String String String
           | Hex8     String String String String
           | Named    Text
           | RGBInt   Word8 Word8 Word8
           | RGBPer   Percentage Percentage Percentage
           | RGBAInt  Word8 Word8 Word8 Alphavalue
           | RGBAPer  Percentage Percentage Percentage Alphavalue
           | HSL      Int Percentage Percentage
           | HSLA     Int Percentage Percentage Alphavalue
  deriving (Show)

-- Equality is slightly relaxed, since we map percentages and real numbers to
-- the [0,255] integer range, and then compare, meaning:
instance Eq Color where 
  (Hex6 r1 g1 b1) == (Hex6 r2 g2 b2) =
    r1 == r2 && g1 == g2 && b1 == b2
  (Hex8 r1 g1 b1 a1) == (Hex8 r2 g2 b2 a2) =
    r1 == r2 && g1 == g2 && b1 == b2 && a1 == a2
  (Hex8 r1 g1 b1 a) == (Hex6 r2 g2 b2)
    | a == "ff" = r1 == r2 && g1 == g2 && b1 == b2
    | otherwise = False
  (Hex6 r1 g1 b1) == (Hex8 r2 g2 b2 a)
    | a == "ff" = r1 == r2 && g1 == g2 && b1 == b2
    | otherwise = False
  c1 == (Named s) = case Map.lookup (T.toLower s) colorMap of
                      Just a  -> a == c1
                      Nothing -> False
  (Named s) == c2 = case Map.lookup (T.toLower s) colorMap of
                      Just a  -> a == c2
                      Nothing -> False
  a == b = toLongHex a == toLongHex b
  
instance Ord Color where
  (Hex6 r1 g1 b1) <= (Hex6 r2 g2 b2) = 
    r1 < r2 || r1 == r2 && (g1 < g2 || (g1 == g2 && b1 <= b2))
  (Hex8 r1 g1 b1 a1) <= (Hex8 r2 g2 b2 a2) = 
    r1 < r2 || r1 == r2 && (g1 < g2 || (g1 == g2 && (b1 < b2 || (b1 == b2 && a1 <= a2))))
  (Hex8 r1 g1 b1 a) <= (Hex6 r2 g2 b2)
    | a == "ff" =  r1 < r2 || r1 == r2 && (g1 < g2 || (g1 == g2 && b1 <= b2))
    | otherwise  = True
  (Hex6 r1 g1 b1) <= (Hex8 r2 g2 b2 a)
    | a == "ff" =  r1 < r2 || r1 == r2 && (g1 < g2 || (g1 == g2 && b1 <= b2))
    | otherwise  = False
  c1 <= c2 = toLongHex c1 <= toLongHex c2

instance Pretty Color where
  ppr (Hex3 r g b)   = char '#' <> char r <> char g <> char b
  ppr (Hex4 r g b a) = char '#' <> char r <> char g <> char b <> char a
  ppr (Hex6 r g b)   = char '#' <> string r <> string g <> string b
  ppr (Hex8 r g b a) = char '#' <> string r <> string g <> string b <> string a
  ppr (Named n)      = strictText n
  ppr (RGBInt r g b) = strictText "rgb(" <> ppr r <> comma
                                        <+> ppr g <> comma
                                        <+> ppr b <> rparen
  ppr (RGBPer r g b) = strictText "rgb(" <> ppr r <> comma
                                        <+> ppr g <> comma
                                        <+> ppr b <> rparen
  ppr (RGBAInt r g b a) = strictText "rgba(" <> ppr r <> comma
                                            <+> ppr g <> comma
                                            <+> ppr b <> comma
                                            <+> ppr a <> rparen
  ppr (RGBAPer r g b a) = strictText "rgba(" <> ppr r <> comma
                                            <+> ppr g <> comma
                                            <+> ppr b <> comma
                                            <+> ppr a <> rparen
  ppr (HSL h s l) = strictText "hsl(" <> ppr h <> comma
                                     <+> ppr s <> comma
                                     <+> ppr l <> rparen
  ppr (HSLA h s l a) = strictText "hsla(" <> ppr h <> comma
                                         <+> ppr s <> comma 
                                         <+> ppr l <> comma
                                         <+> ppr a <> rparen

instance Minifiable Color where
  minifyWith c = do
      conf <- ask
      pure $ case colorSettings conf of
                ColorMinOn  -> minifyColor c
                ColorMinOff -> c

minifyColor :: Color -> Color
minifyColor c@Hex6{} = fromMaybe (toHexShorthand c) (Map.lookup c minimalColorMap)
minifyColor c@(Hex8 r g b a)
    | a == "ff" = minifyColor (Hex6 r g b)
    | otherwise = toHexShorthand c
minifyColor c@(RGBAPer r g b a)
    | a >= 1    = minifyColor (RGBPer r g b)
    | otherwise = minifyColor $ toLongHex c
minifyColor c@(RGBAInt r g b a)
    | a >= 1    = minifyColor (RGBInt r g b)
    | otherwise = minifyColor $ toLongHex c
minifyColor c@(HSLA h s l a)
    | a >= 1    = minifyColor (HSL h s l)
    | otherwise = minifyColor $ toLongHex c
minifyColor c = case toLongHex c of
                n@(Named _) -> n
                other -> minifyColor other

instance ToText Color where
  toBuilder (RGBInt r g b)    = "rgb(" <> values <> singleton ')'
    where values = toBuilderWithCommas [toText r, toText g, toText b] 
  toBuilder (RGBAInt r g b a) = "rgba(" <> values <> singleton ')'
    where values = toBuilderWithCommas [toText r, toText g, toText b, toText a] 
  toBuilder (RGBPer r g b)    = "rgb(" <> values <> singleton ')'
    where values = toBuilderWithCommas [toText r, toText g, toText b] 
  toBuilder (RGBAPer r g b a) = "rgba(" <> values <> singleton ')'
    where values = toBuilderWithCommas [toText r, toText g, toText b, toText a] 
  toBuilder (HSL h s l)       = "hsl(" <> values <> singleton ')'
    where values = toBuilderWithCommas [toText h, toText s, toText l] 
  toBuilder (HSLA h s l a)    = "hsla(" <> values <> singleton ')'
    where values = toBuilderWithCommas [toText h, toText s, toText l, toText a] 
  toBuilder (Named a)         = fromText a
  toBuilder (Hex3 r g b)      = singleton '#' <> singleton r <> singleton g <> singleton b
  toBuilder (Hex4 r g b a)    = singleton '#' <> singleton r <> singleton g <> singleton b <> singleton a
  toBuilder (Hex6 r g b)      = fromText . pack $ mconcat ["#", r, g, b]
  toBuilder (Hex8 r g b a)    = fromText . pack $ mconcat ["#", r, g, b, a]

toBuilderWithCommas :: [Text] -> Builder
toBuilderWithCommas = mconcatIntersperse fromText (singleton ',')

-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
--                              Smart constructors
-- %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mkHex3 :: Char -> Char -> Char -> Color
mkHex3 r g b 
  | isHexDigit r && isHexDigit g && isHexDigit b = Hex3 (toLower r) (toLower g) (toLower b)
  | otherwise = error "passing non hexadecimal arguments to mkHex3"

mkHex6 :: String -> String -> String -> Color
mkHex6 r g b 
  | allHex r && allHex g && allHex b = Hex6 (strToLower r) (strToLower g) (strToLower b)
  | otherwise = error "passing non hexadecimal arguments to mkHex6"

mkHex4 :: Char -> Char -> Char -> Char -> Color
mkHex4 r g b a
  | isHexDigit r && isHexDigit g && isHexDigit b && isHexDigit a = Hex4 (toLower r) (toLower g) (toLower b) (toLower a)
  | otherwise = error "passing non hexadecimal arguments to mkHex4"

mkHex8 :: String -> String -> String -> String -> Color
mkHex8 r g b a 
  | allHex r && allHex g && allHex b = Hex8 (strToLower r) (strToLower g) (strToLower b) (strToLower a)
  | otherwise = error "passing non hexadecimal arguments to mkHex6"

mkNamed :: Text -> Maybe Color
mkNamed colorName
    | Map.member name colorMap = Just (Named name)
    | otherwise                = Nothing
  where name = T.toLower colorName

mkHSL :: Int -> Percentage -> Percentage -> Color
mkHSL h s l = HSL (h `mod` 360) (bound s) (bound l)
  where bound = restrict 0 100

mkHSLA :: Int -> Percentage -> Percentage -> Alphavalue -> Color
mkHSLA h s l = HSLA (h `mod` 360) (bound s) (bound l)
  where bound = restrict 0 100

mkRGBInt :: Word8 -> Word8 -> Word8 -> Color
mkRGBInt r g b = RGBInt (bound r) (bound g) (bound b)
  where bound = restrict 0 255

mkRGBPer :: Percentage -> Percentage -> Percentage -> Color
mkRGBPer r g b = RGBPer (bound r) (bound g) (bound b)
  where bound = restrict 0 100

mkRGBAInt :: Word8 -> Word8 -> Word8 -> Alphavalue -> Color
mkRGBAInt r g b = RGBAInt (bound r) (bound g) (bound b)
  where bound = restrict 0 255

mkRGBAPer :: Percentage -> Percentage -> Percentage -> Alphavalue -> Color
mkRGBAPer r g b =  RGBAPer (bound r) (bound g) (bound b)
  where bound = restrict 0 100

allHex :: String -> Bool
allHex = all isHexDigit

strToLower :: String -> String
strToLower = map toLower
-------------------------------------------------------------------------------
toHexShorthand :: Color -> Color
toHexShorthand c@(Hex6 [r1,r2] [g1,g2] [b1,b2]) 
  | r1 == r2 && g1 == g2 && b1 == b2 = Hex3 r1 g1 b1
  | otherwise                        = c
toHexShorthand c@(Hex8 [r1,r2] [g1,g2] [b1,b2] [a1,a2])
  | r1 == r2 && g1 == g2 && b1 == b2 && a1 == a2 = Hex4 r1 g1 b1 a1
  | otherwise                                    = c
toHexShorthand h = h

-- Returns hexadecimal equivalent as a string of two characters 
-- (i.e. for values in the range [0,15], a leading zero is added).
word8ToHex :: Word8 -> String
word8ToHex n | 0 <= n && n < 16 = '0':[intToDigit num]
             | otherwise        = intToDigit sndRemainder : [intToDigit fstRemainder]
  where num = fromIntegral n
        fstRemainder = num `mod` 16
        sndRemainder = (num `quot` 16) `mod` 16

-- Takes a color to a Hex6 or Hex8 representation unless it's an invalid
-- keyword, in which case it remains the same
toLongHex :: Color -> Color
toLongHex c@(Named s)         = fromMaybe c (Map.lookup (T.toLower s) colorMap)
toLongHex (RGBAInt r g b a)   = Hex8 (word8ToHex r) (word8ToHex g) (word8ToHex b) (ratToHex a)
  where ratToHex :: Alphavalue -> String
        ratToHex n = word8ToHex . round $ toRational n * 255
toLongHex (RGBInt r g b) = Hex6 (word8ToHex r) (word8ToHex g) (word8ToHex b)
toLongHex c@RGBPer{}     = toLongHex $ toRGBAInt c
toLongHex c@RGBAPer{}    = toLongHex $ toRGBAInt c
toLongHex c@HSL{}        = toLongHex $ toRGBAInt c
toLongHex c@HSLA{}       = toLongHex $ toRGBAInt c
toLongHex (Hex3 r g b)   = Hex6 [r,r] [g,g] [b,b]
toLongHex (Hex4 r g b a) = Hex8 [r,r] [g,g] [b,b] [a,a]
toLongHex a              = a

-- This fold works in general to convert hexadecimals into Integers, but we
-- only need it for Word8
hexToWord8 :: String -> Word8 
hexToWord8 = fromIntegral . foldl (\s c -> s*16 + digitToInt c) 0

toRGBAInt :: Color -> Color
toRGBAInt (Named s) = case Map.lookup (T.toLower s) colorMap of
                        Just a  -> toRGBAInt a
                        Nothing -> error "Invalid color keyword. Can't convert to rgba"
toRGBAInt (Hex3 r g b) = RGBAInt (f [r,r]) (f [g,g]) (f [b,b]) 1
  where f = fromIntegral . hexToWord8
toRGBAInt (Hex6 r g b) = RGBAInt (hexToWord8 r) (hexToWord8 g) (hexToWord8 b) 1
toRGBAInt (Hex4 r g b a) = RGBAInt (f [r,r]) (f [g,g]) (f [b,b]) (h [a,a])
  where f = fromIntegral . hexToWord8
        h = toAlphavalue . hexToWord8
toRGBAInt (Hex8 r g b a) = RGBAInt (hexToWord8 r) (hexToWord8 g) 
                                   (hexToWord8 b) (toAlphavalue $ toRational (hexToWord8 a) / 255) 
toRGBAInt (RGBInt r g b) = RGBAInt r g b 1
toRGBAInt (RGBPer r g b) = RGBAInt (f r) (f g) (f b) 1
  where f = round . (2.55*)
toRGBAInt (RGBAPer r g b a) = RGBAInt (f r) (f g) (f b) a
  where f = round . (2.55*)
toRGBAInt c@RGBAInt{}    = c
toRGBAInt (HSL h s l)    = withAlpha 1 $ hslToRgb (h, s, l)
toRGBAInt (HSLA h s l a) = withAlpha a $ hslToRgb (h, s, l)

withAlpha :: Alphavalue -> (Word8, Word8, Word8) -> Color
withAlpha a (r, g, b) = RGBAInt r g b a

hslToRgb :: (Int, Percentage, Percentage) -> (Word8, Word8, Word8)
hslToRgb (hue, sat, light) | s == 0    = (lumToRgb, lumToRgb, lumToRgb)
                           | l <= 0.5  = hslToRgb' h l (l * (s+1))
                           | otherwise = hslToRgb' h l (l + s - l*s)
  where h = toPercentage hue / 360
        s = sat / 100
        l = light / 100
        lumToRgb = round (l * 255)

hslToRgb' :: Percentage -> Percentage -> Percentage -> (Word8, Word8, Word8)
hslToRgb' h l t2 = (r, g, b)
  where t1 = l*2 - t2
        r = round $ 255 * hueToRgb t1 t2 (h + Percentage (1 % 3))
        g = round $ 255 * hueToRgb t1 t2 h
        b = round $ 255 * hueToRgb t1 t2 (h - Percentage (1 % 3))

hueToRgb :: Percentage -> Percentage -> Percentage -> Percentage
hueToRgb t1 t2 hue | hue < 0   = test t1 t2 (hue+1)
                   | hue > 1   = test t1 t2 (hue-1)
                   | otherwise = test t1 t2 hue
  where test :: Percentage -> Percentage -> Percentage -> Percentage
        test a b h | h * 6 < 1 = a + (b-a) * 6 * h
                   | h * 2 < 1 = b
                   | h * 3 < 2 = a + (b-a) * (Percentage (2 % 3) - h) * 6
                   | otherwise = a

-- | A map with hex values as keys, and their minimal colorname as value
minimalColorMap :: Map.Map Color Color
minimalColorMap = Map.fromList minimalColors

minimalColors :: [(Color, Color)]
minimalColors = [(Hex6 "ff" "00" "00", Named "red")
                ,(Hex6 "d2" "b4" "8c", Named "tan")
                ,(Hex6 "00" "ff" "ff", Named "aqua")
                ,(Hex6 "00" "00" "ff", Named "blue")
                ,(Hex6 "00" "ff" "ff", Named "cyan")
                ,(Hex6 "ff" "d7" "00", Named "gold")
                ,(Hex6 "80" "80" "80", Named "gray")
                ,(Hex6 "80" "80" "80", Named "grey")
                ,(Hex6 "00" "ff" "00", Named "lime")
                ,(Hex6 "00" "00" "80", Named "navy")
                ,(Hex6 "cd" "85" "3f", Named "peru")
                ,(Hex6 "ff" "c0" "cb", Named "pink")
                ,(Hex6 "dd" "a0" "dd", Named "plum")
                ,(Hex6 "ff" "fa" "fa", Named "snow")
                ,(Hex6 "00" "80" "80", Named "teal")
                ,(Hex6 "f0" "ff" "ff", Named "azure")
                ,(Hex6 "f5" "f5" "dc", Named "beige")
                ,(Hex6 "a5" "2a" "2a", Named "brown")
                ,(Hex6 "ff" "7f" "50", Named "coral")
                ,(Hex6 "00" "80" "00", Named "green")
                ,(Hex6 "ff" "ff" "f0", Named "ivory")
                ,(Hex6 "f0" "e6" "8c", Named "khaki")
                ,(Hex6 "fa" "f0" "e6", Named "linen")
                ,(Hex6 "80" "80" "00", Named "olive")
                ,(Hex6 "f5" "de" "b3", Named "wheat")
                ,(Hex6 "ff" "e4" "c4", Named "bisque")
                ,(Hex6 "4b" "00" "82", Named "indigo")
                ,(Hex6 "80" "00" "00", Named "maroon")
                ,(Hex6 "ff" "a5" "00", Named "orange")
                ,(Hex6 "da" "70" "d6", Named "orchid")
                ,(Hex6 "80" "00" "80", Named "purple")
                ,(Hex6 "fa" "80" "72", Named "salmon")
                ,(Hex6 "a0" "52" "2d", Named "sienna")
                ,(Hex6 "c0" "c0" "c0", Named "silver")
                ,(Hex6 "ff" "63" "47", Named "tomato")
                ,(Hex6 "ee" "82" "ee", Named "violet")]

-- | Mapping between color names and hex values
colorMap :: Map.Map Text Color
colorMap = Map.fromList keywordColors

keywordColors :: [(Text, Color)]
keywordColors = map (first T.toLower) 
  [("aliceblue",            Hex6 "f0" "f8" "ff")
  ,("antiquewhite",         Hex6 "fa" "eb" "d7")
  ,("aqua",                 Hex6 "00" "ff" "ff")
  ,("aquamarine",           Hex6 "7f" "ff" "d4")
  ,("azure",                Hex6 "f0" "ff" "ff")
  ,("beige",                Hex6 "f5" "f5" "dc")
  ,("bisque",               Hex6 "ff" "e4" "c4")
  ,("black",                Hex6 "00" "00" "00")
  ,("blanchedalmond",       Hex6 "ff" "eb" "cd")
  ,("blue",                 Hex6 "00" "00" "ff")
  ,("blueviolet",           Hex6 "8a" "2b" "e2")
  ,("brown",                Hex6 "a5" "2a" "2a")
  ,("burlywood",            Hex6 "de" "b8" "87")
  ,("cadetblue",            Hex6 "5f" "9e" "a0")
  ,("chartreuse",           Hex6 "7f" "ff" "00")
  ,("chocolate",            Hex6 "d2" "69" "1e")
  ,("coral",                Hex6 "ff" "7f" "50")
  ,("cornflowerblue",       Hex6 "64" "95" "ed")
  ,("cornsilk",             Hex6 "ff" "f8" "dc")
  ,("crimson",              Hex6 "dc" "14" "3c")
  ,("cyan",                 Hex6 "00" "ff" "ff")
  ,("darkblue",             Hex6 "00" "00" "8b")
  ,("darkcyan",             Hex6 "00" "8b" "8b")
  ,("darkgoldenrod",        Hex6 "b8" "86" "0b")
  ,("darkgray",             Hex6 "a9" "a9" "a9")
  ,("darkgrey",             Hex6 "a9" "a9" "a9")
  ,("darkgreen",            Hex6 "00" "64" "00")
  ,("darkkhaki",            Hex6 "bd" "b7" "6b")
  ,("darkmagenta",          Hex6 "8b" "00" "8b")
  ,("darkolivegreen",       Hex6 "55" "6b" "2f")
  ,("darkorange",           Hex6 "ff" "8c" "00")
  ,("darkorchid",           Hex6 "99" "32" "cc")
  ,("darkred",              Hex6 "8b" "00" "00")
  ,("darksalmon",           Hex6 "e9" "96" "7a")
  ,("darkseagreen",         Hex6 "8f" "bc" "8f")
  ,("darkslateblue",        Hex6 "48" "3d" "8b")
  ,("darkslategray",        Hex6 "2f" "4f" "4f")
  ,("darkslategrey",        Hex6 "2f" "4f" "4f")
  ,("darkturquoise",        Hex6 "00" "ce" "d1")
  ,("darkviolet",           Hex6 "94" "00" "d3")
  ,("deeppink",             Hex6 "ff" "14" "93")
  ,("deepskyblue",          Hex6 "00" "bf" "ff")
  ,("dimgray",              Hex6 "69" "69" "69")
  ,("dimgrey",              Hex6 "69" "69" "69")
  ,("dodgerblue",           Hex6 "1e" "90" "ff")
  ,("firebrick",            Hex6 "b2" "22" "22")
  ,("floralwhite",          Hex6 "ff" "fa" "f0")
  ,("forestgreen",          Hex6 "22" "8b" "22")
  ,("fuchsia",              Hex6 "ff" "00" "ff")
  ,("gainsboro",            Hex6 "dc" "dc" "dc")
  ,("ghostwhite",           Hex6 "f8" "f8" "ff")
  ,("gold",                 Hex6 "ff" "d7" "00")
  ,("goldenrod",            Hex6 "da" "a5" "20")
  ,("gray",                 Hex6 "80" "80" "80")
  ,("grey",                 Hex6 "80" "80" "80")
  ,("green",                Hex6 "00" "80" "00")
  ,("greenyellow",          Hex6 "ad" "ff" "2f")
  ,("honeydew",             Hex6 "f0" "ff" "f0")
  ,("hotpink",              Hex6 "ff" "69" "b4")
  ,("indianred",            Hex6 "cd" "5c" "5c")
  ,("indigo",               Hex6 "4b" "00" "82")
  ,("ivory",                Hex6 "ff" "ff" "f0")
  ,("khaki",                Hex6 "f0" "e6" "8c")
  ,("lavender",             Hex6 "e6" "e6" "fa")
  ,("lavenderblush",        Hex6 "ff" "f0" "f5")
  ,("lawngreen",            Hex6 "7c" "fc" "00")
  ,("lemonchiffon",         Hex6 "ff" "fa" "cd")
  ,("lightblue",            Hex6 "ad" "d8" "e6")
  ,("lightcoral",           Hex6 "f0" "80" "80")
  ,("lightcyan",            Hex6 "e0" "ff" "ff")
  ,("lightgoldenrodyellow", Hex6 "fa" "fa" "d2")
  ,("lightgray",            Hex6 "d3" "d3" "d3")
  ,("lightgrey",            Hex6 "d3" "d3" "d3")
  ,("lightgreen",           Hex6 "90" "ee" "90")
  ,("lightpink",            Hex6 "ff" "b6" "c1")
  ,("lightsalmon",          Hex6 "ff" "a0" "7a")
  ,("lightseagreen",        Hex6 "20" "b2" "aa")
  ,("lightskyblue",         Hex6 "87" "ce" "fa")
  ,("lightslategray",       Hex6 "77" "88" "99")
  ,("lightslategrey",       Hex6 "77" "88" "99")
  ,("lightsteelblue",       Hex6 "b0" "c4" "de")
  ,("lightyellow",          Hex6 "ff" "ff" "e0")
  ,("lime",                 Hex6 "00" "ff" "00")
  ,("limegreen",            Hex6 "32" "cd" "32")
  ,("linen",                Hex6 "fa" "f0" "e6")
  ,("magenta",              Hex6 "ff" "00" "ff")
  ,("maroon",               Hex6 "80" "00" "00")
  ,("mediumaquamarine",     Hex6 "66" "cd" "aa")
  ,("mediumblue",           Hex6 "00" "00" "cd")
  ,("mediumorchid",         Hex6 "ba" "55" "d3")
  ,("mediumpurple",         Hex6 "93" "70" "d8")
  ,("mediumseagreen",       Hex6 "3c" "b3" "71")
  ,("mediumslateblue",      Hex6 "7b" "68" "ee")
  ,("mediumspringgreen",    Hex6 "00" "fa" "9a")
  ,("mediumturquoise",      Hex6 "48" "d1" "cc")
  ,("mediumvioletred",      Hex6 "c7" "15" "85")
  ,("midnightblue",         Hex6 "19" "19" "70")
  ,("mintcream",            Hex6 "f5" "ff" "fa")
  ,("mistyrose",            Hex6 "ff" "e4" "e1")
  ,("moccasin",             Hex6 "ff" "e4" "b5")
  ,("navajowhite",          Hex6 "ff" "de" "ad")
  ,("navy",                 Hex6 "00" "00" "80")
  ,("oldlace",              Hex6 "fd" "f5" "e6")
  ,("olive",                Hex6 "80" "80" "00")
  ,("olivedrab",            Hex6 "6b" "8e" "23")
  ,("orange",               Hex6 "ff" "a5" "00")
  ,("orangered",            Hex6 "ff" "45" "00")
  ,("orchid",               Hex6 "da" "70" "d6")
  ,("palegoldenrod",        Hex6 "ee" "e8" "aa")
  ,("palegreen",            Hex6 "98" "fb" "98")
  ,("paleturquoise",        Hex6 "af" "ee" "ee")
  ,("palevioletred",        Hex6 "d8" "70" "93")
  ,("papayawhip",           Hex6 "ff" "ef" "d5")
  ,("peachpuff",            Hex6 "ff" "da" "b9")
  ,("peru",                 Hex6 "cd" "85" "3f")
  ,("pink",                 Hex6 "ff" "c0" "cb")
  ,("plum",                 Hex6 "dd" "a0" "dd")
  ,("powderblue",           Hex6 "b0" "e0" "e6")
  ,("purple",               Hex6 "80" "00" "80")
  ,("red",                  Hex6 "ff" "00" "00")
  ,("rosybrown",            Hex6 "bc" "8f" "8f")
  ,("royalblue",            Hex6 "41" "69" "e1")
  ,("saddlebrown",          Hex6 "8b" "45" "13")
  ,("salmon",               Hex6 "fa" "80" "72")
  ,("sandybrown",           Hex6 "f4" "a4" "60")
  ,("seagreen",             Hex6 "2e" "8b" "57")
  ,("seashell",             Hex6 "ff" "f5" "ee")
  ,("sienna",               Hex6 "a0" "52" "2d")
  ,("silver",               Hex6 "c0" "c0" "c0")
  ,("skyblue",              Hex6 "87" "ce" "eb")
  ,("slateblue",            Hex6 "6a" "5a" "cd")
  ,("slategray",            Hex6 "70" "80" "90")
  ,("slategrey",            Hex6 "70" "80" "90")
  ,("snow",                 Hex6 "ff" "fa" "fa")
  ,("springgreen",          Hex6 "00" "ff" "7f")
  ,("steelblue",            Hex6 "46" "82" "b4")
  ,("tan",                  Hex6 "d2" "b4" "8c")
  ,("teal",                 Hex6 "00" "80" "80")
  ,("thistle",              Hex6 "d8" "bf" "d8")
  ,("transparent",          Hex8 "00" "00" "00" "00")
  ,("tomato",               Hex6 "ff" "63" "47")
  ,("turquoise",            Hex6 "40" "e0" "d0")
  ,("violet",               Hex6 "ee" "82" "ee")
  ,("wheat",                Hex6 "f5" "de" "b3")
  ,("white",                Hex6 "ff" "ff" "ff")
  ,("whitesmoke",           Hex6 "f5" "f5" "f5")
  ,("yellow",               Hex6 "ff" "ff" "00")
  ,("yellowgreen",          Hex6 "9a" "cd" "32")]
