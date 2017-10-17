{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.Value
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Parsers for CSS values.
--
-----------------------------------------------------------------------------
module Hasmin.Parser.Value (
      valuesFor
    , valuesFallback
    , value
    , valuesInParens
    , stringOrUrl
    , percentage
    , url
    , stringtype
    , textualvalue
    , stringvalue    -- used in StringSpec
    , shadowList     -- used in ShadowSpec 
    , timingFunction
    , repeatStyle
    , position
    , color
    , number
    , fontStyle
    ) where

import Control.Applicative ((<|>), many, liftA3)
import Control.Arrow (first, (&&&))
import Control.Monad (mzero)
import Data.Functor (($>))
import Data.Attoparsec.Text (asciiCI, char, count, option, Parser, satisfy,
       skipSpace, string)
import Data.Map.Strict (Map)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Word (Word8)
import Data.Char (isAscii)
import Text.Parser.Permutation ((<|?>), (<$$>), (<$?>), (<||>), permute)
import Numeric (readSigned, readFloat)
import qualified Data.Set as Set
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import qualified Data.Text as T

import Hasmin.Parser.Utils
import Hasmin.Types.BgSize
import Hasmin.Types.Class
import Hasmin.Types.Color
import Hasmin.Types.Dimension
import Hasmin.Types.FilterFunction
import Hasmin.Types.Gradient
import Hasmin.Types.Numeric
import Hasmin.Types.PercentageLength
import Hasmin.Types.Position
import Hasmin.Types.RepeatStyle
import Hasmin.Types.Shadow
import Hasmin.Types.String
import Hasmin.Types.TimingFunction
import Hasmin.Types.TransformFunction
import Hasmin.Types.Value

-- | Given a propery name, it returns a specific parser of values for that
-- property. Fails if no specific parser is found.
valuesFor :: Text -> Parser Values
valuesFor propName =
  case Map.lookup (T.toLower propName) propertyValueParsersMap of
    Just x  -> x <* skipComments
    Nothing -> mzero

-- | Parser for <https://www.w3.org/TR/css-values-3/#numbers \<number\>>.
number :: Parser Number
number = Number <$> rational

-- ---------------------------------------------------------------------------
-- Color Parsers
-- ---------------------------------------------------------------------------

-- Assumes "rgb(" has already been read
rgb :: Parser Color
rgb = functionParser (rgbInt <|> rgbPer)
  where rgbInt = mkRGBInt <$> word8 <* comma <*> word8 <* comma <*> word8
        rgbPer = mkRGBPer <$> percentage <* comma
                          <*> percentage <* comma <*> percentage

-- Assumes "rgba(" has already been read
rgba :: Parser Color
rgba = functionParser (rgbaInt <|> rgbaPer)
  where rgbaInt = mkRGBAInt <$> word8 <* comma <*> word8 <* comma
                            <*> word8 <* comma <*> alphavalue
        rgbaPer = mkRGBAPer <$> percentage <* comma <*> percentage <* comma
                            <*> percentage <* comma <*> alphavalue

-- Assumes "hsl(" has already been read
hsl :: Parser Color
hsl = functionParser p
  where p = mkHSL <$> int <* comma <*> percentage <* comma <*> percentage

-- Assumes "hsla(" has already been read
hsla :: Parser Color
hsla = functionParser p
  where p = mkHSLA <$> int <* comma <*> percentage <* comma
                   <*> percentage <* comma <*> alphavalue

alphavalue :: Parser Alphavalue
alphavalue = mkAlphavalue <$> rational

hexvalue :: Parser Value
hexvalue = ColorV <$> hex

hex :: Parser Color
hex = do
    _ <- char '#'
    a <- hexadecimal
    b <- hexadecimal
    c <- hexadecimal
    x <- optional hexadecimal
    case x of
      Nothing -> pure $ mkHex3 a b c
      Just d  -> do y <- optional hexadecimal
                    case y of
                      Nothing -> pure $ mkHex4 a b c d
                      Just e  -> do f <- hexadecimal
                                    z <- optional hexadecimal
                                    case z of
                                      Nothing -> pure $ mkHex6 [a,b] [c,d] [e,f]
                                      Just g  -> do h <- hexadecimal
                                                    pure $ mkHex8 [a,b] [c,d] [e,f] [g,h]
  where optional w  = option Nothing (Just <$> w)
        hexadecimal = satisfy C.isHexDigit

-- ---------------------------------------------------------------------------
-- Dimensions Parsers
-- ---------------------------------------------------------------------------

-- A map relating dimension units and the percentage symbol,
-- to functions that construct that value. Meant to unify all the numerical
-- parsing in a single parse for generality without losing much efficiency.
-- See numericalvalue.
numericalConstructorsMap :: Map Text (Number -> Value)
numericalConstructorsMap   = Map.fromList $ fmap (first T.toCaseFold) l
  where durationFunc u v   = DurationV (Duration v u)
        frequencyFunc u v  = FrequencyV (Frequency v u)
        resolutionFunc u v = ResolutionV (Resolution v u)
        l = [("s",    durationFunc S)
            ,("ms",   durationFunc Ms)
            ,("hz",   frequencyFunc Hz)
            ,("khz",  frequencyFunc Khz)
            ,("dpi",  resolutionFunc Dpi)
            ,("dpcm", resolutionFunc Dpcm)
            ,("dppx", resolutionFunc Dppx)
            ,("%", \x -> PercentageV (Percentage $ toRational x))
            ] ++ (fmap . fmap) (DistanceV .) distanceConstructorsList
              ++ (fmap . fmap) (AngleV .) angleConstructorsList

-- Unified numerical parser.
-- Parses <number>, dimensions (i.e. <length>, <angle>, ...), and <percentage>
numericalvalue :: Parser Value
numericalvalue = do
    n    <- number
    rest <- opt (string "%" <|> A.takeWhile1 C.isAlpha)
    if T.null rest  -- if true, then it was just a <number> value
       then pure $ NumberV n
       else case Map.lookup (T.toCaseFold rest) numericalConstructorsMap of
              Just f  -> pure $ f n
              Nothing -> mzero -- TODO see if we should return an "Other" value

-- Create a numerical parser based on a Map.
-- See for instance, the "angle" parser
dimensionParser :: Map Text (Number -> a) -> a -> Parser a
dimensionParser m unitlessValue = do
    n <- number
    u <- opt (A.takeWhile1 C.isAlpha)
    if T.null u
       then if n == 0
               then pure unitlessValue -- <angle> 0, without units
               else mzero -- Non-zero <number>, fail
       else case Map.lookup (T.toCaseFold u) m of
              Just f  -> pure $ f n
              Nothing -> mzero -- parsed units aren't angle units, fail

distance :: Parser Distance
distance = dimensionParser distanceConstructorsMap (Distance 0 Q)
  where distanceConstructorsMap = Map.fromList distanceConstructorsList

angle :: Parser Angle
angle = dimensionParser angleConstructorsMap (Angle 0 Deg)
  where angleConstructorsMap = Map.fromList angleConstructorsList

duration :: Parser Duration
duration = do
    n <- number
    u <- opt (A.takeWhile1 C.isAlpha)
    if T.null u
       then mzero
       else case Map.lookup (T.toCaseFold u) durationConstructorsMap of
              Just f  -> pure $ f n
              Nothing -> mzero -- parsed units aren't angle units, fail
  where durationConstructorsMap = Map.fromList $
            fmap (toText &&& flip Duration) [S, Ms]

angleConstructorsList :: [(Text, Number -> Angle)]
angleConstructorsList = fmap (toText &&& flip Angle)
    [Deg, Grad, Rad, Turn]

distanceConstructorsList :: [(Text, Number -> Distance)]
distanceConstructorsList = fmap (toText &&& flip Distance)
    [EM, EX, CH, VH, VW, VMIN, VMAX, REM, Q, CM, MM, IN, PC, PT, PX]

-- | Parser for <https://drafts.csswg.org/css-values-3/#percentages \<percentage\>>.
percentage :: Parser Percentage
percentage = Percentage <$> rational <* char '%'

-- ---------------------------------------------------------------------------
-- Primitives
-- ---------------------------------------------------------------------------

-- | \<integer\> data type parser, but into a String instead of an Int, for other
-- parsers to use (e.g.: see the parsers int, or rational)
int' :: Parser String
int' = do
  sign <- option '+' (char '-' <|> char '+')
  d    <- digits
  case sign of
    '+' -> pure d
    '-' -> pure (sign:d)
    _   -> error "int': parsed a number starting with other than [+|-]"

-- | Parser for \<integer\>: [+|-][0-9]+
int :: Parser Int
int = read <$> int'

word8 :: Parser Word8
word8 = read <$> digits

-- Note that many properties that allow an integer or real number as a value
-- actually restrict the value to some range, often to a non-negative value.
--
-- | Real number parser. \<number\>: <'int' integer> | [0-9]*.[0-9]+
rational :: Parser Rational
rational = do
    sign <- option [] (wrapMinus <$> (char '-' <|> char '+'))
    dgts <- ((++) <$> digits <*> option "" fractionalPart)
           <|> ("0"++) <$> fractionalPart -- append a zero for read not to fail
    e <- option [] expo
    pure . fst . head $ readSigned readFloat (sign ++ dgts ++ e)
  where fractionalPart = (:) <$> char '.' <*> digits
        expo = (:) <$> satisfy (\c -> c == 'e' || c == 'E') <*> int'
        wrapMinus x = [x | x == '-'] -- we use this since read fails with leading '+'

-- | Parser for <https://drafts.csswg.org/css-fonts-3/#propdef-font-style \<font-style\>>,
-- used in the @font-style@ and @font@ properties.
fontStyle :: Parser Value
fontStyle = do
    k <- ident
    if Set.member k keywords
       then pure $ mkOther k
       else mzero
  where keywords = Set.fromList ["normal", "italic", "oblique"]

{-
fontWeight :: Parser Value
fontWeight = do
    k <- ident
    if Set.member k keywords
       then pure $ mkOther k
       else mzero
  where keywords = Set.fromList ["normal", "bold", "lighter", "bolder"]
        validNumbers = Set.fromList [100, 200, 300, 400, 500, 600, 700, 800, 900]
-}

fontSize :: Parser Value
fontSize = fontSizeKeyword
        <|> (DistanceV <$> distance)
        <|> (PercentageV <$> percentage)
  where fontSizeKeyword = do
            v1 <- ident
            if Set.member v1 keywords
               then pure $ mkOther v1
               else mzero
        keywords = Set.fromList ["medium", "xx-small", "x-small", "small"
                                ,"large", "x-large", "xx-large", "smaller"
                                ,"larger"]

{- [ [ <‘font-style’> || <font-variant-css21> || <‘font-weight’> ||
 - <‘font-stretch’> ]? <‘font-size’> [ / <‘line-height’> ]? <‘font-family’> ] |
 - caption | icon | menu | message-box | small-caption | status-bar
where <font-variant-css21> = [normal | small-caps]

-}

-- TODO clean parsers pos1, pos2, and pos4
positionvalue :: Parser Value
positionvalue = PositionV <$> position

-- | Parser for <https://drafts.csswg.org/css-values-3/#position \<position\>>.
position :: Parser Position
position = pos4 <|> pos2 <|> pos1

pos1 :: Parser Position
pos1 =  (asciiCI "left" $> f (Just PosLeft))
    <|> (asciiCI "center" $> f (Just PosCenter))
    <|> (asciiCI "right" $> f (Just PosRight))
    <|> (asciiCI "top" $> f (Just PosTop))
    <|> (asciiCI "bottom" $> f (Just PosBottom))
    <|> ((\a -> Position Nothing a Nothing Nothing) <$> (Just <$> percentageLength))
  where f x = Position x Nothing Nothing Nothing

pos2 :: Parser Position
pos2 = firstx <|> firsty
  where firstx = do
            a <- (asciiCI "left" $> Position (Just PosLeft) Nothing)
                 <|> (asciiCI "center" $> Position (Just PosCenter) Nothing)
                 <|> (asciiCI "right" $> Position (Just PosRight) Nothing)
                 <|> ((Position Nothing . Just) <$> percentageLength)
            skipComments *> ((asciiCI "top" $> a (Just PosTop) Nothing)
                 <|> (asciiCI "center" $> a (Just PosCenter) Nothing)
                 <|> (asciiCI "bottom" $> a (Just PosBottom) Nothing)
                 <|> ((a Nothing . Just) <$> percentageLength))
        firsty = do
            a <- (asciiCI "top" $> Position (Just PosTop) Nothing)
                 <|> (asciiCI "center" $> Position (Just PosCenter) Nothing)
                 <|> (asciiCI "bottom" $> Position (Just PosBottom) Nothing)
                 <|> ((Position Nothing . Just) <$> percentageLength)
            skipComments *> ((asciiCI "left" $> a (Just PosLeft) Nothing)
                 <|> (asciiCI "center" $> a (Just PosCenter) Nothing)
                 <|> (asciiCI "right" $> a (Just PosRight) Nothing)
                 <|> ((a Nothing . Just) <$> percentageLength))

pos4 :: Parser Position
pos4 = firstx <|> firsty
  where posTop    = asciiCI "top" $> Position (Just PosTop)
        posRight  = asciiCI "right" $> Position (Just PosRight)
        posBottom = asciiCI "bottom" $> Position (Just PosBottom)
        posLeft   = asciiCI "left" $> Position (Just PosLeft)
        firstx    = do
            x <- (asciiCI "center" $> Position (Just PosCenter) Nothing)
                 <|> ((posLeft <|> posRight) <*> (skipComments *> option Nothing (Just <$> percentageLength)))
            _ <- skipComments
            (asciiCI "center" $> x (Just PosCenter) Nothing)
                <|> (((asciiCI "top" $> x (Just PosTop)) <|> (asciiCI "bottom" $> x (Just PosBottom)))
                    <*> (skipComments *> option Nothing (Just <$> percentageLength)))
        firsty = do
            x <- (asciiCI "center" $> Position (Just PosCenter) Nothing)
                 <|> ((posTop <|> posBottom) <*> (skipComments *> option Nothing (Just <$> percentageLength)))
            _ <- skipComments
            (asciiCI "center" $> x (Just PosCenter) Nothing)
                <|> (((asciiCI "left" $> x (Just PosLeft)) <|> (asciiCI "right" $> x (Just PosRight)))
                    <*> (skipComments *> option Nothing (Just <$> percentageLength)))

{-
transformOrigin :: Parser Values
transformOrigin = twoVal <|> oneVal
  where oneVal = (singleValue numericalvalue) <|> offsetKeyword
        offsetKeyword = do
          v1 <- ident
          if v1 == "top" || v1 == "right" || v1 == "bottom" || v1 == "left" || v1 == "center"
             then pure $ mkValues [mkOther v1]
             else mzero
        twoVal = do
          (v1, v2) <- ((,) <$> yAxis <*> (skipComments *> (xAxis <|> numericalvalue)))
                    <|> ((,) <$> xAxis <*> (skipComments *> (yAxis <|> numericalvalue)))
                    <|> ((,) <$> numericalvalue <*> (skipComments *> (yAxis <|> xAxis)))
          option (mkValues [v1,v2]) ((\x -> mkValues [v1,v2, DistanceV x]) <$> distance)
        yAxis = do
          v1 <- ident
          if v1 == "top" || v1 == "bottom" || v1 == "center"
             then pure $ mkOther v1
             else mzero
        xAxis = do
          v1 <- ident
          if v1 == "right" || v1 == "left" || v1 == "center"
             then pure $ mkOther v1
             else mzero
-}

bgSize :: Parser BgSize
bgSize = twovaluesyntax <|> contain <|> cover
  where cover   = asciiCI "cover" $> Cover
        contain = asciiCI "contain" $> Contain
        twovaluesyntax = do
            x <- bgsizeValue <* skipComments
            (BgSize2 x <$> bgsizeValue) <|> pure (BgSize1 x)
        bgsizeValue = (Left <$> percentageLength) <|> (Right <$> auto)

bgAttachment :: Parser TextV
bgAttachment = matchKeywords ["scroll", "fixed", "local"]

box :: Parser TextV
box = matchKeywords ["border-box", "padding-box", "content-box"]

-- [ <bg-layer> , ]* <final-bg-layer>
background :: Parser Values
background = do
    xs <- many (bgLayer <* char ',' <* skipComments)
    x  <- finalBgLayer
    pure $ if null xs
              then Values x []
              else Values (head xs) (fmap (Comma,) $ tail xs ++ [x])

-- <final-bg-layer> = <bg-image> || <position> [ / <bg-size> ]? || <repeat-style> || <attachment> || <box> || <box> || <'background-color'>
finalBgLayer :: Parser Value
finalBgLayer = do
    layer <- permute (mkFinalBgLayer <$?> (Nothing, Just <$> image <* skipComments)
                                     <|?> (Nothing, Just <$> positionAndBgSize <* skipComments)
                                     <|?> (Nothing, Just <$> repeatStyle <* skipComments)
                                     <|?> (Nothing, Just <$> bgAttachment <* skipComments)
                                     <|?> (Nothing, Just <$> box <* skipComments)
                                     <|?> (Nothing, Just <$> box <* skipComments)
                                     <|?> (Nothing, Just <$> color <* skipComments))
    if finalBgLayerIsEmpty layer
       then mzero
       else pure layer
  where finalBgLayerIsEmpty (FinalBgLayer Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) = True
        finalBgLayerIsEmpty _                                                                              = False
        -- parameters e and f are being swapped to deal with the permutation
        -- changing the original order of the parsed values.
        mkFinalBgLayer a Nothing c d e f g = FinalBgLayer a Nothing Nothing c d f e g
        mkFinalBgLayer a (Just (p,s)) c d e f g = FinalBgLayer a (Just p) s c d f e g

-- <bg-layer> =       <bg-image> || <position> [ / <bg-size> ]? || <repeat-style> || <attachment> || <box>{1,2}
bgLayer :: Parser Value
bgLayer = do
    layer <- permute (mkBgLayer <$?> (Nothing, Just <$> image <* skipComments)
                                <|?> (Nothing, Just <$> positionAndBgSize <* skipComments)
                                <|?> (Nothing, Just <$> repeatStyle <* skipComments)
                                <|?> (Nothing, Just <$> bgAttachment <* skipComments)
                                <|?> (Nothing, Just <$> box2 <* skipComments))
    if bgLayerIsEmpty layer
       then mzero
       else pure layer
  where bgLayerIsEmpty (BgLayer Nothing Nothing Nothing Nothing Nothing Nothing Nothing) = True
        bgLayerIsEmpty _                                                                 = False
        mkBgLayer a Nothing c d Nothing           = BgLayer a Nothing Nothing c d Nothing Nothing
        mkBgLayer a (Just (p,s)) c d Nothing      = BgLayer a (Just p) s c d Nothing Nothing
        mkBgLayer a Nothing c d (Just (i,j))      = BgLayer a Nothing Nothing c d (Just i) j
        mkBgLayer a (Just (p,s)) c d (Just (i,j)) = BgLayer a (Just p) s c d (Just i) j
        box2 :: Parser (TextV, Maybe TextV)
        box2 = do
            x <- box <* skipComments
            y <- option Nothing (Just <$> box)
            pure (x,y)

-- used for the background property, which takes among other things:
-- <position> [ / <bg-size> ]?
positionAndBgSize :: Parser (Position, Maybe BgSize)
positionAndBgSize = do
    x <- position <* skipComments
    y <- option Nothing (Just <$> (char '/' *> skipComments *> bgSize))
    pure (x,y)

matchKeywords :: [Text] -> Parser TextV
matchKeywords listOfKeywords = do
    i <- ident
    if T.toCaseFold i `elem` Set.fromList listOfKeywords
       then pure $ TextV i
       else mzero

-- <image> = <url> | <image()> | <image-set()> | <element()> | <cross-fade()> | <gradient>
image :: Parser Value
image = do
    i <- ident
    let lowercased = T.toLower i
    if lowercased == "none"
       then pure $ mkOther "none"
       else do _ <- char '('
               if Set.member lowercased possibilities
                  then fromMaybe mzero (Map.lookup lowercased functionsMap)
                  else mzero
  where possibilities = Set.fromList $ map T.toCaseFold
            ["url", "element", "linear-gradient", "radial-gradient"]

transition :: Parser Values
transition = parseCommaSeparated singleTransition

-- [ none | <single-transition-property> ] || <time> || <single-transition-timing-function> || <time>
singleTransition :: Parser Value
singleTransition = do
    st <- permute (mkSingleTransition <$?> (Nothing, Just <$> singleTransitionProperty <* skipComments)
                                      <|?> (Nothing, Just <$> duration <* skipComments)
                                      <|?> (Nothing, Just <$> timingFunction <* skipComments)
                                      <|?> (Nothing, Just <$> duration <* skipComments))
    if singleTransitionIsEmpty st
       then mzero
       else pure st
  where singleTransitionIsEmpty (SingleTransition Nothing Nothing Nothing Nothing) = True
        singleTransitionIsEmpty _                                                  = False
        -- Needed because permute is swapping the order of the time parameters.
        mkSingleTransition a b c d = SingleTransition a d c b
        singleTransitionProperty = do
            i <- ident
            let lowercased = T.toLower i
            if Set.member lowercased excludedKeywords
               then mzero
               else pure $ TextV i
        excludedKeywords = Set.fromList ["initial", "inherit", "unset", "default", "none"]

-- | Parser for <https://drafts.csswg.org/css-timing-1/#single-timing-function-production \<single-timing-function\>>.
timingFunction :: Parser TimingFunction
timingFunction = do
    i <- ident
    let lowercased = T.toLower i
    case Map.lookup lowercased timingFunctionKeywords of
      Just x -> x
      Nothing -> char '(' *> (if lowercased == "steps"
                                 then steps
                                 else if lowercased == "cubic-bezier"
                                         then cubicbezier
                                         else mzero)
  where timingFunctionKeywords = Map.fromList [("ease",        pure Ease)
                                              ,("ease-in",     pure EaseIn)
                                              ,("ease-in-out", pure EaseInOut)
                                              ,("ease-out",    pure EaseOut)
                                              ,("linear",      pure Linear)
                                              ,("step-end",    pure StepEnd)
                                              ,("step-start",  pure StepStart)]

backgroundSize :: Parser Values
backgroundSize = parseCommaSeparated (BgSizeV <$> bgSize)

auto :: Parser Auto
auto = asciiCI "auto" $> Auto

propertyValueParsersMap :: Map Text (Parser Values)
propertyValueParsersMap = Map.fromList
    [--("color",           (:[]) <$> colorvalue)
    ("display",                     singleValue textualvalue)
    ,("font",                       singleValue font)
    ,("font-size",                  singleValue fontSize)
    ,("font-style",                 singleValue fontStyle)
    ,("font-weight",                singleValue numberOrText)
    ,("font-family",                fontFamilyValues)
    ,("background",                 background)
    ,("background-repeat",          parseCommaSeparated (RepeatStyleV <$> repeatStyle))
    ,("background-size",            backgroundSize)
    ,("box-shadow",                 shadowList)
    ,("-o-box-shadow",              shadowList)
    ,("-moz-box-shadow",            shadowList)
    ,("-webkit-box-shadow",         shadowList)
    ,("text-shadow",                textShadow)

    ,("animation",                  animation)
    ,("-o-animation",               animation)
    ,("-ms-animation",              animation)
    ,("-moz-animation",             animation)
    ,("-webkit-animation",          animation)

    ,("transition",                 transition)
    ,("-o-transition",              transition)
    ,("-ms-transition",             transition)
    ,("-moz-transition",            transition)
    ,("-webkit-transition",         transition)

    ,("height",                     singleValue numberOrText) -- <length>|<percentage>|auto
    ,("margin-bottom",              singleValue numberOrText)
    ,("margin-left",                singleValue numberOrText)
    ,("background-position",        singleValue positionvalue)
    ,("perspective-origin",         singleValue positionvalue)
    ,("-o-perspective-origin",      singleValue positionvalue)
    ,("-moz-perspective-origin",    singleValue positionvalue)
    ,("-webkit-perspective-origin", singleValue positionvalue)
    ,("mask-position",              positionList)
    ,("-webkit-mask-position",      positionList)
    -- ,("transform-origin",           transformOrigin)
    -- ,("-ms-transform-origin",       transformOrigin)
    -- ,("-webkit-transform-origin",   transformOrigin)
    -- ,("font",
    -- ,("margin-right",   numberOrText)
    -- ,("margin-top",     numberOrText)
    -- ,("opacity",        someNumericalValue)
    -- ,("overflow",       someText)
    -- ,("padding-bottom", someNumericalValue)
    -- ,("padding-left",   someNumericalValue)
    -- ,("padding-right",  someNumericalValue)
    -- ,("padding-top",    someNumericalValue)
    -- ,("position",       someText)
    -- ,("text-align",     someText)
    -- ,("width",          numberOrText)
    ]
  where numberOrText = numericalvalue <|> textualvalue

-- helper wrapper
singleValue :: Parser Value -> Parser Values
singleValue = (flip Values [] <$>)

-- | Parses until the end of the declaration, i.e. ';' or '}'.
-- Used to deal with invalid input, or an IE specific hack.
invalidvalue :: Parser Value
invalidvalue = mkOther <$> A.takeWhile1 cond
  where cond c = c /= '\\' && c /= ';' && c /= '}' && c /= '!'

-- [ [ <'font-style'> || <font-variant-css21> || <'font-weight'> ||
-- <'font-stretch'> ]? <'font-size'> [ / <'line-height'> ]? <'font-family'> ] |
font :: Parser Value
font = systemFonts <|> do
    (fsty, fvar, fwgt, fstr) <- parse4
    (fsz, lh)                <- fontSizeAndLineHeight
    ff                       <- ((:) <$> fontfamily <* skipComments) <*> many (char ',' *> lexeme fontfamily)
    pure $ FontV fsty fvar fwgt fstr fsz lh ff
  where systemFonts = Other <$> parseIdents ["caption", "icon", "menu", "message-box", "small-caption", "status-bar"]
        fontSizeAndLineHeight = do
            fsz <- fontSize <* skipComments
            lh  <- option Nothing (Just <$> (char '/' *> lexeme lineHeight))
            pure (fsz, lh)
        lineHeight = let validNum = do n <- numericalvalue
                                       case n of
                                         NumberV _     -> pure n
                                         PercentageV _ -> pure n
                                         DistanceV _   -> pure n
                                         _             -> mzero
                     in (Other <$> parseIdents ["normal"]) <|> validNum
        fontWeightNumber :: Parser Value
        fontWeightNumber = do
            n <- number
            if Set.notMember n (Set.fromList [100, 200, 300, 400, 500, 600, 700, 800, 900])
               then mzero
               else do c <- A.peekChar
                       case c of
                         Nothing -> mzero
                         Just x  -> if isAscii x || x == '%'
                                       then mzero
                                       else pure $ NumberV n
        parseFirstFour j = (storeProperty' j <$> fontWeightNumber <* skipComments) <|> do
            i <- ident
            case Map.lookup (T.toLower i) m of
              Just x  -> case x of
                           FontWeight -> pure $ storeProperty' j (mkOther i)
                           _          -> pure $ storeProperty  j x (TextV i)
              Nothing -> mzero
          where m  = Map.fromList $ zip ["ultra-condensed", "extra-condensed", "condensed", "semi-condensed", "semi-expanded", "expanded", "extra-expanded", "ultra-expanded"] (repeat FontStretch)
                                 ++ zip ["small-caps"] (repeat FontVariant)
                                 ++ zip ["italic", "oblique"] (repeat FontStyle)
                                 ++ zip ["bold", "bolder", "lighter"] (repeat FontWeight)
                                 ++ [("normal", Ambiguous)]
        parse4 :: Parser (Maybe TextV, Maybe TextV, Maybe Value, Maybe TextV)
        parse4 = do
            let initialized = (Nothing, Nothing, Nothing, Nothing, 0)
            w <- option initialized (parseFirstFour initialized <* skipComments)
            x <- option w (parseFirstFour w <* skipComments)
            y <- option x (parseFirstFour x <* skipComments)
            (a,b,c,d,e) <- option y (parseFirstFour y <* skipComments)
            pure $ fillTuple (a,b,c,d) e
        fillTuple (a,b,c,d) 0 = (a,b,c,d)
        fillTuple (a,b,c,d) x
            | isNothing a = fillTuple (Just nrml,b,c,d) (x-1)
            | isNothing b = fillTuple (a, Just nrml,c,d) (x-1)
            | isNothing c = fillTuple (a,b, Just $ Other nrml,d) (x-1)
            | isNothing d = fillTuple (a,b,c, Just nrml) (x-1)
            | otherwise   = (a,b,c,d)
          where nrml = TextV "normal"

data FontProperty = FontStyle | FontVariant | FontWeight | FontStretch | Ambiguous
  deriving (Eq, Show)

storeProperty :: (Maybe TextV, Maybe TextV, Maybe Value, Maybe TextV, Int) -> FontProperty -> TextV
              -> (Maybe TextV, Maybe TextV, Maybe Value, Maybe TextV, Int)
storeProperty (a,b,c,d,i) y x = replace y
  where replace FontStyle   = (Just x,b,c,d,i)
        replace FontVariant = (a, Just x,c,d,i)
        replace FontStretch = (a,b,c, Just x,i)
        replace Ambiguous   = (a,b,c,d,i + 1)
        replace _           = (a,b,c,d,i)

storeProperty' :: (Maybe TextV, Maybe TextV, Maybe Value, Maybe TextV, Int) -> Value
               -> (Maybe TextV, Maybe TextV, Maybe Value, Maybe TextV, Int)
storeProperty' (a,b,_,d,i) x = (a,b, Just x,d,i)


-- font-style: normal
-- font-variant: normal
-- font-weight: normal
-- font-stretch: normal
-- font-size: medium
-- line-height: normal
-- font-family: depends on user agent

fontFamilyValues :: Parser Values
fontFamilyValues = singleValue csswideKeyword <|> do
    v <- fontfamily
    vs <- many ((,) <$> separator <*> fontfamily)
    pure $ Values v vs

fontfamily :: Parser Value
fontfamily = (StringV <$> stringtype) <|> (mkOther <$> unquotedFontFamily)

local :: Parser Value
local = functionParser $
    Local <$> ((Left <$> unquotedFontFamily) <|> (Right <$> stringtype))

unquotedFontFamily :: Parser Text
unquotedFontFamily = do
    v  <- ident
    vs <- many (skipComments *> ident)
    pure $ v <> mconcat (map (T.singleton ' ' <>) vs)


textualvalue :: Parser Value
textualvalue = do
    i <- ident
    if i == "\\9" -- iehack
       then mzero
       else do c <- A.peekChar
               case c of
                 Just '(' -> functionParsers i
                 Just ':' -> mzero -- invalid
                 _        -> textualParsers i

textualParsers :: Text -> Parser Value
textualParsers i = let t = T.toCaseFold i
                   in fromMaybe (pure $ mkOther i) (Map.lookup t textualParsersMap)
  where textualParsersMap = Map.union csswideKeywordsMap namedColorsParsersMap

csswideKeyword :: Parser Value
csswideKeyword = do
    i <- ident <* skipComments
    let lowercased = T.toLower i
    case Map.lookup lowercased csswideKeywordsMap of
      Nothing -> mzero
      Just x -> do c <- A.peekChar
                   case c of
                     Nothing -> x
                     Just y -> if y `elem` ['!', ';', '}']
                                  then x
                                  else mzero

csswideKeywordsMap :: Map Text (Parser Value)
csswideKeywordsMap  = Map.fromList $ map (first T.toCaseFold)
                                 [("initial", pure Initial)
                                 ,("inherit", pure Inherit)
                                 ,("unset",   pure Unset)]

stringvalue :: Parser Value
stringvalue = StringV <$> stringtype

functionParsers :: Text -> Parser Value
functionParsers i = char '(' *>
    case Map.lookup (T.toCaseFold i) functionsMap of
      Just x -> x <|> genericFunc i
      Nothing -> genericFunc i
                 <|> (mkOther <$> (f i "(" <$> someText <*> string ")"))
  where f x y z w = x <> y <> z <> w
        someText = A.takeWhile (/= ')')

genericFunc :: Text -> Parser Value
genericFunc i = (GenericFunc i <$> valuesInParens) <* char ')'

valuesInParens :: Parser Values
valuesInParens = Values <$> v <*> many ((,) <$> separator <*> v) <* skipComments
 where v =  textualvalue
         <|> numericalvalue
         <|> hexvalue
         <|> (StringV <$> stringtype)

stringOrUrl :: Parser (Either StringType Url)
stringOrUrl = (Left <$> stringtype) <|> (Right <$> someUrl)
  where someUrl :: Parser Url
        someUrl = asciiCI "url" *> char '(' *> url

-- | Parser for <https://www.w3.org/TR/css-backgrounds-3/#typedef-repeat-style \<repeat-style\>>,
-- used in @background-repeat@ and @background@.
repeatStyle :: Parser RepeatStyle
repeatStyle = do
  i <- ident
  let lowercased = T.toLower i
  case L.lookup lowercased singleKeywords of
    Nothing -> case Map.lookup lowercased keywordPairs of
                 Nothing -> mzero
                 Just y  -> (RepeatStyle2 y <$> secondKeyword)
                            <|> pure (RepeatStyle1 y)
    Just x -> pure x
  where secondKeyword = do
            z <- skipComments *> ident
            maybe mzero pure $ Map.lookup (T.toLower z) keywordPairs
        singleKeywords = [("repeat-x", RepeatX), ("repeat-y", RepeatY)]
        keywordPairs = Map.fromList [("repeat",    RsRepeat)
                                    ,("no-repeat", RsNoRepeat)
                                    ,("space",     RsSpace)
                                    ,("round",     RsRound)]

functionsMap :: Map Text (Parser Value)
functionsMap = Map.fromList $ fmap (first T.toCaseFold)
          [("rgb",                     ColorV <$> rgb)
          ,("rgba",                    ColorV <$> rgba)
          ,("hsl",                     ColorV <$> hsl)
          ,("hsla",                    ColorV <$> hsla)
          ,("url",                     UrlV <$> url)
          ,("format",                  format)
          ,("local",                   local)
          -- <gradient> parsers
          ,("linear-gradient",         GradientV "linear-gradient" <$> lineargradient)
          ,("-o-linear-gradient",      GradientV "-o-linear-gradient" <$> lineargradient)
          ,("-ms-linear-gradient",     GradientV "-ms-linear-gradient" <$> lineargradient)
          ,("-moz-linear-gradient",    GradientV "-moz-linear-gradient" <$> lineargradient)
          ,("-webkit-linear-gradient", GradientV "-webkit-linear-gradient" <$> lineargradient)
          ,("radial-gradient",         GradientV "radial-gradient" <$> radialgradient)
          ,("-o-radial-gradient",      GradientV "-o-radial-gradient" <$> radialgradient)
          ,("-moz-radial-gradient",    GradientV "-moz-radial-gradient" <$> radialgradient)
          ,("-webkit-radial-gradient", GradientV "-webkit-radial-gradient" <$> radialgradient)
          --,("repeating-linear-gradient", ? ) -- TODO
          --,("repeating-radial-gradient", ? ) -- TODO
          -- <shape>
          ,("rect",                    rect)
          -- <transform-function>
          ,("matrix",                  TransformV <$> matrix)
          ,("matrix3d",                TransformV <$> matrix3d)
          ,("rotate",                  (TransformV . Rotate) <$> functionParser angle)
          ,("rotate3d",                TransformV <$> rotate3d)
          ,("rotateX",                 (TransformV . Rotate) <$> functionParser angle)
          ,("rotateY",                 (TransformV . Rotate) <$> functionParser angle)
          ,("rotateZ",                 (TransformV . Rotate) <$> functionParser angle)
          ,("scale",                   TransformV <$> scale)
          ,("scale3d",                 TransformV <$> scale3d)
          ,("scaleX",                  (TransformV . ScaleY) <$> functionParser number)
          ,("scaleY",                  (TransformV . ScaleY) <$> functionParser number)
          ,("scaleZ",                  (TransformV . ScaleZ) <$> functionParser number)
          ,("skew",                    TransformV <$> skew)
          ,("skewX",                   (TransformV . SkewX) <$> functionParser angle)
          ,("skewY",                   (TransformV . SkewY) <$> functionParser angle)
          ,("translate",               TransformV <$> translate)
          ,("translate3d",             TransformV <$> translate3d)
          ,("translateX",              (TransformV . TranslateX) <$> functionParser percentageLength)
          ,("translateY",              (TransformV . TranslateY) <$> functionParser percentageLength)
          ,("translateZ",              (TransformV . TranslateZ) <$> functionParser distance)
          ,("perspective",             (TransformV . Perspective) <$> functionParser distance)
          -- <timing-function>
          ,("cubic-bezier",            TimingFuncV <$> cubicbezier)
          ,("steps",                   TimingFuncV <$> steps)
          -- <filter-function>
          ,("blur",                    (FilterV . Blur) <$> functionParser distance)
          ,("contrast",                (FilterV . Contrast) <$> functionParser numberPercentage)
          ,("grayscale",               (FilterV . Grayscale) <$> functionParser numberPercentage)
          ,("invert",                  (FilterV . Invert) <$> functionParser numberPercentage)
          ,("opacity",                 (FilterV . Opacity) <$> functionParser numberPercentage)
          ,("saturate",                (FilterV . Saturate) <$> functionParser numberPercentage)
          ,("sepia",                   (FilterV . Sepia) <$> functionParser numberPercentage)
          ,("brightness",              (FilterV . Brightness) <$> functionParser numberPercentage)
          ,("drop-shadow",             FilterV <$> dropShadow)
          ,("hue-rotate",              (FilterV . HueRotate) <$> functionParser angle)
          ,("element", genericFunc "element")
          --
          -- <basic-shape>
          -- circle() = circle( [<shape-radius>]? [at <position>]? )
          -- polygon( [<fill-rule>,]? [<shape-arg> <shape-arg>]# )
          -- inset()
          -- ellipse( [<shape-radius>{2}]? [at <position>]? )
          --
          -- <image> https://drafts.csswg.org/css-images
          -- Note: <gradient> is a type of <image> !
          -- cross-fade()
          -- image()
          -- image-set()
          -- image-set()
          -- element()
          --
          --
          --,("stylistic", functionParser ident) -- IDENT
          --,("styleset", ? ) -- ident#
          --,("swash", ? ) -- ident
          --,("annotation", ? ) -- ident
          --,("attr", ? ) --  <attr-name> <type-or-unit>? [, <attr-fallback> ]?
          --,("calc", ? )  -- <calc-sum> , experimental
          --,("character-variant", ? ) -- ident#
          --,("element", ? )  -- id selector, experimental
          --,("local", ? )
          --,("ornaments", ? ) -- ident
          --,("symbols", ? )  --  <symbols-type>? [ <string> | <image> ]+
          --,("var", ? )  -- experimental: <custom-property-name> [, <declaration-value> ]?
-- minmax()
          ]

dropShadow :: Parser FilterFunction
dropShadow = functionParser $ do
    l1 <- distance
    l2 <- lexeme distance
    l3 <- option Nothing ((Just <$> distance) <* skipComments)
    c  <- option Nothing (Just <$> color)
    pure $ DropShadow l1 l2 l3 c

textShadow :: Parser Values
textShadow = parseCommaSeparated shadowText

shadowText :: Parser Value
shadowText = permute (mkShadowText <$$> (lns <* skipComments)
                                   <|?> (Nothing , Just <$> color <* skipComments))
  where mkShadowText (x,y,b) = ShadowText x y b
        lns = do
            l1 <- distance
            l2 <- lexeme distance
            l3 <- option Nothing ((Just <$> distance) <* skipComments)
            pure (l1,l2,l3)

-- | Parser for <https://drafts.csswg.org/css-backgrounds-3/#typedef-shadow \<shadow>>
-- values, used in the @box-shadow@ property.
shadowList :: Parser Values
shadowList = parseCommaSeparated (ShadowV <$> shadow)

positionList :: Parser Values
positionList = parseCommaSeparated positionvalue

parseCommaSeparated :: Parser Value -> Parser Values
parseCommaSeparated p = do
    v  <- p
    vs <- lexeme $ many ((,) <$> commaSeparator <*> p)
    c  <- A.peekChar
    case c of
      Just x  -> if x `elem` ['!', ';', '}']
                    then pure $ Values v vs
                    else mzero
      Nothing -> pure $ Values v vs


shadow :: Parser Shadow
shadow = permute (mkShadow <$?> (False, asciiCI "inset" $> True <* skipComments)
                           <||> fourLengths
                           <|?> (Nothing , Just <$> color <* skipComments))
  where mkShadow i (l1,l2,l3,l4) = Shadow i l1 l2 l3 l4
        fourLengths = do
            l1 <- distance
            l2 <- lexeme distance
            l3 <- option Nothing ((Just <$> distance) <* skipComments)
            l4 <- option Nothing ((Just <$> distance) <* skipComments)
            pure (l1,l2,l3,l4)

radialgradient :: Parser Gradient
radialgradient = functionParser $ do
    (def, c) <- option (True, RadialGradient Nothing Nothing) ((False,) <$> endingShapeAndSize <* skipComments)
    p  <- option Nothing (asciiCI "at" *> skipComments *> (Just <$> position))
    _  <- if def && isNothing p
             then pure '*' -- do nothing
             else comma
    cs <- colorStopList
    pure $ c p cs
  where circle = asciiCI "circle" $> Just Circle <* skipComments
        ellipse = asciiCI "ellipse" $> Just Ellipse <* skipComments
        endingShapeAndSize = r1 <|> r2 <|> r3
          where r1 = permute (RadialGradient <$?> (Nothing, ellipse) <||> (Just <$> (PL <$> percentageLength <*> lexeme percentageLength)))
                r2 = permute (RadialGradient <$?> (Nothing, circle) <||> ((Just . SL) <$> distance <* skipComments))
                r3 = permute (RadialGradient <$?> (Nothing, circle <|> ellipse) <||> extentKeyword)
                   <|> permute (RadialGradient <$$> (circle <|> ellipse) <|?> (Nothing, extentKeyword))
                extentKeyword = do
                    i <- ident
                    _ <- skipComments
                    case Map.lookup i extentKeywords of
                      Just x -> pure (Just x)
                      Nothing -> mzero
                extentKeywords :: Map Text Size
                extentKeywords = Map.fromList [("closest-corner",  ClosestCorner)
                                              ,("closest-side",    ClosestSide)
                                              ,("farthest-corner", FarthestCorner)
                                              ,("farthest-side",   FarthestSide)]

-- | Assumes "linear-gradient(", or one of its prefixed equivalents, has been parsed.
-- : [<angle>|to <side-or-corner> ,]? <color-stop> [, <color-stop>]+
lineargradient :: Parser Gradient
lineargradient = functionParser (lg <|> oldLg)
  where lg = do
            x <- option Nothing angleOrSide
            c <- colorStopList
            pure $ LinearGradient x c
        oldLg = do
            x <- option Nothing ((ga <|> ((Just . Right) <$> sideOrCorner)) <* comma)
            c <- colorStopList
            pure $ OldLinearGradient x c
        angleOrSide = (ga <|> gs) <* comma
        ga = (Just . Left) <$> angle
        gs = asciiCI "to" *> skipComments *> ((Just . Right) <$> sideOrCorner)

-- <side-or-corner> = [left | right] || [top | bottom]
sideOrCorner :: Parser (Side, Maybe Side)
sideOrCorner = orderOne <|> orderTwo
  where orderOne = (,) <$> leftright <* skipComments
                       <*> option Nothing (Just <$> topbottom)
        orderTwo = (,) <$> topbottom <* skipComments
                       <*> option Nothing (Just <$> leftright)

leftright :: Parser Side
leftright =  (asciiCI "left" $> LeftSide)
         <|> (asciiCI "right" $> RightSide)

topbottom :: Parser Side
topbottom =  (asciiCI "top" $> TopSide)
         <|> (asciiCI "bottom" $> BottomSide)

colorStopList :: Parser [ColorStop]
colorStopList = do
    c1 <- colorStop
    _  <- char ',' <* skipComments
    c2 <- colorStop
    cs <- many (char ',' *> skipComments *> colorStop)
    pure $ c1:c2:cs

colorStop :: Parser ColorStop
colorStop = ColorStop <$> color <* skipComments
        <*> option Nothing (Just <$> percentageLength <* skipComments)

-- | Parser for <https://drafts.csswg.org/css-color-3/#colorunits \<color\>>.
color :: Parser Color
color = hex <|> othercolor
  where othercolor = do
            t <- textualvalue
            case t of
              ColorV c -> pure c
              _        -> mzero

-- TODO make parser specifically for it instead of reusing numericalvalue
percentageLength :: Parser PercentageLength
percentageLength = do
    n <- numericalvalue
    case n of
      PercentageV p -> pure $ Left p
      NumberV 0     -> pure $ Right (Distance 0 PX)
      DistanceV d   -> pure $ Right d
      _             -> mzero

numberPercentage :: Parser (Either Number Percentage)
numberPercentage = do
    n <- numericalvalue
    case n of
      NumberV x     -> pure $ Left x
      PercentageV p -> pure $ Right p
      _             -> mzero

-- | Assumes "rect(" has been already parsed
rect :: Parser Value
rect = functionParser $ do
    length1 <- distance <* comma
    length2 <- distance <* comma
    length3 <- distance <* comma
    length4 <- distance
    pure $ Rect length1 length2 length3 length4

-- | Assumes "translate(" has been already parsed
translate :: Parser TransformFunction
translate = functionParser $ do
    pl  <- percentageLength <* skipComments
    mpl <- option Nothing (char ',' *> skipComments *> (Just <$> percentageLength))
    pure $ Translate pl mpl

-- | Parser of scale() function. Assumes "scale(" has been already parsed
scale :: Parser TransformFunction
scale = functionParser $ do
    n  <- number <* skipComments
    mn <- option Nothing (char ',' *> skipComments *> (Just <$> number))
    pure $ Scale n mn

scale3d :: Parser TransformFunction
scale3d = functionParser $ liftA3 Scale3d n n number
  where n = number <* comma

skew :: Parser TransformFunction
skew = functionParser $ do
    a  <- angle <* skipComments
    ma <- option Nothing (char ',' *> skipComments *> (Just <$> angle))
    pure $ Skew a ma

translate3d :: Parser TransformFunction
translate3d = functionParser $ do
    x <- percentageLength <* comma
    y <- percentageLength <* comma
    z <- distance
    pure $ Translate3d x y z

matrix :: Parser TransformFunction
matrix = functionParser $ do
    n  <-  number
    ns <-  count 5 (comma *> number)
    pure $ mkMat (n:ns)

matrix3d :: Parser TransformFunction
matrix3d = functionParser $ do
    n  <-  number
    ns <-  count 15 (comma *> number)
    pure $ mkMat3d (n:ns)

rotate3d :: Parser TransformFunction
rotate3d = functionParser $ do
    x  <-  number <* comma
    y  <-  number <* comma
    z  <-  number <* comma
    a  <-  angle
    pure $ Rotate3d x y z a

cubicbezier :: Parser TimingFunction
cubicbezier = functionParser $ do
    p0 <- number <* comma
    p1 <- number <* comma
    p2 <- number <* comma
    p3 <- number
    pure $ CubicBezier p0 p1 p2 p3

steps :: Parser TimingFunction
steps = functionParser $ do
    i <- int
    s <- option Nothing (comma *> (Just <$> startOrEnd))
    pure $ Steps i s
  where startOrEnd = (asciiCI "end" $> End)
                 <|> (asciiCI "start" $> Start)

-- It uses skipSpace instead of skipComments, since comments aren't valid inside
-- the url-token. From the spec:
-- COMMENT tokens cannot occur within other tokens: thus, "url(/*x*/pic.png)"
-- denotes the URI "/*x*/pic.png", not "pic.png".
--
-- | Parser for <https://drafts.csswg.org/css-values-3/#urls \<url\>>. Assumes
-- @\"url(\"@ has already been parsed.
url :: Parser Url
url = Url <$> (skipSpace *> someUri <* skipSpace <* char ')')
  where someUri = (Right <$> stringtype) <|> (Left <$> nonQuotedUri)
        nonQuotedUri = A.takeWhile1 (/= ')') -- TODO maybe parse ident?

-- format(<string>#)
-- Assumes "format(" has been already parsed
format :: Parser Value
format = Format <$> functionParser p
  where p = (:) <$> stringtype <*> many (comma *> stringtype)

namedColorsParsersMap :: Map Text (Parser Value)
namedColorsParsersMap = Map.fromList $ foldr f [] keywordColors
  where f x xs = let a = fst x
                 in (a, pure $ ColorV (Named a)) : xs

-- | For cases when CSS hacks are used, e.g.: @margin-top: 1px \\9;@.
valuesFallback :: Parser Values
valuesFallback = Values <$> value <*> many ((,) <$> separator <*> value) <* skipComments

value :: Parser Value
value =  textualvalue
     <|> numericalvalue
     <|> hexvalue
     <|> (StringV <$> stringtype)
     <|> invalidvalue

separator :: Parser Separator
separator = lexeme $ (char ',' $> Comma)
                 <|> (char '/' $> Slash)
                 <|> pure Space

commaSeparator :: Parser Separator
commaSeparator = lexeme (char ',' $> Comma)

-- <string> data type parser
stringtype :: Parser StringType
stringtype = doubleQuotesString <|> singleQuotesString

doubleQuotesString :: Parser StringType
doubleQuotesString =  char '\"' *> (DoubleQuotes <$> untilDoubleQuotes)
  where untilDoubleQuotes = mappend <$> A.takeWhile (\c -> c /= '\\' && c /= '\"') <*> checkCharacter
        checkCharacter = (string "\"" $> mempty)
                      <|> (T.cons <$> char '\\' <*> untilDoubleQuotes)

singleQuotesString :: Parser StringType
singleQuotesString = char '\'' *> (SingleQuotes <$> untilSingleQuotes)
  where untilSingleQuotes = mappend <$> A.takeWhile (\c -> c /= '\\' && c /= '\'') <*> checkCharacter
        checkCharacter = (string "\'" $> mempty)
                      <|> (T.cons <$> char '\\' <*> untilSingleQuotes)

-- <single-animation>#
animation :: Parser Values
animation = parseCommaSeparated singleAnimation

-- <single-animation> = <time> || <timing-function> || <time> || <animation-iteration-count> || <animation-direction> || <animation-fill-mode> || <animation-play-state> || [ none | <keyframes-name> ]
singleAnimation :: Parser Value
singleAnimation = do
    sa <- permute (mkSingleAnimation <$?> (Nothing, Just <$> keyframesName <* skipComments)
                                     <|?> (Nothing, Just <$> iterationCount <* skipComments)
                                     <|?> (Nothing, Just <$> duration <* skipComments)
                                     <|?> (Nothing, Just <$> duration <* skipComments)
                                     <|?> (Nothing, Just <$> timingFunction <* skipComments)
                                     <|?> (Nothing, Just <$> animationDirection <* skipComments)
                                     <|?> (Nothing, Just <$> animationFillMode <* skipComments)
                                     <|?> (Nothing, Just <$> animationPlayState <* skipComments))
    if saIsEmpty sa
       then mzero
       else pure sa
  where -- mkSingleAnimation is used to order properly the overlapping values parsed by permute.
        mkSingleAnimation kf ic t1 t2 tf c d e = SingleAnimation t2 tf t1 ic c d e kf
        saIsEmpty (SingleAnimation Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) = True
        saIsEmpty _                                                                                 = False
        iterationCount = (mkOther <$> asciiCI "infinite") <|> (NumberV <$> number)
        animationDirection = parseIdents ["normal", "reverse", "alternate", "alternate-reverse"]
        animationFillMode  = parseIdents ["none", "forwards", "backwards", "both"]
        animationPlayState = parseIdents ["running", "paused"]
        keyframesName = stringvalue <|> (mkOther <$> ident)

parseIdents :: [Text] -> Parser TextV
parseIdents ls = do
    i <- ident
    if Set.member (T.toLower i) s
       then pure $ TextV i
       else mzero
  where s = Set.fromList ls

-- <single-animation-fill-mode> = none | forwards | backwards | both
