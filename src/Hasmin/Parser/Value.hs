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
module Hasmin.Parser.Value
    ( valuesFor
    , valuesFallback
    , value
    , valuesInParens
    , stringOrUrl
    , percentage
    , url
    , stringtype
    , stringvalue    -- used in StringSpec
    , shadowList     -- used in ShadowSpec
    , timingFunction
    , repeatStyle
    , position
    , color
    , number
    , fontStyle
    , textualvalue
    ) where

import Control.Applicative ((<|>), many, liftA3, optional)
import Control.Arrow (first)
import Control.Monad (mzero)
import Data.Functor (($>))
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Char (isAscii)
import Text.Parser.Permutation ((<|?>), (<$$>), (<$?>), (<||>), permute)
import qualified Data.Set as Set
import Data.Attoparsec.Text (asciiCI, char, count, option, Parser,
       skipSpace, string)
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import qualified Data.Text as T

import Hasmin.Parser.Utils
import Hasmin.Parser.Numeric
import Hasmin.Parser.Color
import Hasmin.Parser.Dimension
import Hasmin.Parser.Gradient
import Hasmin.Parser.PercentageLength
import Hasmin.Parser.Position
import Hasmin.Parser.TimingFunction
import Hasmin.Types.BgSize
import Hasmin.Types.Dimension
import Hasmin.Types.FilterFunction
import Hasmin.Types.Numeric
import Hasmin.Types.Position
import Hasmin.Types.RepeatStyle
import Hasmin.Types.Shadow
import Hasmin.Types.String
import Hasmin.Types.TransformFunction
import Hasmin.Types.Value

-- | Given a propery name, it returns a specific parser of values for that
-- property. Fails if no specific parser is found.
valuesFor :: Text -> Parser Values
valuesFor propName =
  case Map.lookup (T.toLower propName) propertyValueParsersMap of
    Just x  -> x <* skipComments
    Nothing -> mzero

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
            ] ++ (fmap . fmap) (LengthV .) distanceConstructorsList
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

hexvalue :: Parser Value
hexvalue = ColorV <$> hex

-- | Parser for <https://drafts.csswg.org/css-fonts-3/#propdef-font-style \<font-style\>>,
-- used in the @font-style@ and @font@ properties.
fontStyle :: Parser Value
fontStyle = Other <$> matchKeywords ["normal", "italic", "oblique"]

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
        <|> (LengthV <$> distance)
        <|> (PercentageV <$> percentage)
  where fontSizeKeyword = Other <$>  matchKeywords
                            ["large", "xx-small", "x-small", "small", "medium",
                            "x-large", "xx-large", "smaller" , "larger"]

{- [ [ <‘font-style’> || <font-variant-css21> || <‘font-weight’> ||
 - <‘font-stretch’> ]? <‘font-size’> [ / <‘line-height’> ]? <‘font-family’> ] |
 - caption | icon | menu | message-box | small-caption | status-bar
where <font-variant-css21> = [normal | small-caps]

-}

positionvalue :: Parser Value
positionvalue = PositionV <$> position

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
          option (mkValues [v1,v2]) ((\x -> mkValues [v1,v2, LengthV x]) <$> distance)
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
bgSize = twovaluesyntax <|> containOrCover
  where containOrCover = parserFromPairs [("cover", pure Cover)
                                         ,("contain", pure Contain)]
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
            y <- optional box
            pure (x,y)

-- used for the background property, which takes among other things:
-- <position> [ / <bg-size> ]?
positionAndBgSize :: Parser (Position, Maybe BgSize)
positionAndBgSize = (,) <$> position <*> optional (slash *> bgSize)

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
            if lowercased `Set.member` excludedKeywords
               then mzero
               else pure $ TextV i
        excludedKeywords = Set.fromList ["initial", "inherit", "unset", "default", "none"]

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
            lh  <- optional (char '/' *> lexeme lineHeight)
            pure (fsz, lh)
        lineHeight = let validNum = do n <- numericalvalue
                                       case n of
                                         NumberV _     -> pure n
                                         PercentageV _ -> pure n
                                         LengthV _     -> pure n
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
          where m =
                  Map.fromList $ zip ["ultra-condensed", "extra-condensed", "condensed", "semi-condensed", "semi-expanded", "expanded", "extra-expanded", "ultra-expanded"] (repeat FontStretch)
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
    v  <- fontfamily
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
    pure $ v <> foldMap (T.singleton ' ' <>) vs

textualParsers :: Text -> Parser Value
textualParsers i = let t = T.toCaseFold i
                   in fromMaybe (pure $ mkOther i) (Map.lookup t textualParsersMap)
  where textualParsersMap = Map.union csswideKeywordsMap namedColorsValueParsersMap
        namedColorsValueParsersMap = (fmap . fmap) ColorV namedColorsParsersMap

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
    case Map.lookup (T.toLower i) functionsMap of
      Just x  -> x <|> genericFunc i
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
functionsMap = Map.fromList (colorFunctionValueParsers ++ l)
  where colorFunctionValueParsers = (fmap . fmap . fmap) ColorV colorFunctionsParsers
        l = [("url",                     UrlV <$> url)
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
            ,("rotatex",                 (TransformV . Rotate) <$> functionParser angle)
            ,("rotatey",                 (TransformV . Rotate) <$> functionParser angle)
            ,("rotatez",                 (TransformV . Rotate) <$> functionParser angle)
            ,("scale",                   TransformV <$> scale)
            ,("scale3d",                 TransformV <$> scale3d)
            ,("scalex",                  (TransformV . ScaleY) <$> functionParser number)
            ,("scaley",                  (TransformV . ScaleY) <$> functionParser number)
            ,("scalez",                  (TransformV . ScaleZ) <$> functionParser number)
            ,("skew",                    TransformV <$> skew)
            ,("skewx",                   (TransformV . SkewX) <$> functionParser angle)
            ,("skewy",                   (TransformV . SkewY) <$> functionParser angle)
            ,("translate",               TransformV <$> translate)
            ,("translate3d",             TransformV <$> translate3d)
            ,("translatex",              (TransformV . TranslateX) <$> functionParser percentageLength)
            ,("translatey",              (TransformV . TranslateY) <$> functionParser percentageLength)
            ,("translatez",              (TransformV . TranslateZ) <$> functionParser distance)
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
            ,("element",                 genericFunc "element")
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
    l3 <- optional (distance <* skipComments)
    c  <- optional color
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
            l3 <- optional (distance <* skipComments)
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
            l3 <- optional (distance <* skipComments)
            l4 <- optional (distance <* skipComments)
            pure (l1,l2,l3,l4)

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
    pl  <- percentageLength
    mpl <- optional (comma *> percentageLength)
    pure $ Translate pl mpl

-- | Parser of scale() function. Assumes "scale(" has been already parsed
scale :: Parser TransformFunction
scale = functionParser $ do
    n  <- number
    mn <- optional (comma *> number)
    pure $ Scale n mn

scale3d :: Parser TransformFunction
scale3d = functionParser $ liftA3 Scale3d n n number
  where n = number <* comma

skew :: Parser TransformFunction
skew = functionParser $ do
    a  <- angle
    ma <- optional (comma *> angle)
    pure $ Skew a ma

translate3d :: Parser TransformFunction
translate3d = functionParser $
    Translate3d <$> percentageLength <* comma
                <*> percentageLength <* comma
                <*> distance

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
