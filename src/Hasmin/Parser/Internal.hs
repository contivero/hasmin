{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.Internal
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Hasmin.Parser.Internal where
 
import Control.Arrow (first)
import Control.Applicative ((<|>), many) 
import Control.Monad (mzero)
import Data.Attoparsec.Combinator (lookAhead, sepBy, endOfInput)
import Data.Attoparsec.Text (asciiCI, char, many1, manyTill, 
  option, Parser, satisfy, string)
import Data.List.NonEmpty (NonEmpty( (:|) ))
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy.Builder as LB
import Hasmin.Parser.Utils
import Hasmin.Parser.Value
import Hasmin.Selector
import Hasmin.Types.Stylesheet
import Hasmin.Types.Declaration
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

-- ---------------------------------------------------------------------------
-- Stylesheet
-- ---------------------------------------------------------------------------
selector :: Parser Selector
selector = Selector <$> compoundSelector 
      <*> many ((,) <$> (combinator <* skipComments) <*> compoundSelector)

-- First tries with '>>' (descendant), '>' (child), '+' (adjacent sibling), and
-- '~' (general sibling) combinators. If those fail, it tries with the
-- descendant (whitespace) combinator. We do this to allow comments in-between.
combinator :: Parser Combinator
combinator =  (skipComments *> ((string ">>" *> pure Descendant)
                            <|> (char '>' *> pure Child) 
                            <|> (char '+' *> pure AdjacentSibling) 
                            <|> (char '~' *> pure GeneralSibling)))
          <|> (satisfy ws *> pure Descendant)
  where ws c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'

compoundSelector :: Parser CompoundSelector
compoundSelector = 
    (:|) <$> (typeSelector <|> universal) 
                           <*> many p
                       <|> ((Universal mempty :|) <$> many1 p)
  where p = idSel <|> classSel <|> attributeSel <|> pseudo 
  
simpleSelector :: Parser SimpleSelector
simpleSelector = typeSelector <|> universal <|> idSel 
             <|> classSel <|> attributeSel <|> pseudo  
--idSel :: Parser SimpleSelector
--idSel = idSel <$> (char '#' *> ((TL.toStrict . toLazyText . mconcat) <$> many1 nmchar))
idSel :: Parser SimpleSelector
idSel = do
    _ <- char '#' 
    name <- mconcat <$> many1 nmchar
    pure $ IdSel (TL.toStrict (toLazyText name))

-- class: '.' IDENT
classSel :: Parser SimpleSelector
classSel = char '.' *> (ClassSel <$> ident)

{-
attrib
  : '[' S* [ namespace_prefix ]? IDENT S*
        [ [ PREFIXMATCH |
            SUFFIXMATCH |
            SUBSTRINGMATCH |
            '=' |
            INCLUDES |
            DASHMATCH ] S* [ IDENT | STRING ] S*
        ]? ']'
  ;
-}
-- FIXME namespace prefixes aren't allowed inside attribute selectors,
-- but they should be.
attributeSel :: Parser SimpleSelector
attributeSel = do
    _     <- char '[' *> skipComments 
    attId <- ident <* skipComments
    g     <- option Attribute attValue
    _     <- char ']'
    pure $ AttributeSel (g attId)
  where attValue = do
          f <- ((string "^=" *> pure (:^=:)) <|> 
                (string "$=" *> pure (:$=:)) <|>
                (string "*=" *> pure (:*=:)) <|>
                (string "="  *> pure (:=:))  <|>
                (string "~=" *> pure (:~=:)) <|>
                (string "|=" *> pure (:|=:))) <* skipComments
          attval <- ((Left <$> ident) <|> (Right <$> stringtype)) <* skipComments
          pure (`f` attval)

{-
-- string1: \"([^\n\r\f\\"]|\\{nl}|{escape})*\"
string1 = char '\"' *> many (o1 <|> o2 <|> escape)
  where o1 = satisfy (c -> c /= '\"' && c /= '\\' && c /= '\n')
        o2 = char '\\' *> newline *> pure mempty
-}

-- Called n1 in "Selectors Level 3, 10.2 Lexical scanner"
-- newline: \n|\r\n|\r|\f
newline :: Parser Text
newline = string "\r\n" <|> (T.singleton <$> satisfy cond)
  where cond c = c == '\n' || c == '\r' || c == '\f'

-- type_selector: [namespace_prefix]? element_name
typeSelector :: Parser SimpleSelector
typeSelector = Type <$> opt namespacePrefix <*> ident

-- universal: [ namespace_prefix ]? '*'
universal :: Parser SimpleSelector
universal = Universal <$> opt namespacePrefix <* char '*'

-- namespace_prefix: [ IDENT | '*' ]? '|'
namespacePrefix :: Parser Text
namespacePrefix = opt (ident <|> string "*") <* char '|'

{- '::' starts a pseudo-element, ':' a pseudo-class
   Exceptions: :first-line, :first-letter, :before and :after.
   Note that pseudo-elements are restricted to one per selector and
   occur only in the last simple_selector_sequence.

   <pseudo-class-selector> = ':' <ident-token> |
                             ':' <function-token> <any-value> ')'
   <pseudo-element-selector> = ':' <pseudo-class-selector>
-}
-- pseudo: ':' ':'? [ IDENT | functional_pseudo ]
pseudo :: Parser SimpleSelector
pseudo = char ':' *> (pseudoElementSelector <|> pseudoClassSelector)
  where pseudoClassSelector = do
            i <- ident
            c <- A.peekChar
            case c of
              Just '(' -> char '(' *> case Map.lookup (T.toCaseFold i) fpcMap of 
                            Just p  -> functionParser p
                            Nothing -> functionParser (FunctionalPseudoClass i <$> A.takeWhile (/= ')'))
              _        -> pure $ PseudoClass i
        pseudoElementSelector = do
            parsedColon <- option False (char ':' *> pure True)
            if parsedColon
               then PseudoElem <$> ident
               else ident >>= handleSpecialCase 
          where handleSpecialCase :: Text -> Parser SimpleSelector
                handleSpecialCase t = if T.toCaseFold t `elem` specialPseudoElements
                                         then pure $ PseudoElem t
                                         else mzero

anplusb :: Parser AnPlusB 
anplusb = (asciiCI "even" *> pure Even)
      <|> (asciiCI "odd" *> pure Odd) 
      <|> do
    s <- option Nothing (Just <$> parseSign)
    x <- option mempty digits
    case x of
      [] -> ciN *> (AB (Nwith s Nothing) <$> (skipComments *> option Nothing (Just <$> bValue)))
      _  -> do n <- option False (ciN *> pure True)
               let a = read x :: Int
               if n
                  then AB (Nwith s (Just a)) <$> (skipComments *> option Nothing (Just <$> bValue))
                  else pure $ AB NoValue (Just $ getSign s * a)
  where ciN       = satisfy (\c -> c == 'N' || c == 'n')
        parseSign = (char '-' *> pure Minus) <|> (char '+' *> pure Plus)
        getSign (Just Minus) = -1
        getSign _            = 1
        bValue    = do
            readPlus <- (char '-' *> pure False) <|> (char '+' *> pure True)
            d        <- skipComments *> digits
            if readPlus
               then pure $ read d
               else pure $ read ('-':d)
                    
-- Functional pseudo classes parsers map
fpcMap :: Map Text (Parser SimpleSelector)
fpcMap = Map.fromList $ fmap (first T.toCaseFold)
    [buildTuple "nth-of-type"      (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "nth-last-of-type" (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "nth-column"       (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "nth-last-column"  (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "not"              (\x -> FunctionalPseudoClass1 x <$> compoundSelectorList)
    ,buildTuple "matches"          (\x -> FunctionalPseudoClass1 x <$> compoundSelectorList)
    ,buildTuple "nth-child"        (anbAndSelectors . FunctionalPseudoClass3)
    ,buildTuple "nth-last-child"   (anbAndSelectors . FunctionalPseudoClass3)
    --
    -- :drop( [ active || valid || invalid ]? ) 
    -- The :drop() functional pseudo-class is identical to :drop
    -- ,("drop", anplusb)
    --
    -- It accepts a comma-separated list of one or more language ranges as its
    -- argument. Each language range in :lang() must be a valid CSS <ident> or
    -- <string>. 
    -- ,("lang", anplusb)
    --
    -- ,("dir", text)
    -- ,("has", relative selectors)
    ]
  where buildTuple t c = (t, c t)
        compoundSelectorList = (:) <$> compoundSelector <*> many (comma *> compoundSelector)
        anbAndSelectors constructor = do
            a <- anplusb <* skipComments
            o <- option [] (asciiCI "of" *> skipComments *> compoundSelectorList)
            pure $ constructor a o

selectors :: Parser [Selector]
selectors = lexeme selector `sepBy` char ','

declaration :: Parser Declaration
declaration = do
    p  <- property <* colon
    v  <- values p <|> valuesFallback
    i  <- important <* skipComments
    ie <- iehack <* skipComments
    pure $ Declaration p v i ie

-- Usually, ident would be enough, but this parser is used to support IE hacks.
-- for example *width:
property :: Parser Text
property = mappend <$> opt ie7orLessHack <*> ident
  where ie7orLessHack = T.singleton <$> satisfy (`Set.member` ie7orLessHacks)
        ie7orLessHacks = Set.fromList ("!$&*()=%+@,./`[]#~?:<>|" :: String)

-- Parses the "!important" at the end of declarations, ignoring comments after
-- the '!'.
important :: Parser Bool
important = option False (char '!' *> skipComments *> asciiCI "important" *> pure True)

iehack :: Parser Bool
iehack = option False (string "\\9" *> pure True)

-- Accepts empty lists of declaration
declarations :: Parser [Declaration]
declarations = many (declaration <* handleSemicolons)
  where handleSemicolons = many (string ";" *> skipComments)

atRule :: Parser Rule
atRule = do
    _ <- char '@'
    ruleType <- ident
    fromMaybe (atBlock ruleType) (Map.lookup ruleType m)
  where m = Map.fromList [("charset",           atCharset)
                         ,("import",            atImport)
                         ,("namespace",         atNamespace)
                         ,("media",             atMedia)
                         ,("font-face",         skipComments *> atBlock "font-face")
                         ,("keyframes",         atKeyframe mempty )
                         ,("-webkit-keyframes", atKeyframe "-webkit-")
                         ,("-moz-keyframes",    atKeyframe "-moz-")
                         ,("-o-keyframes",      atKeyframe "-o-")
                         ]
-- @import [ <string> | <url> ] [<media-query-list>]?;
atImport :: Parser Rule
atImport = do
    esu <- skipComments *> stringOrUrl
    mql <- option [] mediaQueryList
    _   <- skipComments <* char ';'
    pure $ AtImport esu mql

atCharset :: Parser Rule
atCharset = do
    st <- skipComments *> stringtype <* skipComments <* char ';'
    pure $ AtCharset st

-- @namespace <namespace-prefix>? [ <string> | <uri> ];
-- where 
-- <namespace-prefix> = IDENT
atNamespace :: Parser Rule
atNamespace = do
    i   <- skipComments *> option mempty ident
    ret <- if T.null i
              then (AtNamespace i . Left) <$> stringtype
              else decideBasedOn i 
    _ <- skipComments <* char ';'
    pure ret
  where decideBasedOn x = 
            let urltext = T.toCaseFold "url"
            in if T.toCaseFold x == urltext
                  then do c <- A.peekChar
                          case c of
                            Just '(' -> AtNamespace mempty <$> (char '(' *> (Right <$> url))
                            _        -> AtNamespace x <$> (skipComments *> stringOrUrl)
                  else AtNamespace x <$> (skipComments *> stringOrUrl)

atKeyframe :: Text -> Parser Rule
atKeyframe t = do
    _    <- skipComments
    name <- ident <* skipComments <* char '{'
    bs   <- many (keyframeBlock <* skipComments)
    _    <- char '}'
    pure $ AtKeyframes t name bs

keyframeBlock :: Parser KeyframeBlock
keyframeBlock = do
    sel  <- skipComments *> kfsList <* skipComments 
    ds   <- char '{' *> skipComments *> declarations <* char '}'
    pure $ KeyframeBlock sel ds
  where from = asciiCI "from" *> pure From
        to   = asciiCI "to" *> pure To
        keyframeSelector = from <|> to <|> (KFPercentage <$> percentage)
        kfsList = (:) <$> keyframeSelector <*> many (comma *> keyframeSelector)

atMedia :: Parser Rule
atMedia = do
  m <- satisfy C.isSpace *> mediaQueryList
  _ <- char '{' <* skipComments
  r <- manyTill (rule <* skipComments) (lookAhead (char '}'))
  _ <- char '}'
  pure $ AtMedia m r

-- TODO clean code
-- the "manyTill .. lookAhead" was added because if we only used "rules", it
-- doesn't know when to stop, and breaks the parser
atBlock :: Text -> Parser Rule
atBlock i = do 
  t <- mappend i <$> A.takeWhile (/= '{') <* char '{'
  r <- skipComments *> ((AtBlockWithDec t <$> declarations) <|> (AtBlockWithRules t <$> manyTill (rule <* skipComments) (lookAhead (char '}'))))
  _ <- char '}'
  pure r

styleRule :: Parser Rule
styleRule = do
    sels <- selectors <* char '{' <* skipComments 
    decs <- declarations <* char '}'
    pure $ StyleRule sels decs

rule :: Parser Rule
rule = atRule <|> styleRule

rules :: Parser [Rule]
rules = manyTill (rule <* skipComments) endOfInput

stylesheet :: Parser [Rule]
stylesheet = do
  charset <- option [] ((:[]) <$> atCharset <* skipComments)
  imports <- many (atImport <* skipComments)
  namespaces <- many (atNamespace <* skipComments)
  _ <- skipComments -- if there is no charset, import, or namespace at rule we need this here.
  rest <- rules
  pure $ charset <> imports <> namespaces <> rest

-- f ('\\':xs) = 

atMost :: Int -> Parser a -> Parser [a]
atMost 1 p = option [] ((:[]) <$> p)
atMost n p = do 
    a  <- option Nothing (Just <$> p)
    case a of
      Just x -> do xs <- atMost (n-1) p
                   pure $ x : xs
      Nothing -> pure []

-- data AtRule = AtMedia (Maybe [MediaQuery])

-- | Media Feature values. Per the
-- <https://www.w3.org/TR/css3-mediaqueries/#values original spec (6.1)>,
-- \<resolution\> only accepts dpi and dpcm units, dppx was added later.
-- However, the w3c validator considers it valid, so we make no exceptions.
{-
data MediaFeatureValue = MFV_Length Length
                       | MFV_Ratio Ratio
                       | MFV_Number Number
                       | MFV_Resolution Resolution
                       | MFV_Other Text
-}

-- | Specs:
--
-- https://drafts.csswg.org/mediaqueries-3/#syntax
-- https://www.w3.org/TR/css3-mediaqueries/
-- https://www.w3.org/TR/CSS21/grammar.html
--
-- Implementation based on mozilla's pseudo BNF:
-- https://developer.mozilla.org/en-US/docs/Web/CSS/Media_Queries/Using_media_queries
mediaQueryList :: Parser [MediaQuery]
mediaQueryList = lexeme ((:) <$> mediaQuery <*> many (char ',' *> skipComments *> mediaQuery))

mediaQuery :: Parser MediaQuery
mediaQuery = mediaQuery1 <|> mediaQuery2
  where mediaQuery1 = MediaQuery1 <$> optionalNotOrOnly <*> mediaType <*> andExpressions
        mediaQuery2 = MediaQuery2 <$> ((:) <$> expression <*> andExpressions)
        mediaType = lexeme ident
        andExpressions = many (h *> expression)
        h = skipComments *> asciiCI "and" *> satisfy C.isSpace *> skipComments
        optionalNotOrOnly = option mempty (asciiCI "not" <|> asciiCI "only")

expression :: Parser Expression
expression = char '(' *> skipComments *> (expr <|> expFallback)
  where expr = do
             e <- ident <* skipComments 
             v <- option Nothing (char ':' *> lexeme (Just <$> value))
             _ <- char ')'
             pure $ Expression e v
        expFallback = InvalidExpression <$> A.takeWhile (/= ')') <* char ')'
            
            

-- Note: The code below pertains to CSS Media Queries Level 4.
-- Since it is still too new and nobody implements it (afaik),
-- I leave it here for future reference, when the need to cater for it
-- arrives.
{-
mediaCondition = asciiCI "not" 
              <|> asciiCI "and" 
              <|> asciiCI "or" 
              <|> mediaInParens

mediaInParens = char '(' *> skipComments *> mediaCondition <* skipComments <* char ')'

mediaFeature :: Parser MediaFeature
mediaFeature = mfPlain <|> mfRange <|> mfBoolean
  where mfBoolean = ident

mfRange = ident 

data MediaFeature = MFPlain Text Value
                  | MFBoolean Text
                  | MFRange Range

data Range = Range1 Text RangeOp Value
           | Range2 Value RangeOp Text
           | Range3 Value RangeOp Text RangeOp Value

data RangeOp = LTOP | GTOP | EQOP | GEQOP | LEQOP

-- = <mf-name> [ '<' | '>' ]? '='? <mf-value>
--            | <mf-value> [ '<' | '>' ]? '='? <mf-name>
--            | <mf-value> '<' '='? <mf-name> '<' '='? <mf-value>
--            | <mf-value> '>' '='? <mf-name> '>' '='? <mf-value>

mfPlain = ident *> skipComments *> char ':' *> skipComments *> mfValue

mfValue :: Parser Value
mfValue = number <|> dimension <|> ident <|> ratio 

-- TODO check if both integers are positive (required by the spec)
ratio :: Parser Value
ratio = do
    n <- digits
    _ <- skipComments *> char '/' <* skipComments
    m <- digits
    pure $ Other (mconcat [n, "/", m])



-- expr
--  : term [ operator? term ]*
--  ;
--
--term
-- : unary_operator?
--    [ NUMBER S* | PERCENTAGE S* | LENGTH S* | EMS S* | EXS S* | ANGLE S* |
--      TIME S* | FREQ S* ]
--  | STRING S* | IDENT S* | URI S* | hexcolor | function
--  ;
--

data MediaFeatureType = Range | Discrete

t = 
  [("width",               Range)
  ,("height",              Range)
  ,("aspect-ratio",        Range)
  ,("orientation",         Discrete)
  ,("resolution",          Range)
  ,("scan",                Discrete)
  ,("grid",                Discrete)
  ,("update",              Discrete)
  ,("overflow-block",      Discrete)
  ,("overflow-inline",     Discrete)
  ,("color",               Range)
  ,("color-index",         Range)
  ,("monochrome",          Range)
  ,("color-gamut",         Discrete)
  ,("pointer",             Discrete)
  ,("hover",               Discrete)
  ,("any-pointer",         Discrete)
  ,("any-hover",           Discrete)
  ,("scripting",           Discrete)
  ,("device-width",        Range)
  ,("device-height",       Range)
  ,("device-aspect-ratio", Range)
  ]
-}
