{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Parser.Internal
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Parser.Internal
    ( stylesheet
    , atRule
    , atMedia
    , styleRule
    , rule
    , rules
    , declaration
    , declarations
    , selector
    , supportsCondition
    ) where

import Control.Applicative ((<|>), many, some, empty, optional)
import Data.Functor (($>))
import Data.Attoparsec.Combinator (lookAhead, sepBy, endOfInput)
import Data.Attoparsec.Text (asciiCI, char, many1, manyTill,
  option, Parser, satisfy, string)
import Data.List.NonEmpty (NonEmpty( (:|) ))
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy.Builder as LB
import Data.Map.Strict (Map)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Hasmin.Parser.Utils
import Hasmin.Parser.Value
import Hasmin.Types.Selector
import Hasmin.Types.Stylesheet
import Hasmin.Types.Declaration
import Hasmin.Types.String
import Hasmin.Utils

-- | Parser for CSS complex selectors (see 'Selector' for more details).
selector :: Parser Selector
selector = Selector <$> compoundSelector <*> combinatorsAndSelectors
  where combinatorsAndSelectors = many $ mzip (combinator <* skipComments) compoundSelector

-- First tries with '>>' (descendant), '>' (child), '+' (adjacent sibling), and
-- '~' (general sibling) combinators. If those fail, it tries with the
-- descendant (whitespace) combinator. This is done to allow comments in-between.
--
-- | Parser for selector combinators, i.e. ">>" (descendant), '>' (child), '+'
-- (adjacent sibling), '~' (general sibling), and ' ' (descendant) combinators.
combinator :: Parser Combinator
combinator =  (skipComments *> ((string ">>" $> DescendantBrackets)
          <|> (char '>' $> Child)
          <|> (char '+' $> AdjacentSibling)
          <|> (char '~' $> GeneralSibling)))
          <|> (satisfy ws $> DescendantSpace)
  where ws c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'

compoundSelector :: Parser CompoundSelector
compoundSelector =
    (:|) <$> (typeSelector <|> universal)
                           <*> many p
                       <|> ((Universal mempty :|) <$> many1 p)
  where p = idSel <|> classSel <|> attributeSel <|> pseudo
-- | Parses a \"number sign\" (U+0023, \#) immediately followed by the ID value,
-- which must be a CSS identifier.
idSel :: Parser SimpleSelector
idSel = do
    _    <- char '#'
    name <- mconcat <$> many1 nmchar
    pure . IdSel . TL.toStrict $ toLazyText name

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
    _     <- char '['
    attId <- lexeme ident
    g     <- option Attribute attValue
    _     <- char ']'
    pure $ AttributeSel (g attId)
  where attValue = do
          f <- ((string "^=" $> (:^=:)) <|>
                (string "$=" $> (:$=:)) <|>
                (string "*=" $> (:*=:)) <|>
                (string "="  $> (:=:))  <|>
                (string "~=" $> (:~=:)) <|>
                (string "|=" $> (:|=:))) <* skipComments
          attval <- identOrString <* skipComments
          pure (`f` attval)

identOrString :: Parser (Either Text StringType)
identOrString = (Left <$> ident) <|> (Right <$> stringtype)

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
        pseudoElementSelector =
            (char ':' *> (PseudoElem <$> ident)) <|> (ident >>= handleSpecialCase)
          where
            handleSpecialCase :: Text -> Parser SimpleSelector
            handleSpecialCase t
                | isSpecialPseudoElement = pure $ PseudoElem t
                | otherwise              = empty
              where isSpecialPseudoElement = T.toCaseFold t `elem` specialPseudoElements

-- \<An+B> microsyntax parser.
anplusb :: Parser AnPlusB
anplusb = (asciiCI "even" $> Even)
      <|> (asciiCI "odd" $> Odd)
      <|> do
        s    <- optional parseSign
        dgts <- option mempty digits
        case dgts of
          [] -> ciN *> skipComments *> option (A s Nothing) (AB s Nothing <$> bValue)
          _  -> let n = read dgts :: Int
                in (ciN *> skipComments *> option (A s $ Just n) (AB s (Just n) <$> bValue))
                    <|> (pure . B $ getSign s * n)
  where ciN       = satisfy (\c -> c == 'N' || c == 'n')
        parseSign = (char '-' $> Minus) <|> (char '+' $> Plus)
        getSign (Just Minus) = -1
        getSign _            = 1
        bValue    = do
            readPlus <- (char '-' $> False) <|> (char '+' $> True)
            d        <- skipComments *> digits
            if readPlus
               then pure $ read d
               else pure $ read ('-':d)

-- Functional pseudo classes parsers map
fpcMap :: Map Text (Parser SimpleSelector)
fpcMap = Map.fromList
    [buildTuple "nth-of-type"      (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "nth-last-of-type" (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "nth-column"       (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "nth-last-column"  (\x -> FunctionalPseudoClass2 x <$> anplusb)
    ,buildTuple "not"              (\x -> FunctionalPseudoClass1 x <$> compoundSelectorList)
    ,buildTuple "matches"          (\x -> FunctionalPseudoClass1 x <$> compoundSelectorList)
    ,buildTuple "nth-child"        (anbAndSelectors . FunctionalPseudoClass3)
    ,buildTuple "nth-last-child"   (anbAndSelectors . FunctionalPseudoClass3)
    ,buildTuple "lang"             (const (Lang <$> identOrString))
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

-- | Parse a list of comma-separated selectors, ignoring whitespace and
-- comments.
selectors :: Parser [Selector]
selectors = lexeme selector `sepBy` char ','

-- | Parser for a declaration, starting by the property name.
declaration :: Parser Declaration
declaration = do
    p  <- property <* colon
    v  <- valuesFor p <|> valuesFallback
    i  <- important
    ie <- lexeme iehack
    pure $ Declaration p v i ie

-- | Parser for property names. Usually, 'ident' would be enough, but this
-- parser adds support for IE hacks (e.g. *width), which deviate from the CSS
-- grammar.
property :: Parser Text
property = mappend <$> opt ie7orLessHack <*> ident
  where ie7orLessHack = T.singleton <$> satisfy (`Set.member` ie7orLessHacks)
        ie7orLessHacks = Set.fromList ("!$&*()=%+@,./`[]#~?:<>|" :: String)

-- | Used to parse the "!important" at the end of declarations, ignoring spaces
-- and comments after the '!'.
important :: Parser Bool
important = option False (char '!' *> skipComments *> asciiCI "important" $> True)

iehack :: Parser Bool
iehack = option False (string "\\9" $> True)

-- Note: The handleSemicolons outside is needed to handle parsing "h1 { ; }".
--
-- | Parser for a list of declarations, ignoring spaces, comments, and empty
-- declarations (e.g. ; ;)
declarations :: Parser [Declaration]
declarations = many (declaration <* handleSemicolons) <* handleSemicolons
  where handleSemicolons = many (string ";" *> skipComments)

-- | Parser for CSS at-rules (e.g. \@keyframes, \@media)
atRule :: Parser Rule
atRule = do
    _ <- char '@'
    ruleType <- ident
    fromMaybe (atBlock ruleType) (Map.lookup ruleType m)
  where m = Map.fromList [("charset",           atCharset)
                         ,("import",            atImport)
                         ,("namespace",         atNamespace)
                         ,("media",             atMedia)
                         ,("supports",          atSupports)
                         -- ,("document",          atDocument)
                         -- ,("page",              atPage)
                         ,("font-face",         skipComments *> atBlock "font-face")
                         ,("keyframes",         atKeyframe mempty )
                         ,("-webkit-keyframes", atKeyframe "-webkit-")
                         ,("-moz-keyframes",    atKeyframe "-moz-")
                         ,("-o-keyframes",      atKeyframe "-o-")
                         -- ,("viewport",              atViewport)
                         -- ,("counter-style",         atCounterStyle)
                         -- ,("font-feature-value",    atFontFeatureValue)
                         ]
-- @import [ <string> | <url> ] [<media-query-list>]?;
atImport :: Parser Rule
atImport = do
    esu <- skipComments *> stringOrUrl
    mql <- option [] mediaQueryList
    _   <- skipComments <* char ';'
    pure $ AtImport esu mql

atCharset :: Parser Rule
atCharset = AtCharset <$> (lexeme stringtype <* char ';')

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
  where decideBasedOn x
            | T.toCaseFold x == "url" =
                do c <- A.peekChar
                   case c of
                     Just '(' -> AtNamespace mempty <$> (char '(' *> (Right <$> url))
                     _        -> AtNamespace x <$> (skipComments *> stringOrUrl)
            | otherwise = AtNamespace x <$> (skipComments *> stringOrUrl)

atKeyframe :: Text -> Parser Rule
atKeyframe t = do
    name <- lexeme ident <* char '{'
    bs   <- many (keyframeBlock <* skipComments)
    _    <- char '}'
    pure $ AtKeyframes t name bs

keyframeBlock :: Parser KeyframeBlock
keyframeBlock = do
    sel  <- lexeme kfsList
    ds   <- char '{' *> skipComments *> declarations <* char '}'
    pure $ KeyframeBlock sel ds
  where from = asciiCI "from" $> From
        to   = asciiCI "to" $> To
        keyframeSelector = from <|> to <|> (KFPercentage <$> percentage)
        kfsList = (:) <$> keyframeSelector <*> many (comma *> keyframeSelector)

atMedia :: Parser Rule
atMedia = do
  m <- satisfy C.isSpace *> mediaQueryList
  _ <- char '{' <* skipComments
  r <- manyTill (rule <* skipComments) (lookAhead (char '}'))
  _ <- char '}'
  pure $ AtMedia m r

atSupports :: Parser Rule
atSupports = do
  sc <- satisfy C.isSpace *> supportsCondition
  _  <- lexeme (char '{')
  r  <- manyTill (rule <* skipComments) (lookAhead (char '}'))
  _ <- char '}'
  pure $ AtSupports sc r

-- | Parser for a <https://drafts.csswg.org/css-conditional-3/#supports_condition supports_condition>,
-- needed by @\@supports@ rules.
supportsCondition :: Parser SupportsCondition
supportsCondition = asciiCI "not" *> skipComments *> (Not <$> supportsCondInParens)
                <|> supportsConjunction
                <|> supportsDisjunction
                <|> (Parens <$> supportsCondInParens)
  where
    supportsDisjunction :: Parser SupportsCondition
    supportsDisjunction = supportsHelper Or "or"

    supportsConjunction :: Parser SupportsCondition
    supportsConjunction = supportsHelper And "and"

supportsCondInParens :: Parser SupportsCondInParens
supportsCondInParens = do
    _ <- char '('
    x <- lexeme $ (ParensCond <$> supportsCondition) <|> (ParensDec <$> atSupportsDeclaration)
    _ <- char ')'
    pure x

atSupportsDeclaration :: Parser Declaration
atSupportsDeclaration = do
    p  <- property <* colon
    v  <- valuesFor p <|> valuesInParens
    pure $ Declaration p v False False

-- customPropertyIdent :: Parser Text
-- customPropertyIdent = (<>) <$> string "-" <*> ident

supportsHelper :: (SupportsCondInParens -> NonEmpty SupportsCondInParens -> SupportsCondition)
               ->  Text -> Parser SupportsCondition
supportsHelper c t = do
    x  <- supportsCondInParens <* skipComments
    xs <- some (asciiCI t *> lexeme supportsCondInParens)
    pure $ c x (NE.fromList xs)

-- TODO clean code
-- the "manyTill .. lookAhead" was added because if we only used "rules", it
-- doesn't know when to stop, and breaks the parser
atBlock :: Text -> Parser Rule
atBlock i = do
  t <- mappend i <$> A.takeWhile (/= '{') <* char '{'
  r <- skipComments *> ((AtBlockWithDec t <$> declarations) <|> (AtBlockWithRules t <$> manyTill (rule <* skipComments) (lookAhead (char '}'))))
  _ <- char '}'
  pure r

-- | Parses a CSS style rule, e.g. @body { padding: 0; }@
styleRule :: Parser Rule
styleRule = do
    sels <- selectors <* char '{' <* skipComments
    decs <- declarations <* char '}'
    pure $ StyleRule sels decs

-- | Parser for a CSS rule, which can be either an at-rule (e.g. \@charset), or a style
-- rule.
rule :: Parser Rule
rule = atRule <|> styleRule

-- | Parser for CSS rules (both style rules, and at-rules), which can be
-- separated by whitespace or comments.
rules :: Parser [Rule]
rules = manyTill (rule <* skipComments) endOfInput

-- | Parse a stylesheet, starting by the \@charset, \@import and \@namespace
-- rules, followed by the list of rules, and ignoring any unneeded whitespace
-- and comments.
stylesheet :: Parser [Rule]
stylesheet = do
  charset    <- option [] ((:[]) <$> atCharset <* skipComments)
  imports    <- many (atImport <* skipComments)
  namespaces <- many (atNamespace <* skipComments)
  _ <- skipComments -- if there is no charset, import, or namespace at rule we need this here.
  rest <- rules
  pure $ charset <> imports <> namespaces <> rest

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
        h = lexeme (asciiCI "and" *> satisfy C.isSpace)
        optionalNotOrOnly = option mempty (asciiCI "not" <|> asciiCI "only")

-- https://www.w3.org/TR/mediaqueries-4/#typedef-media-condition-without-or
expression :: Parser Expression
expression = char '(' *> skipComments *> (expr <|> expFallback)
  where expr = do
             e <- ident <* skipComments
             v <- optional (char ':' *> lexeme value)
             _ <- char ')' -- Needed here for expFallback to trigger
             pure $ Expression e v
        expFallback = InvalidExpression <$> A.takeWhile (/= ')') <* char ')'

-- TODO implement the whole spec 4, or at least 3.
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
