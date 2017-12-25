{-# LANGUAGE OverloadedStrings, FlexibleInstances, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.Value
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-----------------------------------------------------------------------------
module Hasmin.Types.Value
    ( Value(..)
    , Values(..)
    , TextV(..)
    , Separator(..)
    , Url(..)
    , mkOther
    , mkValues
    , valuesToList
    , optimizeFontFamily
    , lowercaseText
    ) where

import Control.Monad.Reader (ask, Reader, mapReader)
import Data.Monoid ((<>))
import Data.Maybe (isJust, catMaybes, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromText, singleton, Builder)
import Data.String (IsString)

import Hasmin.Class
import Hasmin.Config
import Hasmin.Types.BgSize
import Hasmin.Types.BorderRadius
import Hasmin.Types.Color
import Hasmin.Types.Dimension
import Hasmin.Types.FilterFunction
import Hasmin.Types.Gradient
import Hasmin.Types.Numeric
import Hasmin.Types.Position
import Hasmin.Types.RepeatStyle
import Hasmin.Types.Shadow
import Hasmin.Types.String
import Hasmin.Types.TimingFunction
import Hasmin.Types.TransformFunction
import Hasmin.Utils

-- | A CSS <https://www.w3.org/TR/css-values-3/#value-defs value>.
data Value = Inherit
           | Initial
           | Unset
           | NumberV Number
           | PercentageV Percentage
           | LengthV Length
           | AngleV Angle
           | TimeV Time
           | FrequencyV Frequency
           | ResolutionV Resolution
           | ColorV Color
           | GradientV Text Gradient -- the Text is just to handle function prefixes
           | GenericFunc Text Values
           | TransformV TransformFunction
           | TimingFuncV TimingFunction
           | FilterV FilterFunction
           | ShadowV Shadow
           | ShadowText Length Length (Maybe Length) (Maybe Color) -- <shadow-t> type
           | PositionV Position
           | RepeatStyleV RepeatStyle
           | BorderRadiusV BorderRadius
           | BgSizeV BgSize
           -- <bg-image> || <position> [ / <bg-size> ]? || <repeat-style> || <attachment> || <box>{1,2}
           | BgLayer
              { _bgimage      :: Maybe Value
              , _bgposition   :: Maybe Position
              , _bgsize       :: Maybe BgSize
              , _bgrepeat     :: Maybe RepeatStyle
              , _bgattachment :: Maybe TextV
              , _bgbox1       :: Maybe TextV
              , _bgbox2       :: Maybe TextV
              }
           --              <bg-image> || <position> [ / <bg-size> ]? || <repeat-style> || <attachment> || <box> || <box> || <'background-color'>
           | FinalBgLayer (Maybe Value) (Maybe Position) (Maybe BgSize) (Maybe RepeatStyle) (Maybe TextV) (Maybe TextV) (Maybe TextV) (Maybe Color)
           -- [ none | <single-transition-property> ] || <time> || <single-transition-timing-function> || <time>
           | SingleTransition
              { _transitionproperty :: Maybe TextV
              , _time1 :: Maybe Time
              , _transitiontimingfunction :: Maybe TimingFunction
              , _time2 :: Maybe Time
              }
           --                         <time> || <timing-function> || <time> || <iteration-count> || <animation-direction> || <animation-fill-mode> || <animation-play-state> || [ none | <keyframes-name> ]
           | SingleAnimation (Maybe Time) (Maybe TimingFunction) (Maybe Time) (Maybe Value)  (Maybe TextV) (Maybe TextV) (Maybe TextV) (Maybe Value)
           -- [ [ <'font-style'> || <font-variant-css21> || <'font-weight'> || <'font-stretch'> ]? <'font-size'> [ / <'line-height'> ]? <'font-family'> ] |
           | FontV (Maybe TextV) (Maybe TextV) (Maybe Value) (Maybe TextV) Value (Maybe Value) [Value]
           | StringV StringType
           | UrlV Url
           | Format [StringType]
           | Local (Either Text StringType)
           | Rect Length Length Length Length -- Should be in <shape>, but that accepts only rect()
           | Other TextV -- ^ Unrecognized text
  deriving (Eq, Show)

-- | Redefines equality to be case-insensitive, since CSS literal values such as
-- "auto", "none", etc. are so.
newtype TextV = TextV { getText :: Text }
  deriving (Show, Ord, IsString)

instance Eq TextV where
  TextV t1 == TextV t2 = T.toLower t1 == T.toLower t2
instance ToText TextV where
  toText = getText

mkOther :: Text -> Value
mkOther = Other . TextV

-- A <https://drafts.csswg.org/css-values-3/#urls \<url\>> value, which can
-- contain either a CSS <https://drafts.csswg.org/css-values-3/#string-value \<string\>>
-- (i.e. text surrounded by single or double quotes), or an unquoted string.
newtype Url = Url (Either Text StringType)
  deriving (Eq, Show)
instance ToText Url where
  toBuilder (Url x) = "url(" <> toBuilder x <> singleton ')'
instance Minifiable Url where
  minify u@(Url x) = do
      conf <- ask
      pure $ if shouldRemoveQuotes conf
                then Url $ either Left unquoteUrl x
                else u

instance ToText Value where
  toBuilder Initial         = "initial"
  toBuilder Inherit         = "inherit"
  toBuilder Unset           = "unset"
  toBuilder (NumberV n)     = toBuilder n
  toBuilder (PercentageV p) = (fromText . toText) p
  toBuilder (ColorV c)      = toBuilder c
  toBuilder (LengthV d)     = toBuilder d
  toBuilder (AngleV a)      = toBuilder a
  toBuilder (TimeV d)       = toBuilder d
  toBuilder (FrequencyV f)  = toBuilder f
  toBuilder (ResolutionV r) = toBuilder r
  toBuilder (FilterV f)     = toBuilder f
  toBuilder (ShadowV s)     = toBuilder s
  toBuilder (StringV s)     = toBuilder s
  toBuilder (Other t)       = fromText (getText t)
  toBuilder (UrlV u)        = toBuilder u
  toBuilder (GenericFunc n vs) = toBuilder n <> singleton '(' <> toBuilder vs <> singleton ')'
  toBuilder (Local x)       = "local(" <> toBuilder x <> singleton ')'
  toBuilder (Format x)      = "format(" <> formatString <> singleton ')'
    where formatString = mconcatIntersperse id (singleton ',') (fmap toBuilder x)
  toBuilder (GradientV t g) = fromText t <> singleton '(' <> toBuilder g <> singleton ')'
  toBuilder (TransformV x)  = toBuilder x
  toBuilder (TimingFuncV x) = toBuilder x
  toBuilder (Rect a b c d)  = "rect(" <> funcValues <> singleton ')'
    where funcValues = mconcatIntersperse toBuilder (singleton ',') [a, b, c, d]
  toBuilder (PositionV p)   = toBuilder p
  toBuilder (RepeatStyleV r) = toBuilder r
  toBuilder (BorderRadiusV b) = toBuilder b
  toBuilder (BgSizeV b)     = toBuilder b
  toBuilder (BgLayer a b c d e f g) =
      let sz  = maybe mempty (\x -> singleton '/' <> toBuilder x) c
          list = catMaybes [bld a, bld d, bld e, bld f, bld g]
      in if null list
            then maybe mempty toBuilder b <> sz
            else mconcatIntersperse id (singleton ' ') list <> maybe mempty (\x -> singleton ' ' <> toBuilder x) b <> sz
  toBuilder (FinalBgLayer a b c d e f g col) =
      let sz  = maybe mempty (\x -> singleton '/' <> toBuilder x) c
          list = catMaybes [bld a, bld d, bld e, bld f, bld g]
      in if null list
            then let posAndSize = maybe mempty toBuilder b <> sz
                 in if mempty == posAndSize
                       then maybe mempty toBuilder col
                       else posAndSize <> spacePrefixed col
            else mconcatIntersperse id (singleton ' ') list <> spacePrefixed b <> sz <> spacePrefixed col
    where spacePrefixed :: ToText a => Maybe a -> Builder
          spacePrefixed = maybe mempty (\x -> singleton ' ' <> toBuilder x)
  toBuilder (SingleTransition prop t1 tf t2) =
      mconcatIntersperse id (singleton ' ') $ catMaybes [bld tf, bld t1, bld t2, bld prop]
  toBuilder (SingleAnimation t1 tf t2 ic ad af ap kf) =
      let list = catMaybes [bld t1, bld t2, bld tf, bld ic, bld ad, bld af, bld ap, bld kf]
      in mconcatIntersperse id (singleton ' ') list
  toBuilder (FontV fsty fvar fwgt fstr fsz lh ff) =
      let bldLh = maybe mempty (\x -> singleton '/' <> toBuilder x) lh
          list  = catMaybes [bld fsty, bld fvar, bld fwgt, bld fstr]
          ffam  = singleton ' ' <> mconcatIntersperse toBuilder (singleton ',') ff
      in if null list
            then toBuilder fsz <> bldLh <> ffam
            else mconcatIntersperse id (singleton ' ') list <> singleton ' ' <> toBuilder fsz <> bldLh <> ffam
  toBuilder (ShadowText l1 l2 ml mc) =
      let maybeToBuilder :: ToText a => Maybe a -> Builder
          maybeToBuilder = maybe mempty (\x -> singleton ' ' <> toBuilder x)
      in toBuilder l1 <> singleton ' ' <> toBuilder l2
       <> maybeToBuilder ml <> maybeToBuilder mc

instance Minifiable Value where
  minify (ColorV c)        = ColorV <$> minify c
  minify (LengthV d)       = LengthV <$> minify d
  minify (AngleV a)        = AngleV <$> minify a
  minify (TimeV d)         = TimeV <$> minify d
  minify (FrequencyV f)    = FrequencyV <$> minify f
  minify (ResolutionV r)   = ResolutionV <$> minify r
  minify (GradientV t g)   = GradientV t <$> minify g
  minify (FilterV f)       = FilterV <$> minify f
  minify (TransformV tf)   = TransformV <$> minify tf
  minify (TimingFuncV tf)  = TimingFuncV <$> minify tf
  minify (StringV s)       = StringV <$> minify s
  minify (UrlV u)          = UrlV <$> minify u
  minify (Format x)        = Format <$> mapM minify x
  minify (PositionV p)     = PositionV <$> minify p
  minify (RepeatStyleV r)  = RepeatStyleV <$> minify r
  minify (BgSizeV b)       = BgSizeV <$> minify b
  minify (BorderRadiusV b) = BorderRadiusV <$> minify b
  minify (ShadowV s)      = ShadowV <$> minify s
  minify (ShadowText l1 l2 ml mc) = minifyPseudoShadow ShadowText l1 l2 ml mc
  minify (BgLayer img pos sz rst att b1 b2) = do
      -- conf <- ask
      (i,s,p,r,a) <- minifyBgLayer img pos sz rst att
      (bgOrigin, bgClip) <- handleBoxes b1 b2
      pure $ if isNothing i && isNothing p && isNothing s && isNothing r
                && isNothing a && isNothing bgOrigin && isNothing bgClip
                -- Shortest would be a '0 0' position, but 'none' compresses
                -- better because it is more frequent.
                then BgLayer (Just $ mkOther "none") p s r a bgOrigin bgClip
                else BgLayer i p s r a bgOrigin bgClip
  minify (FinalBgLayer img pos sz rst att b1 b2 col) = do
      -- conf <- ask
      (i,s,p,r,a) <- minifyBgLayer img pos sz rst att
      c <- handleColor col
      (bgOrigin, bgClip) <- handleBoxes b1 b2
      pure $ if isNothing i && isNothing p && isNothing s && isNothing r
                && isNothing a && isNothing bgOrigin && isNothing bgClip && isNothing c
                -- Shortest would be a '0 0' position, but 'none' compresses
                -- better because it is more frequent.
                then FinalBgLayer (Just $ mkOther "none") p s r a bgOrigin bgClip c
                else FinalBgLayer i p s r a bgOrigin bgClip c
  minify (SingleTransition prop tdur tf tdel) = do
      let p = if prop == Just "all"
                 then Nothing
                 else prop -- TODO lowercase here
      (tTime, tDelay) <- handleTime tdur tdel
      tfunc           <- handleTimingFunction tf
      pure $ if isNothing p && isNothing tTime && isNothing tDelay && isNothing tfunc
                then SingleTransition p (Just $ Time 0 S) tfunc tDelay
                else SingleTransition p tTime tfunc tDelay
  minify (SingleAnimation t1 tf t2 ic ad af ap kf) = do
      (tdur, tdel) <- handleTime t1 t2
      tfunc        <- handleTimingFunction tf
      icount       <- handleIterationCount ic
      (kfrms, adir, afm, p) <- handleKeywords kf ad af ap
      if isNothing tdur && isNothing tdel && isNothing tfunc && isNothing
         icount && isNothing kfrms && isNothing adir && isNothing afm && isNothing p
         then pure $ SingleAnimation tdur tfunc tdel (Just $ NumberV 1) adir afm p kfrms
         else pure $ SingleAnimation tdur tfunc tdel icount adir afm p kfrms
    where handleIterationCount :: Maybe Value -> Reader Config (Maybe Value)
          handleIterationCount Nothing  = pure Nothing
          handleIterationCount (Just x) =
              case x of
              NumberV 1 -> pure Nothing
              _         -> pure (Just x)
          handleKeywords Nothing x y z = pure (Nothing, simplifyDirection x, simplifyFillMode y, simplifyPauseState z)
          handleKeywords v@(Just w) x y z
              | w `elem` fmap mkOther ["normal", "reverse", "alternate", "alternate-reverse"] =
                  pure (Just w, x, simplifyFillMode y, simplifyPauseState z)
              | w == mkOther "none" && v == (Other <$> y) =
                  pure (Nothing, simplifyDirection x, Nothing, simplifyPauseState z)
              | w `elem` fmap mkOther ["forwards", "backwards", "both"] =
                  pure (Just w, simplifyDirection x, y, simplifyPauseState z)
              | w `elem` fmap mkOther ["running", "paused"] =
                  pure (Just w, simplifyDirection x, simplifyFillMode y, z)
              | otherwise = pure (Just w, simplifyDirection x, simplifyFillMode y, simplifyPauseState z)
          simplifyDirection  = removeIfEqualTo "normal"
          simplifyPauseState = removeIfEqualTo "running"
          simplifyFillMode   = removeIfEqualTo "none"
  minify (FontV fsty fvar fwgt fstr fsz lh ff) = do
      let sty = removeIfEqualTo "normal" fsty
          var = removeIfEqualTo "normal" fvar
          str = removeIfEqualTo "normal" fstr
      wgt <- optimizeFontWeight fwgt
      sz  <- minify fsz
      l   <- optimizeLineHeight lh
      fam <- traverse optimizeFontFamily ff
      pure $ FontV sty var wgt str sz l fam
    where optimizeFontWeight :: Maybe Value -> Reader Config (Maybe Value)
          optimizeFontWeight Nothing = pure Nothing
          optimizeFontWeight (Just x) = do
              conf <- ask
              pure $ replaceForSynonym (fontweightSettings conf) x
            where replaceForSynonym s (Other t)
                    | t == "normal"                       = Nothing
                    | t == "bold" && s == FontWeightMinOn = Just $ NumberV 700
                    | otherwise                           = Just $ Other t
                  replaceForSynonym _ (NumberV 400) = Nothing
                  replaceForSynonym _ y = Just y
          optimizeLineHeight Nothing                 = pure Nothing
          optimizeLineHeight (Just (Other "normal")) = pure Nothing
          optimizeLineHeight (Just x)                = Just <$> minify x

  minify (GenericFunc n vs) = GenericFunc n <$> minify vs
  minify (Local x) = do
      conf <- ask
      v <- lowercaseParameters x
      pure . Local $ if shouldRemoveQuotes conf
                        then case v of
                               Right s -> unquoteFontFamily s
                               _       -> v
                        else v
    where lowercaseParameters :: Either Text StringType -> Reader Config (Either Text StringType)
          lowercaseParameters y = do
            conf <- ask
            case letterCase conf of
              Lowercase -> case y of
                             Left  a -> mapReader Left $ lowercaseText a
                             Right b -> mapReader Right $ mapString lowercaseText b >>= minify
              Original  -> pure y
  minify x = pure x

handleRepeatStyle :: Maybe RepeatStyle -> Reader Config (Maybe RepeatStyle)
handleRepeatStyle (Just x)
    | x == RepeatStyle1 RsRepeat = pure Nothing
    | otherwise                  = Just <$> minify x
handleRepeatStyle Nothing = pure Nothing

handleImage :: Maybe Value -> Reader Config (Maybe Value)
handleImage (Just x)
    | x == Other "none" = pure Nothing
    | otherwise         = Just <$> minify x
handleImage Nothing = pure Nothing

handleBoxes :: Maybe TextV -> Maybe TextV -> Reader Config (Maybe TextV, Maybe TextV)
handleBoxes (Just o) (Just c)
    | o == c = pure (Just o, Nothing) -- TODO need lowercasing here too.
    | o == "padding-box" && c == "border-box" = pure (Nothing, Nothing)
    | otherwise = pure (Just o, Just c)
handleBoxes x y = pure (x, y)

handleAttachment :: Maybe TextV -> Reader Config (Maybe TextV)
handleAttachment = maybe (pure Nothing) f
  where f x = pure $ if x == "scroll"
                        then Nothing
                        else Just x -- TODO might need to lowercase here.

handleColor :: Maybe Color -> Reader Config (Maybe Color)
handleColor = maybe (pure Nothing) f
  where f x = if x == Named "transparent"
                 then pure Nothing
                 else Just <$> minify x

handleBgSize :: Maybe BgSize -> Reader Config (Maybe BgSize)
handleBgSize (Just b) = do
    minb <- minify b
    pure $ if minb == BgSize1 (Right Auto)
              then Nothing
              else Just minb
handleBgSize x = pure x

handlePosition :: Bool -> Maybe Position -> Reader Config (Maybe Position)
handlePosition _ Nothing = pure Nothing
handlePosition cannotRemovePos (Just p)
    | cannotRemovePos = Just <$> minify p
    | otherwise    = do
        -- conf <- ask
        mp   <- minify p
        pure $ if mp == Position Nothing l0 Nothing l0
                  then Nothing
                  else Just $ if True {- shouldMinifyPosition conf -}
                                 then mp
                                 else p

-- Represents a list of values, an the separators used between them.
data Values = Values Value [(Separator, Value)]
  deriving (Show, Eq)
instance ToText Values where
  toBuilder (Values v vs) = toBuilder v <> foldr f mempty vs
    where f (sep, val) z = toBuilder sep <> toBuilder val <> z
instance Minifiable Values where
  minify (Values v vs) = do
      newV  <- minify v
      newVs <- (mapM . mapM) minify vs
      pure $ Values newV newVs

-- | A value separator.
data Separator = Space | Slash | Comma
  deriving (Show, Eq)
instance ToText Separator where
  toText Space = " "
  toText Comma = ","
  toText Slash = "/" -- Used, for example, by font to separate font-size and line-height

valuesToList :: Values -> [Value]
valuesToList (Values v vs) = v : map snd vs

lowercaseText :: Text -> Reader Config Text
lowercaseText t = do
    conf <- ask
    pure $ case letterCase conf of
             Lowercase -> T.toLower t
             Original  -> t

-- Used to rewrap a list of values into the Values data type.
-- Only call it with non-empty lists!
mkValues :: [Value] -> Values
mkValues (x:xs) = Values x (zip (repeat Space) xs)
mkValues [ ]    = error "An empty list of values isn't valid"

bld :: ToText a => Maybe a -> Maybe Builder
bld = fmap toBuilder

-- Used for SingleAnimation and SingleTransition minification.
handleTimingFunction :: Maybe TimingFunction -> Reader Config (Maybe TimingFunction)
handleTimingFunction Nothing = pure Nothing
handleTimingFunction (Just tfunc)
    | tfunc == Ease = pure Nothing
    | otherwise = Just <$> minify tfunc

-- Used for SingleAnimation and SingleTransition minification.
handleTime :: Maybe Time -> Maybe Time -> Reader Config (Maybe Time, Maybe Time)
handleTime (Just t) Nothing
    | t == Time 0 S = pure (Nothing, Nothing)
    | otherwise     = mzip (Just <$> minify t) (pure Nothing)
handleTime (Just t1) (Just t2)
    | t1 == Time 0 S = if t2 == t1
                          then pure (Nothing, Nothing)
                          else do newT2 <- minify t2
                                  newT1 <- minify t1
                                  pure (Just newT1, Just newT2)
    | otherwise = do newT1 <- minify t1
                     if t2 == Time 0 S
                        then pure (Just t1, Nothing)
                        else do newT2 <- minify t2
                                pure (Just newT1, Just newT2)
handleTime _ _ = pure (Nothing, Nothing)

removeIfEqualTo :: Text -> Maybe TextV -> Maybe TextV
removeIfEqualTo _ Nothing = Nothing
removeIfEqualTo s (Just x)
    | x == TextV s = Nothing
    | otherwise    = Just x

-- Unquotes font family names when possible
optimizeFontFamily :: Value -> Reader Config Value
optimizeFontFamily (Other t)   = mkOther <$> lowercaseText (getText t)
optimizeFontFamily (StringV s) = do
    conf <- ask
    ffamily <- mapString lowercaseText s
    pure $ if shouldRemoveQuotes conf
              then either mkOther StringV (unquoteFontFamily ffamily)
              else StringV ffamily
optimizeFontFamily x = pure x

minifyBgLayer :: Maybe Value -> Maybe Position -> Maybe BgSize
              -> Maybe RepeatStyle -> Maybe TextV
              -> Reader Config (Maybe Value, Maybe BgSize, Maybe Position, Maybe RepeatStyle, Maybe TextV)
minifyBgLayer img pos sz rst att = do
      i <- handleImage img
      s <- handleBgSize sz
      p <- let cannotRemovePos = isJust s -- can only be removed if there is no later <bg-size>
           in handlePosition cannotRemovePos pos
      r <- handleRepeatStyle rst
      a <- handleAttachment att
      pure (i,s,p,r,a)
