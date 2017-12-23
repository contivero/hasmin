{-# LANGUAGE OverloadedStrings #-}

module Hasmin.TestUtils
    ( module Hasmin.TestUtils
    , module Test.QuickCheck
    , module Test.Hspec
    , module Test.Hspec.Attoparsec
    ) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.Attoparsec (parseSatisfies, (~>))

import Control.Applicative (liftA2, liftA3)
import Control.Monad (liftM4)
import Control.Monad.Reader (runReader)
import Data.Text (Text, unpack, singleton)
import Data.Attoparsec.Text (Parser)

import Hasmin.Types.BgSize
import Hasmin.Class
import Hasmin.Config
import Hasmin.Types.Color
import Hasmin.Types.Declaration
import Hasmin.Types.Dimension
import Hasmin.Types.FilterFunction
import Hasmin.Types.Numeric
import Hasmin.Types.Position
import Hasmin.Types.TimingFunction
import Hasmin.Types.RepeatStyle
import Hasmin.Utils


minifyWithTestConfig :: Minifiable a => a -> a
minifyWithTestConfig x = runReader (minify x) cfg
  where cfg = defaultConfig { dimensionSettings = DimMinOn }

-- | Check that a color is equivalent to their minified representation form
prop_minificationEq :: (Minifiable a, Eq a) => a -> Bool
prop_minificationEq d = minifyWithTestConfig d == d

-- Given a parser and a 3-tuple, prints a test description,
-- applies the parser, and compares its result with the expected result
matchSpecWithDesc :: ToText a => Parser a -> (String, Text, Text) -> Spec
matchSpecWithDesc parser (description, textToParse, expectedResult) =
  it description $
    (toText <$> (textToParse ~> parser)) `parseSatisfies` (== expectedResult)

matchSpec :: ToText a => Parser a -> (Text, Text) -> Spec
matchSpec parser (textToParse, expectedResult) =
  it (unpack textToParse) $
    (toText <$> (textToParse ~> parser)) `parseSatisfies` (== expectedResult)

mkGen :: [a] -> Gen a
mkGen xs = (xs !!) <$> choose (0, length xs - 1)

newtype Declarations = Declarations [Declaration]
instance ToText Declarations where
  toText (Declarations ds) = mconcatIntersperse toText (singleton ';') ds

instance Arbitrary Length where
  arbitrary = liftA2 Length arbitrary distanceUnit
    where distanceUnit = mkGen [minBound..]

instance Arbitrary Angle where
  arbitrary = liftA2 Angle arbitrary angleUnit
    where angleUnit = mkGen [minBound..]

instance Arbitrary Time where
  arbitrary = liftA2 Time arbitrary durationUnit
    where durationUnit = mkGen [minBound..]

instance Arbitrary Frequency where
  arbitrary = liftA2 Frequency arbitrary frequencyUnit
    where frequencyUnit = mkGen [minBound..]

instance Arbitrary Resolution where
  arbitrary = liftA2 Resolution arbitrary resolutionUnit
    where resolutionUnit = mkGen [minBound..]

instance Arbitrary Number where
  arbitrary = toNumber <$> (arbitrary :: Gen Rational)

instance Arbitrary PosKeyword where
  arbitrary = mkGen [PosCenter, PosLeft, PosRight, PosTop, PosBottom]

instance Arbitrary Percentage where
  arbitrary = fmap Percentage (arbitrary :: Gen Rational)

instance Arbitrary FilterFunction where
  arbitrary = oneof
    [ Blur <$> arbitrary
    , Brightness <$> arbitrary
    , Contrast <$> arbitrary
    , Grayscale <$> arbitrary
    , Invert <$> arbitrary
    , Opacity <$> arbitrary
    , Saturate <$> arbitrary
    , Sepia <$> arbitrary
    , HueRotate <$> arbitrary
    , liftM4 DropShadow arbitrary arbitrary arbitrary arbitrary
    ]

instance Arbitrary BgSize where
  arbitrary = oneof [ liftA2 BgSize2 arbitrary arbitrary
                    , fmap BgSize1 arbitrary
                    ]

instance Arbitrary Auto where
  arbitrary = pure Auto

instance Arbitrary StepPosition where
  arbitrary = oneof [pure Start, pure End]

instance Arbitrary TimingFunction where
  arbitrary = oneof [ liftM4 CubicBezier arbitrary arbitrary arbitrary arbitrary
                    , liftA2 Steps arbitrary arbitrary
                    , pure Ease
                    , pure EaseIn
                    , pure EaseInOut
                    , pure EaseOut
                    , pure Linear
                    , pure StepEnd
                    , pure StepStart
                    ]

instance Arbitrary Color where
  arbitrary = oneof [ fmap Named colorKeyword
                    , liftA3 mkHex3 hexChar hexChar hexChar
                    , liftA3 mkHex6 hexString hexString hexString
                    , liftM4 mkHex4 hexChar hexChar hexChar hexChar
                    , liftM4 mkHex8 hexString hexString hexString hexString
                    , liftA3 mkRGBInt intRange intRange intRange
                    , liftA3 mkRGBPer ratRange ratRange ratRange
                    , liftM4 mkRGBAInt intRange intRange intRange alphaRange
                    , liftM4 mkRGBAPer ratRange ratRange ratRange alphaRange
                    , liftA3 mkHSL hueRange ratRange ratRange
                    , liftM4 mkHSLA hueRange ratRange ratRange alphaRange
                    ]
    where intRange   = choose (0, 255)
          ratRange   = toPercentage <$> (choose (0, 100) :: Gen Float)
          alphaRange = toAlphavalue <$> (choose (0, 1) :: Gen Float)
          hueRange   = choose (0, 360)

instance Arbitrary RepeatStyle where
  arbitrary = frequency [(1, pure RepeatX)
                        ,(1, pure RepeatY)
                        ,(8, liftA2 RepeatStyle2 arbitrary arbitrary)
                        ,(8, fmap RepeatStyle1 arbitrary)
                        ]

instance Arbitrary RSKeyword where
  arbitrary = mkGen [RsRepeat, RsSpace, RsRound, RsNoRepeat]

-- | Generates color keywords uniformly distributed
colorKeyword :: Gen Text
colorKeyword = mkGen $ fmap fst keywordColors

-- | Generates a hexadecimal character uniformly distributed
hexChar :: Gen Char
hexChar = mkGen hexadecimals

hexString :: Gen String
hexString = liftA2 (\x y -> [x,y]) hexChar hexChar

hexadecimals :: String
hexadecimals = "0123456789abcdef"

