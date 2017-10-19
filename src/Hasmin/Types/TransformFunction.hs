{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin.Types.TransformFunction
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Hasmin.Types.TransformFunction
    ( TransformFunction(..)
    , mkMat
    , mkMat3d
    , combine
    ) where

import Control.Monad.Reader (mapReader, Reader, ask, local)
import Control.Applicative (liftA2)
import Data.Monoid ((<>))
import Data.Either (isRight)
import qualified Data.Text as T
import Data.Number.FixedFunctions (sin, cos, acos, tan, atan)
import Prelude hiding (sin, cos, acos, tan, atan)
import qualified Data.Matrix as M
import Data.Matrix (Matrix)
import Data.List (groupBy)
import Data.Maybe (fromMaybe, isNothing, isJust, fromJust)
import Data.Text.Lazy.Builder (toLazyText, singleton, Builder)
import Data.Text.Lazy (toStrict)

import Hasmin.Config
import Hasmin.Types.Class
import Hasmin.Utils
import Hasmin.Types.Dimension
import Hasmin.Types.PercentageLength
import Hasmin.Types.Numeric

-- Note: In a previous specification, translate3d() took two
-- <https://www.w3.org/TR/css-transforms-1/#typedef-translation-value \<translation-value\>>,
-- however in the <https://drafts.csswg.org/css-transforms/ latest draft>, it
-- takes two \<length-percentage\> (which makes sense since translateX() and
-- translateY() also do).

-- | CSS <https://drafts.csswg.org/css-transforms/#typedef-transform-function \<transform-function\>>
-- data type.
data TransformFunction = Mat (Matrix Number)
                       | Mat3d (Matrix Number)
                       | Perspective Length
                       | Rotate Angle
                       | RotateX Angle
                       | RotateY Angle
                       | RotateZ Angle
                       | Rotate3d Number Number Number Angle
                       | Scale Number (Maybe Number)
                       | ScaleX Number
                       | ScaleY Number
                       | ScaleZ Number
                       | Scale3d Number Number Number
                       | Skew Angle (Maybe Angle)
                       | SkewX Angle
                       | SkewY Angle
                       | Translate PercentageLength (Maybe PercentageLength)
                       | TranslateX PercentageLength
                       | TranslateY PercentageLength
                       | TranslateZ Length
                       | Translate3d PercentageLength PercentageLength Length
  deriving (Eq, Show)

-- There are a series of equivalences to keep in mind: translate(0),
-- translate3d(0, 0, 0), translateX(0), translateY(0), translateZ(0), scale(1),
-- scaleX(1), scaleY(1), scaleZ(1), rotate(0), rotate3d(1,1,1,0), rotateX(0),
-- rotateY(0), rotateZ(0), perspective(infinity), matrix(1,0,0,1,0,0),
-- matrix3d(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), skewX(0), skewY(0), and the
-- shortest of them all: skew(0)
-- All of these translate to the 4x4 identity matrix.
instance Minifiable TransformFunction where
  minifyWith (Mat3d m) = do
      conf <- ask
      if shouldMinifyTransformFunction conf
         then case possibleRepresentations m of
                []     -> pure (Mat3d m)
                (x:xs) -> let simplifyAndConvertUnits a = local (const $ conf { dimensionSettings = DimMinOn }) (simplify a)
                          in go simplifyAndConvertUnits x xs
         else pure (Mat3d m)
    where go f y []     = f y
          go f y (z:zs) = do
              currentLength <- textualLength <$> f y
              newLength     <- textualLength <$> f z
              if currentLength < newLength
                 then go f y zs
                 else go f z zs
  minifyWith x = do
      conf <- ask
      if shouldMinifyTransformFunction conf
         then case toMatrix3d x of
                Just mat3d -> minifyWith mat3d
                Nothing    -> simplify x
         else pure x

{-
s = SkewX (Angle 45 Deg)
tx = TranslateX (Right $ Length (-6) PX)
ry = RotateY (Angle (-9) Deg)
ry2 = Rotate3d 0 (-1) 0 (Angle 9 Deg)
t = TranslateZ (Length 100 PX)
t2 = Translate (Right (Length 100 PX)) Nothing
tr = Rotate3d 0 0 1 (Angle 90 Deg)
tr1 = Rotate3d 0 0 1 (Angle 45 Deg)
g x = runReader x defaultConfig
-}

instance ToText TransformFunction where
  toBuilder (Translate pl mpl)   = "translate("
      <> toBuilder pl <> maybeWithComma mpl <> singleton ')'
  toBuilder (TranslateX pl)      = "translatex("
      <> either toBuilder toBuilder pl <> singleton ')'
  toBuilder (TranslateY pl)      = "translatey(" <> either toBuilder toBuilder pl <> singleton ')'
  toBuilder (TranslateZ d)       = "translatez(" <> toBuilder d <> singleton ')'
  toBuilder (Scale n mn)         = "scale(" <> toBuilder n <> maybeWithComma mn <> singleton ')'
  toBuilder (ScaleX n)           = "scalex(" <> toBuilder n <> singleton ')'
  toBuilder (ScaleY n)           = "scaley(" <> toBuilder n <> singleton ')'
  toBuilder (ScaleZ n)           = "scalez(" <> toBuilder n <> singleton ')'
  toBuilder (Skew a ma)          = "skew(" <> toBuilder a <> maybeWithComma ma <> singleton ')'
  toBuilder (SkewX a)            = "skewx(" <> toBuilder a <> singleton ')'
  toBuilder (SkewY a)            = "skewy(" <> toBuilder a <> singleton ')'
  toBuilder (Rotate a)           = "rotate(" <> toBuilder a <> singleton ')'
  toBuilder (RotateX a)          = "rotatex(" <> toBuilder a <> singleton ')'
  toBuilder (RotateY a)          = "rotatey(" <> toBuilder a <> singleton ')'
  toBuilder (RotateZ a)          = "rotatez(" <> toBuilder a <> singleton ')'
  toBuilder (Rotate3d x y z a)   = "rotate3d(" <> toBuilder x <> singleton ','
      <> toBuilder y <> singleton ',' <> toBuilder z <> singleton ','
      <> toBuilder a <> singleton ')'
  toBuilder (Scale3d x y z)      = "scale3d(" <> toBuilder x <> singleton ','
      <> toBuilder y <> singleton ',' <> toBuilder z <> singleton ')'
  toBuilder (Perspective d)      = "perspective(" <> toBuilder d <> singleton ')'
  toBuilder (Translate3d x y z ) = "translate3d(" <> toBuilder x <> singleton ','
      <> toBuilder y <> singleton ',' <> toBuilder z <> singleton ')'
  toBuilder (Mat m)              = "matrix("
      <> mconcatIntersperse toBuilder (singleton ',') (M.toList m) <> singleton ')'
  toBuilder (Mat3d m)            = "matrix3d("
      <> mconcatIntersperse toBuilder (singleton ',') (M.toList m) <> singleton ')'

maybeWithComma :: ToText a => Maybe a -> Builder
maybeWithComma = maybe mempty (\x -> singleton ',' <> toBuilder x)

mkMat :: [Number] -> TransformFunction
mkMat = Mat . M.fromList 3 2

mkMat3d :: [Number] -> TransformFunction
mkMat3d = Mat3d . M.fromList 4 4

toMatrix3d :: TransformFunction -> Maybe TransformFunction
toMatrix3d m@Mat3d{} = Just m
toMatrix3d (Mat x)   = Just $ toMat3d (M.toList x)
  where toMat3d [a,b,c,d,e,f] = mkMat3d [a, c, 0, e,
                                         b, d, 0, f,
                                         0, 0, 1, 0,
                                         0, 0, 0, 1]
        toMat3d _ = error "invalid matrix size!"
toMatrix3d (Translate pl mpl)
    | isNonZeroPercentage pl = Nothing
    | isJust mpl && isNonZeroPercentage (fromJust mpl) = Nothing
    | otherwise = Just . Mat3d $ mkTranslate3dMatrix x y 0
  where x = either (const 0) fromPixelsToNum pl
        y = maybe 0 (fromPixelsToNum . fromRight') mpl
toMatrix3d (TranslateX pl)
    | isNonZeroPercentage pl = Nothing
    | isRight pl && isRelativeLength (fromRight' pl) = Nothing
    | otherwise = Just . Mat3d $ mkTranslate3dMatrix x 0 0
  where x = either (const 0) fromPixelsToNum pl
toMatrix3d (TranslateY pl)
    | isNonZeroPercentage pl = Nothing
    | isRight pl && isRelativeLength (fromRight' pl) = Nothing
    | otherwise = Just . Mat3d $ mkTranslate3dMatrix 0 y 0
  where y = either (const 0) fromPixelsToNum pl
toMatrix3d (TranslateZ d)
    | isRelativeLength d  = Nothing
    | otherwise = Just . Mat3d $ mkTranslate3dMatrix 0 0 z
  where z = fromPixelsToNum d
toMatrix3d (Scale n mn) = Just . Mat3d $ mkScale3dMatrix n y 1
  where y = fromMaybe n mn
toMatrix3d (ScaleX n) = Just . Mat3d $ mkScale3dMatrix n 1 1
toMatrix3d (ScaleY n) = Just . Mat3d $ mkScale3dMatrix 1 n 1
toMatrix3d (ScaleZ n) = Just . Mat3d $ mkScale3dMatrix 1 1 n
toMatrix3d (Skew a ma) = Just . Mat3d $ mkSkewMatrix α β
  where α = tangent a
        β = maybe 0 tangent ma
toMatrix3d (SkewX a) = Just . Mat3d $ mkSkewMatrix (tangent a) 0
toMatrix3d (SkewY a) = Just . Mat3d $ mkSkewMatrix 0 (tangent a)
toMatrix3d (Translate3d pl1 pl2 d)
    | isNonZeroPercentage pl1 || isNonZeroPercentage pl2 = Nothing
    | isRight pl1 && isRelativeLength (fromRight' pl1) = Nothing
    | isRight pl2 && isRelativeLength (fromRight' pl2) = Nothing
    | isRelativeLength d = Nothing
    | otherwise = let x = either (const 0) fromPixelsToNum pl1
                      y = either (const 0) fromPixelsToNum pl2
                      z = fromPixelsToNum d
                  in Just . Mat3d $ mkTranslate3dMatrix x y z
toMatrix3d (Scale3d x y z) = Just . Mat3d $ mkScale3dMatrix x y z
toMatrix3d (Perspective d)
    | isZeroLen d = Nothing
    | otherwise   = let c = fromPixelsToNum d
                    in Just . Mat3d $ mkPerspectiveMatrix c
-- Note: The commented code is fine, but until we implement the
-- function that converts a matrix back to a rotate function, uncommenting this
-- breaks the minification, e.g. it says that:
-- minify rotate(90deg) == matrix(0,1,-1,0,0,0)
{-
toMatrix3d (Rotate a) = Just $ rotateIn3d (0,0,1) (fromRadiansToNum a)
toMatrix3d (Rotate3d x y z a) = Just $ rotateIn3d (x,y,z) (fromRadiansToNum a)
toMatrix3d (RotateX a) = Just $ rotateIn3d (1,0,0) (fromRadiansToNum a)
toMatrix3d (RotateY a) = Just $ rotateIn3d (0,1,0) (fromRadiansToNum a)
toMatrix3d (RotateZ a) = Just $ rotateIn3d (0,0,1) (fromRadiansToNum a)
-}
toMatrix3d _ = Nothing

-- Used for the rotate3d(), rotate(), ..., functions. Given an [x,y,z] vector
-- and an angle, create its corresponding rotation matrix.
{-
rotateIn3d :: (Number, Number, Number) -> Number -> TransformFunction
rotateIn3d (b,c,d) a = mkMat3d $ fmap toNumber
    [1-2*(y^2 + z^2)*sq, 2*(x*y*sq - z*sc),  2*(x*z*sq + y*sc),  0,
     2*(x*y*sq + z*sc),  1-2*(x^2 + z^2)*sq, 2*(y*z*sq - x*sc),  0,
     2*(x*z*sq - y*sc),  2*(y*z*sq + x*sc),  1-2*(x^2 + y^2)*sq, 0,
     0,                  0,                  0,                  1]
  where sc = sin epsilon (α/2) * cos epsilon (α/2)  :: Rational
        sq = sin epsilon (α/2) ^ 2 :: Rational
        x  = toRational b
        y  = toRational c
        z  = toRational d
        α  = toRational a
-}

-- Code translated from: http://www.euclideanspace.com/maths/geometry/rotations/conversions/matrixToAngle/
-- TODO: see how to get different representations, so that negative values are
-- still properly handled, i.e. make things such as rotateY(-9deg) work.
matrixToRotate3d :: Matrix Number -> [TransformFunction]
matrixToRotate3d _ = []
{-
    -- if it has translation values, then we can't convert it to a rotation function
    | M.unsafeGet 3 4 m /= 0 || M.unsafeGet 2 4 m /= 0 || M.unsafeGet 1 4 m /= 0 = []
    -- | not isRotationMatrix = []
    | abs (m12 - m21) < ep && abs (m13 - m31) < ep
                           && abs (m23 - m32) < ep = [handleSingularity]
    | otherwise = let a = arccos $ (m11 + m22 + m33 - 1)/2
                      qx = (m32 - m23)/(2 * sine a)
                      qy = (m13 - m31)/(2 * sine a)
                      qz = (m21 - m12)/(2 * sine a)
                  in [Rotate3d qx qy qz (Angle a Rad)]
  where m11 = M.unsafeGet 1 1 m
        m22 = M.unsafeGet 2 2 m
        m33 = M.unsafeGet 3 3 m
        m32 = M.unsafeGet 3 2 m
        m23 = M.unsafeGet 2 3 m
        m13 = M.unsafeGet 1 3 m
        m31 = M.unsafeGet 3 1 m
        m21 = M.unsafeGet 2 1 m
        m12 = M.unsafeGet 1 2 m
        ep = 0.01 -- margin to allow for rounding errors
        ep2 = 0.1
-- Make sure that the input is a pure rotation matrix.
-- The condition for this is:
-- R' * R = I, and det(R) = 1
        isRotationMatrix
            |    abs (m11*m12 + m12*m22 + m13*m23) > ep
              || abs (m11*m31 + m12*m32 + m13*m33) > ep
              || abs (m21*m31 + m22*m32 + m23*m33) > ep
              || abs (m11*m11 + m12*m12 + m13*m13 - 1) > ep
              || abs (m21*m21 + m22*m11 + m23*m23 - 1) > ep
              || abs (m31*m31 + m32*m32 + m33*m33 - 1) > ep = False
            | otherwise = abs (detLU m - 1) < ep
        handleSingularity
            |    abs (m12 + m21) < ep2
              && abs (m13 + m31) < ep2
              && abs (m23 - m32) < ep2
              && abs (m11 + m22 + m33 - 3) < ep2 = Rotate3d 1 0 0 (Angle 0 Deg)
            | otherwise = let xx = (m11+1)/2
                              yy = (m22+1)/2
                              zz = (m33+1)/2
                              xy = (m12 + m21)/4
                              xz = (m13 + m31)/4
                              yz = (m23 + m32)/4
                          in handle180Singularity xx yy zz xy xz yz
          where handle180Singularity xx yy zz xy xz yz
                    | xx > yy && xx > zz = if xx < ep
                                              then result 0 0.7071 0.7071
                                              else let x = toNumber . sqrt $ fromNumber xx
                                                   in result x (xy/x) (xz/x)
                    | yy > zz = if yy < ep
                                   then result 0.7071 0 0.7071
                                   else let y = toNumber . sqrt $ fromNumber yy
                                        in result (xy/y) y (yz/y)
                    | otherwise = if zz < ep
                                     then result 0.7071 0.7071 0
                                     else let z = toNumber . sqrt $ fromNumber zz
                                          in result (xz/z) (yz/z) z
                  where result vx vy vz = Rotate3d vx vy vz (Angle 180 Deg)
-}

fromPixelsToNum :: Length -> Number
fromPixelsToNum (Length n u) = toPixels n u
fromPixelsToNum NullLength   = 0

fromRadiansToNum :: Angle -> Number
fromRadiansToNum (Angle n u) = toRadians n u

tangent :: Angle -> Number
tangent =  toNumber . tan epsilon . fromNumber . fromRadiansToNum

arctan :: Number -> Number
arctan = toNumber . atan epsilon . fromNumber

-- sine :: Number -> Number
-- sine = toNumber . sin epsilon . fromNumber

-- arccos :: Number -> Number
-- arccos = toNumber . acos epsilon . fromNumber

getMat :: TransformFunction -> Matrix Number
getMat (Mat q)   = q
getMat (Mat3d q) = q
getMat _         = error "getMat: not a matrix!"

-- toMat :: TransformFunction -> Matrix Number
-- toMat x = getMat . fromJust $ toMatrix3d x

------------------------------------------------------------------------------

-- | Convert a matrix to all the other possible equivalent
-- transformation functions (if any).
possibleRepresentations :: Matrix Number -> [TransformFunction]
possibleRepresentations m = matrixToRotate3d m ++ mconcat
    [matrixToSkewFunctions m, matrixToTranslateFunctions m
    ,matrixToMat m, matrixToPerspective m
    ,matrixToScaleFunctions m] -- TODO add rotation when implemented

matrixToSkewFunctions :: Matrix Number -> [TransformFunction]
matrixToSkewFunctions m
    | skewMatrix == m = Skew a (Just b) : others
    | otherwise       = []
  where α = M.unsafeGet 1 2 m
        β = M.unsafeGet 2 1 m
        a = Angle (arctan α) Rad
        b = Angle (arctan β) Rad
        skewMatrix = mkSkewMatrix α β
        others
            | α /= 0 && β == 0 = [SkewX a]
            | β /= 0 && α == 0 = [SkewY b]
            | otherwise        = [] -- The only case when we can use either of
                                    -- them is when both α and β are zero, but if so
                                    -- skew is already shorter, so don't return it.

matrixToTranslateFunctions :: Matrix Number -> [TransformFunction]
matrixToTranslateFunctions m
    | mkTranslate3dMatrix x y z == m = Translate3d tx ty tz : others
    | otherwise                      = []
  where x  = M.unsafeGet 1 4 m
        tx = Right $ Length x PX
        y  = M.unsafeGet 2 4 m
        ty = Right $ Length y PX
        z  = M.unsafeGet 3 4 m
        tz = Length z PX
        others
            | z == 0 && y == 0 = [TranslateX tx, Translate tx (Just ty)]
            | x == 0 && z == 0 = [TranslateY ty, Translate tx (Just ty)]
            | y == 0 && x == 0 = [TranslateZ tz]
            | otherwise        = []

matrixToScaleFunctions :: Matrix Number -> [TransformFunction]
matrixToScaleFunctions m
    | mkScale3dMatrix x y z == m = Scale3d x y z : others
    | otherwise                  = []
  where x = M.unsafeGet 1 1 m
        y = M.unsafeGet 2 2 m
        z = M.unsafeGet 3 3 m
        others
            | z == 1 && y == 1 = [ScaleX x, Scale x Nothing]
            | y == 1 && x == 1 = [ScaleZ z]
            | x == 1 && z == 1 = [ScaleY y, Scale x (Just y)]
            | otherwise        = []

matrixToPerspective :: Matrix Number -> [TransformFunction]
matrixToPerspective m
    | c /= 0 && mkPerspectiveMatrix d == m = [Perspective $ Length d PX]
    | otherwise = []
  where c = M.unsafeGet 4 3 m
        d = (-1)/c

matrixToMat :: Matrix Number -> [TransformFunction]
matrixToMat m
    | matrix == m = [mkMat [a,b,c,d,e,f]]
    | otherwise   = []
  where a = M.unsafeGet 1 1 m
        b = M.unsafeGet 2 1 m
        c = M.unsafeGet 1 2 m
        d = M.unsafeGet 2 2 m
        e = M.unsafeGet 1 4 m
        f = M.unsafeGet 2 4 m
        matrix = mkMatMatrix a b c d e f

mkMatMatrix :: Number -> Number -> Number -> Number
            -> Number -> Number -> Matrix Number
mkMatMatrix a b c d e f = mk4x4Matrix [a, c, 0, e,
                                       b, d, 0, f,
                                       0, 0, 1, 0,
                                       0, 0, 0, 1]
mkTranslate3dMatrix :: Number -> Number -> Number -> Matrix Number
mkTranslate3dMatrix x y z = mk4x4Matrix [1, 0, 0, x,
                                         0, 1, 0, y,
                                         0, 0, 1, z,
                                         0, 0, 0, 1]

mkScale3dMatrix :: Number -> Number -> Number -> Matrix Number
mkScale3dMatrix x y z = mk4x4Matrix [x, 0, 0, 0,
                                     0, y, 0, 0,
                                     0, 0, z, 0,
                                     0, 0, 0, 1]

mkSkewMatrix :: Number -> Number -> Matrix Number
mkSkewMatrix a b = mk4x4Matrix [1, a, 0, 0,
                                b, 1, 0, 0,
                                0, 0, 1, 0,
                                0, 0, 0, 1]

mkPerspectiveMatrix :: Number -> Matrix Number
mkPerspectiveMatrix c = let d = (-1/c)
                        in mk4x4Matrix [1, 0, 0, 0,
                                        0, 1, 0, 0,
                                        0, 0, 1, 0,
                                        0, 0, d, 0]
mk4x4Matrix :: [Number] -> Matrix Number
mk4x4Matrix = M.fromList 4 4

-- | Simplifies a \<transform-function\> without converting it to a 4x4 matrix,
-- by doing some simple conversions between the different functions, or
-- minifying the dimension arguments (\<length\> and \<angle\> values)
simplify :: TransformFunction -> Reader Config TransformFunction
simplify (Translate pl mpl)
    | isNothing mpl || isZero (fromJust mpl) = do
        x <- mapM minifyWith pl
        pure $ Translate x Nothing
    | otherwise = do x <- mapM minifyWith pl
                     y <- (mapM . mapM) minifyWith mpl
                     pure $ Translate x y
simplify (TranslateX pl) = do
    x <- mapM minifyWith pl
    simplify $ Translate x Nothing
-- Note: It always makes sense to convert from translateX to translate
-- Not sure about translateY. Converting to translate(0,a) might aid
-- compression, even when we are adding one character, because we might be
-- reducing entropy.
simplify (TranslateY pl) = do
    y <- mapM minifyWith pl
    pure $ TranslateY y
-- In scale(), if the second parameter isn't present, it defaults to the first.
-- Therefore:  scale(a,a) == scale(a)
simplify s@(Scale n mn)  = pure $ maybe s removeDefaultArgument mn
  where removeDefaultArgument x
            | n == x    = Scale n Nothing
            | otherwise = s
-- TODO see if it is better to leave them as is, or convert them to scale(x,1)
-- and scale(1,x), respectively, to aid gzip compression
simplify s@(ScaleX _)    = pure s
simplify s@(ScaleY _)    = pure s
-- In skew(), if the second parameter isn't present, it defaults to the zero.
simplify (Skew a ma)
      | defaultSecondArgument = do ang <- minifyWith a
                                   simplify $ Skew ang Nothing
      | otherwise             = liftA2 Skew (minifyWith a) (mapM minifyWith ma)
    where defaultSecondArgument = ma == Just (Angle 0 Deg)
simplify (SkewY a)
      | a == Angle 0 Deg = pure $ Skew (Angle 0 Deg) Nothing
      | otherwise        = fmap SkewY (minifyWith a)
simplify (SkewX a)
      | a == Angle 0 Deg = pure $ Skew (Angle 0 Deg) Nothing
      | otherwise        = fmap SkewX (minifyWith a)
simplify (Rotate a)
      | a == Angle 0 Deg = pure $ Skew (Angle 0 Deg) Nothing
      | otherwise        = fmap Rotate (minifyWith a)
simplify (RotateX a)
      | a == Angle 0 Deg = pure $ Skew (Angle 0 Deg) Nothing
      | otherwise        = fmap RotateX (minifyWith a)
simplify (RotateY a)
      | a == Angle 0 Deg = pure $ Skew (Angle 0 Deg) Nothing
      | otherwise        = fmap RotateY (minifyWith a)
-- rotateZ(a) is the same as rotate3d(0,0,1,a), which also equals rotate(a)
simplify (RotateZ a)
      | a == Angle 0 Deg = pure $ Skew (Angle 0 Deg) Nothing
      | otherwise        = fmap Rotate (minifyWith a)
simplify (Rotate3d x y z a)
      | abs (x - 1) < ep && abs y < ep && abs z < ep = simplify $ RotateX a
      | abs x < ep && abs (y - 1) < ep && abs z < ep = simplify $ RotateY a
      | abs x < ep && abs y < ep && abs (z - 1) < ep = fmap Rotate (minifyWith a)
  where ep = toNumber epsilon
simplify (ScaleZ n)
      | n == 1    = pure $ Skew (Angle 0 Deg) Nothing
      | otherwise = pure $ ScaleZ n
simplify (Perspective d) = fmap Perspective (minifyWith d)
simplify (TranslateZ d)
      | isZeroLen d = pure $ Skew (Angle 0 Deg) Nothing
      | otherwise   = fmap TranslateZ (minifyWith d)
simplify s@(Scale3d x y z)
      | z == 1           = simplify $ Scale x (Just y)
      | x == 1 && y == 1 = simplify $ ScaleZ z
      | otherwise        = pure s
simplify (Translate3d x y z )
      | isZero y && isZeroLen z = either (f TranslateX) (g TranslateX) x
      | isZero x && isZero y    = simplify $ TranslateZ z
      | isZero x && isZeroLen z = either (f TranslateY) (g TranslateY) y
    where f con a | a == 0    = simplify . con . Right $ NullLength
                  | otherwise = simplify . con . Left $ a
          g con a = simplify . con $ Right a -- A distance, transform an minify
simplify x = pure x

-- | Combines consecutive @\<transform-functions\>@ whenever possible, leaving
-- translate functions that can't be converted to a matrix (because they use
-- percentages or relative units) as they are, in the position they are in the
-- list (since matrix multiplication isn't commutative)
-- Example:
--
-- >>> import Control.Monad.Reader
-- >>> let t10 = Translate (Right (Length 10 PX)) Nothing
-- >>> let s45 = Skew (Angle 45 Deg) Nothing
-- >>> let sx5 = ScaleX 5
-- >>> let f x = runReader (combine x) defaultConfig
--
-- >>> fmap toText $ f [t10, s45, sx5]
-- ["matrix(5,0,1,1,10,0)"]
--
-- >>> let tp  = Translate (Left (Percentage 100)) Nothing
-- >>> fmap toText $ f [s45,tp,sx5,sx5,sx5]
-- ["skew(45deg)","translate(100%)","scale(125)"]
combine :: [TransformFunction] -> Reader Config [TransformFunction]
combine xs = do
    combinedLength <- mapReader (getLength . asBuilder) combinedFunctions
    originalLength <- mapReader (getLength . asBuilder) minifiedOriginal
    if combinedLength < originalLength
       then combinedFunctions
       else minifiedOriginal
  where getLength = T.length . toStrict . toLazyText
        asBuilder = mconcatIntersperse toBuilder (singleton ' ')
        combinedFunctions = mapM handleMatrices . groupByMatrices $ zip (fmap toMatrix3d xs) xs
        minifiedOriginal = mapM minifyWith xs
        groupByMatrices   = groupBy (\(a,_) (b,_) -> isJust a && isJust b)
        handleMatrices l@((x,a):_)
            | isJust x  = minifyWith . Mat3d . foldr (*) (M.identity 4 :: Matrix Number) $ fmap (getMat . fromJust . fst) l
            | otherwise = simplify a
        handleMatrices [] = error "empty list as argument to handleMatrices"
