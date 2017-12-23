module Hasmin.Parser.Dimension where

import Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Char as C
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (mzero)
import Control.Arrow ((&&&))

import Hasmin.Parser.Utils
import Hasmin.Parser.Numeric
import Hasmin.Types.Numeric
import Hasmin.Class
import Hasmin.Types.Dimension

distance :: Parser Length
distance = dimensionParser distanceConstructorsMap NullLength
  where distanceConstructorsMap = Map.fromList distanceConstructorsList

angle :: Parser Angle
angle = dimensionParser angleConstructorsMap NullAngle
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
angleConstructorsList = fmap (toText &&& flip Angle) [minBound..]

distanceConstructorsList :: [(Text, Number -> Length)]
distanceConstructorsList = fmap (toText &&& flip Length) [minBound..]

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
