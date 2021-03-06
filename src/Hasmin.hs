-----------------------------------------------------------------------------
-- |
-- Module      : Hasmin
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : unknown
--
-- Recommended module to use the library.
--
-----------------------------------------------------------------------------
module Hasmin (
    -- * Using the library
    -- $use
      minifyCSS
    , minifyCSSWith
    , module Hasmin.Config
    ) where

import Control.Monad.Reader (runReader)
import Data.Attoparsec.Text (parseOnly)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL

import Hasmin.Parser.Internal
import Hasmin.Class
import Hasmin.Types.Stylesheet
import Hasmin.Config

-- | Minify Text, based on a 'Config'. To just use a default set of
-- configurations (i.e. 'defaultConfig'), use 'minifyCSS'.
minifyCSSWith :: Config -> Text -> Either String Text
minifyCSSWith cfg t = do
    sheet <- parseOnly stylesheet t
    let rs = runReader (minifyRules sheet) cfg
    pure . TL.toStrict . toLazyText $ foldMap toBuilder rs

-- | Minify 'Text' CSS, using a default set of configurations (with most
-- minification techniques enabled). 'minifyCSS' is equivalent to
-- @'minifyCSSWith' 'defaultConfig'@.
minifyCSS :: Text -> Either String Text
minifyCSS = minifyCSSWith defaultConfig

-- $use
--
-- This section shows a basic library use case.
--
-- Given a style sheet, say:
--
-- > -- Note: We are using the OverloadedStrings extension here
-- >
-- > sampleSheet :: Text
-- > sampleSheet = "body { color: #ff0000 } div { margin: 0 0 0 0 }"
--
-- Minify it with 'minifyCSS'. In ghci:
--
-- > > minifyCSS sampleSheet
-- > Right "body{color:red}div{margin:0}"
--
-- To modify the minification settings, just use another 'Config', e.g.:
--
-- > cfg :: Config
-- > cfg = defaultConfig { colorSettings = ColorMinOff }
--
-- Once more in ghci, this time using 'minifyCSSWith':
--
-- > > minifyCSSWith cfg sampleSheet
-- > Right "body{color:#ff0000}div{margin:0}"
--
-- The output is once more minified, but this time leaving colors as they were
-- originally.
--
-- For the complete list of possible options, refer to 'Config'.
