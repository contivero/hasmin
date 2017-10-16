{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2017 Cristian Adrián Ontivero
-- License     : BSD3
-- Stability   : experimental
-- Portability : non-portable
--
-----------------------------------------------------------------------------
module Main where

import Codec.Compression.Hopfli
import Control.Applicative (liftA2)
import Data.Monoid ((<>))
import Data.Text (Text)
import Options.Applicative hiding (command)
import Data.Version (showVersion)
import Development.GitRev (gitHash)
import Paths_hasmin (version)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Exit (die)
import Hasmin.Config
import Hasmin

command :: Parser Commands
command = Commands
    <$> switch (long "beautify" <> short 'b' <> help "Beautify output")
    <*> switch (long "zopfli" <> short 'z' <> help "Compress result using zopfli")
    <*> argument str (metavar "FILE")

config :: Parser Config
config = Config
  <$> flag ColorMinOn ColorMinOff (long "no-color-min"
                                <> short 'c'
                                <> help "Disable <color> minification")
  <*> flag DimMinOff DimMinOn (long "dimension-min"
                            <> short 'd'
                            <> help  "Enable normalization of absolute dimensions")
  <*> flag GradientMinOn GradientMinOff (long "-no-gradient-min"
                    <> short 'g' <> help "Disable <gradient> minification")
  <*> flag True False (long "no-property-traits"
                    <> short 't'
                    <> help "Disable use of property traits for declaration minification")
  <*> flag True False (long "no-rule-cleaning"
                    <> short 'a'
                    <> help "Disable deletion of overwritten properties")
  <*> flag True False (long "no-timing-function-min"
                    <> help "Disable <timing-function> minifications")
  <*> flag True False (long "no-filter-function-min"
                    <> help "Disable <filter-function> minifications")
         <*> flag True False (long "no-quotes-removal"
                           <> short 'q'
                           <> help "Disable removing quotes whenever possible")
         <*> flag FontWeightMinOn FontWeightMinOff (long "no-font-weight-minification"
                           <> help "Disable converting normal to 400 and bold to 700 in font-weight")
         <*> flag True False (long "no-transform-origin-minification"
                           <> help "Disable converting left and top to 0%, bottom and right to 100%, and center to 50%")
         <*> flag True False (long "no-microsyntax-min"
                           <> short 'm'
                           <> help "Disable minification of An+B microsyntax")
         <*> flag True False (long "no-@kfsel-min"
                           <> short 'k'
                           <> help "Disable transform function minification")
         <*> flag True False (long "no-transform-function-min"
                           <> help "Disable @keyframe selectors minification")
         <*> switch (long "convert-escaped-characters"
                    <> help "Convert escaped characters to their UTF-8 equivalent")
         <*> flag True False (long "no-null-percentage-conversion"
                           <> help "Disable converting 0% to 0 when possible")
         <*> flag True False (long "no-empty-block-removal"
                           <> short 'e'
                           <> help "Disable empty block removal")
         <*> flag True False (long "no-duplicate-selector-removal"
                           <> help "Disable removal of duplicate selectors")
         <*> flag True False (long "no-quote-normalization"
                           <> help "Disable trying to convert all quotation marks to either \" or \'")
         <*> flag Lowercase Original (long "no-lowercasing"
                           <> short 'l'
                           <> help "Disable lowercasing everything possible")
         <*> flag Lexicographical NoSorting (long "no-selector-sorting"
                           <> help "Disable sorting selectors lexicographically")
         <*> flag Lexicographical NoSorting (long "no-property-sorting"
                           <> help "Disable sorting properties lexicographically")
         <*> flag MergeRulesOn MergeRulesOff (long "no-rule-merging"
                           <> help "Disable merging style rules")

instructions :: ParserInfo Instructions
instructions = info (helper <*> versionOption <*> liftA2 (,) command config)
    (fullDesc <> header "Hasmin - A Haskell CSS Minifier")
  where versionOption = infoOption (showVersion version <> " " <> $(gitHash))
                                   (long "version" <> help "Show version and commit hash")

main :: IO ()
main = do
    (comm, conf) <- execParser instructions
    text         <- TIO.readFile (file comm)
    case minifyCSSWith conf text of
      Right rs -> process rs comm
      Left e   -> die e
  where process :: Text -> Commands -> IO ()
        process ts comm
          | shouldBeautify comm = error "Currently unsupported"
          | shouldCompress comm = B.writeFile "output.gz" . compressWith defaultCompressOptions GZIP . TE.encodeUtf8 $ ts
          | otherwise           = TIO.putStr ts
