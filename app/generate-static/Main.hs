{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Generate static files
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Main (main) where

import Camino.Display.Static
import Camino.Config
import Control.Monad
import Options.Applicative
import System.FilePath
import System.Directory

data Generate = Generate {
  config :: FilePath,
  output :: FilePath,
  cssStatic :: Bool,
  helpStatic :: Bool,
  coloursStatic :: Bool
}

arguments :: Parser Generate
arguments =  Generate
    <$> (strOption (long "config" <> short 'c' <> value "./config.yaml" <> metavar "CONFIG" <> help "Configuration file"))
    <*> (strOption (long "output" <> short 'o' <> value "./static" <> metavar "OUTPUTDIR" <> help "Output directory"))
    <*> (switch (long "css" <> help "Generate CSS"))
    <*> (switch (long "help" <> help "Generate pre-formatted help files"))
    <*> (switch (long "colours" <> help "Generate colour swatch"))

generate :: Generate -> IO ()
generate opts = do
    config' <- readConfigFile (config opts)
    let output' = output opts
    createDirectoryIfMissing True output'
    when (cssStatic opts) (createCssFiles config' (output' </> "css"))
    when (coloursStatic opts) (createColourSwatch config' (output' </> "doc"))
    when (helpStatic opts) (createHelpFiles config' (output' </> "help"))

main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Create static files")
    generate opts