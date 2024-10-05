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
  allStatic :: Bool
}

arguments :: Parser Generate
arguments =  Generate
    <$> (strOption (long "config" <> short 'c' <> value "./config.yaml" <> metavar "CONFIG" <> help "Configuration file"))
    <*> (strOption (long "output" <> short 'o' <> value "./static" <> metavar "OUTPUTDIR" <> help "Output directory"))
    <*> (switch (long "all" <> short 'a' <> help "Generate all static files, including pre-formatted help files"))

generate :: Generate -> IO ()
generate opts = do
    config' <- readConfigFile (config opts)
    let output' = output opts
    createDirectoryIfMissing True output'
    createCssFiles config' (output' </> "css")
    when (allStatic opts) (createHelpFiles config' (output' </> "help"))

main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Create static files")
    generate opts