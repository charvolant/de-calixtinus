{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Prettify json
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Main (main) where

import Camino.Display.JSON
import Camino.Camino
import Data.Aeson
import Data.Aeson.Formatting
import qualified Data.ByteString.Lazy as LB
import Data.Default.Class
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.Util (backupFilePath, roundBy)
import Formatting
import Graph.Graph
import Network.Google.Elevation
import Options.Applicative
import System.FilePath
import System.Directory
import System.IO

data Prettyfy = Prettyfy {
    prettyType :: Text
  , prettyInput :: Maybe FilePath
}

arguments :: Parser Prettyfy
arguments =  Prettyfy
    <$> strOption (long "type" <> short 't' <> value "camino" <> metavar "TYPE" <> help "Type of formatting, one of camino, feature3, feature2 or geojson")
    <*> optional (strArgument (metavar "INPUT" <> help "Source file, if not used than stdin is used. If used a backup is name and the result fed back into the original file name"))


printOptions :: Text -> PrintOptions
printOptions "feature2" = feature2PrintOptions
printOptions "feature3" = feature3PrintOptions
printOptions "geojson" = geojsonPrintOptions
printOptions "camino" = caminoPrintOptions
printOptions _ = geojsonPrintOptions

prettyPrint :: Prettyfy -> Maybe FilePath -> IO ()
prettyPrint opts Nothing = do
  bytes' <- LB.hGetContents stdin
  let evalue = eitherDecode bytes' :: Either String Value
  case evalue of
    Left msg -> hPutStrLn stderr msg
    Right value -> LB.putStr $ encodePretty (printOptions $ prettyType opts) value
prettyPrint opts (Just file) = do
  backup <- backupFilePath file
  renameFile file backup
  bytes' <- LB.readFile backup
  let evalue = eitherDecode bytes' :: Either String Value
  case evalue of
    Left msg -> hPutStrLn stderr msg
    Right value -> LB.writeFile file $ encodePretty (printOptions $ prettyType opts) value

main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Pretty-print JSON from the standard input to thstandard output according to a format")
    prettyPrint opts (prettyInput opts)