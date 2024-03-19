{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Generate a map of locations for use during development
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Main (main) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Camino.Camino
import Graph.Graph
import Camino.Planner
import Data.Text.Lazy (pack, unpack, fromStrict)
import Formatting
import System.FilePath
import Text.XML

import Options.Applicative

data Map = Map {
  camino :: FilePath
}

arguments :: Parser Map
arguments =  Map
    <$> (argument str (metavar "CAMINO-FILE"))

printLocation :: Location -> IO ()
printLocation l = do
  putStr $ unpack $ locFormat id' name' lat' lon'
  where
    locFormat = format (text % " " % text % " " % fixed 4 % "," % fixed 4 % "\n")
    id' = pack $ locationID l
    name' = fromStrict $ locationName l
    position' = locationPosition l
    lat' = maybe 0.0 latitude position'
    lon' = maybe 0.0 longitude position'


main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Plan a camino graph")
    camino' <- readCamino (camino opts)
    mapM_ printLocation (caminoLocations camino')
