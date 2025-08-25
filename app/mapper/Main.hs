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

import Camino.Camino
import qualified Data.ByteString.Lazy as B
import Data.Localised
import Data.Region
import Data.Text (Text, intercalate, unpack)
import Formatting

import Options.Applicative

data Map = Map {
  camino :: FilePath
}

arguments :: Parser Map
arguments =  Map
    <$> (argument str (metavar "CAMINO-FILE"))

poiLabel :: PointOfInterest -> Text
poiLabel poi =
     poiID poi
  <> " "
  <> (maybe "?" plainText (localise [] (poiName poi)))
  <> maybe "" (\t -> " " <> sformat (fixed 1) t) (poiTime poi)

printLocation :: Location -> IO ()
printLocation l = do
  putStr $ unpack $ locFormat id' name' lat' lon' region' pois'
  where
    locFormat = sformat (stext % " " % stext % " " % fixed 5 % "," % fixed 5 % " " % stext % " " % stext % "\n")
    id' = locationID l
    name' = locationNameLabel l
    position' = locationPosition l
    lat' = latitude position'
    lon' = longitude position'
    region' = maybe "-" (\r -> "(" <> (regionID r) <> ")") (locationRegion l)
    pois' = intercalate ", " $ map poiLabel (locationPois l)


main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Plan a camino graph")
    file' <- B.readFile $ camino opts
    let camino' = readCamino file'
    mapM_ printLocation (caminoLocations camino')
