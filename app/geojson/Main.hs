{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Add elevation data to a GeoJSON file
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Main (main) where

import Camino.Display.JSON
import Data.Aeson.Formatting
import qualified Data.ByteString.Lazy as LB
import Data.Default.Class
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.Util (backupFilePath, roundBy)
import Geo.Feature
import Geo.Geometry
import Geo.LatLong
import Network.Google.Elevation
import Options.Applicative
import System.Directory
import System.IO

data Elevations = Elevations {
    mapApiKey :: Text
  , jsonInput :: Maybe FilePath
}

arguments :: Parser Elevations
arguments =  Elevations
    <$> strOption (long "key" <> short 'k' <> value "API_KEY" <> metavar "API_KEY" <> help "Google elevations API key")
    <*> optional (strArgument (metavar "INPUT" <> help "Source geojson definition, if not used than stdin is used"))

roundElevation :: Double -> Double
roundElevation v = roundBy 1.0 v

mapPosition :: M.Map LatLong LatLngElevation -> LatLong -> LatLong
mapPosition elevMap ll@(LatLong lat' long' _elev srs') = maybe ll (\(LatLngElevation _loc elev' _res) -> LatLong lat' long' (roundElevation <$> elev') srs') $ M.lookup (withoutElevation ll) elevMap

addElevations :: MapApi -> Feature SimpleGeometry -> IO (Feature SimpleGeometry)
addElevations api feature = do
  let requests = map withoutElevation $ S.toList $ points feature
  let requests' = map (\ll -> LatLng (latitude ll) (longitude ll)) requests
  elevations' <- getElevations api requests'
  let elevations'' = M.fromList $ zip requests elevations'
  return $ remap (mapPosition elevations'') feature

elevations :: Elevations -> Maybe FilePath -> IO ()
elevations opts Nothing = do
    let api = def { apiKey = mapApiKey opts }
    bytes' <- LB.hGetContents stdin
    let feature = either error id $ readGeoJSONFeature bytes'
    feature' <- addElevations api feature
    let pos = if isMultiGeometry feature' then feature3PrintOptions else feature2PrintOptions
    LB.putStr $ encodePretty pos $ toGeoJSONFeature feature'
elevations opts (Just file) = do
    let api = def { apiKey = mapApiKey opts }
    bytes' <- LB.readFile file
    let feature = either error id $ readGeoJSONFeature bytes'
    feature' <- addElevations api feature
    backup <- backupFilePath file
    renameFile file backup
    let pos = if isMultiGeometry feature' then feature3PrintOptions else feature2PrintOptions
    LB.writeFile file $ encodePretty pos $ toGeoJSONFeature feature'

main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Get elevation data for a camino")
    elevations opts (jsonInput opts)