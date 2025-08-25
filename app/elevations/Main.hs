{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Add elevatio data to a camino
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Main (main) where

import GHC.Generics (Generic)
import Camino.Display.JSON
import Camino.Display.Static
import Camino.Camino
import Camino.Config
import Data.Aeson
import Data.Aeson.Formatting
import qualified Data.ByteString.Lazy as LB
import Data.Default.Class
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Set as S
import Data.Text (Text, intercalate, pack, unpack)
import Network.Google.Elevation
import Options.Applicative
import System.FilePath
import System.Directory
import System.IO

data Elevations = Elevations {
  caminoInput :: FilePath,
  caminoOutput :: FilePath,
  mapApiKey :: Text
}

arguments :: Parser Elevations
arguments =  Elevations
    <$> strArgument (metavar "INPUT" <> value "--" <> help "Source camino definition, if not used than stdin is used")
    <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> value "--" <> help "Output file, if not specified then stdout is used")
    <*> strOption (long "key" <> short 'k' <> value "API_KEY" <> metavar "API_KEY" <> help "Google elevations API key")

loadCamino :: AssetConfig -> IO Camino
loadCamino asset = do
  result <- readAsset asset
  return $ readCamino result

writeElevations' :: [(Text, Text, LatLong, Maybe Double)] -> Handle -> IO ()
writeElevations' elevations handle = do
  hSetEncoding handle utf8_bom -- Needed for excel to play nice
  hPutStrLn handle "ID,Name,Latitude,Longitude,Elevation"
  mapM_ (\(ident, name, position, elevation) ->
    hPutStrLn handle (unpack ident ++ "," ++ unpack name ++ "," ++ (show $ latitude position) ++ "," ++ (show $ longitude position) ++  "," ++ (maybe "" (show . round) elevation))
    ) elevations

writeElevations :: FilePath -> [(Text, Text, LatLong, Maybe Double)] -> IO ()
writeElevations output' elevations =
  withFile output' WriteMode (writeElevations' elevations)

createElevationsFile :: MapApi -> [Camino] -> FilePath -> IO ()
createElevationsFile api caminos' output' = do
  let locations = foldr (\c -> \ls -> S.union ls (S.fromList $ caminoLocations c)) S.empty caminos'
  let pois = foldr (\c -> \ls -> S.union ls (S.fromList $ map fst $ M.elems $ caminoPoiMap c)) S.empty caminos'
  let lrequests = map (\l -> (locationID l, locationNameLabel l, fromJust (locationPosition l))) $ filter (isJust . locationPosition) $ S.toList locations
  let prequests = map (\p -> (poiID p, poiNameLabel p, fromJust (poiPosition p))) $ filter (isJust . poiPosition) $ S.toList pois
  let requests = sortOn (\(v, _, _) -> v) $ lrequests ++ prequests
  let requests' = map (\(_i, _n, loc) -> LatLng (latitude loc) (longitude loc)) requests
  elevations' <- getElevations api requests'
  let elevations'' = map (\((ident, name, position), LatLngElevation _latlng elevation _resolution) -> (ident, name, position, elevation)) (zip requests elevations')
  writeElevations output' elevations''

roundElevation :: Double -> Double
roundElevation v = fromIntegral $ round v

mapPosition :: M.Map LatLong LatLngElevation -> LatLong -> LatLong
mapPosition elevMap ll@(LatLong lat' long' Nothing srs') = maybe ll (\(LatLngElevation _loc elev' _res) -> LatLong lat' long' (roundElevation <$> elev') srs') $ M.lookup ll elevMap
mapPosition elevMap ll = ll

mapPoi ::  M.Map LatLong LatLngElevation -> PointOfInterest -> PointOfInterest
mapPoi elevMap poi = poi { poiPosition = mapPosition elevMap <$> poiPosition poi }

mapLocation :: M.Map LatLong LatLngElevation -> Location -> Location
mapLocation elevMap location = location {
    locationPosition = mapPosition elevMap <$> locationPosition location
  , locationPois = map (mapPoi elevMap) (locationPois location)
  }

-- Hacky job, doesn't map referenced information
mapCamino :: M.Map LatLong LatLngElevation -> Camino -> Camino
mapCamino elevMap camino = let
  locations' = map (mapLocation elevMap) (caminoLocations camino)
  locations'' = M.fromList $ map (\l -> (locationID l, l)) locations'
  in
    camino {
        caminoLocations = locations'
      , caminoLocationMap = locations''
    }

addElevations :: MapApi -> Camino -> IO Camino
addElevations api camino = do
  let locations = S.fromList $ caminoLocations camino
  let pois = S.fromList $ map fst $ M.elems $ caminoPoiMap camino
  let lrequests = S.map (\loc -> fromJust (locationPosition loc)) $ S.filter (\loc -> maybe False (\p -> isNothing (elevation p)) $ locationPosition loc) locations
  let prequests = S.map (\poi -> fromJust (poiPosition poi)) $ S.filter (\poi -> maybe False (\p -> isNothing (elevation p)) $ poiPosition poi) pois
  let requests = S.toList $ S.union lrequests prequests
  let requests' = map (\loc -> LatLng (latitude loc) (longitude loc)) requests
  elevations' <- getElevations api requests'
  let elevations'' = M.fromList $ zip requests elevations'
  return $ mapCamino elevations'' camino

elevations :: Elevations -> IO ()
elevations opts = do
    let api = def { apiKey = mapApiKey opts }
    bytes <- if caminoInput opts == "--" then LB.hGetContents stdin else LB.readFile (caminoInput opts)
    let camino = readCamino bytes
    camino' <- addElevations api camino
    let output' = caminoOutput opts
    if output' == "--" then do
      LB.putStr $ encodePretty caminoPrintOptions $ toJSON camino'
    else do
      createDirectoryIfMissing True (takeDirectory output')
      encodeFile output' camino'

main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Get elevation data for a camino")
    elevations opts