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
import Control.Monad
import Data.Aeson
import Data.Aeson.Formatting
import qualified Data.ByteString.Lazy as LB
import Data.Default.Class
import Data.List (sortOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, unpack)
import Formatting
import Graph.Graph
import Network.Google.Elevation
import Options.Applicative
import System.FilePath
import System.Directory
import System.IO

data Elevations = Elevations {
    caminoOutput :: Maybe FilePath
  , reportOutput :: Maybe FilePath
  , mapApiKey :: Text
  , caminoInput :: FilePath
}

arguments :: Parser Elevations
arguments =  Elevations
    <$> optional (strOption (long "output" <> short 'o' <> metavar "OUTPUT" <> help "Output file, if not specified then stdout is used"))
    <*> optional (strOption (long "report" <> short 'r' <> metavar "REPORT" <> help "Create a CSV report of elevation information"))
    <*> strOption (long "key" <> short 'k' <> value "API_KEY" <> metavar "API_KEY" <> help "Google elevations API key")
    <*> strArgument (metavar "INPUT" <> value "--" <> help "Source camino definition, if not used than stdin is used")

loadCamino :: AssetConfig -> IO Camino
loadCamino asset = do
  result <- readAsset asset
  return $ readCamino result

lineFormat :: Format r (Text -> Text -> Text -> Text -> Text -> Text -> Double -> Double -> Maybe Double -> r)
lineFormat = stext % "," % stext % "," % stext % "," % stext % "," % stext % "," % stext % "," % fixed 5 % "," % fixed 5 % "," % (maybed "--" (fixed 0))

writeReportWaypoint :: Camino -> Handle -> LatLong -> IO ()
writeReportWaypoint _camino handle pos =
  hprintLn handle lineFormat "" "" "" "" "." "" (latitude pos) (longitude pos) (elevation pos)

writeReportLeg :: Camino -> Handle -> Leg -> IO ()
writeReportLeg camino handle leg = do
  let lt = legTo leg
  let pos = locationPosition lt
  mapM_ (writeReportWaypoint camino handle) (legWaypoints leg)
  hprintLn handle lineFormat "" "" "" "" (locationID lt) (locationNameLabel lt) (latitude pos) (longitude pos) (elevation pos)

writeReportPoi :: Camino -> Handle -> PointOfInterest -> IO ()
writeReportPoi _camino handle poi = do
  hprintLn handle lineFormat "" "" (poiID poi) (poiNameLabel poi) "" "" (latitude pos) (longitude pos) (elevation pos)
    where
      pos = poiPosition poi

writeReportLocation :: Camino -> Handle -> Location -> IO ()
writeReportLocation camino handle location = do
  let pos = locationPosition location
  hprintLn handle lineFormat (locationID location) (locationNameLabel location) "" "" "" "" (latitude pos) (longitude pos) (elevation pos)
  mapM_ (writeReportPoi camino handle) (locationPois location)
  mapM_ (writeReportLeg camino handle) (outgoing camino location)

writeReport' :: Camino -> Handle -> IO ()
writeReport' camino handle = do
  hSetEncoding handle utf8_bom -- Needed for excel to play nice
  hPutStrLn handle "ID,Name,PoI ID,PoI Name,To ID,To Name,Latitude,Longitude,Elevation"
  mapM_ (writeReportLocation camino handle) (caminoLocations camino)

writeReport :: Camino -> FilePath -> IO ()
writeReport camino output =
  withFile output WriteMode (writeReport' camino)

roundElevation :: Double -> Double
roundElevation v = fromIntegral $ round v

mapPosition :: M.Map LatLong LatLngElevation -> LatLong -> LatLong
mapPosition elevMap ll@(LatLong lat' long' Nothing srs') = maybe ll (\(LatLngElevation _loc elev' _res) -> LatLong lat' long' (roundElevation <$> elev') srs') $ M.lookup ll elevMap
mapPosition elevMap ll = ll

mapLeg :: M.Map LatLong LatLngElevation -> M.Map Text Location -> Leg -> Leg
mapLeg elevMap locMap leg = leg {
    legFrom = M.findWithDefault lf (locationID lf) locMap
  , legTo = M.findWithDefault lt (locationID lt) locMap
  , legWaypoints = map (mapPosition elevMap) (legWaypoints leg)
  }
  where
    lf = legFrom leg
    lt = legTo leg

mapPoi ::  M.Map LatLong LatLngElevation -> PointOfInterest -> PointOfInterest
mapPoi elevMap poi = poi { poiPosition = mapPosition elevMap (poiPosition poi) }

mapLocation :: M.Map LatLong LatLngElevation -> Location -> Location
mapLocation elevMap location = location {
    locationPosition = mapPosition elevMap (locationPosition location)
  , locationPois = map (mapPoi elevMap) (locationPois location)
  }

-- Hacky job, doesn't map referenced information
mapCamino :: M.Map LatLong LatLngElevation -> Camino -> Camino
mapCamino elevMap camino = let
  locations' = map (mapLocation elevMap) (caminoLocations camino)
  locations'' = M.fromList $ map (\l -> (locationID l, l)) locations'
  legs' = map (mapLeg elevMap locations'') (caminoLegs camino)
  in
    camino {
        caminoLocations = locations'
      , caminoLocationMap = locations''
      , caminoLegs = legs'
    }

addElevations :: MapApi -> Camino -> IO Camino
addElevations api camino = do
  let locations = S.fromList $ caminoLocations camino
  let pois = S.fromList $ map fst $ M.elems $ caminoPoiMap camino
  let waypoints = S.fromList $ concat $ map legWaypoints $ caminoLegs camino
  let lrequests = S.map locationPosition locations
  let prequests = S.map poiPosition pois
  let requests = S.toList $ S.unions [lrequests, prequests, waypoints]
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
    let report' = reportOutput opts
    case report' of
      Nothing -> do
        return ()
      (Just report'') -> do
        writeReport camino' report''
    let output' = caminoOutput opts
    case output' of
      Nothing -> do
        LB.putStr $ encodePretty caminoPrintOptions $ toJSON camino'
      (Just output'') -> do
        createDirectoryIfMissing True (takeDirectory output'')
        encodeFile output'' camino'

main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Get elevation data for a camino")
    elevations opts