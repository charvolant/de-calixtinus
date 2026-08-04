{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Check the camino for possible problems
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Main (main) where

import Control.Monad
import Camino.Camino
import Camino.Config
import Data.Event (getCalendarConfig)
import Data.Region (getRegionConfig)
import Data.Text (Text, unpack)
import Geo.LatLong
import Options.Applicative

data RangeCheck = RangeCheck {
    rangeConfig :: FilePath
  , rangeCamino :: Text
}

arguments :: Parser RangeCheck
arguments =  RangeCheck
    <$> strOption (long "config" <> short 'c' <> value "./config.yaml" <> metavar "CONFIG-FILE" <> showDefault <> help "Configuration file")
    <*> strArgument (metavar "CAMINO" <> help "Source camino identifier")

rangeCheckSegment :: Leg -> LegSegment -> IO ()
rangeCheckSegment leg (LegSegment sf st distance ascent descent) = do
  let lf = legFrom leg
  let lfp = locationPosition lf
  let lt = legTo leg
  let ltp = locationPosition lt
  let de = abs $ maybe (ascent - descent) realToFrac $ (-) <$> (elevation st) <*> (elevation sf)
  let slop = max 20.0 (20.0 * distance) -- allow 20m per km general out of boundness
  let straight = realToFrac $ haversineDistance lfp ltp / 1000.0
  let checkRange = distance > straight * 1.2
  let checkElevation = ascent > de + slop || descent > de + slop
  let latlng ll = show (latitude ll) ++ "," ++ show (longitude ll) ++ "," ++ show (maybe 0.0 id (elevation ll))
  when (checkRange || checkElevation) $ putStrLn (
    (unpack $ locationID lf) ++ "," ++
    (unpack $ locationNameLabel lf) ++ "," ++
    latlng lfp ++ "," ++
    (unpack $ locationID lt) ++ "," ++
    (unpack $ locationNameLabel lt) ++ "," ++
    latlng ltp ++ "," ++
    latlng sf ++ "," ++
    latlng st ++ "," ++
    show distance ++ "," ++
    show ascent ++ "," ++
    show descent ++ "," ++
    show straight ++ "," ++
    (if checkRange then "*," else ",") ++
    show de ++ "," ++
    (if checkElevation then "*," else ",") ++
    show slop
    )

rangeCheckLeg :: Leg -> IO ()
rangeCheckLeg leg = do
  mapM_ (rangeCheckSegment leg) (legSegments leg)

rangeCheckCamino :: Maybe Camino -> IO ()
rangeCheckCamino Nothing = do
  putStrLn "Camino not found"
rangeCheckCamino (Just camino) = do
  mapM_ rangeCheckLeg (caminoLegs camino)

loadCamino :: AssetConfig -> IO Camino
loadCamino asset = do
  result <- readAsset asset
  return $ readCamino result

rangeCheck :: RangeCheck -> IO ()
rangeCheck opts = do
    config <- readConfigFile (rangeConfig opts)
    caminos <- mapM loadCamino (getCaminos config)
    let cconfig = createCaminoConfig (getCalendarConfig config) (getRegionConfig config) caminos
    putStrLn "From ID,From Name,From Latitude,From Longitude,From Elevation,To ID,To Name,To Latitude,To Longitude,To Elevation,Segment From Latitude,Segment From Longitude,Segment From Elevation,Segment To Latitude,Segment To Longitude,Segment To Elevation,Distance,Ascent,Descent,Straight,Range Check,Delta Elevation,Elevation Check,Slop"
    rangeCheckCamino $ (caminoConfigLookup cconfig) (rangeCamino opts)

main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Check the camino for possible errors")
    rangeCheck opts