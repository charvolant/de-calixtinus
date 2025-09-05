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

import GHC.Generics (Generic)
import Control.Monad
import Control.Monad.Reader
import Camino.Display.JSON
import Camino.Display.Static
import Camino.Camino
import Camino.Config
import Data.Aeson
import Data.Aeson.Formatting
import qualified Data.ByteString.Lazy as LB
import Data.Default.Class
import Data.Event (getCalendarConfig)
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Region (getRegionConfig)
import qualified Data.Set as S
import Data.Text (Text, intercalate, pack, unpack)
import Network.Google.Elevation
import Options.Applicative
import System.FilePath
import System.Directory
import System.IO

data RangeCheck = RangeCheck {
    rangeConfig :: FilePath
  , rangeCamino :: Text
}

arguments :: Parser RangeCheck
arguments =  RangeCheck
    <$> strOption (long "config" <> short 'c' <> value "./config.yaml" <> metavar "CONFIG-FILE" <> showDefault <> help "Configuration file")
    <*> strArgument (metavar "CAMINO" <> value "--" <> help "Source camino identifier")

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
  when (checkRange || checkElevation) $ putStrLn (
    (unpack $ locationID lf) ++ "," ++
    (unpack $ locationNameLabel lf) ++ "," ++
    show (latitude lfp, longitude lfp, maybe 0.0 id (elevation lfp)) ++ "," ++
    (unpack $ locationID lt) ++ "," ++
    (unpack $ locationNameLabel lt) ++ "," ++
    show (latitude ltp, longitude ltp, maybe 0.0 id (elevation ltp)) ++ "," ++
    show (latitude sf, longitude sf, maybe 0.0 id (elevation sf)) ++ "," ++
    show (latitude st, longitude st, maybe 0.0 id (elevation st)) ++ "," ++
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
    putStrLn "From ID,From Name,From,To ID,To Name,To,Segment From,Segment To,Distance,Ascent,Descent,Straight,Range Check,Delta Elevation,Elevation Check,Slop"
    rangeCheckCamino $ (caminoConfigLookup cconfig) (rangeCamino opts)

main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Check the camino for possible errors")
    rangeCheck opts