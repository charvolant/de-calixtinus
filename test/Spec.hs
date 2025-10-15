{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
import Test.HUnit
import CacheSpec
import CaminoSpec
import ConfigSpec
import DescriptionSpec
import EventSpec
import EventDateSpec
import FormattingSpec
import WalkingSpec
import PlannerSpec
import RegionSpec
import GraphSpec
import PartialSpec
import ProgrammingSpec
import MetadataSpec
import PropositionalSpec
import LocalisedSpec
import SplineSpec
import UtilSpec
import XlsxSpec
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Default.Class
import Data.Map
import Data.Placeholder
import Camino.Camino
import Camino.Config
import Camino.Preferences
import Data.Description (wildcardDescription)
import Data.Either (fromRight, isLeft)
import Data.Localised (wildcardText)
import Data.Maybe (fromJust)
import Control.Monad (when)

main :: IO ()
main = do
    cf <- B.readFile "camino-portuguese.json"
    let ec = eitherDecode cf :: Either String Camino
    when (isLeft ec) $ putStrLn (show ec)
    let camino = fromRight (Camino { caminoId = "Test", caminoName = wildcardText "Test", caminoDescription = wildcardDescription "", caminoMetadata = def, caminoFragment = False, caminoImports = [], caminoLocations = [], caminoLocationMap = Data.Map.empty, caminoLegs = [], caminoTransportLinks = [], caminoRoutes = [], caminoRouteLogic = [], caminoDefaultRoute = placeholder "X", caminoAccommodationMap = Data.Map.empty, caminoPoiMap = Data.Map.empty }) ec
    config <- readConfigFile "config-static.yaml"
    let cconf = createCaminoConfig (fromJust $ configCalendars config) (fromJust $ configRegions config) [camino]
    pf <- B.readFile "test-preferences.json"
    let ep = eitherDecode pf :: Either String TravelPreferences
    when (isLeft ep) $ putStrLn (show ep)
    let shortPreferences = fromRight (defaultTravelPreferences Walking Normal Pilgrim Nothing) ep
    results <- runTestTT (testList cconf shortPreferences (head $ caminoConfigCaminos cconf))
    putStrLn $ show results

testList config prefs camino = TestList [
   TestLabel "Util" testUtils,
   TestLabel "Spline" testSpline,
   TestLabel "Partial" testPartial,
   TestLabel "Localised" testLocalised,
   TestLabel "Metadata" testMetadata,
   TestLabel "Description" testDescription,
   TestLabel "Event" testEvent,
   TestLabel "Region" testRegion,
   TestLabel "Formatting" testFormatting,
   TestLabel "EventDate" testEventDate,
   TestLabel "Propositional" testPropositional,
   TestLabel "Cache" testCache,
   TestLabel "Xlsx" testXlsx,
   TestLabel "Config" testConfig,
   TestLabel "Camino" testCamino,
   TestLabel "Walking" testWalking,
   TestLabel "Graph" testGraph,
   TestLabel "Programming" testProgramming,
   TestLabel "Planner" (testPlanner config prefs camino)
  ]