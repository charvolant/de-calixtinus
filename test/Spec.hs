{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import CaminoSpec
import ConfigSpec
import DescriptionSpec
import EventSpec
import WalkingSpec
import PlannerSpec
import GraphSpec
import ProgrammingSpec
import MetadataSpec
import PropositionalSpec
import LocalisedSpec
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Default.Class
import Data.Map
import Data.Placeholder
import Camino.Camino
import Camino.Preferences
import Data.Description (wildcardDescription)
import Data.Either (fromRight, isLeft)
import Data.Localised (wildcardText)
import Control.Monad (when)

main :: IO ()
main = do
    cf <- B.readFile "camino-portuguese.json"
    let ec = eitherDecode cf :: Either String Camino
    when (isLeft ec) $ putStrLn (show ec)
    let camino = fromRight (Camino { caminoId = "Test", caminoName = wildcardText "Test", caminoDescription = wildcardDescription "", caminoMetadata = def, caminoLocations = Data.Map.empty, caminoLegs = [], caminoRoutes = [], caminoRouteLogic = [], caminoDefaultRoute = placeholder "X" }) ec
    pf <- B.readFile "test-preferences.json"
    let ep = eitherDecode pf :: Either String TravelPreferences
    when (isLeft ep) $ putStrLn (show ep)
    let shortPreferences = fromRight (defaultTravelPreferences Walking Normal Pilgrim) ep
    results <- runTestTT (testList shortPreferences camino)
    putStrLn $ show results

testList prefs camino = TestList [
   TestLabel "Localised" testLocalised,
   TestLabel "Metadata" testMetadata,
   TestLabel "Description" testDescription,
   TestLabel "Event" testEvent,
   TestLabel "Propositional" testPropositional,
   TestLabel "Config" testConfig,
   TestLabel "Camino" testCamino, 
   TestLabel "Walking" testWalking, 
   TestLabel "Graph" testGraph, 
   TestLabel "Programming" testProgramming, 
   TestLabel "Planner" (testPlanner prefs camino) 
  ]