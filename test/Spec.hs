{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import CaminoSpec
import ConfigSpec
import WalkingSpec
import PlannerSpec
import GraphSpec
import ProgrammingSpec
import MetadataSpec
import PropositionalSpec
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Map
import Data.Placeholder
import Camino.Camino
import Camino.Preferences
import Data.Either (fromRight, isLeft)
import Control.Monad (when)
import Data.Metadata (defaultMetadata)

main :: IO ()
main = do
    cf <- B.readFile "camino-portuguese.json"
    let ec = eitherDecode cf :: Either String Camino
    when (isLeft ec) $ putStrLn (show ec)
    let camino = fromRight (Camino { caminoId = "Test", caminoName = "Test", caminoDescription = "", caminoMetadata = defaultMetadata, caminoLocations = Data.Map.empty, caminoLegs = [], caminoRoutes = [], caminoDefaultRoute = placeholder "X" }) ec
    pf <- B.readFile "short-preferences.json"
    let ep = eitherDecode pf :: Either String TravelPreferences
    when (isLeft ep) $ putStrLn (show ep)
    let shortPreferences = fromRight (defaultTravelPreferences Walking Normal) ep
    results <- runTestTT (testList shortPreferences camino)
    putStrLn $ show results

testList prefs camino = TestList [ 
    TestLabel "Metadata" testMetadata,
    TestLabel "Propositional" testPropositional,
    TestLabel "Config" testConfig,
    TestLabel "Camino" testCamino, 
    TestLabel "Walking" testWalking, 
    TestLabel "Graph" testGraph, 
    TestLabel "Programming" testProgramming, 
    TestLabel "Planner" (testPlanner prefs camino) 
  ]