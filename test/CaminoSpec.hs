{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module CaminoSpec(testCamino) where

import Test.HUnit
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class
import qualified Data.Map as M
import Data.Placeholder
import Data.Propositional
import qualified Data.Set as S
import Camino.Camino
import Text.RawString.QQ


testCamino :: Test
testCamino = TestList [
    TestLabel "Penance" testPenance
  , TestLabel "Leg" testLeg
  , TestLabel "Route Logic" testRouteLogic
  , TestLabel "Camino" testCaminoInstance
  ]
  
testPenance = TestList [
  TestLabel "PenanceAppend" testPenanceAppend
  ]
  
simplePenance1 = Penance 1.0

testPenanceAppend = TestList [
  testPenanceAppend1, testPenanceAppend2, testPenanceAppend3, testPenanceAppend4
  ]

testPenanceAppend1 = TestCase (assertEqual "Penance Plus 1" Reject (Reject <> Reject))

testPenanceAppend2 = TestCase (assertEqual "Penance Plus 2" Reject (Reject <> simplePenance1))

testPenanceAppend3 = TestCase (assertEqual "Penance Plus 3" Reject (simplePenance1 <> Reject))

testPenanceAppend4 = TestCase (assertEqual "Penance Plus 8" (Penance 2.0) (simplePenance1 <> simplePenance1))

testLeg = TestList [
  TestLabel "LegEqual" testLegEqual,
  TestLabel "LegCompare" testLegCompare
  ]

metadata1 = def

location1 = placeholder "L1"

location2 = placeholder "L2"

location3 = placeholder "L3"

locationI1 = placeholder "I1"

locationI2 = placeholder "I2"

locationE1 = placeholder "E1"

leg1 = Leg Road location1 location2 1.2 Nothing 20.0 10.0 Nothing Nothing

leg2 = Leg Trail location1 location2 1.2 Nothing 20.0 10.0 Nothing Nothing

leg3 = Leg Road location1 location3 1.2 Nothing 20.0 10.0 Nothing Nothing

leg4 = Leg Road location2 location3 1.2 Nothing 20.0 10.0 Nothing Nothing

leg5 = Leg Road location1 location3 1.3 Nothing 20.0 10.0 Nothing Nothing

leg6 = Leg Road location1 location3 1.3 Nothing 25.0 0.0 Nothing Nothing

route1 = Route "R1" "Route 1" "Route 1" (S.fromList [location1]) S.empty [] [] def

route2 = Route "R2" "Route 2" "Route 2" (S.fromList [location2]) S.empty [] [] def

route3 = Route "R3" "Route 3" "Route 3" (S.fromList [location3]) S.empty [] [] def

route4 = Route "R4" "Route 4" "Route 4" (S.fromList [location1, location2]) S.empty [] [] def

condition1 = And [Variable route1, Not $ Variable route2]

logic1 = RouteLogic (Just "Route Logic 1") condition1 (S.singleton route3) S.empty (S.singleton route4) (S.fromList [ locationI1, locationI2]) (S.fromList [locationE1] )

camino1 = Camino 
  "C1" 
  "Camino 1" 
  "Test camino 1" 
  metadata1 
  (M.fromList $ map (\l -> (locationID l, l)) [location1, location2, location3, locationI1, locationI2]) 
  [leg1, leg2, leg3, leg4, leg5, leg6]
  [route1, route2, route3, route4]
  [logic1]
  route1

testLegEqual = TestList [
  testLegEqual1, testLegEqual2, testLegEqual3, testLegEqual4, testLegEqual5, testLegEqual6
  ]

testLegEqual1 = TestCase (assertEqual "Leg Equals 1" True (leg1 == leg1))

testLegEqual2 = TestCase (assertEqual "Leg Equals 2" False (leg1 == leg2))

testLegEqual3 = TestCase (assertEqual "Leg Equals 3" False (leg1 == leg3))

testLegEqual4 = TestCase (assertEqual "Leg Equals 4" False (leg3 == leg4))

testLegEqual5 = TestCase (assertEqual "Leg Equals 5" False (leg3 == leg5))

testLegEqual6 = TestCase (assertEqual "Leg Equals 6" True (leg5 == leg6))


testLegCompare = TestList [
  testLegCompare1, testLegCompare2, testLegCompare3, testLegCompare4, testLegCompare5, testLegCompare6,
  testLegCompare7, testLegCompare8, testLegCompare9, testLegCompare10
  ]

testLegCompare1 = TestCase (assertEqual "Leg Compare 1" EQ (leg1 `compare` leg1))

testLegCompare2 = TestCase (assertEqual "Leg Compare 2" LT (leg1 `compare` leg2))

testLegCompare3 = TestCase (assertEqual "Leg Compare 3" LT (leg1 `compare` leg3))

testLegCompare4 = TestCase (assertEqual "Leg Compare 4" LT (leg3 `compare` leg4))

testLegCompare5 = TestCase (assertEqual "Leg Compare 5" LT (leg3 `compare` leg5))

testLegCompare6 = TestCase (assertEqual "Leg Compare 6" EQ (leg5 `compare` leg6))

testLegCompare7 = TestCase (assertEqual "Leg Compare 7" GT (leg2 `compare` leg1))

testLegCompare8 = TestCase (assertEqual "Leg Compare 8" GT (leg3 `compare` leg1))

testLegCompare9 = TestCase (assertEqual "Leg Compare 9" GT (leg4 `compare` leg1))

testLegCompare10 = TestCase (assertEqual "Leg Compare 10" EQ (leg6 `compare` leg5))

logicJson1 = [r|
  {
    "description": "Information",
    "condition": {
      "and": [
        "R1",
        {
          "not": "R2"
        }
      ]
    },
    "allows": [ "R3" ],
    "prohibits": [ "R4" ],
    "include": [ "I1", "I2" ],
    "exclude": [ "E1", "E2" ]
  }
  |] :: ByteString

testRouteLogic = TestList [
      TestLabel "Route Logic Read" testRouteLogicRead
    , TestLabel "Route Logic Clauses" testRouteLogicClauses
  ]
  
testRouteLogicRead = TestList [
    testRouteLogicRead1
  ]

testRouteLogicRead1 =
  let
    mlogic = eitherDecode logicJson1 :: Either String RouteLogic
    logic = either error id mlogic
  in
    TestCase (do
      assertEqual "Read RouteLogic 1 1" (Just "Information") (routeLogicDescription logic)
      assertEqual "Read RouteLogic 1 2" (S.singleton route3) (routeLogicAllows logic)
      assertEqual "Read RouteLogic 1 3" (S.singleton route4) (routeLogicProhibits logic)
      assertEqual "Read RouteLogic 1 4" 2 (S.size $ routeLogicInclude logic)
      assertEqual "Read RouteLogic 1 5" 2 (S.size $ routeLogicExclude logic)
      assertEqual "Read RouteLogic 1 6" condition1 (routeLogicCondition logic)
    )
    
testRouteLogicClauses = TestList [
    testRouteLogicClauses1
  ]

testRouteLogicClauses1 = 
  let
    clauses = createRequiresClauses logic1
  in
    TestCase (do
      assertEqual "Route Logic Clauses 1 1" 1 (length $ clauses)
   )
   
   
testCaminoInstance = TestList [
      TestLabel "Camino Complete Routes" testCompleteRoutes
    , TestLabel "Camino Route Locations" testCaminoRouteLocations
  ]

    
testCompleteRoutes = TestList [
      testCompleteRoutes1
    , testCompleteRoutes2
  ]

testCompleteRoutes1 =
  let
    (routes, membership) = completeRoutes camino1 (S.fromList [route1])
  in
    TestCase (do
      assertEqual "Complete Routes 1 1" (S.fromList [route1, route3]) routes
      assertEqual "Complete Routes 1 2" (Just T) (membership route1)
      assertEqual "Complete Routes 1 3" (Just F) (membership route2)
      assertEqual "Complete Routes 1 4" (Just T) (membership route3)
      assertEqual "Complete Routes 1 5" (Just F) (membership route4)
    )
  
testCompleteRoutes2 =
  let
    (routes, membership) = completeRoutes camino1 (S.fromList [route1, route2])
  in
    TestCase (do
      assertEqual "Complete Routes 2 1" (S.fromList [route1, route2]) routes
      assertEqual "Complete Routes 2 2" (Just T) (membership route1)
      assertEqual "Complete Routes 2 3" (Just T) (membership route2)
      assertEqual "Complete Routes 2 4" (Just F) (membership route3)
      assertEqual "Complete Routes 2 5" (Just F) (membership route4)
    )

    
testCaminoRouteLocations = TestList [
      testCaminoRouteLocations1
    , testCaminoRouteLocations2
  ]

testCaminoRouteLocations1 =
  let
    allowed = caminoRouteLocations camino1 (S.fromList [route1])
  in
    TestCase (do
      assertEqual "Camino Route Locations 1 1" (S.fromList [location1, location3, locationI1, locationI2]) allowed
    )
  
testCaminoRouteLocations2 =
  let
    allowed = caminoRouteLocations camino1 (S.fromList [route1, route2])
  in
    TestCase (do
      assertEqual "Camino Route Locations 2 1" (S.fromList [location1, location2]) allowed
    )
