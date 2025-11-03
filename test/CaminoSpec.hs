{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module CaminoSpec(testCamino) where

import Test.HUnit
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Default.Class
import Data.Description
import Data.Localised
import qualified Data.Map as M
import Data.Placeholder
import Data.Propositional
import qualified Data.Set as S
import Camino.Camino
import Text.RawString.QQ
import TestUtils


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
  TestLabel "LegCompare" testLegCompare,
  TestLabel "BuildLegSegments" testBuildLegSegments
  ]

metadata1 = def

location1 = (placeholder "L1") { locationPosition = LatLong 43.16340 (-1.23579) (Just 185) def }

location2 = (placeholder "L2") { locationPosition = LatLong 43.12428 (-1.24495) (Just 495) def }

location3 = placeholder "L3"

locationI1 = placeholder "I1"

locationI2 = placeholder "I2"

locationE1 = placeholder "E1"

leg1 :: Leg
leg1 = Leg Road location1 location2 Nothing 5.28 Nothing 320.0 20.0 Nothing [] []

leg1a :: Leg
leg1a = Leg Road location1 location2 Nothing 5.28 Nothing 320.0 20.0 Nothing [LatLong 43.14417 (-1.24028) (Just 200) def] []

leg1b :: Leg
leg1b = Leg Road location1 location2 Nothing 5.28 Nothing 330.0 30.0 Nothing [LatLong 43.14417 (-1.24028) (Just 200) def, LatLong 43.13470 (-1.23803) (Just 185) def] []

leg2 :: Leg
leg2 = Leg Trail location1 location2 Nothing 1.2 Nothing 20.0 10.0 Nothing [] []

leg3 :: Leg
leg3 = Leg Road location1 location3 Nothing 1.2 Nothing 20.0 10.0 Nothing [] []

leg4 :: Leg
leg4 = Leg Road location2 location3 Nothing 1.2 Nothing 20.0 10.0 Nothing [] []

leg5 :: Leg
leg5 = Leg Road location1 location3 Nothing 1.3 Nothing 20.0 10.0 Nothing [] []

leg6 :: Leg
leg6 = Leg Road location1 location3 Nothing 1.3 Nothing 25.0 0.0 Nothing [] []

route1 :: Route
route1 = Route "R1" (wildcardText "Route 1") (wildcardDescription "Route 1") True [location1] (S.fromList [location1]) [] [][] [] [] [] def []

route2 :: Route
route2 = Route "R2" (wildcardText "Route 2") (wildcardDescription "Route 2") True [location2] (S.fromList [location2]) [] [] [] [] [] [] def []

route3 :: Route
route3 = Route "R3" (wildcardText "Route 3") (wildcardDescription "Route 3") True [location3] (S.fromList [location3]) [] [][] [] [] [] def []

route4 :: Route
route4 = Route "R4" (wildcardText "Route 4") (wildcardDescription "Route 4") True [location1, location2] (S.fromList [location1, location2]) [] [] [] [] [] [] def []

condition1 = And [Variable route1, Not $ Variable route2]

logic1 = RouteLogic (Just "Route Logic 1") condition1 (S.singleton route3) S.empty (S.singleton route4) [locationI1, locationI2] [locationE1]

camino1 :: Camino
camino1 = Camino {
    caminoId = "C1"
  , caminoName =   (wildcardText "Camino 1")
  , caminoDescription = (wildcardDescription "Test camino 1")
  , caminoMetadata = metadata1
  , caminoFragment = False
  , caminoImports = []
  , caminoLocations = locations
  , caminoLegs = [leg1, leg2, leg3, leg4, leg5, leg6]
  , caminoTransportLinks = []
  , caminoRoutes = [route1, route2, route3, route4]
  , caminoRouteLogic = [logic1]
  , caminoDefaultRoute = route1
  , caminoLocationMap = locationMap
  , caminoAccommodationMap = accommodationMap
  , caminoPoiMap = M.empty
  }
  where
    locations = [location1, location2, location3, locationI1, locationI2]
    locationMap = M.fromList $ map (\l -> (locationID l, l)) locations
    accommodation = foldl (\as -> \l -> map (\a -> (a, l)) (filter (not . isGenericAccommodation) (locationAccommodation l)) ++ as) [] locations
    accommodationMap = M.fromList $ map (\a -> (accommodationID $ fst a, a)) accommodation


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

testBuildLegSegments = TestList [
      testBuildLegSegments1
    , testBuildLegSegments2
    , testBuildLegSegments3
  ]

testBuildLegSegments1 = TestCase (do
  let segs = buildLegSegments (legFrom leg1) (legTo leg1) (legWaypoints leg1) (legDistance leg1) (legAscent leg1) (legDescent leg1)
  assertEqual "Build Leg Segments 1 1" 1 (length segs)
  let seg1 = segs !! 0
  assertEqual "Build Leg Segments 1 2" (locationPosition $ legFrom leg1) (lsFrom seg1)
  assertEqual "Build Leg Segments 1 3" (locationPosition $ legTo leg1) (lsTo seg1)
  assertEqual "Build Leg Segments 1 4" (legDistance leg1) (lsDistance seg1)
  assertEqual "Build Leg Segments 1 5" (legAscent leg1) (lsAscent seg1)
  assertEqual "Build Leg Segments 1 6" (legDescent leg1) (lsDescent seg1)
  )

testBuildLegSegments2 = TestCase (do
  let segs = buildLegSegments (legFrom leg1a) (legTo leg1a) (legWaypoints leg1a) (legDistance leg1a) (legAscent leg1a) (legDescent leg1a)
  assertEqual "Build Leg Segments 2 1" 2 (length segs)
  let seg1 = segs !! 0
  let seg2 = segs !! 1
  assertEqual "Build Leg Segments 2 2" (locationPosition $ legFrom leg1a) (lsFrom seg1)
  assertEqual "Build Leg Segments 2 3" (locationPosition $ legTo leg1a) (lsTo seg2)
  assertFloatEqual "Build Leg Segments 2 4" (legDistance leg1a) (lsDistance seg1 + lsDistance seg2) 0.001
  assertFloatEqual "Build Leg Segments 2 5" (legAscent leg1a) (lsAscent seg1 + lsAscent seg2) 0.001
  assertFloatEqual "Build Leg Segments 2 6" (legDescent leg1a) (lsDescent seg1 + lsDescent seg2) 0.001
  assertFloatEqual "Build Leg Segments 2 7" 2.60 (lsDistance seg1) 0.01
  assertFloatEqual "Build Leg Segments 2 8" 17.0 (lsAscent seg1) 1.0
  assertFloatEqual "Build Leg Segments 2 9" 10.0 (lsDescent seg1) 1.0
  assertFloatEqual "Build Leg Segments 2 10" 2.68 (lsDistance seg2) 0.01
  assertFloatEqual "Build Leg Segments 2 11" 303 (lsAscent seg2) 1.0
  assertFloatEqual "Build Leg Segments 2 12" 10 (lsDescent seg2) 1.0
  )

testBuildLegSegments3 = TestCase (do
  let segs = buildLegSegments (legFrom leg1b) (legTo leg1b) (legWaypoints leg1b) (legDistance leg1b) (legAscent leg1b) (legDescent leg1b)
  assertEqual "Build Leg Segments 3 1" 3 (length segs)
  let seg1 = segs !! 0
  let seg2 = segs !! 1
  let seg3 = segs !! 2
  assertEqual "Build Leg Segments 3 2" (locationPosition $ legFrom leg1b) (lsFrom seg1)
  assertEqual "Build Leg Segments 3 3" (locationPosition $ legTo leg1b) (lsTo seg3)
  assertFloatEqual "Build Leg Segments 3 4" (legDistance leg1b) (lsDistance seg1 + lsDistance seg2 + lsDistance seg3) 0.001
  assertFloatEqual "Build Leg Segments 3 5" (legAscent leg1b) (lsAscent seg1 + lsAscent seg2 + lsAscent seg3) 0.001
  assertFloatEqual "Build Leg Segments 3 6" (legDescent leg1b) (lsDescent seg1 + lsDescent seg2 + lsDescent seg3) 0.001
  assertFloatEqual "Build Leg Segments 3 7" 2.53 (lsDistance seg1) 0.01
  assertFloatEqual "Build Leg Segments 3 8" 16.0 (lsAscent seg1) 1.0
  assertFloatEqual "Build Leg Segments 3 9" 7.0 (lsDescent seg1) 1.0
  assertFloatEqual "Build Leg Segments 3 10" 1.25 (lsDistance seg2) 0.01
  assertFloatEqual "Build Leg Segments 3 11" 1 (lsAscent seg2) 1.0
  assertFloatEqual "Build Leg Segments 3 12" 20.0 (lsDescent seg2) 1.0
  assertFloatEqual "Build Leg Segments 3 13" 1.50 (lsDistance seg3) 0.01
  assertFloatEqual "Build Leg Segments 3 14" 314 (lsAscent seg3) 1.0
  assertFloatEqual "Build Leg Segments 3 15" 4.0 (lsDescent seg3) 1.0
  )

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
      assertEqual "Read RouteLogic 1 4" 2 (length $ routeLogicInclude logic)
      assertEqual "Read RouteLogic 1 5" 2 (length $ routeLogicExclude logic)
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
