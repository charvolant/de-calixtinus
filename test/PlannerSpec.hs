{-# LANGUAGE OverloadedStrings #-}

module PlannerSpec(testPlanner) where

import Test.HUnit(Test(..), assertEqual, assertBool, Assertion)
import Camino.Planner
import Camino.Camino
import Camino.Preferences
import TestUtils
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Default.Class
import Data.Either
import Data.Maybe
import Data.Metadata
import Graph.Graph (identifier)

assertPenanceEqual :: String -> Penance -> Penance -> Float -> Assertion
assertPenanceEqual msg Reject Reject _precision = assertBool msg True
assertPenanceEqual msg expected Reject _precision =
  assertBool (msg ++ " expected " ++ show expected ++ " but got rejected") False
assertPenanceEqual msg Reject actual _precision =
  assertBool (msg ++ " expected rejected but got " ++ show actual) False
assertPenanceEqual msg (Penance expected) (Penance actual) precision =
  assertBool (msg ++ " expected penance value " ++ show expected ++ " but got " ++ show actual) (abs (expected - actual) < precision)

testPlanner :: TravelPreferences -> Camino -> Test
testPlanner preferences camino = TestList [
  TestLabel "Hours Simple" testHoursSimple,
  TestLabel "Travel Simple" testTravelSimple,
  TestLabel "Accommodation Simple" testAccommodationSimple,
  TestLabel "Penance Simple" testPenanceSimple,
  TestLabel "Plan Camino" (testPlanCamino preferences camino)
  ]

preferences1 = TravelPreferences {
    preferenceTravelFunction = Walking,
    preferenceFitness = Normal,
    preferenceDistance = PreferenceRange Nothing 4.0 2.0 8.0 Nothing (Just 10.0),
    preferenceTime = PreferenceRange Nothing 6.0 0.0 8.0 Nothing (Just 10.0),
    preferencePerceivedDistance = PreferenceRange Nothing 4.0 2.0 8.0 Nothing (Just 10.0),
    preferenceStop = Penance 0.5,
    preferenceAccommodation = M.fromList [
        (MunicipalAlbergue, (Penance 1.5)),
        (PrivateAlbergue, (Penance 0.9))
      ],
    preferenceStopServices = M.fromList [
      (Restaurant, (Penance 0.2)),
      (Groceries, (Penance 0.3))
    ],
    preferenceDayServices = M.fromList [
      (Pharmacy, (Penance 0.1)),
      (Bank, (Penance 0.1))
    ]
  }

location1 = Location {
    locationID = "A",
    locationName = "A",
    locationType = Poi,
    locationDescription = Nothing,
    locationHref = Nothing,
    locationPosition = Nothing,
    locationServices = S.empty,
    locationAccommodation = []
  }

location2 = Location {
    locationID = "B",
    locationName = "B",
    locationType = Poi,
    locationDescription = Nothing,
    locationHref = Nothing,
    locationPosition = Nothing,
    locationServices = S.empty,
    locationAccommodation = [
      GenericAccommodation MunicipalAlbergue,
      Accommodation "B2" PrivateAlbergue (S.fromList [ Handwash, Bedlinen, Towels ]) (S.fromList [ Shared, Double ])
    ]  
  }
 
location3 = Location {
    locationID = "C",
    locationName = "C",
    locationType = Poi,
    locationDescription = Nothing,
    locationHref = Nothing,
    locationPosition = Nothing,
    locationServices = S.empty,
    locationAccommodation = [
      Accommodation "C1" Hotel (S.fromList [ Restaurant, Breakfast, Dinner, Bedlinen, Towels, Heating ]) (S.fromList [ DoubleWC ])
    ]  
  }
  
  
location4 = Location {
    locationID = "D",
    locationName = "D",
    locationType = Poi,
    locationDescription = Nothing,
    locationHref = Nothing,
    locationPosition = Nothing,
    locationServices = S.empty,
    locationAccommodation = [
      GenericAccommodation MunicipalAlbergue
    ]  
  }
  
services0 = S.fromList [ Restaurant, Groceries ]


legs0 = [
  Leg { legType = Road, legFrom = location1, legTo = location2, legDistance = 2.0, legTime = Nothing, legAscent = 100, legDescent = 50, legPenance = Nothing, legNotes = Nothing }
  ]

legs1 = [
  Leg { legType = Road, legFrom = location1, legTo = location2, legDistance = 2.0, legTime = Nothing, legAscent = 100, legDescent = 50, legPenance = Nothing, legNotes = Nothing },
  Leg { legType = Road, legFrom = location2, legTo = location3, legDistance = 3.5, legTime = Nothing, legAscent = 0, legDescent = 350, legPenance = Nothing, legNotes = Nothing }
  ]
  
legs2 = [
  Leg { legType = Road, legFrom = location1, legTo = location2, legDistance = 2.0, legTime = Nothing, legAscent = 100, legDescent = 5, legPenance = Nothing, legNotes = Nothing },
  Leg { legType = Road, legFrom = location2, legTo = location3, legDistance = 3.5, legTime = Nothing, legAscent = 0, legDescent = 350, legPenance = Nothing, legNotes = Nothing },
  Leg { legType = Road, legFrom = location3, legTo = location4, legDistance = 4.5, legTime = Nothing, legAscent = 200, legDescent = 0, legPenance = Nothing, legNotes = Nothing }
  ]

route1 = Route { 
  routeID = "R1", 
  routeName = "R1", 
  routeDescription = "Route 1",
  routeLocations = S.fromList [location1, location2, location3], 
  routeStops = S.empty,
  routePalette = def 
}

camino1 = Camino {
  caminoId = "Test",
  caminoName = "Test",
  caminoDescription = "Test camino",
  caminoMetadata = def,
  caminoLocations = M.fromList [("A", location1), ("B", location2), ("C", location3)],
  caminoLegs = legs1,
  caminoRoutes = [route1],
  caminoDefaultRoute = route1
}

cpreferences1 = CaminoPreferences {
  preferenceCamino = camino1,
  preferenceRoutes = S.singleton route1,
  preferenceStart = location1,
  preferenceFinish = location3,
  preferenceStops = S.empty,
  preferenceExcluded = S.empty
}
  
testHoursSimple = TestList [testHoursSimple1, testHoursSimple2, testHoursSimple3]

testHoursSimple1 = TestCase (assertMaybeFloatEqual "Hours Simple 1" (Just 1.160) (walkingHours preferences1 legs1) 0.001)

testHoursSimple2 = TestCase (assertMaybeFloatEqual "Hours Simple 2" (Just 2.313) (walkingHours preferences1 legs2) 0.001)

testHoursSimple3 = TestCase (assertMaybeFloatEqual "Hours Simple 3" (Just 0.465) (walkingHours preferences1 legs0) 0.001)

  
testTravelSimple = TestList [testTravelSimple1, testTravelSimple2, testTravelSimple3]

testTravelSimple1 = TestCase (assertFloatEqual "Travel Simple 1" 5.5 (travel preferences1 legs1) 0.001)

testTravelSimple2 = TestCase (assertFloatEqual "Travel Simple 2" 10.0 (travel preferences1 legs2) 0.001)

testTravelSimple3 = TestCase (assertFloatEqual "Travel Simple 3" 2.0 (travel preferences1 legs0) 0.001)

testAccommodationSimple = TestList [ testAccommodationSimple1, testAccommodationSimple2 ]

testAccommodationSimple1 = let
    (accom, serv, pen) = accommodation preferences1 camino1 legs0 services0 False
  in
    TestCase (do
      assertEqual "Accommodation Simple 1 1" True (isJust accom)
      assertEqual "Accommodation Simple 1 2" services0 serv
      assertPenanceEqual "Accommodation Simple 1 3" (Penance 1.4) pen 0.001
    )
testAccommodationSimple2 = let
    (accom, serv, pen) = accommodation preferences1 camino1 legs1 services0 False
  in
    TestCase (do
      assertEqual "Accommodation Simple 2 1" True (isNothing accom)
      assertEqual "Accommodation Simple 2 2" services0 serv
      assertPenanceEqual "Accommodation Simple 2 3" Reject pen 0.001
    )


testPenanceSimple = TestList [ testPenanceSimple1, testPenanceSimple2 ]

testPenanceSimple1 = TestCase (assertPenanceEqual "Penance Simple 1" (Penance 4.5) (metricsPenance $ penance preferences1 cpreferences1 legs0) 0.1)

testPenanceSimple2 = TestCase (assertPenanceEqual "Penance Simple 2" (Penance 15.6) (metricsPenance $ penance preferences1 cpreferences1 legs1) 0.1)

testPlanCamino preferences camino = TestList [ testPlanCamino1 preferences camino]

testPlanCamino1 preferences camino =
  let
    cpreferences = CaminoPreferences {
      preferenceCamino = camino,
      preferenceRoutes = S.singleton $ caminoDefaultRoute camino,
      preferenceStart = (caminoLocations camino) M.! "P-P1",
      preferenceFinish = (caminoLocations camino) M.! "P-P12",
      preferenceStops = S.empty,
      preferenceExcluded = S.empty
    }
    mroute = planCamino preferences cpreferences
  in
    TestCase (do
      assertBool "Plan Camino 1 1" (isRight mroute)
      let route = fromRight (error "Bad route") mroute
      assertEqual "Plan Camino 1 2" (preferenceStart cpreferences) (start route)
      assertEqual "Plan Camino 1 3" (preferenceFinish cpreferences) (finish route)
      assertEqual "Plan Camino 1 4" 2 (length $ path route)
      -- assertPenanceEqual "Plan Camino 1 5" (Penance 0) (score route) 0.01
      let day1 = path route !! 0
      assertEqual "Plan Camino 1 6" "P-P1" (identifier $ start day1)
      assertEqual "Plan Camino 1 7" "P-P7" (identifier $ finish day1)
      assertEqual "Plan Camino 1 8" 4 (length $ path day1)
      assertPenanceEqual "Plan Camino 1 9" (Penance 26.5) (metricsPenance $ score day1) 0.1
      let day2 = path route !! 1
      assertEqual "Plan Camino 1 10" "P-P7" (identifier $ start day2)
      assertEqual "Plan Camino 1 11" "P-P12" (identifier $ finish day2)
      assertEqual "Plan Camino 1 12" 5 (length $ path day2)
      assertPenanceEqual "Plan Camino 1 13" (Penance 23.5) (metricsPenance $ score day2) 0.1
    )