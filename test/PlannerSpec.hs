{-# LANGUAGE OverloadedStrings #-}

module PlannerSpec(testPlanner) where

import Test.HUnit(Test(..), assertEqual, assertBool, Assertion)
import Camino.Planner
import Camino.Camino
import Camino.Preferences
import TestUtils
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import Graph.Graph (vertex, identifier)

assertPenanceEqual :: String -> Penance -> Penance -> Float -> Assertion
assertPenanceEqual msg Reject Reject _precision = assertBool msg True
assertPenanceEqual msg expected Reject _precision =
  assertBool (msg ++ " expected " ++ show expected ++ " but got rejected") False
assertPenanceEqual msg Reject actual _precision =
  assertBool (msg ++ " expected rejected but got " ++ show actual) False
assertPenanceEqual msg (Penance expected) (Penance actual) precision =
  assertBool (msg ++ " expected penance value " ++ show expected ++ " but got " ++ show actual) (abs (expected - actual) < precision)

testPlanner :: Preferences -> Camino -> Test
testPlanner preferences camino = TestList [
  TestLabel "Hours Simple" testHoursSimple,
  TestLabel "Travel Simple" testTravelSimple,
  TestLabel "Accomodation Simple" testAccomodationSimple,
  TestLabel "Penance Simple" testPenanceSimple,
  TestLabel "Plan Camino" (testPlanCamino preferences camino)
  ]

preferences1 = Preferences { 
    preferenceWalkingFunction = "tobler",
    preferenceFitness = Normal,
    preferenceDistance = PreferenceRange Nothing 4.0 2.0 8.0 0.0 10.0,
    preferenceTime = PreferenceRange Nothing 6.0 0.0 8.0 0.0 10.0,
    preferencePerceivedDistance = PreferenceRange Nothing 4.0 2.0 8.0 0.0 10.0,
    preferenceAccommodation = M.fromList [
        (MunicipalAlbergue, (Penance 1.5)),
        (PrivateAlbergue, (Penance 0.9))
      ],
    preferenceRequired = S.empty,
    preferenceExcluded = S.empty
  }

location1 = Location {
    locationID = "A",
    locationName = "A",
    locationPosition = Nothing,
    locationServices = S.empty,
    locationAccommodation = []
  }

location2 = Location {
    locationID = "B",
    locationName = "B",
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
    locationPosition = Nothing,
    locationServices = S.empty,
    locationAccommodation = [
      Accommodation "C1" Hotel (S.fromList [ Restaurant, Breakfast, Dinner, Bedlinen, Towels, Heating ]) (S.fromList [ DoubleWC ])
    ]  
  }
  
  
location4 = Location {
    locationID = "D",
    locationName = "D",
    locationPosition = Nothing,
    locationServices = S.empty,
    locationAccommodation = [
      GenericAccommodation MunicipalAlbergue
    ]  
  }


legs0 = [
  Leg { legFrom = location1, legTo = location2, legDistance = 2.0, legAscent = 100, legDescent = 50 }
  ]

legs1 = [
  Leg { legFrom = location1, legTo = location2, legDistance = 2.0, legAscent = 100, legDescent = 50 },
  Leg { legFrom = location2, legTo = location3, legDistance = 3.5, legAscent = 0, legDescent = 350 }
  ]
  
legs2 = [
  Leg { legFrom = location1, legTo = location2, legDistance = 2.0, legAscent = 100, legDescent = 50 },
  Leg { legFrom = location2, legTo = location3, legDistance = 3.5, legAscent = 0, legDescent = 350 },
  Leg { legFrom = location3, legTo = location4, legDistance = 4.5, legAscent = 200, legDescent = 0 }
  ]

camino1 = Camino {
  locations = M.fromList [("A", location1), ("B", location2), ("C", location3)],
  legs = legs1,
  routes = [],
  palette = defaultPalette
}
  
testHoursSimple = TestList [testHoursSimple1, testHoursSimple2, testHoursSimple3]

testHoursSimple1 = TestCase (assertMaybeFloatEqual "Hours Simple 1" (Just 1.160) (hours preferences1 legs1) 0.001)

testHoursSimple2 = TestCase (assertMaybeFloatEqual "Hours Simple 2" (Just 2.306) (hours preferences1 legs2) 0.001)

testHoursSimple3 = TestCase (assertMaybeFloatEqual "Hours Simple 3" (Just 0.465) (hours preferences1 legs0) 0.001)

  
testTravelSimple = TestList [testTravelSimple1, testTravelSimple2, testTravelSimple3]

testTravelSimple1 = TestCase (assertFloatEqual "Travel Simple 1" 5.5 (travel preferences1 legs1) 0.001)

testTravelSimple2 = TestCase (assertFloatEqual "Travel Simple 2" 10.0 (travel preferences1 legs2) 0.001)

testTravelSimple3 = TestCase (assertFloatEqual "Travel Simple 3" 2.0 (travel preferences1 legs0) 0.001)

testAccomodationSimple = TestList [ testAccomodationSimple1, testAccomodationSimple2 ]

testAccomodationSimple1 = TestCase (assertPenanceEqual "Accomodation Simple 1" (Penance 0.9) (accommodation preferences1 camino1 legs0) 0.001)

testAccomodationSimple2 = TestCase (assertPenanceEqual "Accomodation Simple 2" Reject (accommodation preferences1 camino1 legs1) 0.001)


testPenanceSimple = TestList [ testPenanceSimple1, testPenanceSimple2 ]

testPenanceSimple1 = TestCase (assertPenanceEqual "Penance Simple 1" (Penance 8.9) (metricsPenance $ penance preferences1 camino1 legs0) 0.1)

testPenanceSimple2 = TestCase (assertPenanceEqual "Penance Simple 2" Reject (metricsPenance $ penance preferences1 camino1 legs1) 0.1)

testPlanCamino preferences camino = TestList [ testPlanCamino1 preferences camino]

testPlanCamino1 preferences camino =
  let
    begin = vertex camino "P1"
    end = vertex camino "P12"
    mroute = planCamino preferences camino begin end
  in
    TestCase (do
      assertBool "Plan Camino 1 1" (isJust mroute)
      let route = fromJust mroute
      assertEqual "Plan Camino 1 2" begin (start route)
      assertEqual "Plan Camino 1 3" end (finish route)
      assertEqual "Plan Camino 1 4" 2 (length $ path route)
      -- assertPenanceEqual "Plan Camino 1 5" (Penance 0) (score route) 0.01
      let day1 = path route !! 0
      assertEqual "Plan Camino 1 6" "P1" (identifier $ start day1)
      assertEqual "Plan Camino 1 7" "P7" (identifier $ finish day1)
      assertEqual "Plan Camino 1 8" 4 (length $ path day1)
      assertPenanceEqual "Plan Camino 1 9" (Penance 28.0) (metricsPenance $ score day1) 0.1
      let day2 = path route !! 1
      assertEqual "Plan Camino 1 10" "P7" (identifier $ start day2)
      assertEqual "Plan Camino 1 11" "P12" (identifier $ finish day2)
      assertEqual "Plan Camino 1 12" 5 (length $ path day2)
      assertPenanceEqual "Plan Camino 1 13" (Penance 25.0) (metricsPenance $ score day2) 0.1
    )