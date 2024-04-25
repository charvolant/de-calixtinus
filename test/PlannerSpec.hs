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
  TestLabel "Accommodation Map Simple" testAccommodationMapSimple,
  TestLabel "Accommodation Simple" testAccommodationSimple,
  TestLabel "Penance Simple" testPenanceSimple,
  TestLabel "Plan Camino" (testPlanCamino preferences camino)
  ]

distanceRange1 = PreferenceRange Nothing 4.0 2.0 8.0 Nothing (Just 10.0)

preferences1 = TravelPreferences {
    preferenceTravel = Walking,
    preferenceFitness = Normal,
    preferenceComfort = Pilgrim,
    preferenceDistance = distanceRange1,
    preferenceTime = PreferenceRange Nothing 6.0 0.0 8.0 Nothing (Just 10.0),
    preferenceStop = Penance 0.5,
    preferenceAccommodation = M.fromList [
        (MunicipalAlbergue, (Penance 1.5)),
        (PrivateAlbergue, (Penance 0.9)),
        (Hotel, (Penance 0.5))
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


preferences2 = TravelPreferences {
    preferenceTravel = Cycling,
    preferenceFitness = Normal,
    preferenceComfort = Pilgrim,
    preferenceDistance = distanceRange1,
    preferenceTime = PreferenceRange Nothing 6.0 0.0 8.0 Nothing (Just 10.0),
    preferenceStop = Penance 0.5,
    preferenceAccommodation = M.fromList [
        (MunicipalAlbergue, (Penance 4.0)),
        (PrivateAlbergue, (Penance 3.0))
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
  routeStarts = [],
  routeFinishes = [],
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
  caminoRouteLogic = [],
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

accommodationMap1 = buildAccommodationMap preferences1 camino1 location1 location3 (const True)
  
testHoursSimple = TestList [testHoursSimple1, testHoursSimple2, testHoursSimple3, testHoursSimple4, testHoursSimple5, testHoursSimple6]

testHoursSimple1 = TestCase (assertMaybeFloatEqual "Hours Simple 1" (Just 1.160) (travelHours preferences1 legs1) 0.001)

testHoursSimple2 = TestCase (assertMaybeFloatEqual "Hours Simple 2" (Just 2.313) (travelHours preferences1 legs2) 0.001)

testHoursSimple3 = TestCase (assertMaybeFloatEqual "Hours Simple 3" (Just 0.465) (travelHours preferences1 legs0) 0.001)

testHoursSimple4 = TestCase (assertMaybeFloatEqual "Hours Simple 4" (Just 0.358) (travelHours preferences2 legs1) 0.001)

testHoursSimple5 = TestCase (assertMaybeFloatEqual "Hours Simple 5" (Just 0.745) (travelHours preferences2 legs2) 0.001)

testHoursSimple6 = TestCase (assertMaybeFloatEqual "Hours Simple 6" (Just 0.212) (travelHours preferences2 legs0) 0.001)

  
testTravelSimple = TestList [testTravelSimple1, testTravelSimple2, testTravelSimple3]

testTravelSimple1 = TestCase (assertFloatEqual "Travel Simple 1" 5.5 (travel preferences1 legs1) 0.001)

testTravelSimple2 = TestCase (assertFloatEqual "Travel Simple 2" 10.0 (travel preferences1 legs2) 0.001)

testTravelSimple3 = TestCase (assertFloatEqual "Travel Simple 3" 2.0 (travel preferences1 legs0) 0.001)

testAccommodationMapSimple = TestList [ testAccommodationMapSimple1 ]

testAccommodationMapSimple1 = TestCase (do
    assertEqual "Accommodation Map Simple 1 1" Reject (accommodationChoicePenance $ accommodationMap1 M.! location1)
  )

testAccommodationSimple = TestList [ testAccommodationSimple1, testAccommodationSimple2, testAccommodationSimple3, testAccommodationSimple4 ]

testAccommodationSimple1 = let
    accom = accommodation preferences1 location2
  in
    TestCase (do
      assertEqual "Accommodation Simple 1 1" MunicipalAlbergue (accommodationType $ accommodationChoice accom)
      assertEqual "Accommodation Simple 1 2" S.empty (accommodationChoiceServices accom)
      assertPenanceEqual "Accommodation Simple 1 3" (Penance 1.5) (accommodationChoicePenance accom) 0.001
    )
testAccommodationSimple2 = let
    accom = accommodation preferences1 location1
  in
    TestCase (do
      assertEqual "Accommodation Simple 2 1" Camping (accommodationType $ accommodationChoice accom)
      assertEqual "Accommodation Simple 2 2" S.empty (accommodationChoiceServices accom)
      assertPenanceEqual "Accommodation Simple 2 3" Reject (accommodationChoicePenance accom) 0.001
    )
testAccommodationSimple3 = let
    accom = accommodation preferences1 location3
  in
    TestCase (do
      assertEqual "Accommodation Simple 3 1" Hotel (accommodationType $ accommodationChoice accom)
      assertEqual "Accommodation Simple 3 2" (S.singleton Restaurant) (accommodationChoiceServices accom)
      assertPenanceEqual "Accommodation Simple 3 3" (Penance 0.5) (accommodationChoicePenance accom) 0.001
    )
testAccommodationSimple4 = let
    accom = accommodation preferences2 location3
  in
    TestCase (do
      assertEqual "Accommodation Simple 4 1" Hotel (accommodationType $ accommodationChoice accom)
      assertEqual "Accommodation Simple 4 2" (S.singleton Restaurant) (accommodationChoiceServices accom)
      assertPenanceEqual "Accommodation Simple 4 3" Reject (accommodationChoicePenance accom) 0.001
    )


testPenanceSimple = TestList [ testPenanceSimple1, testPenanceSimple2, testPenanceSimple3, testPenanceSimple4 ]

testPenanceSimple1 = TestCase (assertPenanceEqual "Penance Simple 1" (Penance 2.9) (metricsPenance $ penance preferences1 cpreferences1 accommodationMap1 legs0) 0.1)

testPenanceSimple2 = TestCase (assertPenanceEqual "Penance Simple 2" (Penance 5.4) (metricsPenance $ penance preferences1 cpreferences1 accommodationMap1 legs1) 0.1)

testPenanceSimple3 = TestCase (assertPenanceEqual "Penance Simple 3" (Penance 1.8) (metricsPenance $ penance preferences2 cpreferences1 accommodationMap1 legs0) 0.1)

testPenanceSimple4 = TestCase (assertPenanceEqual "Penance Simple 4" (Penance 2.23) (metricsPenance $ penance preferences2 cpreferences1 accommodationMap1 legs1) 0.1)

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
      let route = fst $ fromRight (error "Bad route") mroute
      assertEqual "Plan Camino 1 2" (preferenceStart cpreferences) (start route)
      assertEqual "Plan Camino 1 3" (preferenceFinish cpreferences) (finish route)
      assertEqual "Plan Camino 1 4" 2 (length $ path route)
      -- assertPenanceEqual "Plan Camino 1 5" (Penance 0) (score route) 0.01
      let day1 = path route !! 0
      assertEqual "Plan Camino 1 6" "P-P1" (identifier $ start day1)
      assertEqual "Plan Camino 1 7" "P-P7" (identifier $ finish day1)
      assertEqual "Plan Camino 1 8" 4 (length $ path day1)
      assertPenanceEqual "Plan Camino 1 9" (Penance 24.0) (metricsPenance $ score day1) 0.1
      let day2 = path route !! 1
      assertEqual "Plan Camino 1 10" "P-P7" (identifier $ start day2)
      assertEqual "Plan Camino 1 11" "P-P12" (identifier $ finish day2)
      assertEqual "Plan Camino 1 12" 5 (length $ path day2)
      assertPenanceEqual "Plan Camino 1 13" (Penance 23.0) (metricsPenance $ score day2) 0.1
    )