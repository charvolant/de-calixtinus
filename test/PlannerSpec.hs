{-# LANGUAGE OverloadedStrings #-}

module PlannerSpec(testPlanner) where

import Test.HUnit(Test(..), assertEqual, assertBool, Assertion)
import Camino.Planner
import Camino.Camino
import Camino.Preferences
import TestUtils
import Data.Aeson
import Data.Description
import Data.Localised
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Default.Class
import Data.Placeholder
import Data.Time.Calendar
import Graph.Graph (identifier)
import System.Directory
import System.FilePath

assertPenanceEqual :: String -> Penance -> Penance -> Float -> Assertion
assertPenanceEqual msg Reject Reject _precision = assertBool msg True
assertPenanceEqual msg expected Reject _precision =
  assertBool (msg ++ " expected " ++ show expected ++ " but got rejected") False
assertPenanceEqual msg Reject actual _precision =
  assertBool (msg ++ " expected rejected but got " ++ show actual) False
assertPenanceEqual msg (Penance expected) (Penance actual) precision =
  assertBool (msg ++ " expected penance value " ++ show expected ++ " but got " ++ show actual) (abs (expected - actual) < precision)

testPlanner :: CaminoConfig -> TravelPreferences -> Camino -> Test
testPlanner config preferences camino = TestList [
    TestLabel "Hours Simple" testHoursSimple
  , TestLabel "Travel Simple" testTravelSimple
  , TestLabel "Accommodation Map Simple" testAccommodationMapSimple
  , TestLabel "Accommodation Simple" testAccommodationSimple
  , TestLabel "Penance Simple" testPenanceSimple
  , TestLabel "Stock Up Day" (testIsStockUpDay config)
  , TestLabel "Plan Camino" (testPlanCamino config preferences camino)
  , TestLabel "Persist Solution" (testSolution config preferences camino)
  ]

distanceRange1 = PreferenceRange Nothing 4.0 2.0 8.0 Nothing (Just 10.0)

timeRange1 = PreferenceRange Nothing 6.0 0.0 8.0 Nothing (Just 10.0)

restRange1 = PreferenceRange Nothing 6 5 7 Nothing (Just 8)

stopPrefs1 = StopPreferences {
  stopTransportLinks = True,
  stopLocation = M.fromList [
       (Village, (Penance 1.5)),
       (Town, (Penance 0.9)),
       (City, (Penance 0.5))
    ],
  stopAccommodation = M.fromList [
        (PilgrimAlbergue, (Penance 1.5)),
        (PrivateAlbergue, (Penance 0.9)),
        (Hotel, (Penance 0.5))
    ],
  stopServices = M.fromList [
      (Restaurant, (Penance 0.2)),
      (Groceries, (Penance 0.3))
    ],
  stopRouteServices = M.fromList [
      (Pharmacy, (Penance 0.1)),
      (Bank, (Penance 0.1))
    ]
}

stopPrefs2 = StopPreferences {
    stopTransportLinks = False,
    stopLocation = M.fromList [
       (Village, (Penance 1.5)),
       (Town, (Penance 0.9)),
       (City, (Penance 0.5))
     ],
    stopAccommodation = M.fromList [
        (PilgrimAlbergue, (Penance 4.0)),
        (PrivateAlbergue, (Penance 3.0))
      ],
    stopServices = M.fromList [
      (Restaurant, (Penance 0.2)),
      (Groceries, (Penance 0.3))
    ],
    stopRouteServices = M.fromList [
      (Pharmacy, (Penance 0.1)),
      (Bank, (Penance 0.1))
    ]
}


stopPrefs3 = StopPreferences {
    stopTransportLinks = False,
    stopLocation = M.fromList [
       (Town, (Penance 0.9)),
       (City, (Penance 0.5))
     ],
    stopAccommodation = M.fromList [
        (PilgrimAlbergue, (Penance 4.0)),
        (PrivateAlbergue, (Penance 3.0))
      ],
    stopServices = M.fromList [
      (Restaurant, (Penance 0.5)),
      (Groceries, (Penance 0.5))
    ],
    stopRouteServices = M.fromList [
      (Pharmacy, (Penance 0.1)),
      (Bank, (Penance 0.1))
    ]
}

preferences1 = TravelPreferences {
    preferenceTravel = Walking,
    preferenceFitness = Normal,
    preferenceComfort = Pilgrim,
    preferenceDistance = distanceRange1,
    preferenceTime = timeRange1,
    preferenceRest = restRange1,
    preferenceStop = stopPrefs1,
    preferenceStockStop = stopPrefs1,
    preferenceRestStop = stopPrefs1,
    preferencePoiCategories = S.singleton ReligiousPoi
  }


preferences2 = TravelPreferences {
    preferenceTravel = Cycling,
    preferenceFitness = Normal,
    preferenceComfort = Pilgrim,
    preferenceDistance = distanceRange1,
    preferenceTime = timeRange1,
    preferenceRest = restRange1,
    preferenceStop = stopPrefs2,
    preferenceStockStop = stopPrefs2,
    preferenceRestStop = stopPrefs3,
    preferencePoiCategories = S.singleton ReligiousPoi
  }


location1 = Location {
    locationID = "A",
    locationName = wildcardText "A",
    locationType = Poi,
    locationDescription = Nothing,
    locationCamping = True,
    locationAlwaysOpen = False,
    locationPosition = Nothing,
    locationRegion = Just $ placeholder "PT",
    locationServices = S.empty,
    locationAccommodation = [],
    locationPois = [],
    locationEvents = []
  }

location2 = Location {
    locationID = "B",
    locationName = wildcardText "B",
    locationType = Poi,
    locationDescription = Nothing,
    locationCamping = True,
    locationAlwaysOpen = False,
    locationPosition = Nothing,
    locationRegion = Just $ placeholder "ES-GA",
    locationServices = S.empty,
    locationAccommodation = [
      GenericAccommodation PilgrimAlbergue,
      Accommodation "B#2" (wildcardText "B2") PrivateAlbergue (S.fromList [ Handwash, Bedlinen, Towels ]) (S.fromList [ Shared, Double ]) Nothing
    ],
    locationPois = [],
    locationEvents = []
  }
 
location3 = Location {
    locationID = "C",
    locationName = wildcardText "C",
    locationType = Poi,
    locationDescription = Nothing,
    locationCamping = True,
    locationAlwaysOpen = False,
    locationPosition = Nothing,
    locationRegion = Just $ placeholder "ES-GA",
    locationServices = S.empty,
    locationAccommodation = [
      Accommodation "C#1" (wildcardText "C1") Hotel (S.fromList [ Restaurant, Breakfast, Dinner, Bedlinen, Towels, Heating ]) (S.fromList [ DoubleWC ]) Nothing
    ],
    locationPois = [],
    locationEvents = []
  }
  
  
location4 = Location {
    locationID = "D",
    locationName = wildcardText "D",
    locationType = Poi,
    locationDescription = Nothing,
    locationPosition = Nothing,
    locationRegion = Just $ placeholder "ES",
    locationCamping = True,
    locationAlwaysOpen = False,
    locationServices = S.empty,
    locationAccommodation = [
      GenericAccommodation PilgrimAlbergue
    ],
    locationPois = [],
    locationEvents = []
  }
  

legs0 = [
  Leg { legType = Road, legFrom = location1, legTo = location2, legDistance = 2.0, legTime = Nothing, legAscent = 100, legDescent = 50, legPenance = Nothing, legDescription = Nothing }
  ]

legs1 = [
  Leg { legType = Road, legFrom = location1, legTo = location2, legDistance = 2.0, legTime = Nothing, legAscent = 100, legDescent = 50, legPenance = Nothing, legDescription = Nothing },
  Leg { legType = Road, legFrom = location2, legTo = location3, legDistance = 3.5, legTime = Nothing, legAscent = 0, legDescent = 350, legPenance = Nothing, legDescription = Nothing }
  ]
  
legs2 = [
  Leg { legType = Road, legFrom = location1, legTo = location2, legDistance = 2.0, legTime = Nothing, legAscent = 100, legDescent = 5, legPenance = Nothing, legDescription = Nothing },
  Leg { legType = Road, legFrom = location2, legTo = location3, legDistance = 3.5, legTime = Nothing, legAscent = 0, legDescent = 350, legPenance = Nothing, legDescription = Nothing },
  Leg { legType = Road, legFrom = location3, legTo = location4, legDistance = 4.5, legTime = Nothing, legAscent = 200, legDescent = 0, legPenance = Nothing, legDescription = Nothing }
  ]

legs4 = [
  Leg { legType = Road, legFrom = location2, legTo = location3, legDistance = 3.5, legTime = Nothing, legAscent = 0, legDescent = 350, legPenance = Nothing, legDescription = Nothing }
  ]

links1 = [
  Leg { legType = BusLink, legFrom = location1, legTo = location2, legDistance = 0.0, legTime = Just 0.5, legAscent = 0, legDescent = 0, legPenance = Nothing, legDescription = Nothing }
  ]

route1 = Route { 
  routeID = "R1", 
  routeName = wildcardText "R1", 
  routeDescription = wildcardDescription "Route 1",
  routeMajor = True,
  routeLocations = S.fromList [location1, location2, location3], 
  routeStarts = [],
  routeFinishes = [],
  routeStops = S.empty,
  routeSuggestedPois = S.empty,
  routePalette = def 
}

camino1 = Camino {
  caminoId = "Test",
  caminoName = wildcardText "Test",
  caminoDescription = wildcardDescription "Test camino",
  caminoMetadata = def,
  caminoFragment = False,
  caminoImports = [],
  caminoLocations = M.fromList [("A", location1), ("B", location2), ("C", location3)],
  caminoLegs = legs1,
  caminoTransportLinks = [],
  caminoRoutes = [route1],
  caminoRouteLogic = [],
  caminoDefaultRoute = route1,
  caminoPois = M.empty
}

cpreferences1 = CaminoPreferences {
  preferenceCamino = camino1,
  preferenceRoutes = S.singleton route1,
  preferenceStart = location1,
  preferenceFinish = location3,
  preferenceStops = S.empty,
  preferenceExcluded = S.empty,
  preferencePois = S.empty,
  preferenceStartDate = Just $ fromGregorian 2025 January 16
}

accommodationMap1 = buildTripChoiceMap (accommodationChoice (const True) stopPrefs1 camino1) preferences1 camino1 location1 location3 (const True)

locationMap1 = buildTripChoiceMap (locationChoice stopPrefs1) preferences1 camino1 location1 location3 (const True)

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
    assertEqual "Accommodation Map Simple 1 1" Reject (tripChoicePenance $ accommodationMap1 M.! location1)
  )

testAccommodationSimple = TestList [ testAccommodationSimple1, testAccommodationSimple2, testAccommodationSimple3, testAccommodationSimple4, testAccommodationSimple5, testAccommodationSimple6 ]

testAccommodationSimple1 = let
    accom = accommodationChoice (const True) stopPrefs1 camino1 location2
  in
    TestCase (do
      assertEqual "Accommodation Simple 1 1" PilgrimAlbergue (accommodationType $ tripChoice accom)
      assertEqual "Accommodation Simple 1 2" S.empty (tripChoiceFeatures accom)
      assertPenanceEqual "Accommodation Simple 1 3" (Penance 1.5) (tripChoicePenance accom) 0.001
    )
testAccommodationSimple2 = let
    accom = accommodationChoice (const True) stopPrefs1 camino1 location1
  in
    TestCase (do
      assertEqual "Accommodation Simple 2 1" Camping (accommodationType $ tripChoice accom)
      assertEqual "Accommodation Simple 2 2" S.empty (tripChoiceFeatures accom)
      assertPenanceEqual "Accommodation Simple 2 3" Reject (tripChoicePenance accom) 0.001
    )
testAccommodationSimple3 = let
    accom = accommodationChoice (const True) stopPrefs1 camino1 location3
  in
    TestCase (do
      assertEqual "Accommodation Simple 3 1" Hotel (accommodationType $ tripChoice accom)
      assertEqual "Accommodation Simple 3 2" (S.singleton Restaurant) (tripChoiceFeatures accom)
      assertPenanceEqual "Accommodation Simple 3 3" (Penance 0.5) (tripChoicePenance accom) 0.001
    )
testAccommodationSimple4 = let
    accom = accommodationChoice (const True) stopPrefs2 camino1 location3
  in
    TestCase (do
      assertEqual "Accommodation Simple 4 1" Hotel (accommodationType $ tripChoice accom)
      assertEqual "Accommodation Simple 4 2" (S.singleton Restaurant) (tripChoiceFeatures accom)
      assertPenanceEqual "Accommodation Simple 4 3" Reject (tripChoicePenance accom) 0.001
    )
testAccommodationSimple5 = let
    camino1' = camino1 { caminoTransportLinks = links1 }
    accom = accommodationChoice (const True) stopPrefs1 camino1' location1
  in
    TestCase (do
      assertEqual "Accommodation Simple 5 1" PilgrimAlbergue (accommodationType $ tripChoice accom)
      assertEqual "Accommodation Simple 5 2" (S.empty) (tripChoiceFeatures accom)
      assertPenanceEqual "Accommodation Simple 5 3" (Penance 1.5) (tripChoicePenance accom) 0.001
    )

testAccommodationSimple6 = let
    sps1 = stopPrefs1 { stopTransportLinks = False }
    camino1' = camino1 { caminoTransportLinks = links1 }
    accom = accommodationChoice (const True) sps1 camino1' location1
  in
    TestCase (do
      assertEqual "Accommodation Simple 6 1" Camping (accommodationType $ tripChoice accom)
      assertEqual "Accommodation Simple 6 2" (S.empty) (tripChoiceFeatures accom)
      assertPenanceEqual "Accommodation Simple 6 3" Reject (tripChoicePenance accom) 0.001
    )


testPenanceSimple = TestList [ testPenanceSimple1, testPenanceSimple2, testPenanceSimple3, testPenanceSimple4 ]

testPenanceSimple1 = TestCase (assertPenanceEqual "Penance Simple 1" (Penance 4.4) (metricsPenance $ penance stopPrefs1 preferences1 cpreferences1 accommodationMap1 locationMap1 legs0) 0.1)

testPenanceSimple2 = TestCase (assertPenanceEqual "Penance Simple 2" (Penance 6.9) (metricsPenance $ penance stopPrefs1 preferences1 cpreferences1 accommodationMap1 locationMap1 legs1) 0.1)

testPenanceSimple3 = TestCase (assertPenanceEqual "Penance Simple 3" (Penance 3.3) (metricsPenance $ penance stopPrefs2 preferences2 cpreferences1 accommodationMap1 locationMap1 legs0) 0.1)

testPenanceSimple4 = TestCase (assertPenanceEqual "Penance Simple 4" (Penance 3.7) (metricsPenance $ penance stopPrefs2 preferences2 cpreferences1 accommodationMap1 locationMap1 legs1) 0.1)

testIsStockUpDay config = TestList [
  testIsStockUpDay1 config, testIsStockUpDay2 config, testIsStockUpDay3 config, testIsStockUpDay4 config,
  testIsStockUpDay5 config
  ]

testIsStockUpDay1 config = TestCase (assertEqual "IsStockUpDay 1" False (isStockUpDay config (fromGregorian 2025 May 10) []))

testIsStockUpDay2 config = let
    day1 = Chain location1 location2 legs0 mempty
  in
    TestCase (assertEqual "IsStockUpDay 2" False (isStockUpDay config (fromGregorian 2025 May 10) [day1]))

testIsStockUpDay3 config = let
    day1 = Chain location1 location2 legs0 mempty
    day2 = Chain location2 location3 legs4 mempty
  in
    TestCase (assertEqual "IsStockUpDay 3" True (isStockUpDay config (fromGregorian 2025 May 10) [day1, day2]))

testIsStockUpDay4 config = let
    day1 = Chain location1 location2 legs0 mempty
    day2 = Chain location2 location3 legs4 mempty
  in
    TestCase (assertEqual "IsStockUpDay 4" False (isStockUpDay config (fromGregorian 2025 May 11) [day1, day2]))

testIsStockUpDay5 config = let
    day1 = Chain location1 location2 legs0 mempty
    day2 = Chain location2 location3 legs4 mempty
  in
    TestCase (assertEqual "IsStockUpDay 5" True (isStockUpDay config (fromGregorian 2025 December 24) [day1, day2]))

testPlanCamino config preferences camino = TestList [ testPlanCamino1 config preferences camino]

testPlanCamino1 config preferences camino =
  let
    cpreferences = CaminoPreferences {
      preferenceCamino = camino,
      preferenceRoutes = S.singleton $ caminoDefaultRoute camino,
      preferenceStart = (caminoLocations camino) M.! "P-P1",
      preferenceFinish = (caminoLocations camino) M.! "P-P12",
      preferenceStops = S.empty,
      preferenceExcluded = S.empty,
      preferencePois = S.empty,
      preferenceStartDate = Just $ fromGregorian 2025 January 16
    }
    solution = planCamino config preferences cpreferences
    mpilgrimage = solutionPilgrimage solution
  in
    TestCase (do
      assertBool "Plan Camino 1 1" (isJust mpilgrimage)
      let pilgrimage = fromJust mpilgrimage
      assertEqual "Plan Camino 1 2" (preferenceStart cpreferences) (start pilgrimage)
      assertEqual "Plan Camino 1 3" (preferenceFinish cpreferences) (finish pilgrimage)
      assertEqual "Plan Camino 1 4" 1 (length $ path pilgrimage)
      let route = path pilgrimage !! 0
      assertEqual "Plan Camino 1 5" 2 (length $ path route)
      let day1 = path route !! 0
      assertEqual "Plan Camino 1 6" "P-P1" (identifier $ start day1)
      assertEqual "Plan Camino 1 7" "P-P7" (identifier $ finish day1)
      assertEqual "Plan Camino 1 8" 4 (length $ path day1)
      assertPenanceEqual "Plan Camino 1 9" (Penance 21.0) (metricsPenance $ score day1) 0.1
      let day2 = path route !! 1
      assertEqual "Plan Camino 1 10" "P-P7" (identifier $ start day2)
      assertEqual "Plan Camino 1 11" "P-P12" (identifier $ finish day2)
      assertEqual "Plan Camino 1 12" 5 (length $ path day2)
      assertPenanceEqual "Plan Camino 1 13" (Penance 20.0) (metricsPenance $ score day2) 0.1
    )

testSolution config preferences camino = TestList [
    testSolution1 config
  , testSolution2 config preferences camino
  , testSolution3 config
  ]

testSolution1 config = TestCase (do
    testdir <- openTestDir
    let solution = planCamino config preferences1 cpreferences1
    let solution' = solution { solutionID = Just "S-1" }
    let file = testdir </> "solution-1" <.> "json"
    encodeFile file solution'
    exists <- doesFileExist file
    assertBool "Solution 1 1" exists
    actual <- readFile file
    expected <- readFile ("test" </> "solution-1" <.> "json")
    assertEqualStripped "Solution 1 2" expected actual
    closeTestDir testdir
  )

testSolution2 config preferences camino = TestCase (do
    testdir <- openTestDir
    let cprefs = CaminoPreferences {
          preferenceCamino = camino,
          preferenceRoutes = S.singleton $ caminoDefaultRoute camino,
          preferenceStart = (caminoLocations camino) M.! "P-P1",
          preferenceFinish = (caminoLocations camino) M.! "P-P12",
          preferenceStops = S.empty,
          preferenceExcluded = S.empty,
          preferencePois = S.empty,
          preferenceStartDate = Just $ fromGregorian 2025 January 16
        }
    let solution = planCamino config preferences cprefs
    let solution' = solution { solutionID = Just "S-2" }
    let file = testdir </> "solution-2" <.> "json"
    encodeFile file solution'
    exists <- doesFileExist file
    assertBool "Solution 2 1" exists
    actual <- readFile file
    expected <- readFile ("test" </> "solution-2" <.> "json")
    assertEqualStripped "Solution 2 2" expected actual
    closeTestDir testdir
  )

testSolution3 config = TestCase (do
    let file = "test" </> "solution-2" <.> "json"
    msolution <- decodeFileStrict file :: IO (Maybe Solution)
    assertBool "Solution 3 1" (isJust msolution)
    let solution = normaliseSolution config $ fromJust msolution
    assertEqual "Solution 3 2" (Just "S-2") (solutionID solution)
    assertBool "Solution 3 3" (isJust $ solutionPilgrimage solution)
    let pilgrimage = fromJust $ solutionPilgrimage solution
    assertEqual "Solution 3 4" "P-P1" (locationID $ start pilgrimage)
    assertEqual "Solution 3 5" "P-P12" (locationID $ finish pilgrimage)
    let finish' = finish pilgrimage
    assertEqual "Solution 3 5" "Vila Franca de Xira" (locationName finish')
    assertEqual "Solution 3 6" 4 (length $ locationAccommodation finish')
    let sc = score pilgrimage
    let ac = metricsAccommodationChoice sc
    assertEqual "Solution 3 7" 2 (length ac)
    let (TripChoice ac1 s1 p1) = ac !! 1
    assertEqual "Solution 3 8" "Hostel DP" (accommodationName ac1)
    assertEqual "Solution 3 9" S.empty s1
    assertEqual "Solution 3 10" mempty p1
  )