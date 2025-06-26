{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Planner
Description : The camino planner
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Create an optimal camino plan, based on the preferences
-}

module Camino.Planner (
    Day
  , Metrics(..)
  , Solution(..)
  , Journey
  , Pilgrimage
  , TripChoice(..)
  , TripChoiceMap

  , accommodationChoice
  , buildTripChoiceMap
  , daysLabel
  , findDay
  , findStage
  , locationChoice
  , hasNonTravel
  , isStockUpDay
  , nonTravelHours
  , normaliseSolution
  , penance
  , openSleeping
  , pilgrimageLegs
  , pilgrimageRests
  , pilgrimageStockpoints
  , pilgrimageStops
  , pilgrimageWaypoints
  , planCamino
  , travel
  , travelHours
  , tripLabel
) where

import GHC.Generics
import Camino.Walking
import Camino.Camino
import Camino.Preferences
import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Monad.Reader (runReader)
import Data.Aeson
import Data.Event
import Data.Event.Date
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe, mapMaybe)
import Data.Metadata (Metadata(..))
import Data.Placeholder
import Data.Region
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Time.Calendar as C
import Data.Util (maybeSum, initOrEmpty, lastWithError, tailOrEmpty)
import Graph.Graph (Edge(..), available, incoming, mirror, reachable, subgraph)
import Graph.Programming
import Data.Placeholder (Placeholder)

instance (Placeholder T.Text v, FromJSON v, FromJSON e, FromJSON s, Edge e v, Score s) => FromJSON (Chain v e s) where
    parseJSON (Object v) = do
      start' <- v .: "start"
      finish' <- v .: "finish"
      path' <- v .: "path"
      score' <- v .: "score"
      return Chain {
          start = placeholder start'
        , finish = placeholder finish'
        , path = path'
        , score = score'
        }
    parseJSON v = error ("Unable to parse chain object " ++ show v)

instance (Placeholder T.Text v, ToJSON v, ToJSON e, ToJSON s, Edge e v, Score s) => ToJSON (Chain v e s) where
    toJSON (Chain start' finish' path' score') =
      object [
          "start" .= placeholderID start'
        , "finish" .= placeholderID finish'
        , "path" .= path'
        , "score" .= score'
        ]

-- | A pre-selected choice for a location, accommodation etc.
data (Ord f) => TripChoice v f = TripChoice {
      tripChoice :: v -- ^ The element selected
    , tripChoiceFeatures :: S.Set f -- ^ The additional features (services, points of interest etc) supplied by this choice
    , tripChoicePenance :: Penance -- ^ The penance associated with this choice
} deriving (Show)

instance (Ord f, FromJSON v, FromJSON f) => FromJSON (TripChoice v f) where
  parseJSON (Object v) = do
    choice' <- v .: "choice"
    features' <- v .:? "features" .!= S.empty
    penance' <- v .:? "penance" .!= mempty
    return $ TripChoice {
        tripChoice = choice'
      , tripChoiceFeatures = features'
      , tripChoicePenance = penance'
      }
  parseJSON v = error ("Unable to parse trip choice object " ++ show v)
  
instance (Ord f, ToJSON v, ToJSON f) => ToJSON (TripChoice v f) where
  toJSON (TripChoice choice' features' penance') =
    object [
        "choice" .= choice'
      , "features" .= if S.null features' then Nothing else Just features'
      , "penance" .= if penance' == mempty then Nothing else Just penance'
      ]

instance (Ord f, NFData v, NFData f) => NFData (TripChoice v f) where
  rnf tc = tripChoice tc
    `deepseq` tripChoiceFeatures tc
    `deepseq` tripChoicePenance tc
    `deepseq` ()

-- | Select trip choice based on penance / services covered / services available
selectTripChoice :: (Ord f) => (v -> S.Set f) -> TripChoice v f -> TripChoice v f -> TripChoice v f
selectTripChoice _ c1@(TripChoice _ s1 Reject) c2@(TripChoice _ s2 Reject) =
  if S.size s1 > S.size s2 then c1 else c2
selectTripChoice _ (TripChoice _ _ Reject) c2 = c2
selectTripChoice _ c1 (TripChoice _ _ Reject) = c1
selectTripChoice services c1@(TripChoice a1 s1 p1) c2@(TripChoice a2 s2 p2)
  | p1 > p2 = c1
  | p2 > p1 = c2
  | S.size s1 > S.size s2 = c1
  | S.size s2 > S.size s1 = c2
  | S.size (services a1) > S.size (services a2) = c1
  | S.size (services a2) > S.size (services a1) = c2
  | otherwise = c1

-- A map of choices for each location
type TripChoiceMap v f = M.Map Location (TripChoice v f)
-- | The trail of past/future choices
type TripChoiceTail v f = [(TripChoice v f, Penance)]
-- | The tail of past/future choices
type TripChoiceTrail v f = (TripChoice v f, TripChoiceTail v f)

-- Group together trip choice information for convienience, otherwise there's a massive argument spill
data TripChoices = TripChoices {
    tcStopAccommodation :: TripChoiceMap Accommodation Service
  , tcStopLocation :: TripChoiceMap Location Service
  , tcStockAccommodation :: TripChoiceMap Accommodation Service
  , tcStockLocation :: TripChoiceMap Location Service
  , tcRestAccommodation :: TripChoiceMap Accommodation Service
  , tcRestLocation :: TripChoiceMap Location Service
} deriving (Generic)

instance FromJSON TripChoices where
  parseJSON (Object v) = do
    stopaccommodation' <- v .: "stop-accommodation"
    stoplocation' <- v .: "stop-location"
    stockaccommodation' <- v .: "stock-accommodation"
    stocklocation' <- v .: "stock-location"
    restaccommodation' <- v .: "rest-accommodation"
    restlocation' <- v .: "rest-location"
    return $ TripChoices {
        tcStopAccommodation = stopaccommodation'
      , tcStopLocation = stoplocation'
      , tcStockAccommodation = stockaccommodation'
      , tcStockLocation = stocklocation'
      , tcRestAccommodation = restaccommodation'
      , tcRestLocation = restlocation'
      }
  parseJSON v = error ("Unable to parse trip choices object " ++ show v)

instance ToJSON TripChoices where
  toJSON (TripChoices stopaccommodation' stoplocation' stockaccommodation' stocklocation' restaccommodation' restlocation') =
    object [
        "stop-accommodation" .= stopaccommodation'
      , "stop-location" .= stoplocation'
      , "stock-accommodation" .= stockaccommodation'
      , "stock-location" .= stocklocation'
      , "rest-accommodation" .= restaccommodation'
      , "rest-location" .= restlocation'
      ]

instance NFData TripChoices

-- | The metrics for a day, journey or complete pilgrimage
data Metrics = Metrics {
      metricsDistance :: Float -- ^ Actual distance in km
    , metricsTime :: Maybe Float -- ^ Total time taken in hours, including visits to points of interest
    , metricsPoiTime :: Maybe Float -- ^ Any additional time spent visiting points of interest
    , metricsPerceivedDistance :: Maybe Float -- ^ Perceived distance in km (Nothing if the distance is too long)
    , metricsAscent :: Float -- ^ Ascent in metres
    , metricsDescent :: Float -- ^ Descent in metres
    , metricsStop :: Penance -- ^ The penance associated with day's end
    , metricsLocation :: Penance -- ^ The penance associated with the location
    , metricsAccommodationChoice :: [TripChoice Accommodation Service] -- ^ The places to stay on the way
    , metricsAccommodation :: Penance -- ^ Accommodation penance in km-equivalent. This represents the distance you would be prepared to walk to avoid this accommodation
    , metricsMissingStopServices :: S.Set Service -- ^ The services missing at the end of the day
    , metricsStopServices :: Penance -- ^ Adjustment to penance in km-equivalnet caused by missing services at the stop point
    , metricsMissingDayServices :: S.Set Service -- ^ The services missing at the end of the day
    , metricsDayServices :: Penance -- ^ Adjustment to penance in km-equivalnet caused by missing services throughout the day
    , metricsDistanceAdjust :: Penance -- ^ Adjustments to distance cost in km-equivalent caused by going over/under target
    , metricsTimeAdjust :: Penance -- ^ Adjustments to time cost in km-equivalent caused by going over/under target
    , metricsFatigue :: Penance -- ^ Cost of accumulated fatigue
    , metricsRest :: Penance -- ^ Cost of taking a rest
    , metricsMisc :: Penance -- ^ Additional miscellaneous penance costs and causes
    , metricsRestDays :: Int -- ^ Number of rest days taken
    , metricsDate :: Maybe (C.Day, C.Day) -- ^ An associated date range for this location
    , metricsStockpoint :: Bool -- ^ Stock-up at the end of the day 
    , metricsRestpoint :: Bool -- ^ The next day is a resting day
    , metricsPenance :: Penance -- ^ Total penance for this leg of the metrics
  } deriving (Show, Generic)

instance Eq Metrics where
  m1 == m2 = metricsPenance m1 == metricsPenance m2

instance Ord Metrics where
  compare m1 m2 = compare (metricsPenance m1) (metricsPenance m2)

instance Semigroup Metrics where
  m1 <> m2 = Metrics {
      metricsDistance = metricsDistance m1 + metricsDistance m2
    , metricsTime = metricsTime m1 `maybeSum` metricsTime m2
    , metricsPoiTime = metricsPoiTime m1 `maybeSum` metricsPoiTime m2
    , metricsPerceivedDistance = metricsPerceivedDistance m1 `maybeSum` metricsPerceivedDistance m2
    , metricsAscent = metricsAscent m1 + metricsAscent m2
    , metricsDescent = metricsDescent m1 + metricsDescent m2
    , metricsStop = metricsStop m1 <> metricsStop m2
    , metricsLocation = metricsLocation m1 <> metricsLocation m2
    , metricsAccommodationChoice = metricsAccommodationChoice m1 <> metricsAccommodationChoice m2
    , metricsAccommodation = metricsAccommodation m1 <> metricsAccommodation m2
    , metricsMissingStopServices = metricsMissingStopServices m1 `S.union` metricsMissingStopServices m2
    , metricsStopServices = metricsStopServices m1 <> metricsStopServices m2
    , metricsMissingDayServices = metricsMissingDayServices m1 `S.union` metricsMissingDayServices m2
    , metricsDayServices = metricsDayServices m1 <> metricsDayServices m2
    , metricsDistanceAdjust = metricsDistanceAdjust m1 <> metricsDistanceAdjust m2
    , metricsTimeAdjust = metricsTimeAdjust m1 <> metricsTimeAdjust m2
    , metricsFatigue = metricsFatigue m1 <> metricsFatigue m2
    , metricsRest = metricsRest m1 <> metricsRest m2
    , metricsMisc = metricsMisc m1 <> metricsMisc m2
    , metricsRestDays = metricsRestDays m1 + metricsRestDays m2
    , metricsDate = combineDates (metricsDate m1) (metricsDate m2)
    , metricsStockpoint = (metricsStockpoint m1) || (metricsStockpoint m2)
    , metricsRestpoint = (metricsRestpoint m1) || (metricsRestpoint m2)
    , metricsPenance = metricsPenance m1 <> metricsPenance m2
  }

instance FromJSON Metrics where
  parseJSON (Object v) = do
    distance' <- v .: "distance"
    time' <- v .:? "time"
    poitime' <- v .:? "poi-time"
    perceiveddistance' <- v .:? "perceived-distance"
    ascent' <- v .: "ascent"
    descent' <- v .: "descent"
    stop' <- v .: "stop"
    location' <- v .: "location"
    accommodationchoice' <- v .: "accommodation-choice"
    let accommodationchoice'' = map (\(TripChoice id' ss' pe') -> TripChoice (placeholder id') ss' pe') accommodationchoice'
    accommodation' <- v .: "accommodation"
    missingstopservices' <- v .: "missing-stop-services"
    stopservices' <- v .: "stop-services"
    missingdayservices' <- v .: "missing-day-services"
    dayservices' <- v .: "day-services"
    distanceadjust' <- v .: "distance-adjust"
    timeadjust' <- v .: "time-adjust"
    fatigue' <- v .: "fatigue"
    rest' <- v .: "rest"
    misc' <- v .: "misc"
    restdays' <- v .: "rest-days"
    date' <- v .:? "date"
    stockpoint' <- v .: "stock-point"
    restpoint' <- v .: "rest-point"
    penance' <- v .: "penance"
    return $ Metrics {
        metricsDistance = distance'
      , metricsTime = time'
      , metricsPoiTime = poitime'
      , metricsPerceivedDistance = perceiveddistance'
      , metricsAscent = ascent'
      , metricsDescent = descent'
      , metricsStop = stop'
      , metricsLocation = location'
      , metricsAccommodationChoice = accommodationchoice''
      , metricsAccommodation = accommodation'
      , metricsMissingStopServices = missingstopservices'
      , metricsStopServices = stopservices'
      , metricsMissingDayServices = missingdayservices'
      , metricsDayServices = dayservices'
      , metricsDistanceAdjust = distanceadjust'
      , metricsTimeAdjust = timeadjust'
      , metricsFatigue = fatigue'
      , metricsRest = rest'
      , metricsMisc = misc'
      , metricsRestDays = restdays'
      , metricsDate = date'
      , metricsStockpoint = stockpoint'
      , metricsRestpoint = restpoint'
      , metricsPenance = penance'
    }
  parseJSON v = error ("Unable to parse metrics object " ++ show v)

instance ToJSON Metrics where
  toJSON (Metrics distance' time' poitime' perceiveddistance' ascent' descent' stop' location' accommodationchoice' accommodation' missingstopservices' stopservices' missingdayservices' dayservices' distanceadjust' timeadjust' fatigue' rest' misc' restdays' date' stockpoint' restpoint' penance') =
    object [
        "distance" .= distance'
      , "time" .= time'
      , "poi-time" .= poitime'
      , "perceived-distance" .= perceiveddistance'
      , "ascent" .= ascent'
      , "descent" .= descent'
      , "stop" .= stop'
      , "location" .= location'
      , "accommodation-choice" .= map (\(TripChoice ac' ss' pe') -> TripChoice (placeholderID ac') ss' pe') accommodationchoice'
      , "accommodation" .= accommodation'
      , "missing-stop-services" .= missingstopservices'
      , "stop-services" .= stopservices'
      , "missing-day-services" .= missingdayservices'
      , "day-services" .= dayservices'
      , "distance-adjust" .= distanceadjust'
      , "time-adjust" .= timeadjust'
      , "fatigue" .= fatigue'
      , "rest" .= rest'
      , "misc" .= misc'
      , "rest-days" .= restdays'
      , "date" .= date'
      , "stock-point" .= stockpoint'
      , "rest-point" .= restpoint'
      , "penance" .= penance'
      ]

instance NFData Metrics

normaliseAccommodationChoice :: CaminoConfig -> TripChoice Accommodation Service -> TripChoice Accommodation Service
normaliseAccommodationChoice config (TripChoice accommodation' services' penance') =
  TripChoice (dereference config accommodation') services' penance'

-- Normalise metric information
normaliseMetrics :: CaminoConfig -> Metrics -> Metrics
normaliseMetrics config metrics = metrics {
    metricsAccommodationChoice = map (normaliseAccommodationChoice config) (metricsAccommodationChoice metrics)
  }

-- Combine date ranges
combineDates :: Maybe (C.Day, C.Day) -> Maybe (C.Day, C.Day) -> Maybe (C.Day, C.Day)
combineDates Nothing Nothing = Nothing
combineDates Nothing r2 = r2
combineDates r1 Nothing = r1
combineDates (Just (min1, max1)) (Just (min2, max2)) = Just (min min1 min2, max max1 max2)

instance Monoid Metrics where
  mempty = Metrics 0.0 (Just 0.0) Nothing (Just 0.0) 0.0 0.0 mempty mempty [] mempty S.empty mempty S.empty mempty mempty mempty mempty mempty mempty 0 Nothing False False mempty

instance Score Metrics where
  invalid = Metrics 0.0 (Just 0.0) Nothing (Just 0.0) 0.0 0.0 mempty mempty [] mempty S.empty mempty S.empty mempty mempty mempty mempty mempty Reject 0 Nothing False False Reject

-- | A day's stage
type Day = Chain Location Leg Metrics

-- | Normalise a day
normaliseDay :: CaminoConfig -> Day -> Day
normaliseDay config day = 
    day {
        start = dereference config (start day)
      , finish = dereference config (finish day)
      , path = map (normaliseLeg' config) (path day)
      , score = normaliseMetrics config (score day)
    }

-- | The complete camino stages
type Journey = Chain Location Day Metrics

-- | Normalise a journey
normaliseJourney :: CaminoConfig -> Journey -> Journey
normaliseJourney config journey = 
    journey {
        start = dereference config (start journey)
      , finish = dereference config (finish journey)
      , path = map (normaliseDay config) (path journey)
      , score = normaliseMetrics config (score journey)
    }

-- | A full pilgrimage, broken into stages
type Pilgrimage = Chain Location Journey Metrics

-- | Normalise a pilgrimage
normalisePilgrimage :: CaminoConfig -> Pilgrimage -> Pilgrimage
normalisePilgrimage config pilgrimage = 
    pilgrimage {
        start = dereference config (start pilgrimage)
      , finish = dereference config (finish pilgrimage)
      , path = map (normaliseJourney config) (path pilgrimage)
      , score = normaliseMetrics config (score pilgrimage)
    }

-- | A complete solution, including accommodation and location metrics
data Solution = Solution {
    solutionID :: Maybe T.Text -- ^ The unique solution identifier
  , solutionMetadata :: Maybe Metadata -- ^ Any metadata associates with the solution
  , solutionTravelPreferences :: TravelPreferences -- ^ The travel preferences used
  , solutionCaminoPreferences :: CaminoPreferences -- ^ The camino preferences used
  , solutionLocations :: S.Set Location -- ^ The locations that might be part of the solution
  , solutionJourneyFailure :: Maybe (Failure Location Leg Metrics Metrics)
  , solutionPilgrimageFailure :: Maybe (Failure Location Day Metrics Metrics)
  , solutionPilgrimage :: Maybe Pilgrimage -- ^ If successful, a pilgrimage solution
} deriving (Generic)

-- | We onblt read/write successful solutions
instance FromJSON Solution where
  parseJSON (Object v) = do
    id' <- v .:? "id"
    metadata' <- v .:? "metadata"
    locations' <- v .: "locations"
    tprefs' <- v .: "travel-preferences"
    cprefs' <- v .: "camino-preferences"
    pilgrimage' <- v .: "pilgrimage"
    return $ Solution {
        solutionID = id'
      , solutionMetadata = metadata'
      , solutionTravelPreferences = tprefs'
      , solutionCaminoPreferences = cprefs'
      , solutionLocations = locations'
      , solutionJourneyFailure = Nothing
      , solutionPilgrimageFailure = Nothing
      , solutionPilgrimage = Just pilgrimage'
      }
  parseJSON v = error ("Unable to parse solution object " ++ show v)

instance ToJSON Solution where
  toJSON (Solution id' metadata' tprefs' cprefs' locations' _jf _pf (Just pilgrimage')) =
    object [
        "id" .= id'
      , "metadata" .= metadata'
      , "travel-preferences" .= tprefs'
      , "camino-preferences" .= cprefs'
      , "locations" .= S.map placeholderID locations'
      , "pilgrimage" .= pilgrimage'
      ]
    where

  toJSON _ = error "Unable to jsonify unsuccessful solution"

instance NFData Solution

-- | Normalise a solution
normaliseSolution :: CaminoConfig -> Solution -> Solution
normaliseSolution config solution = let
    cprefs' = normalisePreferences config (solutionCaminoPreferences solution)
    locations' = dereferenceS config (solutionLocations solution)
    pilgrimage' = normalisePilgrimage config <$> solutionPilgrimage solution
  in
    solution {
       solutionCaminoPreferences = cprefs'
      , solutionLocations = locations'
      , solutionPilgrimage = pilgrimage'
    }

-- Is this the last day's travel?
isLastDay :: Location -> [Leg] -> Bool
isLastDay end day = legTo (last day) == end

-- | Get the locations associated with a day
dayLocations :: [Leg] -> [Location]
dayLocations [] = []
dayLocations day@(fl:_) = legFrom fl : map legTo day

travelFunction :: Travel -> Fitness -> (Fitness -> Float -> Float -> Float -> Float)
travelFunction Walking SuperFit = naismith
travelFunction Walking VeryFit = naismith
travelFunction Walking _ = tobler
travelFunction Cycling _ = cycling

-- | Calculate the expected hours of travel, for a sequence of legs
travelHours :: TravelPreferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Maybe Float -- ^ The hours equivalent
travelHours preferences day = let
    fitness = preferenceFitness preferences
    baseHours = travelFunction (preferenceTravel preferences) fitness
    simple = sum $ map (\l -> baseHours fitness (legDistance l) (legAscent l) (legDescent l)) day
  in
    tranter (preferenceFitness preferences) simple

-- | Calculate the expected non-travel hours, for a sequence of legs
--   Usually associated with something like a ferry
nonTravelHours :: TravelPreferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Maybe Float -- ^ The hours equivalent
nonTravelHours _preferences day =
  Just $ sum $ map (\l -> fromMaybe 0.0 (legTime l)) day

-- | Does this sequence of legs have a non-travel component?
hasNonTravel ::  TravelPreferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Bool -- ^ The hours equivalent
hasNonTravel _preferences day =
  any (\l -> legDistance l <= 0.0 && isJust (legTime l)) day

-- | Calculate the total distance covered by a sequence of legs
travel :: TravelPreferences -- ^ The calculation preferences
  -> [Leg]-- ^ The sequence of legs to use
  -> Float -- ^ The total distance covered by the sequence
travel _preferences day = sum $ map legDistance day

-- | Calculate the total ascent of a sequence of legs
totalAscent :: TravelPreferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Float -- ^ The total distance covered by the sequence
totalAscent _preferences day = sum $ map legAscent day

-- | Calculate the total descent of a sequence of legs
totalDescent :: TravelPreferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Float -- ^ The total distance covered by the sequence
totalDescent _preferences day = sum $ map legDescent day

-- | Calculate any additional penance associated with these legs
travelAdditional :: TravelPreferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Penance -- ^ The total additional penance
travelAdditional _preferences day = mconcat $ mapMaybe legPenance day

-- | Calculate the amount of additional time spent visiting attractions
-- The points of interest at the start and end of the leg are not included, as they are assumed to be done at the start/end of the day
pointOfInterestTime :: CaminoPreferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Maybe Float
pointOfInterestTime camino day = let
  dpois = foldl (\pois location -> selectedPois camino location ++ pois) [] $ map legFrom (tailOrEmpty day)
 in
  foldl maybeSum Nothing $ map poiTime dpois

-- | Calculate the travel metrics for a seqnece of legs
travelMetrics :: TravelPreferences -> CaminoPreferences -> [Leg] -> (Float, Float, Maybe Float, Float, Maybe Float, Float, Float, Maybe Float, Bool)
travelMetrics preferences camino day =
  let
    travelType = preferenceTravel preferences
    fitness = preferenceFitness preferences
    walkingSpeed = nominalSpeed Walking Normal
    normalSpeed = nominalSpeed travelType Normal
    actualSpeed = nominalSpeed travelType fitness
    travelTime = travelHours preferences day
    otherTime = nonTravelHours preferences day
    distance = travel preferences day
    ascent = totalAscent preferences day
    descent = totalDescent preferences day
    perceived = (walkingSpeed *) <$> travelTime
    pois = pointOfInterestTime camino day
    nonTravel = hasNonTravel preferences day
  in
    (normalSpeed, actualSpeed, travelTime `maybeSum` otherTime `maybeSum` pois, distance, perceived, ascent, descent, pois, nonTravel)

-- | Work out what services are missing from the desired stop list
missingStopServices :: StopPreferences -- ^ The calculation preferences
  -> Camino -- ^ The camino model
  -> Location -- ^ The sequence of legs to use
  -> S.Set Service -- ^ The list of desired but missing services from the stop location
missingStopServices preferences camino stop = (M.keysSet $ stopServices preferences) `S.difference` (completeLocationServices preferences camino stop)

-- | Work out what services are missing along the route
missingRouteServices :: StopPreferences -- ^ The calculation preferences
  -> Camino -- ^ The camino model
  -> [TripChoice Accommodation Service] -- ^ The chosen accommodation
  -> [Leg] -- ^ The sequence of legs to use
  -> S.Set Service -- ^ The list of desired but missing services from the stop location
missingRouteServices preferences _camino accoms day = let
      desired = M.keysSet $ stopRouteServices preferences
      desired' = desired `S.difference` (maybe S.empty (tripChoiceFeatures . fst) (L.uncons accoms))
    in
      foldl S.difference desired' $ map locationServices (dayLocations day)

-- | Default sleeping in the open option
openSleeping :: Accommodation
openSleeping = GenericAccommodation Camping

-- | Invalid accommodation choice
invalidAccommodation :: TripChoice Accommodation Service
invalidAccommodation = TripChoice openSleeping S.empty Reject

completeLocationServices :: StopPreferences -> Camino -> Location -> S.Set Service
completeLocationServices preferences camino location = if stopTransportLinks preferences then
    locationAllServices camino location
  else
    locationServices location

completeLocationAccommodation :: StopPreferences -> Camino -> Location -> [Accommodation]
completeLocationAccommodation preferences  camino location = if stopTransportLinks preferences then
    locationAllAccommodation camino location
  else
    locationAccommodation location

-- Select the most favourable accommodation
accommodationChoice' :: M.Map AccommodationType Penance -> S.Set Service -> Accommodation -> TripChoice Accommodation Service
accommodationChoice' accPrefs services accom = let
    base = M.findWithDefault Reject (accommodationType accom) accPrefs
    covered = services `S.intersection` accommodationServices accom
  in
    TripChoice accom covered base

accommodationChoice :: (Accommodation -> Bool) -- ^ The accommodation filter
  -> StopPreferences -- ^ The calculation preferences
  -> Camino -- ^ The complete camino
  -> Location -- ^ The location
  -> TripChoice Accommodation Service -- ^ The chosen accommodation for the location
accommodationChoice select preferences camino location = let
    services = (M.keysSet $ M.filter (/= mempty) (stopServices preferences)) `S.difference` completeLocationServices preferences camino location
    ap = stopAccommodation preferences
    defaultAccommodation = if locationCamping location then [openSleeping] else []
    accommodationOptions = filter select $  if null ao then defaultAccommodation else ao where ao = completeLocationAccommodation preferences camino location
    lp = map (accommodationChoice' ap services) accommodationOptions
    choice = foldl (selectTripChoice accommodationServices) invalidAccommodation lp
  in
    choice

-- Does this leg have no acceptable accommodation (except possibly at the start and end)
isAccommodationFree :: TravelPreferences -> TripChoiceMap Accommodation Service -> [Leg] -> Bool
isAccommodationFree _preferences accommodationMap day = all (\l -> (tripChoicePenance $ M.findWithDefault invalidAccommodation (legFrom l) accommodationMap) == Reject) (tailOrEmpty day)

-- Make a choice based on the location
locationChoice :: StopPreferences -> Location -> TripChoice Location Service
locationChoice preferences location = let
    services' =  (M.keysSet $ M.filter (/= mempty) (stopServices preferences)) `S.intersection` locationServices location
    penance' = M.findWithDefault mempty (locationType location) (stopLocation preferences)
  in
    TripChoice location services' penance'

missingServicePenance :: M.Map Service Penance -> S.Set Service -> Penance
missingServicePenance prefs services = mconcat $ map (\s -> M.findWithDefault mempty s prefs) $ S.toList services

stopPenance' :: Comfort -> Penance
stopPenance' Austere = Penance 4.0
stopPenance' Frugal = Penance 3.0
stopPenance' Pilgrim = Penance 2.0
stopPenance' Comfortable = Penance 1.0
stopPenance' Luxurious = Penance 0.0

stopPenance :: TravelPreferences -> Camino -> Location -> Penance
stopPenance preferences _camino _stop = stopPenance' (preferenceComfort preferences)

locationPenance :: TravelPreferences -> Camino -> TripChoiceMap Location Service -> Location -> Penance
locationPenance _preferences _camino locationMap location = maybe Reject tripChoicePenance $ M.lookup location locationMap

adjustment :: PreferenceRange Float -> Float -> Float -> Float -> Penance
adjustment range bscale rscale value
  | isOutOfRange range value = Reject
  | isOutOfBounds range value = Penance (rscale * rangeDistance range value)
  | otherwise = Penance (bscale * boundsDistance range value)

-- | Calculate metrics associated with stopping at a location
locationMetrics :: StopPreferences -- ^ The stop preferences to use
  -> TravelPreferences -- ^ The travel preferences
  -> CaminoPreferences -- ^ The camino travelled
  -> TripChoiceMap Accommodation Service -- ^ The map of accommodation choices
  -> TripChoiceMap Location Service -- ^ The map of location choices
  -> Location -- ^ The location
  -> (Penance, Penance, [TripChoice Accommodation Service], Penance, S.Set Service, Penance) -- ^ The metrics associated with the stop (stop penance, location penanance, accommodation choice, accommodation penance, missing services, missing service penance)
locationMetrics sprefs tprefs cprefs accommodationMap locationMap stop =
  let
    camino = preferenceCamino cprefs
    stopMissing = missingStopServices (preferenceStop tprefs) camino stop
    accom = M.findWithDefault invalidAccommodation stop accommodationMap -- preferred accommodation
    stopMissing' = stopMissing `S.difference` tripChoiceFeatures accom
    stopMissingCost = missingServicePenance (stopServices sprefs) stopMissing'
    stopCost = stopPenance tprefs camino stop
    locationCost = locationPenance tprefs camino locationMap stop
  in
    (
        stopCost
      , locationCost
      , (if tripChoicePenance accom == Reject then [] else [accom])
      , (tripChoicePenance accom)
      , stopMissing'
      , stopMissingCost
    )

-- | Calculate metrics associated with requirements along the route
routeMetrics :: StopPreferences -- ^ The stop preferences to use
  -> TravelPreferences -- ^ The travel preferences
  -> CaminoPreferences -- ^ The camino travelled
  -> [TripChoice Accommodation Service] -- ^ The chosen accommodations
  -> [Leg] -- ^ The route
  -> (S.Set Service, Penance) -- ^ The metrics associated with the route (missing services, missing service penance)
routeMetrics sprefs _tprefs cprefs accoms day =
  let
    camino = preferenceCamino cprefs
    dayMissing = missingRouteServices sprefs camino accoms day
    dayMissingCost = missingServicePenance (stopServices sprefs) dayMissing
  in
    (
        dayMissing
      , dayMissingCost
    )

-- | Calculate the total penance implicit in a sequence of legs
penance :: StopPreferences -- ^ The stop preferences to use
  -> TravelPreferences -- ^ The travel preferences
  -> CaminoPreferences -- ^ The camino travelled
  -> TripChoiceMap Accommodation Service -- ^ The map of accommodation choices
  -> TripChoiceMap Location Service -- ^ The map of location choices
  -> [Leg] -- ^ The sequence of legs
  -> Metrics -- ^ The penance value
penance sprefs tprefs cprefs accommodationMap locationMap day =
  let
    stop = legTo $ last day
    (_normalSpeed, _actualSpeed, time, distance, perceived, ascent, descent, poi, nonTravel) = travelMetrics tprefs cprefs day
    distanceCost = maybe Reject Penance perceived
    -- If there is no accommodation within this leg, then accept any distance.
    atEnd = isLastDay (preferenceFinish cprefs) day
    walkingSpeed = nominalSpeed Walking Normal
    accommodationFree = isAccommodationFree tprefs accommodationMap day
    -- If not travelling or the last day, then skip lower bounds
    rangeFilter = (if accommodationFree then withoutMaximum else id) . (if atEnd then withoutMinimum else id) . (if nonTravel then withoutLower else id)
    timePreferences = rangeFilter $ preferenceTime tprefs
    distancePreferences = rangeFilter $ preferenceDistance tprefs
    timeAdjust = maybe Reject (adjustment timePreferences 0.0 walkingSpeed) time
    distanceAdjust = adjustment distancePreferences 0.0 walkingSpeed distance
    (stopCost, locationCost, accChoice, accCost, stopMissing, stopMissingCost) = locationMetrics sprefs tprefs cprefs accommodationMap locationMap stop
    (dayMissing, dayMissingCost) = routeMetrics sprefs tprefs cprefs accChoice day
    misc = travelAdditional tprefs day
    total = distanceCost <> locationCost <> accCost <> stopCost <> distanceAdjust <> timeAdjust <> stopMissingCost <> dayMissingCost <> misc
    metrics = Metrics
      distance
      time
      poi
      perceived
      ascent
      descent
      stopCost
      locationCost
      accChoice
      accCost
      stopMissing
      stopMissingCost
      dayMissing
      dayMissingCost
      distanceAdjust
      timeAdjust
      mempty
      mempty
      misc
      0
      Nothing
      False
      False
      total
  in
    -- trace ("From " ++ (T.unpack $ locationName $ legFrom $ head day) ++ " -> " ++ (T.unpack $ locationName $ legTo $ last day) ++ " = " ++ (show $ metricsPenance metrics) ++ ", " ++ show metrics ++ (if atEnd then  " [end-trip]" else "") ++ (if accommodationFree then  " [accommodation-free]" else "") ++ (if nonTravel then  " [non-travel]" else "")) metrics
    metrics

-- | Accept a leg for a specific travel type
legAccept :: TravelPreferences -> Leg -> Bool
legAccept tprefs leg = legAccept' (preferenceTravel tprefs) (legType leg)

legAccept' :: Travel -> LegType -> Bool
legAccept' Walking CyclePath = False
legAccept' Cycling Trail = False
legAccept' _ _ = True

-- | Accept a day's stage as a possibility
--   Acceptable if the time taken or distance travelled is not beyond the hard limits and all locations are part of the selected routes.
--   Or if there is no acceptable accommodation available.
dayAccept :: TravelPreferences -> CaminoPreferences -> TripChoices -> [Leg] -> Bool
dayAccept preferences camino choices day =
  let
    legs = all (legAccept preferences) day
    atEnd = isLastDay (preferenceFinish camino) day
    accommodationFree = isAccommodationFree preferences (tcStopAccommodation choices) day
    time = travelHours preferences day
    distance = travel preferences day
    inside = isJust time && isInsideMaximum (preferenceDistance preferences) distance && isInsideMaximum (preferenceTime preferences) (fromJust time)
  in
   -- trace ("Day from " ++ (T.unpack $ locationName $ legFrom $ head day) ++ " -> " ++ (T.unpack $ locationName $ legTo $ last day) ++ " accept=" ++ show (atEnd || inside || accommodationFree) ++ " distance= " ++ show distance ++ " time=" ++ show time ++ " end=" ++ show atEnd ++  " inside=" ++ show inside ++ " accommodation free=" ++ show accommodationFree) (atEnd || inside || accommodationFree)
   legs && (atEnd || inside || accommodationFree)


-- | Evaluate a day's stage for penance
--   Acceptable if the time taken or distance travelled is not beyond the hard limits
dayEvaluate :: TravelPreferences -> CaminoPreferences -> TripChoices -> [Leg] -> ([Leg], Metrics)
dayEvaluate preferences camino choices legs =
  (legs, penance (preferenceStop preferences) preferences camino (tcStopAccommodation choices) (tcStopLocation choices) legs)

-- | Choose a day's stage based on minimum penance
dayChoice :: TravelPreferences -> CaminoPreferences -> Day -> Day -> Day
dayChoice _preferences _camino day1 day2 = if score day1 < score day2 then day1 else day2

-- | Choose whether to accept an entire camino
--   Refuse any day that has a rejection in the intermediate points
caminoAccept :: TravelPreferences -> CaminoPreferences -> [Day] -> Bool
caminoAccept _preferences _camino days =
  let
    middle = if length days < 3 then [] else tailOrEmpty (initOrEmpty days)
  in
    null middle || all (\d -> (metricsPenance $ score d) /= Reject) middle

-- | Evaluate a complete camino 
--   Currently, the sum of all day scores
--   Refuse any camino that doesn't include all required stops and exclude all excluded stops
caminoEvaluate :: TravelPreferences -> CaminoPreferences -> [Day] -> ([Day], Metrics)
caminoEvaluate _preferences camino days =
  let
    final = finish $ last days
    waypoints = if final == (preferenceFinish camino) then preferenceStops camino else foldl (S.union) S.empty (map passed days)
    requiredOk = S.null (S.difference (preferenceStops camino `S.intersection` waypoints) (S.fromList $ map finish days))
    excludedOk = not $ any (\d -> S.member (finish d) (preferenceExcluded camino)) days
    total = mconcat $  map score days
    evaluation = if not requiredOk || not excludedOk then invalid else total
  in
    -- trace ("Evaluate " ++ (T.unpack $ daysLabel days) ++ " = " ++ (show $ metricsPenance evaluation) ++ " requiredOk=" ++ show requiredOk ++ " excludedOk=" ++ show excludedOk ++ " total=" ++ show total ) evaluation
    (days, evaluation)

-- | Choose a camino stage.
--   First check that one or the other has passed through a required stop.
--   Otherwise based on minimum penance
caminoChoice :: TravelPreferences -> CaminoPreferences -> Journey -> Journey -> Journey
caminoChoice _preferences camino trip1 trip2 =
  let
    req = preferenceStops camino
    via1 = req `S.intersection` (S.fromList $ map finish $ path trip1)
    via2 = req `S.intersection` (S.fromList $ map finish $ path trip2)
    c1 = S.size via1
    c2 = S.size via2
    score1 = score trip1
    score2 = score trip2
    len1 = length $ path trip1
    len2 = length $ path trip2
    selection = if c1 > c2 then
        trip1
      else if c2 > c1 then
        trip2
      else if score1 == score2 then (
        if len1 < len2 then
          trip1
        else
          trip2
      ) else if score1 < score2 then
        trip1
      else
        trip2
  in
    -- trace ("Choice between " ++ (T.unpack $ tripLabel trip1) ++ " and " ++ (T.unpack $ tripLabel trip2) ++ " is " ++ (T.unpack $ tripLabel selection)) selection
    selection

-- | Accept a stage as a possibility
--   Acceptable if the the length of the stage is below the maximum
stageAccept :: TravelPreferences -> CaminoPreferences -> [Day] ->  Bool
stageAccept preferences _camino stage =
  isInsideMaximum (preferenceRest preferences) (length stage)


-- | Evaluate a stage for penance
stageEvaluate :: TravelPreferences -> CaminoPreferences -> TripChoices -> [Day] -> ([Day], Metrics)
stageEvaluate tprefs cprefs choices stage = let
    camino' = preferenceCamino cprefs
    sprefs = preferenceStop tprefs
    last' = last stage
    total = mconcat $ map score stage
    stop = finish last'
    stageLength = length stage
    restPrefs = withoutMinimum $ preferenceRest tprefs
    fatigue = if isOutOfRange restPrefs stageLength then Reject else Penance ((rangeTarget $ preferenceDistance tprefs) * (rangeDistanceInt restPrefs stageLength))
    stopMissing = missingStopServices sprefs camino' stop
    accom = M.findWithDefault invalidAccommodation stop (tcStopAccommodation choices)
    stopMissing' = stopMissing `S.difference` tripChoiceFeatures accom
    stopMissingCost = missingServicePenance (stopServices sprefs) stopMissing'
    stopCost = stopPenance tprefs camino' stop
    locationCost = locationPenance tprefs camino' (tcStopLocation choices) stop
    stop' = stopMissingCost <> stopCost <> locationCost
    metrics = if stop == preferenceFinish cprefs then
        mempty { metricsFatigue = fatigue, metricsPenance = fatigue }
      else
        mempty { metricsFatigue = fatigue, metricsRest = stop', metricsPenance = fatigue <> stop' }
  in
    (stage, total <> metrics)

-- | Choose a day's stage based on minimum penance
stageChoice :: TravelPreferences -> CaminoPreferences -> Journey -> Journey -> Journey
stageChoice _preferences _camino stage1 stage2 = if score stage1 < score stage2 then stage1 else stage2

-- | Accept a pilgrimage as a possibility
--   Acceptable if the the length of all stages are within range
--   And there is multi-day accommodation at the end of each stage
pilgrimageAccept :: TravelPreferences -> CaminoPreferences -> [Journey] -> Bool
pilgrimageAccept preferences _camino pilgrimage =
    all (\j -> test (length $ path j)) pilgrimage
  where
    test = if length pilgrimage == 1 then isInsideMaximum (preferenceRest preferences) else (\stage -> not $ isOutOfRange (preferenceRest preferences) stage)

-- | Evaluate an entire pilgrimage for penance
pilgrimageEvaluate :: CaminoConfig -> TravelPreferences -> CaminoPreferences -> TripChoices -> [Journey] -> ([Journey], Metrics)
pilgrimageEvaluate cc tprefs cprefs choices pilgrimage = let
    startDate = maybe (error "No start date") id (preferenceStartDate cprefs)
    pilgrimage' = pilgrimageEvaluate' cc tprefs cprefs choices startDate pilgrimage
  in
    (pilgrimage', mconcat $ map score pilgrimage')


-- Recast stages to take advantage of better accomodation options
-- and account for any holidays/sundays
pilgrimageEvaluate' :: CaminoConfig -> TravelPreferences -> CaminoPreferences -> TripChoices -> C.Day -> [Journey] -> [Journey]
pilgrimageEvaluate' _cc _tprefs _cprefs _choices _current [] = []
pilgrimageEvaluate' cc tprefs cprefs choices current (stage:rest) =  let
    days' = pilgrimageEvaluate'' cc tprefs cprefs choices current (path stage)
    (days'', score') = stageEvaluate tprefs cprefs choices days'
    stage' = stage { path = days'', score = score' }
    (_, restDay) = fromJust $ metricsDate score'
  in
    stage':(pilgrimageEvaluate' cc tprefs cprefs choices (C.addDays 1 restDay) rest)

-- Fill out dates of arrival and adjust rest day and stock-up day preferences
pilgrimageEvaluate'' :: CaminoConfig -> TravelPreferences -> CaminoPreferences -> TripChoices -> C.Day -> [Day] -> [Day]
pilgrimageEvaluate'' _cc _tprefs _cprefs _choices _current [] = []
pilgrimageEvaluate'' _cc tprefs cprefs choices current [day] =
  let
    atEnd = finish day == preferenceFinish cprefs
    metrics' = if atEnd then score day else penance (preferenceRestStop tprefs) tprefs cprefs (tcRestAccommodation choices) (tcRestLocation choices) (path day)
    rests' = if atEnd then 0 else 1
    restDay = C.addDays rests' current
    day' = day {
      score = metrics' {
          metricsRestDays = fromInteger rests'
        , metricsDate = Just (current, restDay)
        , metricsRestpoint = True
      }
    }
  in
    [day']
pilgrimageEvaluate'' cc tprefs cprefs choices current days@(day:rest) =  let
    next = C.addDays 1 current
    metrics' = if isStockUpDay cc current days then let
        metrics'' = penance (preferenceStockStop tprefs) tprefs cprefs (tcStockAccommodation choices) (tcStockLocation choices) (path day)
      in
        metrics'' { metricsStockpoint = True }
     else
        score day
    day' = day { 
      score = metrics' { 
          metricsDate = Just (current, current)
      }
    }
  in
    day':(pilgrimageEvaluate'' cc tprefs cprefs choices next rest)

-- Is the next day a day where we need to stock up on this day
-- Need to check the region the next day is going to end up at
isStockUpDay :: CaminoConfig -> C.Day -> [Day] -> Bool
isStockUpDay _config _current [] = False
isStockUpDay _config _current [_now] = False
isStockUpDay config current (_now:day:_rest) = let -- Is next day on a non-service day?
    location = finish day
    mregion = locationRegion location
  in
    if isNothing mregion || locationAlwaysOpen location then
      False
    else let
        region = regionID $ fromJust mregion
        next = C.addDays 1 current
      in
        runReader (inCalendar (ClosedDay region) next) config || runReader (inCalendar (PublicHoliday region) next) config

-- | Choose a day's stage based on minimum penance
pilgrimageChoice :: TravelPreferences -> CaminoPreferences -> Pilgrimage -> Pilgrimage -> Pilgrimage
pilgrimageChoice _preferences _camino pilgrimage1 pilgrimage2 = if score pilgrimage1 < score pilgrimage2 then pilgrimage1 else pilgrimage2

rundownTail :: M.Map Location (TripChoiceTrail v f) -> Leg -> TripChoiceTail v f
rundownTail trail leg = let
    feed = maybe [] snd (M.lookup (legFrom leg) trail)
    distance = Penance (legDistance leg)
    feed' = filter (\(_c, p) -> p /= mempty) $ map (\(c, p) -> (c, subtractFloor p distance)) feed
  in
    feed'

buildTripChoiceMap'' :: (Ord f) => (Location -> TripChoice v f) -> TravelPreferences -> Camino -> M.Map Location (TripChoiceTrail v f) -> Location -> TripChoiceTrail v f
buildTripChoiceMap'' chooser _preferences camino trail location = let
    legs = incoming camino location
    feed = concat $ map (rundownTail trail) legs
    best = chooser location -- ^ The best local choice
  in
    if tripChoicePenance best == Reject then
      (best, feed)
    else let
        local = tripChoicePenance best
        feed' = (best, local):feed
        score' = maximum (map (\t -> subtractFloor (snd t) local) feed')
      in
        (best { tripChoicePenance = score' }, feed')


buildTripChoiceMap' :: (Ord f) => (Location -> TripChoice v f) -> TravelPreferences -> Camino -> S.Set Location -> S.Set Location -> S.Set Location -> M.Map Location (TripChoiceTrail v f) -> TripChoiceMap v f
buildTripChoiceMap' chooser preferences camino reachable' visited next current = if S.null next
  then
    M.map (\(c, _t) -> c) current
  else
    buildTripChoiceMap' chooser preferences camino reachable' visited' next' (current `M.union` update)
  where
    update = M.fromSet (buildTripChoiceMap'' chooser preferences camino current) next
    visited' = visited `S.union` M.keysSet update
    next' = available camino reachable' visited'

-- Build a forward map of accommodation choices and a backward map of choices and then choose the
-- entry with the highest penance (meaning that there's a better option either before or after this spot)
buildTripChoiceMap :: (Ord f) => (Location -> TripChoice v f) -> TravelPreferences -> Camino -> Location -> Location -> (Location -> Bool) -> TripChoiceMap v f
buildTripChoiceMap chooser preferences camino begin end select = let
    reachable' = reachable camino begin end select
    sgf = subgraph camino reachable'
    sgb = mirror sgf
    acf = buildTripChoiceMap' chooser preferences sgf reachable' S.empty (S.singleton begin) M.empty
    acb = buildTripChoiceMap' chooser preferences sgb reachable' S.empty (S.singleton end) M.empty
  in
    M.unionWith (\f -> \b -> if tripChoicePenance f > tripChoicePenance b then f else b) acf acb

-- | Plan a camino based on the stated preferences
planCamino :: CaminoConfig -> TravelPreferences -> CaminoPreferences -> Solution
planCamino cc tprefs cprefs  = Solution {
        solutionID = Nothing
      , solutionMetadata = Nothing
      , solutionTravelPreferences = tprefs
      , solutionCaminoPreferences = cprefs
      , solutionLocations = allowed
      , solutionJourneyFailure = either Just (const Nothing) journey
      , solutionPilgrimageFailure = either Just (const Nothing) pilgrimage
      , solutionPilgrimage = either (const Nothing) Just pilgrimage
  }
  where
    sprefs = preferenceStop tprefs
    stprefs = preferenceStockStop tprefs
    rprefs = preferenceRestStop tprefs
    camino = preferenceCamino cprefs
    start' = preferenceStart cprefs
    finish' = preferenceFinish cprefs
    allowed = allowedLocations cprefs
    select l = S.member l allowed
    stopAm = buildTripChoiceMap (accommodationChoice (const True) sprefs camino) tprefs camino start' finish' select
    stopLm = buildTripChoiceMap (locationChoice sprefs) tprefs camino start' finish' select
    stockAm = buildTripChoiceMap (accommodationChoice (const True) stprefs camino) tprefs camino start' finish' select
    stockLm = buildTripChoiceMap (locationChoice stprefs) tprefs camino start' finish' select
    restAm = buildTripChoiceMap (accommodationChoice accommodationMulti rprefs camino) tprefs camino start' finish' select
    restLm = buildTripChoiceMap (locationChoice rprefs) tprefs camino start' finish' select
    choices = TripChoices {
        tcStopAccommodation = stopAm
      , tcStopLocation = stopLm
      , tcStockAccommodation = stockAm
      , tcStockLocation = stockLm
      , tcRestAccommodation = restAm
      , tcRestLocation = restLm
    }
    journey = program
      camino
      (caminoChoice tprefs cprefs)
      (caminoAccept tprefs cprefs)
      (caminoEvaluate tprefs cprefs)
      (dayChoice tprefs cprefs)
      (dayAccept tprefs cprefs choices)
      (dayEvaluate tprefs cprefs choices)
      select
      (preferenceStart cprefs)
      (preferenceFinish cprefs)
    pilgrimage = either
      (\_f -> Left $ emptyFailure "Unable to build journey" Nothing)
      (\j -> program
        (fromChains (const mempty) (path j))
        (pilgrimageChoice tprefs cprefs)
        (pilgrimageAccept tprefs cprefs)
        (pilgrimageEvaluate cc tprefs cprefs choices)
        (stageChoice tprefs cprefs)
        (stageAccept tprefs cprefs)
        (stageEvaluate tprefs cprefs choices)
        select
        (start j)
        (finish j)
      ) journey


-- | Get all the rests (ie stage ends) on a trip in order
pilgrimageRests :: Pilgrimage -> [Location]
pilgrimageRests pilgrimage = foldr (\j -> \s -> (start j):s) [finish pilgrimage] (path pilgrimage)

-- | Get all the stock points (ie stops where you need to stock up for the next day) on a trip in order
pilgrimageStockpoints :: Pilgrimage -> [Location]
pilgrimageStockpoints pilgrimage = foldr (\j -> \s -> foldr (\d -> \s' -> (finish d):s') s (filter (metricsStockpoint . score) (path j))) [] (path pilgrimage)

-- | Get all the stops (ie start and finish locations) on a trip in order
pilgrimageStops :: Pilgrimage -> [Location]
pilgrimageStops pilgrimage = foldr (\j -> \s -> foldr (\d -> \s' -> (start d):s') s (path j)) [finish pilgrimage] (path pilgrimage)

-- | Get all the waypoints on a pilgrimage in order
pilgrimageWaypoints :: Pilgrimage -> [Location]
pilgrimageWaypoints pilgrimage = foldr (\j -> \s -> foldr (\d -> \s' -> foldr (\l -> \s'' -> (legTo l):s'') s' (path d)) s (path j)) [finish pilgrimage] (path pilgrimage)

-- | Get all the legs actually used by a trip
pilgrimageLegs :: Pilgrimage -> [Leg]
pilgrimageLegs pilgrimage = foldr (\j -> \s -> foldr (\d -> \s' -> path d ++ s') s (path j)) [] (path pilgrimage)

-- | Find the day that starts at a location
findDay :: Pilgrimage -> Location -> Maybe Day
findDay pilgrimage location = findDay' (path pilgrimage) location

findDay' [] _location = Nothing
findDay' (stage:rest) location = L.find (\d -> start d == location) (path stage) <|> findDay' rest location

-- | Find the stage that starts at a location
findStage :: Pilgrimage -> Location -> Maybe Journey
findStage pilgrimage location = findStage' (path pilgrimage) location

findStage' [] _location = Nothing
findStage' (stage:rest) location = if (start stage == location) then Just stage else findStage' rest location

-- | Useful label for a day sequence
daysLabel :: [Day] -> T.Text
daysLabel [] = ""
daysLabel days@(h:t) = T.concat [locationNameLabel $ start $ h, " -> ", locationNameLabel $ finish $ lastWithError days, " [", T.intercalate "," (map (locationNameLabel . start) t), "]" ]

-- | Useful label for trips
tripLabel :: Journey -> T.Text
tripLabel trip = T.concat [daysLabel $ path trip, " <", T.pack $ show $ metricsPenance $ score trip, ">"]
