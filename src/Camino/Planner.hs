{-# LANGUAGE OverloadedStrings #-}
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
    AccommodationChoice(..)
  , AccommodationMap
  , Day
  , Metrics(..)
  , Trip

  , accommodation
  , buildAccommodationMap
  , daysLabel
  , hasNonTravel
  , nonTravelHours
  , penance
  , openSleeping
  , planCamino
  , travel
  , travelHours
  , tripLabel
  , tripLegs
  , tripStops
  , tripWaypoints
) where

import Camino.Walking
import Camino.Camino
import Camino.Preferences
import Graph.Graph (available, incoming, mirror, reachable, subgraph)
import Graph.Programming()
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (isJust, fromJust, fromMaybe, mapMaybe)
import qualified Data.Text as T

data AccommodationChoice = AccommodationChoice {
      accommodationChoice :: Accommodation -- ^ The accommodation selected
    , accommodationChoiceServices :: S.Set Service -- ^ The additional services supplied by this accommodation
    , accommodationChoicePenance :: Penance -- ^ The penance associated with this choice
} deriving (Show)

-- | Select accommodation based on penance / services covered / services available
selectAccommodationChoice :: AccommodationChoice -> AccommodationChoice -> AccommodationChoice
selectAccommodationChoice c1@(AccommodationChoice _ s1 Reject) c2@(AccommodationChoice _ s2 Reject) =
  if S.size s1 > S.size s2 then c1 else c2
selectAccommodationChoice (AccommodationChoice _ _ Reject) c2 = c2
selectAccommodationChoice c1 (AccommodationChoice _ _ Reject) = c1
selectAccommodationChoice c1@(AccommodationChoice a1 s1 p1) c2@(AccommodationChoice a2 s2 p2)
  | p1 > p2 = c1
  | p2 > p1 = c2
  | S.size s1 > S.size s2 = c1
  | S.size s2 > S.size s1 = c2
  | S.size (accommodationServices a1) > S.size (accommodationServices a2) = c1
  | S.size (accommodationServices a2) > S.size (accommodationServices a1) = c2
  | otherwise = c1

-- A map of accommodation choices for each location
type AccommodationMap = M.Map Location AccommodationChoice
-- | The trail of past/future accommodation options
type AccommodationTail = [(AccommodationChoice, Penance)]
-- | The tail of past/future accommodation options
type AccommodationTrail = (AccommodationChoice, AccommodationTail)

-- | The metrics for a day, segment or complete trip
data Metrics = Metrics {
      metricsDistance :: Float -- ^ Actual distance in km
    , metricsTime :: Maybe Float -- ^ Time taken in hours
    , metricsPerceivedDistance :: Maybe Float -- ^ Perceived distance in km (Nothing if the distance is too long)
    , metricsAscent :: Float -- ^ Ascent in metres
    , metricsDescent :: Float -- ^ Descent in metres
    , metricsAccommodationChoice :: [AccommodationChoice] -- ^ The places to stay on the way
    , metricsAccommodation :: Penance -- ^ Accommodation penance in km-equivalent. This represents the distance you would be prepared to walk to avoid this accommodation
    , metricsStop :: Penance -- ^ Stop penance in km-equivalent. The represents the costs of stopping for the night in food urge to get on whatever
    , metricsMissingStopServices :: S.Set Service -- ^ The services missing at the end of the day
    , metricsStopServices :: Penance -- ^ Adjustment to penance in km-equivalnet caused by missing services at the stop point
    , metricsMissingDayServices :: S.Set Service -- ^ The services missing at the end of the day
    , metricsDayServices :: Penance -- ^ Adjustment to penance in km-equivalnet caused by missing services throughout the day
    , metricsDistanceAdjust :: Penance -- ^ Adjustments to distance cost in km-equivalent caused by going over/under target
    , metricsTimeAdjust :: Penance -- ^ Adjustments to time cost in km-equivalent caused by going over/under target
    , metricsMisc :: Penance -- ^ Additional miscellaneous penance costs and causes
    , metricsPenance :: Penance -- ^ Total penance for this leg of the metrics
  } deriving (Show)

instance Eq Metrics where
  m1 == m2 = metricsPenance m1 == metricsPenance m2
  
instance Ord Metrics where
  compare m1 m2 = compare (metricsPenance m1) (metricsPenance m2)
  
instance Semigroup Metrics where
  m1 <> m2 = Metrics {
    metricsDistance = metricsDistance m1 + metricsDistance m2,
    metricsTime = (+) <$> metricsTime m1 <*> metricsTime m2,
    metricsPerceivedDistance = (+) <$> metricsPerceivedDistance m1 <*> metricsPerceivedDistance m2,
    metricsAscent = metricsAscent m1 + metricsAscent m2,
    metricsDescent = metricsDescent m1 + metricsDescent m2,
    metricsAccommodationChoice = metricsAccommodationChoice m1 <> metricsAccommodationChoice m2,
    metricsAccommodation = metricsAccommodation m1 <> metricsAccommodation m2,
    metricsStop = metricsStop m1 <> metricsStop m2,
    metricsMissingStopServices = metricsMissingStopServices m1 `S.union` metricsMissingStopServices m2,
    metricsStopServices = metricsStopServices m1 <> metricsStopServices m2,
    metricsMissingDayServices = metricsMissingDayServices m1 `S.union` metricsMissingDayServices m2,
    metricsDayServices = metricsDayServices m1 <> metricsDayServices m2,
    metricsDistanceAdjust = metricsDistanceAdjust m1 <> metricsDistanceAdjust m2,
    metricsTimeAdjust = metricsTimeAdjust m1 <> metricsTimeAdjust m2,
    metricsMisc = metricsMisc m1 <> metricsMisc m2,
    metricsPenance = metricsPenance m1 <> metricsPenance m2
  }

instance Monoid Metrics where
  mempty = Metrics 0.0 (Just 0.0) (Just 0.0) 0.0 0.0 [] mempty mempty S.empty mempty S.empty mempty mempty mempty mempty mempty

instance Score Metrics where
  invalid = Metrics 0.0 (Just 0.0) (Just 0.0) 0.0 0.0 [] mempty mempty S.empty mempty S.empty mempty mempty mempty Reject Reject

-- | A day's stage
type Day = Chain Location Leg Metrics

-- | A complete camino and accommodation map
type Trip = Chain Location Day Metrics

-- Is this the last day's travel?
isLastDay :: Location -> [Leg] -> Bool
isLastDay end day = (legTo $ last day) == end

-- | Get the locations associated with a day
dayLocations :: [Leg] -> [Location]
dayLocations day = (legFrom $ head day) : (map legTo day)

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

-- | Calculate the travel metrics for a seqnece of legs
travelMetrics :: TravelPreferences -> [Leg] -> (Float, Float, Maybe Float, Float, Maybe Float, Float, Float, Bool)
travelMetrics preferences day =
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
    nonTravel = hasNonTravel preferences day
  in
    (normalSpeed, actualSpeed, (+) <$> travelTime <*> otherTime, distance, perceived, ascent, descent, nonTravel)

-- | Work out what services are missing from the desired stop list
missingStopServices :: TravelPreferences -- ^ The calculation preferences
  -> Camino -- ^ The camino model
  -> [Leg] -- ^ The sequence of legs to use
  -> S.Set Service -- ^ The list of desired but missing services from the stop location
missingStopServices preferences _camino day = (M.keysSet $ preferenceStopServices preferences) `S.difference` (locationServices $ legTo $ last day)

-- | Work out what services are missing from the desired day list
missingDayServices :: TravelPreferences -- ^ The calculation preferences
  -> Camino -- ^ The camino model
  -> AccommodationChoice -- ^ The chosen accommodation
  -> [Leg] -- ^ The sequence of legs to use
  -> S.Set Service -- ^ The list of desired but missing services from the stop location
missingDayServices preferences _camino accom day = let
    desired = M.keysSet $ preferenceDayServices preferences
    desired' = desired `S.difference` accommodationChoiceServices accom
  in
    foldl (S.difference) desired' $ map locationServices (dayLocations day)

-- | Default sleeping in the open option
openSleeping :: Accommodation
openSleeping = GenericAccommodation Camping

-- | Invalid accommodation choice
invalidAccommodation :: AccommodationChoice
invalidAccommodation = AccommodationChoice openSleeping S.empty Reject

-- Select the most favourable
accommodation' :: (M.Map AccommodationType Penance) -> S.Set Service -> Accommodation -> AccommodationChoice
accommodation' accPrefs services accom = let
    base = M.findWithDefault Reject (accommodationType accom) accPrefs
    covered = services `S.intersection` (accommodationServices accom)
  in
    AccommodationChoice accom covered base
    
accommodation :: TravelPreferences -- ^ The calculation preferences
  -> Location -- ^ The sequence of legs
  -> AccommodationChoice -- ^ The chosen accommodation for the location
accommodation preferences location = let
    services = (M.keysSet $ M.filter (/= mempty) (preferenceStopServices preferences)) `S.difference` locationServices location
    ap = preferenceAccommodation preferences
    accommodationOptions = if null ao then [openSleeping] else ao where ao = locationAccommodation location
    lp = map (accommodation' ap services) accommodationOptions
    choice = foldl selectAccommodationChoice invalidAccommodation lp
  in
    choice

-- Does this leg have no acceptable accommodation (except possibly at the start and end)
isAccommodationFree :: TravelPreferences -> AccommodationMap -> [Leg] -> Bool
isAccommodationFree _preferences accommodationMap day = all (\l -> (accommodationChoicePenance $ M.findWithDefault invalidAccommodation (legFrom l) accommodationMap) == Reject) (tail day)

missingServicePenance :: M.Map Service Penance -> S.Set Service -> Penance
missingServicePenance prefs services = mconcat $ map (\s -> M.findWithDefault mempty s prefs) $ S.toList services

adjustment :: PreferenceRange Float -> Float -> Float -> Float -> Penance
adjustment range bscale rscale value
  | isOutOfRange range value = Reject
  | isOutOfBounds range value = Penance (rscale * rangeDistance range value)
  | otherwise = Penance (bscale * boundsDistance range value)

-- | Calculate the total penance implicit in a sequence of legs
penance :: TravelPreferences -- ^ The travel preferences
  -> CaminoPreferences -- ^ The camino travelled
  -> AccommodationMap -- ^ The map of accommodation choices
  -> [Leg] -- ^ The sequence of legs
  -> Metrics -- ^ The penance value
penance preferences camino accommodationMap day =
  let
    (_normalSpeed, _actualSpeed, time, distance, perceived, ascent, descent, nonTravel) = travelMetrics preferences day
    -- If there is no accommodation within this leg, then accept any distance. If not travelling or the last day, then skip lower bounds
    atEnd = isLastDay (preferenceFinish camino) day
    walkingSpeed = nominalSpeed Walking Normal
    accommodationFree = isAccommodationFree preferences accommodationMap day
    rangeFilter = (if accommodationFree then withoutMaximum else id) . (if atEnd then withoutMinimum else id) . (if nonTravel then withoutLower else id)
    timePreferences = rangeFilter $ preferenceTime preferences
    distancePreferences = rangeFilter $ preferenceDistance preferences
    timeAdjust = maybe Reject (adjustment timePreferences 0.0 walkingSpeed) time
    distanceAdjust = adjustment distancePreferences 0.0 walkingSpeed distance
    stopMissing = missingStopServices preferences (preferenceCamino camino) day
    accom = M.findWithDefault invalidAccommodation (legTo $ last day) accommodationMap -- preferred accommodation penance
    stopMissing' = stopMissing `S.difference` accommodationChoiceServices accom
    stopMissingCost = missingServicePenance (preferenceStopServices preferences) stopMissing'
    dayMissing = missingDayServices preferences (preferenceCamino camino) accom day
    dayMissingCost = missingServicePenance (preferenceStopServices preferences) dayMissing
    stopCost = preferenceStop preferences
    distanceCost = maybe Reject Penance perceived
    misc = travelAdditional preferences day
    total = distanceCost <> (accommodationChoicePenance accom) <> stopCost <> distanceAdjust <> timeAdjust <> stopMissingCost <> dayMissingCost <> misc
    metrics = Metrics
      distance
      time
      perceived
      ascent
      descent
      (if accommodationChoicePenance accom == Reject then [] else [accom])
      (accommodationChoicePenance accom)
      stopCost
      stopMissing'
      stopMissingCost
      dayMissing
      dayMissingCost
      distanceAdjust
      timeAdjust
      misc
      total
  in
    -- trace ("From " ++ (T.unpack $ locationName $ legFrom $ head day) ++ " -> " ++ (T.unpack $ locationName $ legTo $ last day) ++ " = " ++ (show $ metricsPenance metrics) ++ ", " ++ show metrics ++ (if atEnd then  " [end-trip]" else "") ++ (if accommodationFree then  " [accommodation-free]" else "") ++ (if nonTravel then  " [non-travel]" else "")) metrics
    metrics

-- | Accept a day's stage as a possibility
--   Acceptable if the time taken or distance travelled is not beyond the hard limits and all locations are part of the selected routes.
--   Or if there is no acceptable accommodation available.
dayAccept :: TravelPreferences -> CaminoPreferences -> AccommodationMap -> [Leg] -> Bool
dayAccept preferences camino accommodationMap day =
  let
    atEnd = isLastDay (preferenceFinish camino) day
    accommodationFree = isAccommodationFree preferences accommodationMap day
    time = travelHours preferences day
    distance = travel preferences day
    inside = isJust time && isInsideMaximum (preferenceDistance preferences) distance && isInsideMaximum (preferenceTime preferences) (fromJust time)
  in
   -- trace ("Day from " ++ (T.unpack $ locationName $ legFrom $ head day) ++ " -> " ++ (T.unpack $ locationName $ legTo $ last day) ++ " accept=" ++ show (atEnd || inside || accommodationFree) ++ " distance= " ++ show distance ++ " time=" ++ show time ++ " end=" ++ show atEnd ++  " inside=" ++ show inside ++ " accommodation free=" ++ show accommodationFree) (atEnd || inside || accommodationFree)
   atEnd || inside || accommodationFree


-- | Evaluate a day's stage for penance
--   Acceptable if the time taken or distance travelled is not beyond the hard limits
dayEvaluate :: TravelPreferences -> CaminoPreferences -> AccommodationMap -> [Leg] -> Metrics
dayEvaluate = penance

-- | Choose a day's stage based on minimum penance
dayChoice :: TravelPreferences -> CaminoPreferences -> Day -> Day -> Day
dayChoice _preferences _camino day1 day2 = if score day1 < score day2 then day1 else day2

-- | Choose whether to accept an entire camino
--   Refuse any day that has a rejection in the intermediate points
caminoAccept :: TravelPreferences -> CaminoPreferences -> [Day] -> Bool
caminoAccept _preferences _camino days = 
  let
    middle = if length days < 3 then [] else tail (init days)
  in
    null middle || all (\d -> (metricsPenance $ score d) /= Reject) middle

-- | Evaluate a complete camino 
--   Currently, the sum of all day scores
--   Refuse any camino that doesn't include all required stops and exclude all excluded stops
caminoEvaluate :: TravelPreferences -> CaminoPreferences -> [Day] -> Metrics
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
    evaluation

-- | Choose a camino stage.
--   First check that one or the other has passed through a required stop.
--   Otherwise based on minimum penance
caminoChoice :: TravelPreferences -> CaminoPreferences -> Trip -> Trip -> Trip
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

rundownTail :: M.Map Location AccommodationTrail -> Leg -> AccommodationTail
rundownTail trail leg = let
    feed = maybe [] snd (M.lookup (legFrom leg) trail)
    distance = Penance (legDistance leg)
    feed' = filter (\(_c, p) -> p /= mempty) $ map (\(c, p) -> (c, subtractFloor p distance)) feed
  in
    feed'

buildAccommodationMap'' :: TravelPreferences -> Camino -> M.Map Location AccommodationTrail -> Location -> AccommodationTrail
buildAccommodationMap'' preferences camino trail location = let
    legs = incoming camino location
    feed = concat $ map (rundownTail trail) legs
    best = accommodation preferences location -- ^ The best local accommodation
  in
    if accommodationChoicePenance best == Reject
    then
      (best, feed)
    else let
        local = accommodationChoicePenance best
        feed' = (best, local):feed
        score' = maximum (map (\t -> subtractFloor (snd t) local) feed')
      in
        (best { accommodationChoicePenance = score' }, feed')


buildAccommodationMap' :: TravelPreferences -> Camino -> S.Set Location -> S.Set Location -> S.Set Location -> M.Map Location AccommodationTrail -> AccommodationMap
buildAccommodationMap' preferences camino reachable' visited next current = if S.null next
  then
    M.map (\(c, _t) -> c) current
  else
    buildAccommodationMap' preferences camino reachable' visited' next' (current `M.union` update)
  where
    update = M.fromSet (buildAccommodationMap'' preferences camino current) next
    visited' = visited `S.union` M.keysSet update
    next' = available camino reachable' visited'

-- Build a forward map of accommodation choices and a backward map of choices and then choose the
-- entry with the highest penance (meaning that there's a better option either before or after this spot)
buildAccommodationMap :: TravelPreferences -> Camino -> Location -> Location -> (Location -> Bool) -> AccommodationMap
buildAccommodationMap preferences camino begin end select = let
    reachable' = reachable camino begin end select
    sgf = subgraph camino reachable'
    sgb = mirror sgf
    acf = buildAccommodationMap' preferences sgf reachable' S.empty (S.singleton begin) M.empty
    acb = buildAccommodationMap' preferences sgb reachable' S.empty (S.singleton end) M.empty
  in
    M.unionWith (\f -> \b -> if accommodationChoicePenance f > accommodationChoicePenance b then f else b) acf acb

-- | Plan a camino based on the stated preferences
planCamino :: TravelPreferences -> CaminoPreferences -> Either Location (Trip, AccommodationMap)
planCamino preferences camino  = either Left (\t -> Right (t, accommodationMap)) trip
  where
    allowed = allowedLocations camino
    select l = S.member l allowed
    accommodationMap = buildAccommodationMap preferences (preferenceCamino camino) (preferenceStart camino) (preferenceFinish camino) select
    trip = program
      (preferenceCamino camino)
      (caminoChoice preferences camino)
      (caminoAccept preferences camino)
      (caminoEvaluate preferences camino)
      (dayChoice preferences camino)
      (dayAccept preferences camino accommodationMap)
      (dayEvaluate preferences camino accommodationMap)
      select
      (preferenceStart camino)
      (preferenceFinish camino)

-- | Get all the stops (ie start and finish locations) on a trip in order
tripStops :: Trip -> [Location]
tripStops trip = (start trip) : (map finish $ path trip)

-- | Get all the waypoints on a trip in order
tripWaypoints :: Trip -> [Location]
tripWaypoints trip = foldr (\c -> \w -> w ++ map legTo (path c)) [start trip] (path trip)

-- | Get all the legs actually used by a trip
tripLegs :: Trip -> [Leg]
tripLegs trip = foldr (\c -> \w -> w ++ path c) [] (path trip)

-- | Useful label for a day sequence
daysLabel :: [Day] -> T.Text
daysLabel days = T.concat [locationName $ start $ head days, " -> ", locationName $ finish $ last days, " [", T.intercalate "," (map (locationName . start) (tail $ days)), "]" ]

-- | Useful label for trips
tripLabel :: Trip -> T.Text
tripLabel trip = T.concat [daysLabel $ path trip, " <", T.pack $ show $ metricsPenance $ score trip, ">"]
