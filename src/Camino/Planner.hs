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
  Day,
  Metrics(..),
  Trip,

  accommodation,
  daysLabel,
  hasNonWalking,
  nonWalkingHours,
  penance,
  openSleeping,
  planCamino,
  travel,
  tripLabel,
  tripLegs,
  tripStops,
  tripWaypoints,
  walkingHours
) where

import Camino.Walking
import Camino.Camino
import Camino.Preferences
import Graph.Programming()
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe, mapMaybe, maybeToList)
import qualified Data.Text as T
-- import Debug.Trace

-- | The metrics for a day, segment or complete trip
data Metrics = Metrics {
    metricsDistance :: Float, -- ^ Actual distance in km
    metricsTime :: Maybe Float, -- ^ Time taken in hours
    metricsPerceivedDistance :: Maybe Float, -- ^ Perceived distance in km (Nothing if the distance is too long)
    metricsAscent :: Float, -- ^ Ascent in metres
    metricsDescent :: Float, -- ^ Descent in metres
    metricsAccommodationChoice :: [Accommodation], -- ^ The places to stay on the way
    metricsAccommodation :: Penance, -- ^ Accommodation penance in km-equivalent. This represents the distance you would be prepared to walk to avoid this accommodation
    metricsStop :: Penance, -- ^ Stop penance in km-equivalent. The represents the costs of stopping for the night in food, urge to get on, whatever
    metricsStopServices :: Penance, -- ^ Adjustment to penance in km-equivalnet caused by missing services at the stop point
    metricsDayServices :: Penance, -- ^ Adjustment to penance in km-equivalnet caused by missing services throughout the day
    metricsDistanceAdjust :: Penance, -- ^ Adjustments to distance cost in km-equivalent caused by going over/under target
    metricsTimeAdjust :: Penance, -- ^ Adjustments to time cost in km-equivalent caused by going over/under target
    metricsMisc :: Penance, -- ^ Additional miscellaneous penance costs and causes
    metricsPenance :: Penance -- ^ Total penance for this leg of the metrics
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
    metricsStopServices = metricsStopServices m1 <> metricsStopServices m2,
    metricsDayServices = metricsDayServices m1 <> metricsDayServices m2,
    metricsDistanceAdjust = metricsDistanceAdjust m1 <> metricsDistanceAdjust m2,
    metricsTimeAdjust = metricsTimeAdjust m1 <> metricsTimeAdjust m2,
    metricsMisc = metricsMisc m1 <> metricsMisc m2,
    metricsPenance = metricsPenance m1 <> metricsPenance m2
  }

instance Monoid Metrics where
  mempty = Metrics 0.0 (Just 0.0) (Just 0.0) 0.0 0.0 [] mempty mempty mempty mempty mempty mempty mempty mempty

instance Score Metrics where
  invalid = Metrics 0.0 (Just 0.0) (Just 0.0) 0.0 0.0 [] mempty mempty mempty mempty mempty mempty Reject Reject

-- | A day's stage
type Day = Chain Location Leg Metrics

-- | A complete camino
type Trip = Chain Location Day Metrics

-- Is this the last day's walking?
isLastDay :: Location -> [Leg] -> Bool
isLastDay end day = (legTo $ last day) == end

-- | Get the locations associated with a day
dayLocations :: [Leg] -> [Location]
dayLocations day = (legFrom $ head day) : (map legTo day)

travelFunction :: Travel -> (Float -> Float -> Float -> Float)
travelFunction Walking = tobler
travelFunction Walking_Naismith  = naismith
travelFunction Cycling = error "Cycling has not yet been implemented"

-- | Calculate the expected hours of walking, for a sequence of legs
walkingHours :: TravelPreferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Maybe Float -- ^ The hours equivalent
walkingHours preferences day = let
    baseHours = travelFunction (preferenceTravelFunction preferences)
    simple = sum $ map (\l -> baseHours (legDistance l) (legAscent l) (legDescent l)) day
  in
    tranter (preferenceFitness preferences) simple

-- | Calculate the expected non-walking hours, for a sequence of legs
--   Usually associated with something like a ferry
nonWalkingHours :: TravelPreferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Maybe Float -- ^ The hours equivalent
nonWalkingHours _preferences day =
  Just $ sum $ map (\l -> fromMaybe 0.0 (legTime l)) day

-- | Does this sequence of legs have a non-walking component?
hasNonWalking ::  TravelPreferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Bool -- ^ The hours equivalent
hasNonWalking _preferences day =
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
    normalSpeed = nominalSpeed Normal
    actualSpeed = nominalSpeed $ preferenceFitness preferences
    walkingTime = walkingHours preferences day
    otherTime = nonWalkingHours preferences day
    distance = travel preferences day
    ascent = totalAscent preferences day
    descent = totalDescent preferences day
    perceived = fmap (normalSpeed *) walkingTime
    nonWalking = hasNonWalking preferences day
  in
    (normalSpeed, actualSpeed, (+) <$> walkingTime <*> otherTime, distance, perceived, ascent, descent, nonWalking)

-- | Work out what services are missing from the desired stop list
missingStopServices :: TravelPreferences -- ^ The calculation preferences
  -> Camino -- ^ The camino model
  -> [Leg] -- ^ The sequence of legs to use
  -> S.Set Service -- ^ The list of desired but missing services from the stop location
missingStopServices preferences _camino day = (M.keysSet $ preferenceStopServices preferences) `S.difference` (locationServices $ legTo $ last day)

-- | Work out what services are missing from the desired day list
missingDayServices :: TravelPreferences -- ^ The calculation preferences
  -> Camino -- ^ The camino model
  -> Maybe Accommodation -- ^ The chosen accomodation
  -> [Leg] -- ^ The sequence of legs to use
  -> S.Set Service -- ^ The list of desired but missing services from the stop location
missingDayServices preferences _camino accom day = let
    desired = M.keysSet $ preferenceDayServices preferences
    desired' = desired `S.difference` (maybe S.empty accommodationServices accom)
  in
    foldl (S.difference) desired' $ map locationServices (dayLocations day)

-- | Default sleeping in the open option
openSleeping :: Accommodation
openSleeping = GenericAccommodation Camping

accommodation'' :: (M.Map AccommodationType Penance) -> (M.Map Service Penance) -> S.Set Service -> Accommodation -> (Maybe Accommodation, S.Set Service, Penance)
accommodation'' accPrefs servPrefs services accom = let
    base = M.findWithDefault Reject (accommodationType accom) accPrefs
    missing = services `S.difference` (accommodationServices accom)
    missingPref s = M.findWithDefault mempty s servPrefs
    mprefs = map missingPref (S.toList missing)
  in
    (Just accom, missing, foldl (<>) base mprefs)
    
accommodation' :: TravelPreferences -- ^ The calculation preferences
  -> S.Set Service -- ^ Any services not provided by the location
  -> Location -- ^ The location to stay at
  -> (Maybe Accommodation, S.Set Service, Penance) -- ^ Nothing if nothing is found, otherwise the penance value for the accommodation
accommodation' preferences services location =
  let
    ap = preferenceAccommodation preferences
    sp = preferenceStopServices preferences
    accommodationOptions = if null ao then [openSleeping] else ao where ao = locationAccommodation location
    lp = map (accommodation'' ap sp services) accommodationOptions
    up = filter (\(_, _, p) -> p /= Reject) lp
  in
    foldl (\t1@(_, _, p1) -> \t2@(_, _, p2) -> if p1 < p2 then t1 else t2) (Nothing, services, Reject) up

-- | Calculate the accommodation penance, based on preferences
--   If there are missing services at the stop, then the suggested accommodation will be tested to see 
--   whether it offers the required services
accommodation :: TravelPreferences -- ^ The calculation preferences
  -> Camino -- ^ The camino model
  -> [Leg] -- ^ The sequence of legs to use
  -> S.Set Service -- ^ The services that, ideally, should be provided by the accomodation
  -> Bool -- ^ Don't worry about missing accommodation
  -> (Maybe Accommodation, S.Set Service, Penance) -- ^ Either nothing for no suitable accommodation or the resulting penance
accommodation preferences _camino day services allowNone = 
  let
    acc@(accom, missing, _penance) = accommodation' preferences services (legTo $ last day)
  in
    if isNothing accom && allowNone then
      (Nothing, missing, Penance 10.0)
    else
      acc


-- Does this leg have no acceptable accommodation (except possibly at the start and end)
isAccomodationFree :: TravelPreferences -> [Leg] -> Bool
isAccomodationFree preferences day = all (\l -> let (acc, _, _) = accommodation' preferences S.empty (legFrom l) in isNothing acc) (tail day)

missingServicePenance :: M.Map Service Penance -> S.Set Service -> Penance
missingServicePenance prefs services = mconcat $ map (\s -> M.findWithDefault mempty s prefs) $ S.toList services

boundsAdjustement = 2.0

adjustment :: PreferenceRange Float -> Float -> Float -> Float -> Penance
adjustment range bscale rscale value
  | isOutOfRange range value = Reject
  | isOutOfBounds range value = Penance (rscale * rangeDistance range value) <> Penance boundsAdjustement
  | otherwise = if bd < 0.25 then mempty else Penance (bscale * bd) where bd = boundsDistance range value -- Allow "close enough" to the target

-- | Calculate the total penance implicit in a sequence of legs
penance :: TravelPreferences -- ^ The travel preferences
  -> CaminoPreferences -- ^ The camino travelled
  -> [Leg] -- ^ The sequence of legs
  -> Metrics -- ^ The penance value
penance preferences camino day =
  let
    (normalSpeed, actualSpeed, time, distance, perceived, ascent, descent, nonWalking) = travelMetrics preferences day
    -- If there is no accomodation within this leg, then accept any distance. If not walking or the last day, then skip lower bounds
    atEnd = isLastDay (preferenceFinish camino) day
    accomodationFree = isAccomodationFree preferences day
    rangeFilter = (if accomodationFree then withoutMaximum else id) . (if atEnd then withoutMinimum else id) . (if nonWalking then withoutLower else id)
    timePreferences = rangeFilter $ preferenceTime preferences
    distancePreferences = rangeFilter $ preferencePerceivedDistance preferences
    timeAdjust = maybe Reject (adjustment timePreferences 0.0 normalSpeed) time
    distanceAdjust = maybe Reject (adjustment distancePreferences boundsAdjustement normalSpeed) perceived
    stopMissing = missingStopServices preferences (preferenceCamino camino) day
    (accom, stopMissing', accommodationAdjust) = accommodation preferences (preferenceCamino camino) day stopMissing atEnd -- preferred accommodation penance
    stopMissingCost = missingServicePenance (preferenceStopServices preferences) stopMissing'
    dayMissing = missingDayServices preferences (preferenceCamino camino) accom day
    dayMissingCost = missingServicePenance (preferenceStopServices preferences) dayMissing
    stopCost = preferenceStop preferences
    distanceCost = maybe Reject Penance perceived
    misc = travelAdditional preferences day
    total = distanceCost <> accommodationAdjust <> stopCost <> distanceAdjust <> timeAdjust <> stopMissingCost <> dayMissingCost <> misc
    metrics = Metrics
      distance
      time
      perceived
      ascent
      descent
      (maybeToList accom)
      accommodationAdjust
      stopCost
      stopMissingCost
      dayMissingCost
      distanceAdjust
      timeAdjust
      misc
      total
  in
    -- trace ("From " ++ (T.unpack $ locationName $ legFrom $ head day) ++ " -> " ++ (T.unpack $ locationName $ legTo $ last day) ++ " = " ++ (show $ metricsPenance metrics) ++ ", " ++ show metrics ++ (if atEnd then  " [end-trip]" else "") ++ (if accomodationFree then  " [accomodation-free]" else "") ++ (if nonWalking then  " [non-walking]" else "")) metrics
    metrics

-- | Accept a day's stage as a possibility
--   Acceptable if the time taken or distance travelled is not beyond the hard limits and allo locations are part of the selected routes.
--   Or if there is no acceptable accommodation available.
dayAccept :: TravelPreferences -> CaminoPreferences -> [Leg] -> Bool
dayAccept preferences camino day =
  let
    atEnd = isLastDay (preferenceFinish camino) day
    accommodationFree = isAccomodationFree preferences day
    time = walkingHours preferences day
    distance = travel preferences day
    inside = isJust time && isInsideMaximum (preferenceDistance preferences) distance && isInsideMaximum (preferenceTime preferences) (fromJust time)
  in
   -- trace ("Day from " ++ (T.unpack $ locationName $ legFrom $ head day) ++ " -> " ++ (T.unpack $ locationName $ legTo $ last day) ++ " accept=" ++ show (atEnd || inside || accommodationFree) ++ " distance= " ++ show distance ++ " time=" ++ show time ++ " end=" ++ show atEnd ++  " inside=" ++ show inside ++ " accomodation free=" ++ show accommodationFree) (atEnd || inside || accommodationFree)
   atEnd || inside || accommodationFree


-- | Evaluate a day's stage for penance
--   Acceptable if the time taken or distance travelled is not beyond the hard limits
dayEvaluate :: TravelPreferences -> CaminoPreferences -> [Leg] -> Metrics
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
caminoEvaluate preferences camino days =
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

-- | Plan a camino based on the stated preferences
planCamino :: TravelPreferences -> CaminoPreferences -> Either Location Trip
planCamino preferences camino  = 
  program 
    (preferenceCamino camino)
    (caminoChoice preferences camino)
    (caminoAccept preferences camino)
    (caminoEvaluate preferences camino)
    (dayChoice preferences camino)
    (dayAccept preferences camino)
    (dayEvaluate preferences camino)
    (\l -> S.member l allowed)
    (preferenceStart camino)
    (preferenceFinish camino)
  where
    allowed = allowedLocations camino

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
