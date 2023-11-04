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
  hours,
  penance,
  planCamino,
  travel,
  tripStops,
  tripWaypoints
) where

import Camino.Walking
import Camino.Camino
import Camino.Preferences
import Graph.Programming()
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (isJust, fromJust)

-- | The metrics for a day, segment or complete trip
data Metrics = Metrics {
    metricsDistance :: Float, -- ^ Actual distance in km
    metricsTime :: Maybe Float, -- ^ Time taken in hours
    metricsPerceivedDistance :: Maybe Float, -- ^ Perceived distance in km (Nothing if the distance is too long)
    metricsAscent :: Float, -- ^ Ascent in metres
    metricsDescent :: Float, -- ^ Descent in metres
    metricsAccommodation :: Penance, -- ^ Accommodation penance in km-equivalent. This represents the distance you would be prepared to walk to avoid this accommodation
    metricsStop :: Penance, -- ^ Stop penance in km-equivalent. The represents the costs of stopping for the night in food, urge to get on, whatever
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
    metricsAccommodation = metricsAccommodation m1 <> metricsAccommodation m2,
    metricsStop = metricsStop m1 <> metricsStop m2,
    metricsDistanceAdjust = metricsDistanceAdjust m1 <> metricsDistanceAdjust m2,
    metricsTimeAdjust = metricsTimeAdjust m1 <> metricsTimeAdjust m2,
    metricsMisc = metricsMisc m1 <> metricsMisc m2,
    metricsPenance = metricsPenance m1 <> metricsPenance m2
  }

instance Monoid Metrics where
  mempty = Metrics 0.0 (Just 0.0) (Just 0.0) 0.0 0.0 mempty mempty mempty mempty mempty mempty

instance Score Metrics where
  invalid = Metrics 0.0 (Just 0.0) (Just 0.0) 0.0 0.0 mempty mempty mempty mempty Reject Reject

-- | A day's stage
type Day = Chain Location Leg Metrics

-- | A complete camino
type Trip = Chain Location Day Metrics

walking' :: String -> (Float -> Float -> Float -> Float)
walking' n | n == "naismith" = naismith
  | n == "tobler" = tobler
  | otherwise = error ("Planner.walking': bad argument " ++ n)

-- | Calculate the expected hours of walking, for a sequence of legs
hours :: Preferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Maybe Float -- ^ The hours equivalent
hours preferences legSeq = let
    baseHours = walking' (preferenceWalkingFunction preferences)
    simple = sum $ map (\l -> baseHours (legDistance l) (legAscent l) (legDescent l)) legSeq
  in
    tranter (preferenceFitness preferences) simple

-- | Calculate the total distance covered by a sequence of legs
travel :: Preferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Float -- ^ The total distance covered by the sequence
travel _preferences legSeq = sum $ map legDistance legSeq

-- | Calculate the total ascent of a sequence of legs
totalAscent :: Preferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Float -- ^ The total distance covered by the sequence
totalAscent _preferences legSeq = sum $ map legAscent legSeq

-- | Calculate the total descent of a sequence of legs
totalDescent :: Preferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Float -- ^ The total distance covered by the sequence
totalDescent _preferences legSeq = sum $ map legDescent legSeq

-- | Calculate the travel metrics for a seqnece of legs
travelMetrics :: Preferences -> [Leg] -> (Float, Float, Maybe Float, Float, Maybe Float, Float, Float)
travelMetrics preferences legSeq =
  let
    normalSpeed = nominalSpeed Normal
    actualSpeed = nominalSpeed $ preferenceFitness preferences
    time = hours preferences legSeq
    distance = travel preferences legSeq
    ascent = totalAscent preferences legSeq
    descent = totalDescent preferences legSeq
    perceived = fmap (normalSpeed *) time
  in
    (normalSpeed, actualSpeed, time, distance, perceived, ascent, descent)

-- | Default sleeping in the open option
openSleeping :: Accommodation
openSleeping = GenericAccommodation Camping

accommodation' :: Preferences -- ^ The calculation preferences
  -> Location -- ^ The location to stay at
  -> Penance -- ^ Nothing if nothing is found, otherwise the penance value for the accommodation
accommodation' preferences location =
  let
    ap = preferenceAccommodation preferences
    accommodationOptions = if null ao then [openSleeping] else ao where ao = locationAccommodation location
    lp = map (\a -> M.findWithDefault Reject (accommodationType a) ap) accommodationOptions
    up = filter (Reject /=) lp
  in
    if null up then Reject else minimum up

-- | Calculate the accommodation penance, based on preferences
accommodation :: Preferences -- ^ The calculation preferences
  -> Camino -- ^ The camino model
  -> [Leg] -- ^ The sequence of legs to use
  -> Penance -- ^ Either nothing for no suitable accommodation or the resulting penance
accommodation preferences _camino legSeq = accommodation' preferences stop
  where
    stop = legTo $ last legSeq


adjustment :: PreferenceRange Float -> Float -> Float -> Penance
adjustment range scale value
  | isOutOfRange range value = Reject
  | isOutOfBounds range value = Penance (scale * rangeDistance range value)
  | otherwise = mempty

-- | Calculate the total penance implicit in a sequence of legs
penance :: Preferences -- ^ The user preferences
  -> Camino -- ^ The camino travelled
  -> [Leg] -- ^ The sequence of legs
  -> Metrics -- ^ The penance value
penance preferences camino legSeq =
  let
    (normalSpeed, actualSpeed, time, distance, perceived, ascent, descent) = travelMetrics preferences legSeq
    timeAdjust = maybe Reject (adjustment (preferenceTime preferences) normalSpeed) time
    distanceAdjust = maybe Reject (adjustment (preferencePerceivedDistance preferences) normalSpeed) perceived
    accommodationAdjust = accommodation preferences camino legSeq -- accommodation penance
    dayCost = Penance actualSpeed -- One hour of travel not used
    distanceCost = maybe Reject Penance perceived
    total = distanceCost <> accommodationAdjust <> dayCost <> distanceAdjust <> timeAdjust
  in
    -- trace ("From " ++ (T.unpack $ locationName $ legFrom $ head legSeq) ++ " -> " ++ (T.unpack $ locationName $ legTo $ last legSeq) ++ " = " ++ show totalPenance) totalPenance
    Metrics distance time perceived ascent descent accommodationAdjust dayCost distanceAdjust timeAdjust mempty total

-- | Accept a day's stage as a possibility
--   Acceptable if the time taken or distance travelled is not beyond the hard limits
dayAccept :: Preferences -> Camino -> [Leg] -> Bool
dayAccept preferences _camino lseq =
  let
    time = hours preferences lseq
    distance = travel preferences lseq
    inside = isJust time && isInsideMaximum (preferenceDistance preferences) distance && isInsideMaximum (preferenceTime preferences) (fromJust time)
  in
   -- trace ("From " ++ (T.unpack $ locationName $ legFrom $ head lseq) ++ " -> " ++ (T.unpack $ locationName $ legTo $ last lseq) ++ " distance= " ++ show distance ++ " time=" ++ show time ++ " inside=" ++ show inside) inside
   inside


-- | Evaluate a day's stage for penance
--   Acceptable if the time taken or distance travelled is not beyond the hard limits
dayEvaluate :: Preferences -> Camino -> [Leg] -> Metrics
dayEvaluate = penance

-- | Choose a day's stage based on minimum penance
dayChoice :: Preferences -> Camino -> Day -> Day -> Day
dayChoice _preferences _camino day1 day2 = if score day1 < score day2 then day1 else day2

-- | Choose whether to accept an entire camino
--   Require no excluded stops
caminoAccept :: Preferences -> Camino -> [Day] -> Bool
caminoAccept _preferences _camino _days = True

-- | Evaluate a complete camino 
--   Currently, the sum of all day scores
--   Refuse any camino that doesn't include all required stops and exclude all excluded stops
caminoEvaluate :: Preferences -> Camino -> Location -> [Day] -> Metrics
caminoEvaluate preferences _camino end days =
  let
    final = finish $ last days
    waypoints = if final == end then preferenceRequired preferences else foldl (S.union) S.empty (map passed days)
    requiredOk = S.null (S.difference (preferenceRequired preferences `S.intersection` waypoints) (S.fromList $ map finish days))
    excludedOk = not $ any (\d -> S.member (finish d) (preferenceExcluded preferences)) days
    total = mconcat $  map score days
  in
    if not requiredOk || not excludedOk then invalid else total

-- | Choose a camino stage.
--   First check that one or the other has passed through a required stop.
--   Otherwise based on minimum penance
caminoChoice :: Preferences -> Camino -> Trip -> Trip -> Trip
caminoChoice preferences _camino trip1 trip2 = 
  let 
    req = preferenceRequired preferences
    via1 = req `S.intersection` (S.fromList $ map finish $ path trip1)
    via2 = req `S.intersection` (S.fromList $ map finish $ path trip2)
    c1 = S.size via1
    c2 = S.size via2
  in
    if c1 > c2 then trip1
    else if c2 > c1 then trip2
    else if score trip1 < score trip2 then trip1 else trip2

-- | Plan a camino based on the stated preferences
planCamino :: Preferences -> Camino -> Location -> Location -> Maybe Trip
planCamino preferences camino begin end = 
  program 
    camino
    (caminoChoice preferences camino) 
    (caminoAccept preferences camino) 
    (caminoEvaluate preferences camino end)
    (dayChoice preferences camino) 
    (dayAccept preferences camino) 
    (dayEvaluate preferences camino)
    begin
    end

-- | Get all the stops (ie start and finish locations) on a trip in order
tripStops :: Trip -> [Location]
tripStops trip = (start trip) : (map finish $ path trip)

-- | Get all the waypoints on a trip in order
tripWaypoints :: Trip -> [Location]
tripWaypoints trip = foldr (\c -> \w -> w ++ map legTo (path c)) [start trip] (path trip)