{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Planner
Description : The camino planner
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Create an optimal camino plan, based on the peferences
-}

module Camino.Planner where

import Camino.Walking
import Camino.Camino
import Graph.Programming
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.List (intersperse)
import Formatting
import Debug.Trace (trace)

-- | A day's stage
type Day = Chain Location Leg Penance

-- | A complete camino
type Trip = Chain Location Day Penance

walking' :: String -> (Float -> Float -> Float -> Float)
walking' n | n == "naismith" = naismith
  | n == "tobler" = tobler
  | otherwise = error ("Planner.walking': bad argument " ++ n)

-- | Calculate the expected hours of walking, for a sequence of legs
hours :: Preferences -- ^ The calculation preferences
  -> [Leg] -- ^ The sequence of legs to use
  -> Float -- ^ The hours equivalent
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

-- | Calculate the travel metrics for a seqnece of legs
travelMetrics :: Preferences -> [Leg] -> (Float, Float, Float, Float, Float)
travelMetrics preferences legSeq =
  let
    normalSpeed = nominalSpeed Normal
    actualSpeed = nominalSpeed $ preferenceFitness preferences
    time = hours preferences legSeq
    distance = travel preferences legSeq
    perceivedDistance = normalSpeed * time
  in
    (normalSpeed, actualSpeed, time, distance, perceivedDistance)

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

-- | Default sleeping in the open option
openSleeping :: Accommodation
openSleeping = GenericAccommodation Camping

accommodation' :: Preferences -- ^ The calculation preferences
  -> Location -- ^ The location to stay at
  -> Penance -- ^ Nothing if nothing is found, otherwise the penance value for the accomodation
accommodation' preferences location =
  let
    ap = preferenceAccommodation preferences
    accommodationOptions = if null ao then [openSleeping] else ao where ao = locationAccommodation location
    lp = map (\a -> M.findWithDefault Reject (accommodationType a) ap) accommodationOptions
    up = filter (Reject /=) lp
  in
    if null up then Reject else minimum up

-- | Calculate the accomodation penance, based on preferences
accommodation :: Preferences -- ^ The calculation preferences
  -> Camino -- ^ The camino model
  -> [Leg] -- ^ The sequence of legs to use
  -> Penance -- ^ Either nothing for no suitable accomodation or the
accommodation preferences _camino legSeq = accommodation' preferences stop
  where
    stop = legTo $ last legSeq


adjustment range scale value
  | isOutOfRange range value = Reject
  | isOutOfBounds range value = SimplePenance (scale * rangeDistance range value)
  | otherwise = mempty

-- | Calculate the total penance implicit in a sequence of legs
penance :: Preferences -- ^ The user preferences
  -> Camino -- ^ The camino travelled
  -> [Leg] -- ^ The sequence of legs
  -> Penance -- ^ The penance value
penance preferences camino legSeq =
  let
    (normalSpeed, actualSpeed, time, _distance, perceivedDistance) = travelMetrics preferences legSeq
    timeAdjust = adjustment (preferenceTime preferences) normalSpeed time
    distanceAdjust = adjustment (preferenceDistance preferences) 1.0 perceivedDistance
    accomodationAdjust = accommodation preferences camino legSeq -- accomodation penance
    dayCost = (SimplePenance actualSpeed) -- One hour of travel not used
    totalPenance =  (LabeledPenance "perceivedDistance" (SimplePenance perceivedDistance))
      <> (LabeledPenance "timeAdjustment" timeAdjust)
      <> (LabeledPenance "distanceAdjust" distanceAdjust)
      <> accomodationAdjust
      <> (LabeledPenance "dayCost" dayCost)
  in
    -- trace ("From " ++ (T.unpack $ locationName $ legFrom $ head legSeq) ++ " -> " ++ (T.unpack $ locationName $ legTo $ last legSeq) ++ " = " ++ show totalPenance) totalPenance
    totalPenance

-- | Accept a day's stage as a possibility
--   Acceptable if the time taken or distance travelled is not beyond the hard limits
dayAccept :: Preferences -> Camino -> [Leg] -> Bool
dayAccept preferences _camino lseq =
  let
    time = hours preferences lseq
    distance = travel preferences lseq
    inside = isInsideMaximum (preferenceDistance preferences) distance && isInsideMaximum (preferenceTime preferences) time
  in
   -- trace ("From " ++ (T.unpack $ locationName $ legFrom $ head lseq) ++ " -> " ++ (T.unpack $ locationName $ legTo $ last lseq) ++ " distance= " ++ show distance ++ " time=" ++ show time ++ " inside=" ++ show inside) inside
   inside


-- | Evaluate a day's stage for penance
--   Acceptable if the time taken or distance travelled is not beyond the hard limits
dayEvaluate :: Preferences -> Camino -> [Leg] -> Penance
dayEvaluate = penance

-- | Choose a day's stage based on minimum penance
dayChoice :: Preferences -> Camino -> Day -> Day -> Day
dayChoice _preferences _camino day1 day2 = if score day1 < score day2 then day1 else day2

-- | Choose whether to accept an entire camino
--   Require no exlcuded stops
caminoAccept :: Preferences -> Camino -> [Day] -> Bool
caminoAccept _preferences _camino _days = True

-- | Evaluate a complete camino 
--   Currently, the sum of all day scores
--   Refuse any camino that doesn't include all required stops and exclude all excluded stops
caminoEvaluate :: Preferences -> Camino -> Location -> [Day] -> Penance
caminoEvaluate preferences _camino end days =
  let
    final = finish $ last days
    waypoints = if final == end then preferenceRequired preferences else foldl (S.union) S.empty (map passed days)
    requiredOk = S.null (S.difference (preferenceRequired preferences `S.intersection` waypoints) (S.fromList $ map finish days))
    excludedOk = not $ any (\d -> S.member (finish d) (preferenceExcluded preferences)) days
    total = mconcat $  map score days
  in
    if not requiredOk || not excludedOk then Reject else total

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

-- | Print a penance for a trip
showPenance :: Penance -> LT.Text
showPenance Reject = "reject"
showPenance (SimplePenance value) = format (fixed 1) value
showPenance (LabeledPenance label value) = format (text % "=" % text) (LT.pack label) (showPenance value)
showPenance (CompoundPenance components value) = format (text % " (" % text % ")") (showPenance value) desc where
   desc = (LT.concat $ intersperse ", " (map (\(l, v) -> LT.pack (l ++ "=" ++ (LT.unpack $ showPenance v))) (M.assocs components)))

showDay :: Preferences -> Camino -> Int -> Day -> LT.Text
showDay preferences _camino dn (Chain _begin end links pnce) =
  dayFormat dn chain distance perceivedDistance time ascent' descent' (showPenance pnce)
  where
    (_normalSpeed, _actualSpeed, time, distance, perceivedDistance) = travelMetrics preferences links
    ascent' = totalAscent preferences links
    descent' = totalDescent preferences links
    locs = (map legFrom links) ++ [end]
    chain = LT.concat $ intersperse " -> " (map (LT.fromStrict . locationName) locs)
    dayFormat = format (
        "  Day " % int % " "
        % text % " "
        % (fixed 1) % "km (feels like " % (fixed 1) % "km) "
        % (fixed 1) % "hr "
        % (fixed 0) % "m ascent "
        % (fixed 0) % "m descent "
        % "with penance " % text % "\n"
      )

-- | Print a plan in comprehenisble form
showTrip :: Preferences -> Camino -> Maybe Trip -> LT.Text
showTrip _preferences _camino Nothing = "Unable to find solution"
showTrip preferences camino (Just (Chain begin end days pnce)) =
  LT.concat ([header] ++ (zipWith (\i -> \d -> showDay preferences camino i d) [0..] days))
  where
    headerFormat = format ("From " % text % " to " % text % " with penance " % text % "\n")
    header = headerFormat (LT.fromStrict $ locationName begin) (LT.fromStrict $ locationName end) (showPenance pnce)


-- | Get all the stops (ie start and finish locations) on a trip in order
tripStops :: Trip -> [Location]
tripStops trip = (start trip) : (map finish $ path trip)

-- | Get all the waypoints on a trip in order
tripWaypoints :: Trip -> [Location]
tripWaypoints trip = foldr (\c -> \w -> w ++ map legTo (path c)) [start trip] (path trip)