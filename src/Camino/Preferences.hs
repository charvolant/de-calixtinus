{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Camino
Description : Preference models for deciding where the limits lie
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Preferences contain the ranges of acceptable distance, time etc. for someone walking the camino.
-}
module Camino.Preferences (
    CaminoPreferences(..)
  , PreferenceRange(..)
  , TravelPreferences(..)

  , allowedLocations
  , defaultCaminoPreferences
  , defaultTravelPreferences
  , isInsideMaximum
  , isOutOfBounds
  , isOutOfRange
  , normalisePreferences
  , perceivedDistanceRange
  , reachableLocations
  , recommendedStops
  , rangeDistance
  , selectedRoutes
  , suggestedAccomodation
  , suggestedDayServices
  , suggestedDistanceRange
  , suggestedFinishes
  , suggestedStarts
  , suggestedStopServices
  , suggestedTimeRange
  , validRange
  , withoutLower
  , withoutMaximum
  , withoutMinimum
  , withRoutes
) where

import Data.Aeson
import Data.List (find)
import Data.Text (Text)
import Camino.Camino
import Camino.Util
import Camino.Walking
import qualified Data.Map as M (Map, (!), fromList)
import qualified Data.Set as S (Set, difference, empty, fromList, insert, intersection, map, member, singleton, union, unions)
import Graph.Graph (successors, predecessors)
-- import Debug.Trace

-- | Acceptable range boundaries for various parameters.
-- 
--   Ranges have a target value, /preferred/ lower and upper bound and a /hard/ minimum and maximum.
--   Generally, values should stay within the lower and upper bounds, as close to the target as possible.
--   In unusual circumstances, the value may slop outside the the bounds but cannot go beyond the
--   minimum and maximum values
data PreferenceRange a = PreferenceRange {
  rangeDerived :: Maybe Text, -- ^ Set if this range has been derived from another source
  rangeTarget :: a, -- ^ The preferred target for a range
  rangeLower :: a, -- ^ The preferred lower bound for a range
  rangeUpper :: a, -- ^ The preferred upper bound for a range
  rangeMinimum :: Maybe a, -- ^ The /hard/ lower bound for a range
  rangeMaximum :: Maybe a -- ^ The /hard/ upper bound for a range
} deriving (Show)

instance (FromJSON a) => FromJSON (PreferenceRange a) where
  parseJSON (Object v) = do
    derived' <- v .:? "derived" .!= Nothing
    target' <- v .: "target"
    lower' <- v .: "lower"
    upper' <- v .: "upper"
    minimum' <- v .:? "min" .!= Nothing
    maximum' <- v .:? "max" .!= Nothing
    return PreferenceRange { 
      rangeDerived = derived', 
      rangeTarget = target', 
      rangeLower = lower', 
      rangeUpper = upper', 
      rangeMinimum = minimum', 
      rangeMaximum = maximum'
    }
  parseJSON v = error ("Unable to parse preference range object " ++ show v)

instance (ToJSON a) => ToJSON (PreferenceRange a) where
  toJSON (PreferenceRange derived targ low up mini maxi) =
    object [ "derived" .= derived, "target" .= targ, "lower" .= low, "upper" .= up, "min" .= mini, "max" .= maxi]

-- | Check to see if a range is valid, meaning that the minimum - lower - target - upper - maximum values are in order
validRange :: (Ord a) => 
  PreferenceRange a -- ^ The preference change to check
  -> Bool -- ^ True if the range is in order, false otherwise
validRange pr =
  (maybe True (<= rangeLower pr) (rangeMinimum pr)) &&
  (rangeLower pr <= rangeTarget pr) &&
  (rangeTarget pr <= rangeUpper pr) &&
  (maybe True (>= rangeUpper pr) (rangeMaximum pr))


-- | Create a preference range without a lower bound
withoutLower :: (Num a) => PreferenceRange a -- ^ The source preference
  -> PreferenceRange a -- ^ The same preference without a lower bound
withoutLower (PreferenceRange derived target _lower upper _mini maxi) =
  PreferenceRange derived target 0 upper Nothing maxi

-- | Create a preference range without a minimum
withoutMinimum :: PreferenceRange a -- ^ The source preference
  -> PreferenceRange a -- ^ The same preference without a minimum
withoutMinimum (PreferenceRange derived target lower upper _mini maxi) =
  PreferenceRange derived target lower upper Nothing maxi

-- | Create a preference range without a maximum
withoutMaximum :: PreferenceRange a -- ^ The source preference
  -> PreferenceRange a -- ^ The same preference without an upper bound
withoutMaximum (PreferenceRange derived target lower upper mini _maxi) =
  PreferenceRange derived target lower upper mini Nothing

-- | Is a value at or below the absolute maximum in the preference range?
isInsideMaximum :: (Ord a) => PreferenceRange a -- ^ The preference range
  -> a -- ^ The value to test
  -> Bool -- ^ True if we haven't exceeded the maximum
isInsideMaximum (PreferenceRange _derived _target _lower _upper _mini maxi) value =
  maybe True (value <=) maxi

-- | Is a value outside the preference range?
isOutOfRange :: (Ord a) => PreferenceRange a -- ^ The preference range
  -> a -- ^ The value to test
  -> Bool -- ^ True if out of range
isOutOfRange (PreferenceRange _derived _target _lower _upper mini maxi) value =
  maybe False (value <) mini || maybe False (value >) maxi

-- | Is a value outside the preference bounds?
isOutOfBounds :: (Ord a) => PreferenceRange a -- ^ The preference range
  -> a -- ^ The value to test
  -> Bool -- ^ True if out of bounds
isOutOfBounds (PreferenceRange _dervived _target lower upper _minimum _maximum) value =
  value < lower || value > upper

-- | Get the normalised distance to the outer range of a value
-- 
-- The scale of the range is from the target to the lower or upper bounds, with 0 being at the bound and 1 being a scale away
rangeDistance :: (Ord a, Fractional a) => PreferenceRange a -> a -> a
rangeDistance (PreferenceRange _derived targ lower upper _mini _maxi) value
  | value < targ = ((targ - value) / (targ - lower)) - 1
  | otherwise = ((value - targ) / (upper - targ)) - 1
  

-- | Convert a normal distance range into a perceived distance range      
perceivedDistanceRange :: Fitness -> PreferenceRange Float -> PreferenceRange Float
perceivedDistanceRange fitness range = let
    mini' = fmap (\v -> perceivedDistance fitness v False) (rangeMinimum range)
    lower' = perceivedDistance fitness (rangeLower range) False
    target' = perceivedDistance fitness (rangeTarget range) True  
    upper' = perceivedDistance fitness (rangeUpper range) True
    maxi' = fmap (\v -> perceivedDistance fitness v True) (rangeMaximum range)
  in
    PreferenceRange (Just "Derived from distance preferences") target' lower' upper' mini' maxi'
    
-- | Preferences for hwo far, how long and what sort of comforts one might expect.
--   These can be reasonably expected to remain constant across different caminos   
data TravelPreferences = TravelPreferences {
    preferenceTravelFunction :: Travel -- ^ The name of the base walking function
  , preferenceFitness :: Fitness -- ^ The base fitness level
  , preferenceDistance :: PreferenceRange Float -- ^ The preferred distance range
  , preferenceTime :: PreferenceRange Float -- ^ The preferred time walking range
  , preferencePerceivedDistance :: PreferenceRange Float -- ^ The preferred distance range; if nothing then built from the distance range
  , preferenceStop :: Penance -- ^ The amount of penance associated with stopping for a day (larger will tend to increase legs, smaller the opposite)
  , preferenceAccommodation :: M.Map AccommodationType Penance -- ^ Accommodation preferences (absence implies unacceptable accommodation)
  , preferenceStopServices :: M.Map Service Penance -- ^ Desired services at a stop (absence implies zero desire)
  , preferenceDayServices :: M.Map Service Penance -- ^ Desired services during a day (absence implies zero desire)
} deriving (Show)

instance FromJSON TravelPreferences where
  parseJSON (Object v) = do
    travel' <- v .: "travel"
    fitness' <- v .: "fitness"
    distance' <- v .: "distance"
    time' <- v .: "time"
    perceived' <- v .:? "perceived" .!= perceivedDistanceRange fitness' distance'
    stop' <- v .: "stop"
    accommodation' <- v .: "accommodation"
    sstop' <- v .: "services-stop"
    sday' <- v .: "services-day"
    return TravelPreferences {
          preferenceTravelFunction = travel'
        , preferenceFitness = fitness'
        , preferenceDistance = distance'
        , preferenceTime = time'
        , preferencePerceivedDistance = perceived'
        , preferenceAccommodation = accommodation'
        , preferenceStop = stop'
        , preferenceStopServices = sstop'
        , preferenceDayServices = sday'
      }
  parseJSON v = error ("Unable to parse preferences object " ++ show v)

instance ToJSON TravelPreferences where
  toJSON (TravelPreferences travel' fitness' distance' time' perceived' stop' accommodation' sstop' sday') =
    object [ "travel" .= travel', "fitness" .= fitness', "distance" .= distance', "time" .= time', "perceived" .= perceived', "stop" .= stop', "accommodation" .= accommodation', "services-stop" .= sstop', "services-day" .= sday']

    
-- | Preferences for where to go and where to stop on a camino  
data CaminoPreferences = CaminoPreferences {
    preferenceCamino :: Camino -- ^ The camino route to walk
  , preferenceStart :: Location -- ^ The start location
  , preferenceFinish :: Location -- ^ The finish location
  , preferenceRoutes :: S.Set Route -- ^ The routes to use
  , preferenceStops :: S.Set Location -- ^ Locations that we must visit (end a day at)
  , preferenceExcluded :: S.Set Location -- ^ Locations that we will not visit (end a day at, although passing through is OK)
} deriving (Show)

instance FromJSON CaminoPreferences where
  parseJSON (Object v) = do
    camino' <- v .: "camino"
    start' <- v .: "start"
    finish' <- v .: "finish"
    routes' <- v .:? "routes" .!= S.empty
    stops' <- v .:? "stops" .!= S.empty
    excluded' <- v .:? "excluded" .!= S.empty
    let camino'' = placeholderCamino camino'
    let start'' = placeholderLocation start'
    let finish'' = placeholderLocation finish'
    let routes'' = S.map placeholderRoute routes'
    let stops'' = S.map placeholderLocation stops'
    let excluded'' = S.map placeholderLocation excluded'
    return CaminoPreferences {
        preferenceCamino = camino''
      , preferenceStart = start''
      , preferenceFinish = finish''
      , preferenceRoutes = routes''
      ,  preferenceStops = stops''
      ,  preferenceExcluded = excluded''
      }
  parseJSON v = error ("Unable to parse preferences object " ++ show v)

instance ToJSON CaminoPreferences where
  toJSON (CaminoPreferences camino' start' finish' routes' stops' excluded') =
    let
      routes'' = S.map routeID routes'
      stops'' = S.map locationID stops'
      excluded'' = S.map locationID excluded'
    in
      object [ "camino" .= caminoId camino', "start" .= locationID start', "finish" .= locationID finish', "routes" .= routes'', "stops" .= stops'', "excluded" .= excluded'']

-- | Normalise preferences to the correct locations and routes, based on placeholders
normalisePreferences :: [Camino] -- ^ A list of possible caminos
  -> CaminoPreferences -- ^ The preferences with placeholders
  -> CaminoPreferences -- ^ The preferences with locations updated
normalisePreferences caminos preferences =
  let
    caminoId' = caminoId $ preferenceCamino preferences
    camino = maybe (error ("Can't find camino with ID " ++ caminoId')) id (find (\c -> caminoId c == caminoId') caminos)
    locs = caminoLocations camino
    routes = M.fromList $ map (\r -> (routeID r, r)) (caminoRoutes camino)
  in
    preferences {
        preferenceCamino = camino
      , preferenceStart = locs M.! (locationID $ preferenceStart preferences)
      , preferenceFinish = locs M.! (locationID $ preferenceFinish preferences)
      , preferenceRoutes = S.map (\r -> routes M.! (routeID r)) (preferenceRoutes preferences)
      , preferenceStops = S.map (\l -> locs M.! (locationID l)) (preferenceStops preferences)
      , preferenceExcluded = S.map (\l -> locs M.! (locationID l)) (preferenceExcluded preferences)
    }

-- | Update with a new set of routes and, if necessary, start/finish/stops etc normalised
withRoutes :: CaminoPreferences -> S.Set Route -> CaminoPreferences
withRoutes preferences routes = let
    camino' = preferenceCamino preferences
    routes' = normaliseRoutes camino' routes
    prefs' = preferences { preferenceRoutes = routes' }
    allowed = caminoRouteLocations (preferenceCamino preferences) routes'
    start' = if S.member (preferenceStart prefs') allowed then preferenceStart prefs' else head $ suggestedStarts prefs'
    finish' = if S.member (preferenceFinish prefs') allowed then preferenceFinish prefs' else head $ suggestedFinishes prefs'
    stops' = preferenceStops prefs' `S.intersection` allowed
    excluded' = preferenceExcluded prefs' `S.intersection` allowed
  in
    CaminoPreferences camino' start' finish' routes' stops' excluded'

-- | The list of routes selected, in camino order
selectedRoutes :: CaminoPreferences -- ^ The preference set
  -> [Route] -- ^ The selected routes, including the default route in route order
selectedRoutes preferences = selectFromList routes (caminoRoutes camino)
  where 
    camino = preferenceCamino preferences
    routes = (S.singleton $ caminoDefaultRoute camino) `S.union` (preferenceRoutes preferences) 
    
-- | Work out what locations are acceptable in a camino, based on the chosen routes in the preferences.
allowedLocations :: CaminoPreferences -- ^ The preferences (normalised, see `normalisePreferences`)
  -> S.Set Location -- ^ The allowed locations
allowedLocations preferences = caminoRouteLocations (preferenceCamino preferences) (preferenceRoutes preferences)

-- | Work out what locations are reachable for a set of preferences
reachableLocations :: CaminoPreferences -> S.Set Location
reachableLocations preferences = let
    camino = preferenceCamino preferences
    start = preferenceStart preferences
    finish = preferenceFinish preferences
  in 
    S.insert start $ S.insert finish $ (successors camino start) `S.intersection` (predecessors camino finish) `S.intersection` (allowedLocations preferences)
    
-- | Generate a set of recommended stops, based on the selected routes
recommendedStops :: CaminoPreferences -- ^ The preferences (normalised, see `normalisePreferences`)
  -> S.Set Location -- ^ The allowed locations
recommendedStops preferences =
  let
    camino = preferenceCamino preferences
    routes = S.insert (caminoDefaultRoute camino) (preferenceRoutes preferences)
    baseStops = S.unions (S.map routeStops routes)
  in
    baseStops `S.intersection` reachableLocations preferences

-- | Create a suggested range for distances, based on the travel mode and fitness level.
--   Derived from estimated time limits plus some sanity.
--   Note that "Normal" is a little rufty-tufty for many people and allows legs of up to 34km to cover some of the more challenging stages
--   Cycling distances for the super-fit are vaguely based on Tour de France stages
suggestedDistanceRange :: Travel -- ^ The method of travel
  -> Fitness -- ^ The fitness level
  -> PreferenceRange Float -- ^ The suggested distance ranges
suggestedDistanceRange Walking SuperFit = PreferenceRange Nothing 30.0 26.0 36.0 (Just 8.0) (Just 44.0)
suggestedDistanceRange Walking VeryFit = PreferenceRange Nothing 28.0 24.0 32.0 (Just 8.0) (Just 40.0)
suggestedDistanceRange Walking Fit = PreferenceRange Nothing 24.0 20.0 30.0 (Just 8.0) (Just 36.0)
suggestedDistanceRange Walking Normal = PreferenceRange Nothing 20.0 18.0 26.0 (Just 8.0) (Just 34.0)
suggestedDistanceRange Walking Unfit = PreferenceRange Nothing 20.0 16.0 24.0 (Just 8.0) (Just 28.0)
suggestedDistanceRange Walking VeryUnfit = PreferenceRange Nothing 12.0 6.0 16.0 (Just 3.0) (Just 18.0)
suggestedDistanceRange Walking_Naismith f = suggestedDistanceRange Walking f
suggestedDistanceRange Cycling SuperFit = PreferenceRange Nothing 150.0 100.0 175.0 (Just 20.0) (Just 225.0)
suggestedDistanceRange Cycling VeryFit = PreferenceRange Nothing 120.0 80.0 150.0 (Just 20.0) (Just 175)
suggestedDistanceRange Cycling Fit = PreferenceRange Nothing 100.0 70.0 130.0 (Just 20.0) (Just 150.0)
suggestedDistanceRange Cycling Normal = PreferenceRange Nothing 80.0 50.0 100.0 (Just 20.0) (Just 120.0)
suggestedDistanceRange Cycling Unfit = PreferenceRange Nothing 50.0 30.0 80.0 (Just 20.0) (Just 100.0)
suggestedDistanceRange Cycling VeryUnfit = PreferenceRange Nothing 30.0 20.0 40.0 (Just 10.0) (Just 50.0)

-- | Create a suggested penance map for accommodation type, based on travel type and fitness level
suggestedAccomodation :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> M.Map AccommodationType Penance -- ^ The suggested accomodation map
suggestedAccomodation _ SuperFit = M.fromList [
    (MunicipalAlbergue, Penance 0.0),
    (PrivateAlbergue, Penance 1.0),
    (Hostel, Penance 1.0),
    (GuestHouse, Penance 2.0),
    (HomeStay, Penance 2.0),
    (House, Penance 3.0),
    (Hotel, Penance 5.0),
    (CampGround, Penance 2.0),
    (Camping, Penance 5.0)
  ]
suggestedAccomodation t VeryFit = suggestedAccomodation t SuperFit
suggestedAccomodation _ Fit = M.fromList [
    (MunicipalAlbergue, Penance 0.0),
    (PrivateAlbergue, Penance 0.5),
    (Hostel, Penance 0.5),
    (GuestHouse, Penance 1.0),
    (HomeStay, Penance 1.0),
    (House, Penance 1.0),
    (Hotel, Penance 2.0),
    (CampGround, Penance 3.0),
    (Camping, Reject)
  ]
suggestedAccomodation t Normal = suggestedAccomodation t Fit
suggestedAccomodation t Unfit = suggestedAccomodation t Fit
suggestedAccomodation t VeryUnfit = suggestedAccomodation t Fit

-- | Create a suggested penance map for stop services, based on travel type and fitness level
suggestedStopServices :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> M.Map Service Penance -- ^ The suggested services map
suggestedStopServices Walking _ = M.fromList [
    (Restaurant, Penance 1.0),
    (Groceries, Penance 0.5)
  ]
suggestedStopServices Walking_Naismith f = suggestedStopServices Walking f
suggestedStopServices Cycling _ = M.fromList [
    (Restaurant, Penance 1.0),
    (Groceries, Penance 0.5),
    (BicycleStorage, Penance 1.0)
  ]


-- | Create a suggested penance map for day services, based on travel type and fitness level
suggestedDayServices :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> M.Map Service Penance -- ^ The suggested services map
suggestedDayServices Walking _ = M.fromList [
    (Groceries, Penance 1.0),
    (Pharmacy, Penance 0.5),
    (Bank, Penance 0.5)
  ]
suggestedDayServices Walking_Naismith f = suggestedDayServices Walking f
suggestedDayServices Cycling _ = M.fromList [
    (Groceries, Penance 1.0),
    (Pharmacy, Penance 0.5),
    (Bank, Penance 0.5),
    (BicycleRepair, Penance 2.0)
  ]


-- | Create a suggested range for times, based on the travel mode and fitness level.
--   Derived from estimated time limits plus some sanity
suggestedTimeRange :: Travel -- ^ The method of travel
  -> Fitness -- ^ The fitness level
  -> PreferenceRange Float -- ^ The suggested Time ranges
suggestedTimeRange _ SuperFit = PreferenceRange Nothing 6.0 0.0 8.0 (Just 0.0) (Just 12.0)
suggestedTimeRange _ VeryFit = PreferenceRange Nothing 6.0 0.0 8.0 (Just 0.0) (Just 12.0)
suggestedTimeRange _ Fit = PreferenceRange Nothing 6.0 0.0 8.0 (Just 0.0) (Just 10.0)
suggestedTimeRange _ Normal = PreferenceRange Nothing 6.0 0.0 8.0 (Just 0.0) (Just 10.0)
suggestedTimeRange _ Unfit = PreferenceRange Nothing 5.0 0.0 6.0 (Just 0.0) (Just 9.0)
suggestedTimeRange _ VeryUnfit = PreferenceRange Nothing 4.0 0.0 5.0 (Just 0.0) (Just 6.0)

suggested :: (Route -> [Location]) -> CaminoPreferences -> [Location]
suggested accessor preferences  = foldl merge (accessor $ caminoDefaultRoute camino) (filter (\r -> S.member r selected) (caminoRoutes camino))
  where
     camino = preferenceCamino preferences
     merge current route = current ++ (filter (\r -> not (elem r current)) (accessor route))
     selected = preferenceRoutes preferences
     
-- | Create a list of suggested start points from the camino and selected routes
suggestedStarts :: CaminoPreferences -- ^ The current set of preferences
  -> [Location] -- A list of locations giving start points
suggestedStarts = suggested routeStarts

     
-- | Create a list of suggested finish from the camino and selected routes
suggestedFinishes :: CaminoPreferences -- ^ The current set of preferences
  -> [Location] -- A list of locations giving start points
suggestedFinishes = suggested routeFinishes

-- | The default travel preference set for a travel style and fitness level
-- | This provides an overridable skeleton containing values that cover the suggested legs for a pilgrim of the nominated fitness
defaultTravelPreferences :: Travel -- ^ The travel style of the pilgrim
 -> Fitness -- ^ The fitness level of the pilgrim, see the notes on @suggestedDistanceRange@
 -> TravelPreferences -- ^ The resulting preferences skeleton
defaultTravelPreferences travel fitness = let
      distance = suggestedDistanceRange travel fitness
    in
      TravelPreferences {
        preferenceTravelFunction = travel,
        preferenceFitness = fitness,
        preferenceDistance = distance,
        preferenceTime = suggestedTimeRange travel fitness,
        preferencePerceivedDistance = perceivedDistanceRange fitness distance,
        preferenceStop = Penance 2.0,
        preferenceAccommodation = suggestedAccomodation travel fitness,
        preferenceStopServices = suggestedStopServices travel fitness,
        preferenceDayServices = suggestedDayServices travel fitness
      }


-- | The default camino preference set for a particular camino
-- | This provides an overridable skeleton containing values that cover the suggested legs for a pilgrim of the nominated fitness
defaultCaminoPreferences :: Camino-- ^ The camino to default to
 -> CaminoPreferences -- ^ The resulting preferences skeleton
defaultCaminoPreferences camino = let
      dr = caminoDefaultRoute camino
    in
      CaminoPreferences {
          preferenceCamino = camino
        , preferenceStart = head $ routeStarts dr
        , preferenceFinish = head $ routeFinishes dr
        , preferenceRoutes = S.singleton dr
        , preferenceStops = routeStops dr
        , preferenceExcluded = S.empty
      }
