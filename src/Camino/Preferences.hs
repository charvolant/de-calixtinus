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
  , boundsDistance
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
  , suggestedAccommodation
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
  , withStartFinish
) where

import Data.Aeson
import Data.List (find)
import Data.Placeholder
import Data.Text (Text)
import Camino.Camino
import Camino.Util
import Camino.Walking
import qualified Data.Map as M (Map, (!), fromList, singleton, union)
import qualified Data.Set as S (Set, delete, empty, insert, intersection, map, member, singleton, union, unions)
import Graph.Graph (successors, predecessors)

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


-- | Get the normalised distance to the outer bounds of a value
-- 
-- The scale of the range is from the target to the lower or upper bounds, with 0 being at the target and 1 being at the target
boundsDistance :: (Ord a, Fractional a) => PreferenceRange a -> a -> a
boundsDistance (PreferenceRange _derived targ lower upper _mini _maxi) value
  | value < targ = ((targ - value) / (targ - lower))
  | otherwise = ((value - targ) / (upper - targ))

-- | Get the normalised distance to the outer range of a value
-- 
-- The scale of the range is from the target to the lower or upper bounds, with 0 being at the bound and 1 being a scale away
rangeDistance :: (Ord a, Fractional a) => PreferenceRange a -> a -> a
rangeDistance (PreferenceRange _derived targ lower upper _mini _maxi) value
  | value < targ = ((targ - value) / (targ - lower)) - 1
  | otherwise = ((value - targ) / (upper - targ)) - 1
  

-- | Convert a normal distance range into a perceived distance range      
perceivedDistanceRange :: Travel -> Fitness -> PreferenceRange Float -> PreferenceRange Float
perceivedDistanceRange travel fitness range = let
    mini' = fmap (\v -> perceivedDistance travel fitness v False) (rangeMinimum range)
    lower' = perceivedDistance travel fitness (rangeLower range) False
    target' = perceivedDistance travel fitness (rangeTarget range) True  
    upper' = perceivedDistance travel fitness (rangeUpper range) True
    maxi' = fmap (\v -> perceivedDistance travel fitness v True) (rangeMaximum range)
  in
    PreferenceRange (Just "Normal fitness walking equivalent") target' lower' upper' mini' maxi'
    
-- | Preferences for hwo far, how long and what sort of comforts one might expect.
--   These can be reasonably expected to remain constant across different caminos   
data TravelPreferences = TravelPreferences {
    preferenceTravel :: Travel -- ^ The name of the base walking function
  , preferenceFitness :: Fitness -- ^ The base fitness level
  , preferenceComfort :: Comfort -- ^ The base comfort level
  , preferenceDistance :: PreferenceRange Float -- ^ The preferred distance range
  , preferenceTime :: PreferenceRange Float -- ^ The preferred time walking range
  , preferenceStop :: Penance -- ^ The amount of penance associated with stopping for a day (larger will tend to increase legs, smaller the opposite)
  , preferenceAccommodation :: M.Map AccommodationType Penance -- ^ Accommodation preferences (absence implies unacceptable accommodation)
  , preferenceStopServices :: M.Map Service Penance -- ^ Desired services at a stop (absence implies zero desire)
  , preferenceDayServices :: M.Map Service Penance -- ^ Desired services during a day (absence implies zero desire)
} deriving (Show)

instance FromJSON TravelPreferences where
  parseJSON (Object v) = do
    travel' <- v .: "travel"
    fitness' <- v .: "fitness"
    comfort' <- v .: "comfort"
    distance' <- v .: "distance"
    time' <- v .: "time"
    stop' <- v .: "stop"
    accommodation' <- v .: "accommodation"
    sstop' <- v .: "services-stop"
    sday' <- v .: "services-day"
    return TravelPreferences {
          preferenceTravel = travel'
        , preferenceFitness = fitness'
        , preferenceComfort = comfort'
        , preferenceDistance = distance'
        , preferenceTime = time'
        , preferenceAccommodation = accommodation'
        , preferenceStop = stop'
        , preferenceStopServices = sstop'
        , preferenceDayServices = sday'
      }
  parseJSON v = error ("Unable to parse preferences object " ++ show v)

instance ToJSON TravelPreferences where
  toJSON (TravelPreferences travel' fitness' comfort' distance' time' stop' accommodation' sstop' sday') =
    object [ 
        "travel" .= travel'
      , "fitness" .= fitness'
      , "comfort" .= comfort'
      , "distance" .= distance'
      , "time" .= time'
      , "stop" .= stop'
      , "accommodation" .= accommodation'
      , "services-stop" .= sstop'
      , "services-day" .= sday'
    ]

    
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
    let camino'' = placeholder camino'
    let start'' = placeholder start'
    let finish'' = placeholder finish'
    let routes'' = S.map placeholder routes'
    let stops'' = S.map placeholder stops'
    let excluded'' = S.map placeholder excluded'
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
    routes' = S.map (normalise camino') routes
    prefs' = preferences { preferenceRoutes = routes' }
    allowed = caminoRouteLocations (preferenceCamino preferences) routes'
    start' = if S.member (preferenceStart prefs') allowed then preferenceStart prefs' else head $ suggestedStarts prefs'
    finish' = if S.member (preferenceFinish prefs') allowed then preferenceFinish prefs' else head $ suggestedFinishes prefs'
    stops' = preferenceStops prefs' `S.intersection` allowed
    excluded' = preferenceExcluded prefs' `S.intersection` allowed
  in
    CaminoPreferences camino' start' finish' routes' stops' excluded'


-- | Update with a new start and finish and, if necessary, stops etc normalised
withStartFinish :: CaminoPreferences -> Location -> Location -> CaminoPreferences
withStartFinish preferences st fin = let
    camino' = preferenceCamino preferences
    routes' = preferenceRoutes preferences
    start' = normalise camino' st
    finish' = normalise camino' fin
    prefs' = preferences { preferenceStart = start', preferenceFinish = finish' }
    allowed = caminoRouteLocations (preferenceCamino preferences) routes'
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
    start' = preferenceStart preferences
    finish' = preferenceFinish preferences
  in 
    S.insert start' $ S.insert finish' $ (successors camino start') `S.intersection` (predecessors camino finish') `S.intersection` (allowedLocations preferences)
    
-- | Generate a set of recommended stops, based on the selected routes
recommendedStops :: CaminoPreferences -- ^ The preferences (normalised, see `normalisePreferences`)
  -> S.Set Location -- ^ The allowed locations
recommendedStops preferences =
  let
    camino = preferenceCamino preferences
    routes = S.insert (caminoDefaultRoute camino) (preferenceRoutes preferences)
    baseStops = S.unions (S.map routeStops routes)
  in
    S.delete (preferenceStart preferences) $ S.delete (preferenceFinish preferences) $ baseStops `S.intersection` reachableLocations preferences

-- | Create a suggested range for distances, based on the travel mode and fitness level.
--   Derived from estimated time limits plus some sanity.
--   Note that "Normal" is a little rufty-tufty for many people and allows legs of up to 34km to cover some of the more challenging stages
--   Cycling distances for the super-fit are vaguely based on Tour de France stages
suggestedDistanceRange :: Travel -- ^ The method of travel
  -> Fitness -- ^ The fitness level
  -> PreferenceRange Float -- ^ The suggested distance ranges
suggestedDistanceRange Walking SuperFit = PreferenceRange Nothing 30.0 26.0 34.0 (Just 12.0) (Just 44.0)
suggestedDistanceRange Walking VeryFit = PreferenceRange Nothing 28.0 24.0 32.0 (Just 12.0) (Just 40.0)
suggestedDistanceRange Walking Fit = PreferenceRange Nothing 24.0 20.0 28.0 (Just 12.0) (Just 36.0)
suggestedDistanceRange Walking Normal = PreferenceRange Nothing 20.0 18.0 22.0 (Just 12.0) (Just 34.0)
suggestedDistanceRange Walking Unfit = PreferenceRange Nothing 20.0 18.0 22.0 (Just 10.0) (Just 28.0)
suggestedDistanceRange Walking VeryUnfit = PreferenceRange Nothing 12.0 6.0 16.0 (Just 3.0) (Just 18.0)
suggestedDistanceRange Cycling SuperFit = PreferenceRange Nothing 150.0 100.0 175.0 (Just 20.0) (Just 225.0)
suggestedDistanceRange Cycling VeryFit = PreferenceRange Nothing 120.0 80.0 150.0 (Just 20.0) (Just 175)
suggestedDistanceRange Cycling Fit = PreferenceRange Nothing 100.0 70.0 130.0 (Just 20.0) (Just 150.0)
suggestedDistanceRange Cycling Normal = PreferenceRange Nothing 80.0 50.0 100.0 (Just 20.0) (Just 120.0)
suggestedDistanceRange Cycling Unfit = PreferenceRange Nothing 50.0 30.0 80.0 (Just 20.0) (Just 100.0)
suggestedDistanceRange Cycling VeryUnfit = PreferenceRange Nothing 30.0 20.0 40.0 (Just 10.0) (Just 50.0)

-- | Base accommodation from comfort level
suggestedAccommodation' :: Comfort -> M.Map AccommodationType Penance
suggestedAccommodation' Austere = M.fromList [
    (MunicipalAlbergue, Penance 8.0),
    (PrivateAlbergue, Penance 6.0),
    (Hostel, Penance 6.0),
    (GuestHouse, Penance 2.0),
    (HomeStay, Penance 1.0),
    (House, Penance 0.0),
    (Hotel, Reject),
    (CampGround, Penance 6.0),
    (Camping, Penance 5.0)
  ]
suggestedAccommodation' Frugal = M.fromList [
    (MunicipalAlbergue, Penance 8.0),
    (PrivateAlbergue, Penance 7.0),
    (Hostel, Penance 7.0),
    (GuestHouse, Penance 5.0),
    (HomeStay, Penance 4.0),
    (House, Penance 3.0),
    (Hotel, Penance 0.0),
    (CampGround, Penance 6.0),
    (Camping, Penance 4.0)
  ]
suggestedAccommodation' Pilgrim = M.fromList [
    (MunicipalAlbergue, Penance 8.0),
    (PrivateAlbergue, Penance 7.0),
    (Hostel, Penance 7.0),
    (GuestHouse, Penance 5.0),
    (HomeStay, Penance 4.0),
    (House, Penance 2.0),
    (Hotel, Penance 0.0),
    (CampGround, Penance 4.0),
    (Camping, Reject)
  ]
suggestedAccommodation' Comfortable = M.fromList [
    (MunicipalAlbergue, Penance 1.0),
    (PrivateAlbergue, Penance 3.0),
    (Hostel, Penance 2.0),
    (GuestHouse, Penance 3.0),
    (HomeStay, Penance 4.0),
    (House, Penance 5.0),
    (Hotel, Penance 8.0),
    (CampGround, Penance 0.0),
    (Camping, Reject)
  ]
suggestedAccommodation' Luxurious = M.fromList [
    (MunicipalAlbergue, Penance 0.0),
    (PrivateAlbergue, Penance 1.0),
    (Hostel, Penance 1.0),
    (GuestHouse, Penance 3.0),
    (HomeStay, Penance 6.0),
    (House, Penance 7.0),
    (Hotel, Penance 8.0),
    (CampGround, Reject),
    (Camping, Reject)
  ]

-- | Create a suggested penance map for accommodation type, based on travel type and fitness level
suggestedAccommodation :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> Comfort -- ^ The comfort level
  -> M.Map AccommodationType Penance -- ^ The suggested accommodation map
suggestedAccommodation _ SuperFit comfort = suggestedAccommodation' comfort
suggestedAccommodation t VeryFit comfort = suggestedAccommodation t SuperFit comfort
suggestedAccommodation _ Fit comfort = suggestedAccommodation' comfort
suggestedAccommodation t Normal comfort = suggestedAccommodation t Fit comfort
suggestedAccommodation t Unfit comfort = suggestedAccommodation t Normal comfort
suggestedAccommodation t VeryUnfit comfort = suggestedAccommodation t Unfit comfort

-- Default services, based on comfort
suggestedStopServices'  :: Comfort -> M.Map Service Penance -- ^ The suggested services map
suggestedStopServices' Austere = M.fromList [
      (Kitchen, Penance 0.5)
  ]
suggestedStopServices' Frugal = M.fromList [
      (Kitchen, Penance 0.5)
  ]
suggestedStopServices' Pilgrim = M.fromList [
      (Restaurant, Penance 1.0)
    , (Groceries, Penance 0.5)
  ]
suggestedStopServices' Comfortable = M.fromList [
      (Restaurant, Penance 2.0)
    , (Groceries, Penance 1.0)
    , (Bedlinen, Penance 2.0)
    , (Breakfast, Penance 0.5)
    , (Dinner, Penance 0.5)
  ]
suggestedStopServices' Luxurious = M.fromList [
      (Restaurant, Penance 4.0)
    , (Bedlinen, Penance 3.0)
    , (Towels, Penance 1.0)
    , (Breakfast, Penance 1.0)
    , (Dinner, Penance 1.0)
  ]

-- | Create a suggested penance map for stop services, based on travel type and fitness level
suggestedStopServices :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> Comfort -- ^ The comfort level
  -> M.Map Service Penance -- ^ The suggested services map
suggestedStopServices Walking _ comfort = suggestedStopServices' comfort
suggestedStopServices Cycling fitness comfort = M.union 
  (M.singleton BicycleStorage (Penance 2.0)) 
  (suggestedStopServices Walking fitness comfort)


-- Default services, based on comfort
suggestedDayServices'  :: Comfort -> M.Map Service Penance -- ^ The suggested services map
suggestedDayServices' Austere = M.fromList [
      (Groceries, Penance 2.0)
    , (Pharmacy, Penance 0.5)
  ]
suggestedDayServices' Frugal = M.fromList [
      (Groceries, Penance 2.0)
    , (Pharmacy, Penance 0.5)
  ]
suggestedDayServices' Pilgrim = M.fromList [
      (Groceries, Penance 1.0)
    , (Pharmacy, Penance 0.5)
    , (Bank, Penance 0.5)
  ]
suggestedDayServices' Comfortable = M.fromList [
      (Restaurant, Penance 1.0)
    , (Groceries, Penance 1.0)
    , (Pharmacy, Penance 0.5)
    , (Bank, Penance 0.5)
  ]
suggestedDayServices' Luxurious = M.fromList [
      (Restaurant, Penance 2.0)
    , (Groceries, Penance 1.0)
    , (Pharmacy, Penance 1.0)
    , (Bank, Penance 0.5)
    , (Bus, Penance 1.0)
  ]

-- | Create a suggested penance map for stop services, based on travel type and fitness level
suggestedDayServices :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> Comfort -- ^ The comfort level
  -> M.Map Service Penance -- ^ The suggested services map
suggestedDayServices Walking _ comfort = suggestedDayServices' comfort
suggestedDayServices Cycling fitness comfort = M.union 
  (M.singleton BicycleRepair (Penance 3.0)) 
  (suggestedDayServices Walking fitness comfort)


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
 -> Comfort -- ^ The desired comfort level
 -> TravelPreferences -- ^ The resulting preferences skeleton
defaultTravelPreferences travel fitness comfort = TravelPreferences {
    preferenceTravel = travel,
    preferenceFitness = fitness,
    preferenceComfort = comfort,
    preferenceDistance = suggestedDistanceRange travel fitness,
    preferenceTime = suggestedTimeRange travel fitness,
    preferenceStop = Penance 2.0,
    preferenceAccommodation = suggestedAccommodation travel fitness comfort,
    preferenceStopServices = suggestedStopServices travel fitness comfort,
    preferenceDayServices = suggestedDayServices travel fitness comfort
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
