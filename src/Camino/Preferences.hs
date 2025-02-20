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
  , StopPreferences(..)
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
  , rangeDistance
  , rangeDistanceInt
  , reachableLocations
  , reachablePois
  , recommendedPois
  , recommendedStops
  , selectedPois
  , selectedRoutes
  , suggestedAccommodation
  , suggestedRouteServices
  , suggestedDistanceRange
  , suggestedFinishes
  , suggestedLocation
  , suggestedPoiCategories
  , suggestedRestLocation
  , suggestedRestServices
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
import Data.Placeholder
import Data.Text (Text)
import Camino.Camino
import Camino.Util
import Camino.Walking
import Data.Time.Calendar (Day)
import qualified Data.Map as M (Map, difference, fromList, singleton, union)
import qualified Data.Set as S (Set, delete, disjoint, empty, filter, fold, fromList, insert, intersection, map, member, singleton, union, unions)
import Data.Summary
import qualified Data.Text as T (Text, intercalate, pack)
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
  (rangeLower pr < rangeTarget pr) &&
  (rangeTarget pr < rangeUpper pr) &&
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

-- | Get the normalised distance to the outer range of a value for fractional values
-- 
-- The scale of the range is from the target to the lower or upper bounds, with 0 being at the bound and 1 being a scale away
rangeDistance :: (Ord a, Fractional a) => PreferenceRange a -> a -> a
rangeDistance (PreferenceRange _derived targ lower upper _mini _maxi) value =
  if value < targ then
    max 0 ((targ - value) / (targ - lower) - 1)
  else
    max 0 ((value - targ) / (upper  - targ) - 1)

 -- | Get the normalised distance to the outer range of a value for integer values
 --
 -- The scale of the range is from the target to the lower or upper bounds, with 0 being at the bound and 1 being a scale away
rangeDistanceInt :: PreferenceRange Int -> Int -> Float
rangeDistanceInt (PreferenceRange _derived targ lower upper _mini _maxi) value = let
    tf = fromIntegral targ :: Float
    lf = fromIntegral lower :: Float
    uf = fromIntegral upper :: Float
    vf = fromIntegral value :: Float
  in
    if value < targ then
      max 0 ((tf - vf) / (tf - lf) - 1)
    else
      max 0 ((vf - tf) / (uf  - tf) - 1)

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

-- | Preferences for a stop.
--   Different types of stops have packages of location, accommodation and service preferences.
data StopPreferences = StopPreferences {
    stopTransportLinks :: Bool -- ^ Allow transport links for accomodation and services
  , stopLocation :: M.Map LocationType Penance -- ^ Location stop preferences (absence implies zero preference)
  , stopAccommodation :: M.Map AccommodationType Penance -- ^ Accommodation preferences (absence implies unacceptable accommodation)
  , stopServices :: M.Map Service Penance -- ^ Desired services at a stop (absence implies zero desire)
  , stopRouteServices :: M.Map Service Penance -- ^ Desired services on the road before reaching a stop
} deriving (Show)

instance FromJSON StopPreferences where
  parseJSON (Object v) = do
    transport' <- v .:? "transport-links" .!= False
    location' <- v .: "location"
    accommodation' <- v .: "accommodation"
    stop' <- v .: "services"
    route' <- v .: "services-route"
    return StopPreferences {
          stopTransportLinks = transport'
        , stopLocation = location'
        , stopAccommodation = accommodation'
        , stopServices = stop'
        , stopRouteServices = route'
      }
  parseJSON v = error ("Unable to parse stop preferences object " ++ show v)

instance ToJSON StopPreferences where
  toJSON (StopPreferences transport' location' accommodation' services' route') =
    object [
        "transport-links" .= transport'
      , "location" .= location'
      , "accommodation" .= accommodation'
      , "services" .= services'
      , "services-route" .= route'
    ]

-- | Preferences for hwo far, how long and what sort of comforts one might expect.
--   These can be reasonably expected to remain constant across different caminos   
data TravelPreferences = TravelPreferences {
    preferenceTravel :: Travel -- ^ The name of the base walking function
  , preferenceFitness :: Fitness -- ^ The base fitness level
  , preferenceComfort :: Comfort -- ^ The base comfort level
  , preferenceDistance :: PreferenceRange Float -- ^ The preferred distance range
  , preferenceTime :: PreferenceRange Float -- ^ The preferred time walking range
  , preferenceRest :: PreferenceRange Int -- ^ The preferred amount of days walking before a rest
  , preferenceStop :: StopPreferences -- ^ Preferences for a normal end of day stop
  , preferenceStockStop :: StopPreferences -- ^ Preferences for the day before a day where everything will be closed (ie Sunday)
  , preferenceRestStop :: StopPreferences -- ^ Preferences for a rest stop (a stop where you spend a day resting up)
  , preferencePoiCategories :: S.Set PoiCategory -- ^ The types of Poi to visit
} deriving (Show)

instance FromJSON TravelPreferences where
  parseJSON (Object v) = do
    travel' <- v .: "travel"
    fitness' <- v .: "fitness"
    comfort' <- v .: "comfort"
    distance' <- v .: "distance-range"
    time' <- v .: "time-range"
    rest' <- v .: "rest-range"
    stop' <- v .: "stop"
    stock' <- v .: "stock-stop"
    rests' <- v .: "rest-stop"
    pois' <- v .: "poi-categories"
    return TravelPreferences {
          preferenceTravel = travel'
        , preferenceFitness = fitness'
        , preferenceComfort = comfort'
        , preferenceDistance = distance'
        , preferenceTime = time'
        , preferenceRest = rest'
        , preferenceStop = stop'
        , preferenceStockStop = stock'
        , preferenceRestStop = rests'
        , preferencePoiCategories = pois'
      }
  parseJSON v = error ("Unable to parse preferences object " ++ show v)

instance ToJSON TravelPreferences where
  toJSON (TravelPreferences travel' fitness' comfort' distance' time' rest' stop' stock' rests' pois') =
    object [ 
        "travel" .= travel'
      , "fitness" .= fitness'
      , "comfort" .= comfort'
      , "distance-range" .= distance'
      , "time-range" .= time'
      , "rest-range" .= rest'
      , "stop" .= stop'
      , "stock-stop" .= stock'
      , "rest-stop" .= rests'
      , "poi-categories" .= pois'
    ]

-- | Preferences for where to go and where to stop on a camino  
data CaminoPreferences = CaminoPreferences {
    preferenceCamino :: Camino -- ^ The camino route to walk
  , preferenceStart :: Location -- ^ The start location
  , preferenceFinish :: Location -- ^ The finish location
  , preferenceRoutes :: S.Set Route -- ^ The routes to use
  , preferenceStops :: S.Set Location -- ^ Locations that we must visit (end a day at)
  , preferenceExcluded :: S.Set Location -- ^ Locations that we will not visit (end a day at, although passing through is OK)
  , preferencePois :: S.Set PointOfInterest -- ^ The significant points of interest that deserve a stop
  , preferenceStartDate :: Maybe Day -- ^ The proposed start date
} deriving (Show)

instance FromJSON CaminoPreferences where
  parseJSON (Object v) = do
    camino' <- v .: "camino"
    start' <- v .: "start"
    finish' <- v .: "finish"
    routes' <- v .:? "routes" .!= S.empty
    stops' <- v .:? "stops" .!= S.empty
    excluded' <- v .:? "excluded" .!= S.empty
    pois' <- v .:? "pois" .!= S.empty
    startDate' <- v .:? "start-date"
    let camino'' = placeholder camino'
    let start'' = placeholder start'
    let finish'' = placeholder finish'
    let routes'' = S.map placeholder routes'
    let stops'' = S.map placeholder stops'
    let excluded'' = S.map placeholder excluded'
    let pois'' = S.map placeholder pois'
    return CaminoPreferences {
        preferenceCamino = camino''
      , preferenceStart = start''
      , preferenceFinish = finish''
      , preferenceRoutes = routes''
      , preferenceStops = stops''
      , preferenceExcluded = excluded''
      , preferencePois = pois''
      , preferenceStartDate = startDate'
      }
  parseJSON v = error ("Unable to parse preferences object " ++ show v)

instance ToJSON CaminoPreferences where
  toJSON (CaminoPreferences camino' start' finish' routes' stops' excluded' pois' startDate') =
    let
      routes'' = S.map routeID routes'
      stops'' = S.map locationID stops'
      excluded'' = S.map locationID excluded'
      pois'' = S.map poiID pois'
    in
      object [
          "camino" .= caminoId camino'
        , "start" .= locationID start'
        , "finish" .= locationID finish'
        , "routes" .= routes''
        , "stops" .= stops''
        , "excluded" .= excluded''
        , "pois" .= pois''
        , "start-date" .= startDate'
      ]

instance Summary CaminoPreferences where
  summary cprefs = "{"
    <> caminoId (preferenceCamino cprefs)
    <> ", " <> summary (preferenceStart cprefs)
    <> ", " <> summary (preferenceFinish cprefs)
    <> ", " <> summary (S.map routeID $ preferenceRoutes cprefs)
    <> ", " <> (summary $ preferenceStops cprefs)
    <> ", " <> (summary $ preferenceExcluded cprefs)
    <> ", " <> (summary $ preferencePois cprefs)
    <> ", " <> (T.pack $ show $ preferenceStartDate cprefs)
    <> "}"

-- | Normalise preferences to the correct locations and routes, based on placeholders
normalisePreferences :: CaminoConfig -- ^ The camino configutation
  -> CaminoPreferences -- ^ The preferences with placeholders
  -> CaminoPreferences -- ^ The preferences with locations updated
normalisePreferences config preferences =
  let
    camino = dereference config (preferenceCamino preferences)
  in
    preferences {
        preferenceCamino = camino
      , preferenceStart = dereference camino (preferenceStart preferences)
      , preferenceFinish = dereference camino (preferenceFinish preferences)
      , preferenceRoutes = dereferenceS camino (preferenceRoutes preferences)
      , preferenceStops = dereferenceS camino (preferenceStops preferences)
      , preferenceExcluded = dereferenceS camino (preferenceExcluded preferences)
      , preferencePois = dereferenceS camino (preferencePois preferences)
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
    pois' = preferencePois prefs'
    startDate' = preferenceStartDate preferences
  in
    CaminoPreferences camino' start' finish' routes' stops' excluded' pois' startDate'


-- | Update with a new start and finish and, if necessary, stops etc normalised
withStartFinish :: CaminoPreferences -> Location -> Location -> CaminoPreferences
withStartFinish preferences st fin = let
    camino' = preferenceCamino preferences
    routes' = preferenceRoutes preferences
    start' = dereference camino' st
    finish' = dereference camino' fin
    prefs' = preferences { preferenceStart = start', preferenceFinish = finish' }
    allowed = caminoRouteLocations (preferenceCamino preferences) routes'
    stops' = preferenceStops prefs' `S.intersection` allowed
    excluded' = preferenceExcluded prefs' `S.intersection` allowed
    pois' = preferencePois prefs'
    startDate' = preferenceStartDate preferences
  in
    CaminoPreferences camino' start' finish' routes' stops' excluded' pois' startDate'

-- | The list of routes selected, in camino order
selectedRoutes :: CaminoPreferences -- ^ The preference set
  -> [Route] -- ^ The selected routes, including the default route in route order
selectedRoutes preferences = selectFromList routes (caminoRoutes camino)
  where 
    camino = preferenceCamino preferences
    routes = (S.singleton $ caminoDefaultRoute camino) `S.union` (preferenceRoutes preferences) 

-- | Get the significant points of interest for a location
selectedPois :: CaminoPreferences -- ^ The preference set
  -> Location -- ^ The location
  -> [PointOfInterest] -- ^ The points of interest at that location that have been selected
selectedPois preferences location = filter (\poi -> S.member poi selected) (locationPois location) where selected = preferencePois preferences

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

-- | Show possible pionts of interest selected by the preferences
reachablePois :: CaminoPreferences -- ^ The preferences
  -> S.Set PointOfInterest -- ^ The possible points of interest
reachablePois preferences = S.fold (\l -> \s -> s `S.union` (S.fromList $ locationPois l)) S.empty $ reachableLocations preferences

-- | Show suggested points of interest at a particular location selected by the preferences
recommendedPois :: TravelPreferences -> CaminoPreferences -> S.Set PointOfInterest
recommendedPois preferences camino = S.filter (\poi -> (S.member poi possible) && (not $ prefPois `S.disjoint` poiCategories poi)) suggestions
  where
    camino' = preferenceCamino camino
    possible = reachablePois camino
    suggestions = S.unions $ (routeSuggestedPois $ caminoDefaultRoute camino'):(map routeSuggestedPois $ caminoRoutes camino')
    prefPois = preferencePoiCategories preferences

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
suggestedDistanceRange Walking Casual = PreferenceRange Nothing 16.0 14.0 18.0 (Just 8.0) (Just 24.0)
suggestedDistanceRange Walking VeryUnfit = PreferenceRange Nothing 12.0 6.0 16.0 (Just 3.0) (Just 18.0)
suggestedDistanceRange Cycling SuperFit = PreferenceRange Nothing 150.0 100.0 175.0 (Just 20.0) (Just 225.0)
suggestedDistanceRange Cycling VeryFit = PreferenceRange Nothing 120.0 80.0 150.0 (Just 20.0) (Just 175)
suggestedDistanceRange Cycling Fit = PreferenceRange Nothing 100.0 70.0 130.0 (Just 20.0) (Just 150.0)
suggestedDistanceRange Cycling Normal = PreferenceRange Nothing 80.0 50.0 100.0 (Just 20.0) (Just 120.0)
suggestedDistanceRange Cycling Unfit = PreferenceRange Nothing 50.0 30.0 80.0 (Just 20.0) (Just 100.0)
suggestedDistanceRange Cycling Casual = PreferenceRange Nothing 40.0 30.0 60.0 (Just 20.0) (Just 80.0)
suggestedDistanceRange Cycling VeryUnfit = PreferenceRange Nothing 30.0 20.0 40.0 (Just 10.0) (Just 50.0)


-- | Create a suggested rest period, based on the travel mode and fitness level.
suggestedRestRange :: Travel -- ^ The method of travel
  -> Fitness -- ^ The fitness level
  -> PreferenceRange Int -- ^ The suggested distance ranges
suggestedRestRange _ SuperFit = PreferenceRange Nothing 7 6 9 (Just 5) (Just 10)
suggestedRestRange _ VeryFit = PreferenceRange Nothing 7 6 8 (Just 5) (Just 9)
suggestedRestRange _ Fit = PreferenceRange Nothing 6 5 8 (Just 4) (Just 9)
suggestedRestRange _ Normal = PreferenceRange Nothing 6 5 7 (Just 3) (Just 8)
suggestedRestRange _ Unfit = PreferenceRange Nothing 5 4 7 (Just 3) (Just 8)
suggestedRestRange _ Casual = PreferenceRange Nothing 5 4 6 (Just 2) (Just 7)
suggestedRestRange _ VeryUnfit = PreferenceRange Nothing 4 3 5 (Just 2) (Just 6)

-- | Base location preferences from comfort level
suggestedLocation' :: Comfort -> M.Map LocationType Penance
suggestedLocation' Austere = M.fromList [
    (Village, Penance 6.0),
    (Town, Penance 3.0),
    (City, Penance 0.0),
    (Monastery, Penance 5.0),
    (Bridge, Penance 4.0),
    (Intersection, Penance 4.0),
    (Peak, Penance 5.0),
    (Poi, Penance 4.0)
  ]
suggestedLocation' Frugal = M.fromList [
    (Village, Penance 5.0),
    (Town, Penance 4.0),
    (City, Penance 0.0),
    (Monastery, Penance 4.0),
    (Bridge, Penance 4.0),
    (Intersection, Penance 4.0),
    (Peak, Penance 4.0),
    (Poi, Penance 3.0)
  ]
suggestedLocation' Pilgrim = M.fromList [
    (Village, Penance 2.0),
    (Town, Penance 2.0),
    (City, Penance 2.0),
    (Monastery, Penance 3.0),
    (Bridge, Penance 2.0),
    (Intersection, Penance 0.0),
    (Peak, Penance 2.0),
    (Poi, Penance 1.0)
  ]
suggestedLocation' Comfortable = M.fromList [
    (Village, Penance 2.0),
    (Town, Penance 3.0),
    (City, Penance 4.0),
    (Monastery, Penance 2.0),
    (Bridge, Penance 2.0),
    (Intersection, Penance 0.0),
    (Peak, Penance 1.0),
    (Poi, Penance 1.0)
  ]
suggestedLocation' Luxurious = M.fromList [
    (Village, Penance 4.0),
    (Town, Penance 6.0),
    (City, Penance 7.0),
    (Monastery, Penance 3.0),
    (Bridge, Penance 2.0),
    (Intersection, Penance 0.0),
    (Peak, Penance 1.0),
    (Poi, Penance 1.0)
  ]

-- | Create a suggested penance map for location type, based on travel type and fitness level
suggestedLocation :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> Comfort -- ^ The comfort level
  -> M.Map LocationType Penance -- ^ The suggested location map
suggestedLocation _ SuperFit comfort = suggestedLocation' comfort
suggestedLocation t VeryFit comfort = suggestedLocation t SuperFit comfort
suggestedLocation _ Fit comfort = suggestedLocation' comfort
suggestedLocation t Normal comfort = suggestedLocation t Fit comfort
suggestedLocation t Unfit comfort = suggestedLocation t Normal comfort
suggestedLocation t Casual comfort = suggestedLocation t Unfit comfort
suggestedLocation t VeryUnfit comfort = suggestedLocation t Unfit comfort

-- | Base location preferences from comfort level
suggestedStockLocation' :: Comfort -> M.Map LocationType Penance
suggestedStockLocation' Austere = M.fromList [
    (Village, Penance 2.0),
    (Town, Penance 2.0),
    (City, Penance 3.0),
    (Monastery, Penance 0.0),
    (Bridge, Reject),
    (Intersection, Reject),
    (Peak, Reject),
    (Poi, Reject)
  ]
suggestedStockLocation' Frugal = M.fromList [
    (Village, Penance 1.0),
    (Town, Penance 4.0),
    (City, Penance 5.0),
    (Monastery, Penance 0.0),
    (Bridge, Reject),
    (Intersection, Reject),
    (Peak, Reject),
    (Poi, Reject)
  ]
suggestedStockLocation' Pilgrim = M.fromList [
    (Town, Penance 4.0),
    (City, Penance 5.0),
    (Village, Penance 0.0),
    (Monastery, Reject),
    (Bridge, Reject),
    (Intersection, Reject),
    (Peak, Reject),
    (Poi, Reject)
  ]
suggestedStockLocation' Comfortable = M.fromList [
    (Town, Penance 4.0),
    (City, Penance 8.0),
    (Village, Penance 0.0),
    (Monastery, Reject),
    (Bridge, Reject),
    (Intersection, Reject),
    (Peak, Reject),
    (Poi, Reject)
  ]
suggestedStockLocation' Luxurious = M.fromList [
    (Town, Penance 10.0),
    (City, Penance 20.0),
    (Village, Penance 0.0),
    (Monastery, Reject),
    (Bridge, Reject),
    (Intersection, Reject),
    (Peak, Reject),
    (Poi, Reject)
  ]

-- | Create a suggested penance map for location type, based on travel type and fitness level
suggestedStockLocation :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> Comfort -- ^ The comfort level
  -> M.Map LocationType Penance -- ^ The suggested location map
suggestedStockLocation _ SuperFit comfort = suggestedStockLocation' comfort
suggestedStockLocation t VeryFit comfort = suggestedStockLocation t SuperFit comfort
suggestedStockLocation _ Fit comfort = suggestedStockLocation' comfort
suggestedStockLocation t Normal comfort = suggestedStockLocation t Fit comfort
suggestedStockLocation t Unfit comfort = suggestedStockLocation t Normal comfort
suggestedStockLocation t Casual comfort = suggestedStockLocation t Unfit comfort
suggestedStockLocation t VeryUnfit comfort = suggestedStockLocation t Unfit comfort

-- | Base location preferences from comfort level
suggestedRestLocation' :: Comfort -> M.Map LocationType Penance
suggestedRestLocation' Austere = M.fromList [
    (Village, Penance 2.0),
    (Town, Penance 2.0),
    (City, Penance 3.0),
    (Monastery, Penance 0.0),
    (Bridge, Reject),
    (Intersection, Reject),
    (Peak, Reject),
    (Poi, Reject)
  ]
suggestedRestLocation' Frugal = M.fromList [
    (Village, Penance 1.0),
    (Town, Penance 4.0),
    (City, Penance 5.0),
    (Monastery, Penance 0.0),
    (Bridge, Reject),
    (Intersection, Reject),
    (Peak, Reject),
    (Poi, Reject)
  ]
suggestedRestLocation' Pilgrim = M.fromList [
    (Town, Penance 5.0),
    (City, Penance 8.0),
    (Village, Penance 0.0),
    (Monastery, Reject),
    (Bridge, Reject),
    (Intersection, Reject),
    (Peak, Reject),
    (Poi, Reject)
  ]
suggestedRestLocation' Comfortable = M.fromList [
    (Town, Penance 5.0),
    (City, Penance 10.0),
    (Village, Penance 0.0),
    (Monastery, Reject),
    (Bridge, Reject),
    (Intersection, Reject),
    (Peak, Reject),
    (Poi, Reject)
  ]
suggestedRestLocation' Luxurious = M.fromList [
    (Town, Penance 5.0),
    (City, Penance 10.0),
    (Village, Penance 0.0),
    (Monastery, Reject),
    (Bridge, Reject),
    (Intersection, Reject),
    (Peak, Reject),
    (Poi, Reject)
  ]

-- | Create a suggested penance map for location type, based on travel type and fitness level
suggestedRestLocation :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> Comfort -- ^ The comfort level
  -> M.Map LocationType Penance -- ^ The suggested location map
suggestedRestLocation _ SuperFit comfort = suggestedRestLocation' comfort
suggestedRestLocation t VeryFit comfort = suggestedRestLocation t SuperFit comfort
suggestedRestLocation _ Fit comfort = suggestedRestLocation' comfort
suggestedRestLocation t Normal comfort = suggestedRestLocation t Fit comfort
suggestedRestLocation t Unfit comfort = suggestedRestLocation t Normal comfort
suggestedRestLocation t Casual comfort = suggestedRestLocation t Unfit comfort
suggestedRestLocation t VeryUnfit comfort = suggestedRestLocation t Unfit comfort

-- | Base accommodation from comfort level
suggestedAccommodation' :: Comfort -> M.Map AccommodationType Penance
suggestedAccommodation' Austere = M.fromList [
    (PilgrimAlbergue, Penance 8.0),
    (PrivateAlbergue, Penance 6.0),
    (Hostel, Penance 6.0),
    (GuestHouse, Penance 2.0),
    (HomeStay, Penance 1.0),
    (House, Penance 0.0),
    (Hotel, Reject),
    (Gite, Penance 7.0),
    (CampGround, Penance 6.0),
    (Refuge, Penance 5.0),
    (Camping, Penance 5.0)
  ]
suggestedAccommodation' Frugal = M.fromList [
    (PilgrimAlbergue, Penance 8.0),
    (PrivateAlbergue, Penance 7.0),
    (Hostel, Penance 7.0),
    (GuestHouse, Penance 5.0),
    (HomeStay, Penance 4.0),
    (House, Penance 3.0),
    (Hotel, Penance 0.0),
    (Gite, Penance 7.0),
    (CampGround, Penance 6.0),
    (Refuge, Penance 5.0),
    (Camping, Penance 4.0)
  ]
suggestedAccommodation' Pilgrim = M.fromList [
    (PilgrimAlbergue, Penance 8.0),
    (PrivateAlbergue, Penance 7.0),
    (Hostel, Penance 7.0),
    (GuestHouse, Penance 5.0),
    (HomeStay, Penance 4.0),
    (House, Penance 2.0),
    (Hotel, Penance 0.0),
    (Gite, Penance 6.0),
    (CampGround, Penance 4.0),
    (Refuge, Reject),
    (Camping, Reject)
  ]
suggestedAccommodation' Comfortable = M.fromList [
    (PilgrimAlbergue, Penance 1.0),
    (PrivateAlbergue, Penance 3.0),
    (Hostel, Penance 2.0),
    (GuestHouse, Penance 3.0),
    (HomeStay, Penance 4.0),
    (House, Penance 5.0),
    (Hotel, Penance 8.0),
    (Gite, Penance 2.0),
    (CampGround, Penance 0.0),
    (Refuge, Reject),
    (Camping, Reject)
  ]
suggestedAccommodation' Luxurious = M.fromList [
    (PilgrimAlbergue, Penance 0.0),
    (PrivateAlbergue, Penance 1.0),
    (Hostel, Penance 1.0),
    (GuestHouse, Penance 3.0),
    (HomeStay, Penance 6.0),
    (House, Penance 7.0),
    (Hotel, Penance 8.0),
    (Gite, Penance 0.0),
    (CampGround, Reject),
    (Refuge, Reject),
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
suggestedAccommodation t Casual comfort = suggestedAccommodation t Unfit comfort
suggestedAccommodation t VeryUnfit comfort = suggestedAccommodation t Unfit comfort


-- | Create a suggested penance map for rest stop accommodation, generally, without any pilgrim albergues or camping options
suggestedRestAccommodation :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> Comfort -- ^ The comfort level
  -> M.Map AccommodationType Penance -- ^ The suggested accommodation map
suggestedRestAccommodation travel fitness comfort =
  M.fromList [(PilgrimAlbergue, Reject), (Camping, Reject), (Refuge, Reject), (CampGround, Reject)] `M.union` suggestedAccommodation travel fitness comfort

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


-- | Create a suggested penance map for stock stops, the main thing is making sure that groceries and a pharmacy are available
suggestedStockServices :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> Comfort -- ^ The comfort level
  -> M.Map Service Penance -- ^ The suggested services map
suggestedStockServices travel fitness comfort = M.union
  (M.fromList [(Groceries, (Penance 1.0)), (Pharmacy, (Penance 1.0))])
  (suggestedStopServices travel fitness comfort)


-- Default services, based on comfort
suggestedRouteServices'  :: Comfort -> M.Map Service Penance -- ^ The suggested services map
suggestedRouteServices' Austere = M.fromList [
      (Groceries, Penance 2.0)
    , (Pharmacy, Penance 0.5)
  ]
suggestedRouteServices' Frugal = M.fromList [
      (Groceries, Penance 2.0)
    , (Pharmacy, Penance 0.5)
  ]
suggestedRouteServices' Pilgrim = M.fromList [
      (Groceries, Penance 1.0)
    , (Pharmacy, Penance 0.5)
    , (Bank, Penance 0.5)
  ]
suggestedRouteServices' Comfortable = M.fromList [
      (Restaurant, Penance 1.0)
    , (Groceries, Penance 1.0)
    , (Pharmacy, Penance 0.5)
    , (Bank, Penance 0.5)
  ]
suggestedRouteServices' Luxurious = M.fromList [
      (Restaurant, Penance 2.0)
    , (Groceries, Penance 1.0)
    , (Pharmacy, Penance 1.0)
    , (Bank, Penance 0.5)
    , (Bus, Penance 1.0)
  ]

-- | Create a suggested penance map for route services, based on travel type and fitness level
suggestedRouteServices :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> Comfort -- ^ The comfort level
  -> M.Map Service Penance -- ^ The suggested services map
suggestedRouteServices Walking _ comfort = suggestedRouteServices' comfort
suggestedRouteServices Cycling fitness comfort = M.union
  (M.singleton BicycleRepair (Penance 3.0))
  (suggestedRouteServices Walking fitness comfort)


-- | Create a suggested penance map for stock-up route services, based on travel type and fitness level
suggestedStockRouteServices :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> Comfort -- ^ The comfort level
  -> M.Map Service Penance -- ^ The suggested services map
suggestedStockRouteServices travel fitness comfort = M.union
  (M.singleton Groceries (Penance 5.0))
  (suggestedRouteServices travel fitness comfort)

-- Default rest services, based on comfort
suggestedRestServices'  :: Comfort -> M.Map Service Penance -- ^ The suggested services map
suggestedRestServices' Austere = M.fromList [
      (Kitchen, Penance 5.0)
    , (Groceries, Penance 5.0)
    , (Pharmacy, Penance 3.0)
    , (Medical, Penance 2.0)
    , (Handwash, Penance 2.0)
  ]
suggestedRestServices' Frugal = M.fromList [
      (Kitchen, Penance 4.0)
    , (Restaurant, Penance 1.0)
    , (Groceries, Penance 5.0)
    , (Pharmacy, Penance 3.0)
    , (Medical, Penance 3.0)
    , (Bank, Penance 1.0)
    , (Handwash, Penance 1.0)
    , (WashingMachine, Penance 1.0)
  ]
suggestedRestServices' Pilgrim = M.fromList [
      (Kitchen, Penance 2.0)
    , (Restaurant, Penance 4.0)
    , (Groceries, Penance 5.0)
    , (Pharmacy, Penance 3.0)
    , (Medical, Penance 3.0)
    , (Bank, Penance 2.0)
    , (Bedlinen, Penance 2.0)
    , (WashingMachine, Penance 3.0)
 ]
suggestedRestServices' Comfortable = M.fromList [
      (Restaurant, Penance 6.0)
    , (Groceries, Penance 5.0)
    , (Pharmacy, Penance 3.0)
    , (Medical, Penance 3.0)
    , (Bank, Penance 2.0)
    , (Bedlinen, Penance 3.0)
    , (WashingMachine, Penance 3.0)
  ]
suggestedRestServices' Luxurious = M.fromList [
      (Restaurant, Penance 6.0)
    , (Groceries, Penance 5.0)
    , (Pharmacy, Penance 3.0)
    , (Medical, Penance 3.0)
    , (Bank, Penance 2.0)
    , (Bedlinen, Penance 5.0)
    , (WashingMachine, Penance 5.0)
    , (Breakfast, Penance 2.0)
    , (Dinner, Penance 2.0)
  ]

-- | Create a suggested penance map for stop services, based on travel type and fitness level
suggestedRestServices :: Travel -- ^ The style of travel
  -> Fitness -- ^ The fitness level
  -> Comfort -- ^ The comfort level
  -> M.Map Service Penance -- ^ The suggested services map
suggestedRestServices Walking _ comfort = suggestedRestServices' comfort
suggestedRestServices Cycling fitness comfort = M.union 
  (M.singleton BicycleStorage (Penance 2.0)) 
  (suggestedRestServices Walking fitness comfort)

-- | Create a suggested set of point of interest prefereneces based on travel type, fitness and comfort level
suggestedPoiCategories :: Travel -> Fitness -> Comfort -> S.Set PoiCategory
suggestedPoiCategories _ _ Austere = S.fromList [ReligiousPoi, PilgrimPoi]
suggestedPoiCategories _ _ Frugal = S.fromList [ReligiousPoi, CulturalPoi, PilgrimPoi]
suggestedPoiCategories _ _ Pilgrim = S.fromList [ReligiousPoi, HistoricalPoi, CulturalPoi, RecreationPoi, PilgrimPoi]
suggestedPoiCategories _ _ Comfortable = S.fromList [HistoricalPoi, CulturalPoi, RecreationPoi, PilgrimPoi]
suggestedPoiCategories _ _ Luxurious = S.fromList [HistoricalPoi, CulturalPoi, RecreationPoi, PilgrimPoi]

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
suggestedTimeRange _ Casual = PreferenceRange Nothing 4.0 0.0 6.0 (Just 0.0) (Just 8.0)
suggestedTimeRange _ VeryUnfit = PreferenceRange Nothing 4.0 0.0 5.0 (Just 0.0) (Just 6.0)

-- | The default transport link choice. If it looks like you're going to be a bit slow, default to allow it
suggestedTransportLinks :: Travel -> Fitness -> Comfort -> Bool
suggestedTransportLinks Cycling _ _ = False
suggestedTransportLinks _ Casual _ = True
suggestedTransportLinks _ VeryUnfit _ = True
suggestedTransportLinks _ _ Comfortable = True
suggestedTransportLinks _ _ Luxurious = True
suggestedTransportLinks _ _ _ = False

-- | The default stop preferences for a normal stop
suggestedStopPreferences :: Travel -> Fitness -> Comfort -> StopPreferences
suggestedStopPreferences travel fitness comfort = let
    services = suggestedStopServices travel fitness comfort
  in
    StopPreferences {
        stopTransportLinks = suggestedTransportLinks travel fitness comfort
      , stopLocation = suggestedLocation travel fitness comfort
      , stopAccommodation = suggestedAccommodation travel fitness comfort
      , stopServices = services
      , stopRouteServices = suggestedRouteServices travel fitness comfort
  }

-- | The default stop preferences for a stock-up stop
suggestedStockPreferences :: Travel -> Fitness -> Comfort -> StopPreferences
suggestedStockPreferences travel fitness comfort = let
    services = suggestedStockServices travel fitness comfort
  in
    StopPreferences {
        stopTransportLinks = suggestedTransportLinks travel fitness comfort
      , stopLocation = suggestedStockLocation travel fitness comfort
      , stopAccommodation = suggestedAccommodation travel fitness comfort
      , stopServices = services
      , stopRouteServices = suggestedStockRouteServices travel fitness comfort
    }

-- | The default stop preferences for a rest stop
suggestedRestPreferences :: Travel -> Fitness -> Comfort -> StopPreferences
suggestedRestPreferences travel fitness comfort = let
    services = suggestedRestServices travel fitness comfort
  in
    StopPreferences {
        stopTransportLinks = suggestedTransportLinks travel fitness comfort
      , stopLocation = suggestedRestLocation travel fitness comfort
      , stopAccommodation = suggestedRestAccommodation travel fitness comfort
      , stopServices = services
      , stopRouteServices = (suggestedRouteServices travel fitness comfort) `M.difference` services
    }

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
      preferenceTravel = travel
    , preferenceFitness = fitness
    , preferenceComfort = comfort
    , preferenceDistance = suggestedDistanceRange travel fitness
    , preferenceTime = suggestedTimeRange travel fitness
    , preferenceRest = suggestedRestRange travel fitness
    , preferenceStop = suggestedStopPreferences travel fitness comfort
    , preferenceStockStop = suggestedStockPreferences travel fitness comfort
    , preferenceRestStop = suggestedRestPreferences travel fitness comfort
    , preferencePoiCategories = suggestedPoiCategories travel fitness comfort
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
        , preferencePois = S.empty
        , preferenceStartDate = Nothing
      }
