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
  PreferenceRange(..),
  Preferences(..),

  allowedLocations,
  boundsDistance,
  defaultPreferences,
  isInsideMaximum,
  isOutOfBounds,
  isOutOfRange,
  normalisePreferences,
  recommendedStops,
  rangeDistance,
  withoutLower,
  withoutRange,
  withoutUpper
) where

import Data.Aeson
import Data.Text (Text)
import Camino.Camino
import Camino.Walking
import qualified Data.Map as M (Map, (!), fromList)
import qualified Data.Set as S (Set, difference, empty, insert, intersection, map, member, union, unions)
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
  rangeMinimum :: a, -- ^ The /hard/ lower bound for a range
  rangeMaximum :: a -- ^ The /hard/ upper bound for a range
} deriving (Show)

instance (FromJSON a) => FromJSON (PreferenceRange a) where
  parseJSON (Object v) = do
    derived' <- v .:? "derived" .!= Nothing
    target' <- v .: "target"
    lower' <- v .: "lower"
    upper' <- v .: "upper"
    minimum' <- v .: "min"
    maximum' <- v .: "max"
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

-- | Create a preference range without a lower bound (set to zero)
withoutLower :: (Num a) => PreferenceRange a -- ^ The source preference
  -> PreferenceRange a -- ^ The same preference without a lower bound
withoutLower (PreferenceRange derived target _lower upper _mini maxi) =
  PreferenceRange derived target 0 upper 0 maxi

-- | Create a preference range without an upp bound (set to 1 million)
withoutUpper :: (Num a) => PreferenceRange a -- ^ The source preference
  -> PreferenceRange a -- ^ The same preference without an upper bound
withoutUpper (PreferenceRange derived target lower _upper mini _maxi) =
  PreferenceRange derived target lower 1000000 mini 1000000

-- | Create a preference range without a minimum and maximum
withoutRange :: (Num a) => PreferenceRange a -- ^ The source rpeferences
  -> PreferenceRange a -- ^ The same rrange without minimum and maximum
withoutRange (PreferenceRange derived target lower upper _mini _maxi) =
  PreferenceRange derived target lower upper 0 1000000

-- | Is a value at or below the absolute maximum in the preference range?
isInsideMaximum :: (Ord a) => PreferenceRange a -- ^ The preference range
  -> a -- ^ The value to test
  -> Bool -- ^ True if we haven't exceeded the maximum
isInsideMaximum (PreferenceRange _derived _target _lower _upper _mini maxi) value =
  value <= maxi

-- | Is a value outside the preference range?
isOutOfRange :: (Ord a) => PreferenceRange a -- ^ The preference range
  -> a -- ^ The value to test
  -> Bool -- ^ True if out of range
isOutOfRange (PreferenceRange _derived _target _lower _upper mini maxi) value =
  value < mini || value > maxi

-- | Is a value outside the preference bounds?
isOutOfBounds :: (Ord a) => PreferenceRange a -- ^ The preference range
  -> a -- ^ The value to test
  -> Bool -- ^ True if out of bounds
isOutOfBounds (PreferenceRange _dervived _target lower upper _minimum _maximum) value =
  value < lower || value > upper

-- | Get the normalised distance to the outer range of a value
-- 
--   If the value is less than the target, the result is a value from 0 to 1
--   where 0 is at the target and 1 is at the minimum.
--   Conversely, if the value is more than the target, the result is a value from 0 to 1
--   where 0 is at the target and 1 is at the maximum.
rangeDistance :: (Ord a, Fractional a) => PreferenceRange a -> a -> a
rangeDistance (PreferenceRange _derived targ _lower _upper mini maxi) value
  | value < targ = (targ - value) / (targ - mini)
  | otherwise = (value - targ) / (maxi - targ)
  
-- | Get the normalised distance to the bounds of a value
-- 
--   If the value is less than the target, the result is a value from 0 to 1
--   where 0 is at the target and 1 is at the lower bound.
--   Conversely, if the value is more than the target, the result is a value from 0 to 1
--   where 0 is at the target and 1 is at the upper bound.
boundsDistance :: (Ord a, Fractional a) => PreferenceRange a -- ^ The preference range
  -> a -- ^ The actual value
  -> a -- ^ The normalised distance
boundsDistance (PreferenceRange _derived targ lower upper _minimum _maximum) value
  | value < targ = (targ - value) / (targ - lower)
  | otherwise = (value - targ) / (upper - targ)
  

-- | Convert a normal distance range into a perceived distance range      
perceivedDistanceRange :: Fitness -> PreferenceRange Float -> PreferenceRange Float
perceivedDistanceRange fitness range = let
    mini' = perceivedDistance fitness (rangeMinimum range) False
    lower' = perceivedDistance fitness (rangeLower range) False
    target' = perceivedDistance fitness (rangeTarget range) True  
    upper' = perceivedDistance fitness (rangeUpper range) True
    maxi' = perceivedDistance fitness (rangeMaximum range) True
  in
    PreferenceRange (Just "Derived from distance preferences") target' lower' upper' mini' maxi'
    
-- | Preferences for how calculations are made    
data Preferences = Preferences {
  preferenceWalkingFunction :: String, -- ^ The name of the base walking function
  preferenceFitness :: Fitness, -- ^ The base fitness level
  preferenceDistance :: PreferenceRange Float, -- ^ The preferred distance range
  preferenceTime :: PreferenceRange Float, -- ^ The preferred time walking range
  preferencePerceivedDistance :: PreferenceRange Float, -- ^ The preferred distance range; if nothing then built from the distance range
  preferenceStop :: Penance, -- ^ The amount of penance associated with stopping for a day (larger will tend to increase legs, smaller the opposite)
  preferenceAccommodation :: M.Map AccommodationType Penance, -- ^ Accommodation preferences (absence implies unacceptable accommodation)
  preferenceStopServices :: M.Map Service Penance, -- ^ Desired services at a stop (absence implies zero desire)
  preferenceDayServices :: M.Map Service Penance, -- ^ Desired services during a day (absence implies zero desire)
  preferenceRoutes :: S.Set Route, -- ^ Routes to use
  preferenceStops :: S.Set Location, -- ^ Locations that we must visit (end a day at)
  preferenceExcluded :: S.Set Location -- ^ Locations that we will not visit (end a day at, although passing through is OK)
} deriving (Show)

instance FromJSON Preferences where
  parseJSON (Object v) = do
    walking' <- v .: "walking"
    fitness' <- v .: "fitness"
    distance' <- v .: "distance"
    time' <- v .: "time"
    perceived' <- v .:? "perceived" .!= perceivedDistanceRange fitness' distance'
    stop' <- v .: "stop"
    accommodation' <- v .: "accommodation"
    sstop' <- v .: "services-stop"
    sday' <- v .: "services-day"
    routes' <- v .:? "routes" .!= S.empty
    stops' <- v .:? "stops" .!= S.empty
    excluded' <- v .:? "excluded" .!= S.empty
    let routes'' = S.map placeholderRoute routes'
    let stops'' = S.map placeholderLocation stops'
    let excluded'' = S.map placeholderLocation excluded'
    return Preferences {
        preferenceWalkingFunction = walking',
        preferenceFitness = fitness',
        preferenceDistance = distance',
        preferenceTime = time',
        preferencePerceivedDistance = perceived',
        preferenceAccommodation = accommodation',
        preferenceStop = stop',
        preferenceStopServices = sstop',
        preferenceDayServices = sday',
        preferenceRoutes = routes'',
        preferenceStops = stops'',
        preferenceExcluded = excluded''
      }
  parseJSON v = error ("Unable to parse preferences object " ++ show v)

instance ToJSON Preferences where
  toJSON (Preferences walking' fitness' distance' time' perceived' accommodation' stop' sstop' sday' routes' stops' excluded') =
    let
      routes'' = S.map routeID routes'
      stops'' = S.map locationID stops'
      excluded'' = S.map locationID excluded'
    in
      object [ "walking" .= walking', "fitness" .= fitness', "distance" .= distance', "time" .= time', "perceived" .= perceived', "stop" .= stop', "accommodation" .= accommodation', "services-stop" .= sstop', "services-day" .= sday', "routes" .= routes'', "stops" .= stops'', "excluded" .= excluded'']

-- | Normalise preferences to the correct locations and routes, based on placeholders
normalisePreferences :: Camino -- ^ The camino that contains the correct locations
  -> Preferences -- ^ The preferences with placeholders
  -> Preferences -- ^ The preferences with locations updated
normalisePreferences camino preferences =
  let
    locs = caminoLocations camino
    routes = M.fromList $ map (\r -> (routeID r, r)) (caminoRoutes camino)
  in
    preferences {
      preferenceRoutes = S.map (\r -> routes M.! (routeID r)) (preferenceRoutes preferences),
      preferenceStops = S.map (\l -> locs M.! (locationID l)) (preferenceStops preferences),
      preferenceExcluded = S.map (\l -> locs M.! (locationID l)) (preferenceExcluded preferences)
    }

-- | Work out what locations are acceptable in a camino, based on the chosen routes.
--   The default route is always included, followed by the routes specified in the preferences.
--   The routes are worked through in order (with the default route always first).
--   That way, locations can be included by one route and then excluded by a ltere route
allowedLocations :: Preferences -- ^ The preferences (normalised, see `normalisePreferences`)
  -> Camino -- ^ The base camino definition
  -> S.Set Location -- ^ The allowed locations
allowedLocations preferences camino =
  let
    usedRoutes = S.insert (caminoDefaultRoute camino) (preferenceRoutes preferences)
    routes = filter (\r -> S.member r usedRoutes) (caminoRoutes camino)
  in
    foldl (\allowed -> \route -> (allowed `S.union` routeLocations route `S.union` routeInclusions route) `S.difference` routeExclusions route) S.empty routes

-- | Generate a set of recommended stops, based on the selected routes
recommendedStops :: Preferences -- ^ The preferences (normalised, see `normalisePreferences`)
  -> Camino -- ^ The base camino definition
  -> S.Set Location -- ^ The allowed locations
recommendedStops preferences camino =
  let
    routes = S.insert (caminoDefaultRoute camino) (preferenceRoutes preferences)
    baseStops = S.unions (S.map routeStops routes)
  in
    baseStops `S.intersection` allowedLocations preferences camino

-- | The default preference set.
-- | This provides an overridable skeleton containing values that cover the suggested legs for a walker of normal fitness.
-- | "Normal" is a little rufty-tufty for many people and allows legs of up to 34km to cover some of the more challenging stages
defaultPreferences :: Preferences
defaultPreferences = let
      fitness = Normal
      distance = PreferenceRange { 
       rangeDerived = Nothing,
       rangeTarget = 20.0, 
       rangeLower = 16.0, 
       rangeUpper = 28.0, 
       rangeMinimum = 8.0, 
       rangeMaximum = 34.0 
      }
    in
      Preferences {
        preferenceWalkingFunction = "tobler",
        preferenceFitness = fitness,
        preferenceDistance = distance,
        preferenceTime = PreferenceRange { 
          rangeDerived = Nothing,
          rangeTarget = 6.0, 
          rangeLower = 5.0, 
          rangeUpper = 8.0, 
          rangeMinimum = 0.0, 
          rangeMaximum = 10.0 
        },
        preferencePerceivedDistance = perceivedDistanceRange Normal distance,
        preferenceStop = Penance 2.0,
        preferenceAccommodation = M.fromList [ 
          (MunicipalAlbergue, Penance 0.0), 
          (PrivateAlbergue, Penance 0.5), 
          (GuestHouse, Penance 1.0),
          (House, Penance 1.5),
          (Hotel, Penance 2.0),
          (Camping, Penance 5.0)
        ],
        preferenceStopServices = M.fromList [ 
          (Restaurant, Penance 1.0), 
          (Groceries, Penance 0.5)
        ],
        preferenceDayServices = M.fromList [ 
          (Groceries, Penance 1.0),
          (Pharmacy, Penance 0.5),
          (Bank, Penance 0.5)
        ],
        preferenceRoutes = S.empty,
        preferenceStops = S.empty,
        preferenceExcluded = S.empty
      } 