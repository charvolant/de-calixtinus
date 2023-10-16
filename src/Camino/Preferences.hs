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

  boundsDistance,
  defaultPreferences,
  isInsideMaximum,
  isOutOfBounds,
  isOutOfRange,
  normalisePreferences,
  rangeDistance
) where

import Data.Aeson
import Data.Text (Text)
import Camino.Camino
import Camino.Walking
import qualified Data.Map as M (Map, (!), fromList, mapWithKey)
import qualified Data.Set as S (Set, empty, map)

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
  preferenceAccommodation :: M.Map AccommodationType Penance, -- ^ Accommodation preferences (absence implies unacceptable accomodation)
  preferenceRequired :: S.Set Location, -- ^ Locations that we must visit (end a day at)
  preferenceExcluded :: S.Set Location -- ^ Locations that we will not visit (end a day at, although passing through is OK)
} deriving (Show)

instance FromJSON Preferences where
  parseJSON (Object v) = do
    walking' <- v .: "walking"
    fitness' <- v .: "fitness"
    distance' <- v .: "distance"
    time' <- v .: "time"
    perceived' <- v .:? "perceived" .!= perceivedDistanceRange fitness' distance'
    accomodation' <- v .: "accomodation"
    required' <- v .:? "required" .!= S.empty
    excluded' <- v .:? "excluded" .!= S.empty
    let accomodation'' = M.mapWithKey (\_k -> \p -> p) accomodation'
    let required'' = S.map placeholderLocation required'
    let excluded'' = S.map placeholderLocation excluded'
    return Preferences {
        preferenceWalkingFunction = walking',
        preferenceFitness = fitness',
        preferenceDistance = distance',
        preferenceTime = time',
        preferencePerceivedDistance = perceived',
        preferenceAccommodation = accomodation'',
        preferenceRequired = required'',
        preferenceExcluded = excluded''
      }
  parseJSON v = error ("Unable to parse preferences object " ++ show v)

instance ToJSON Preferences where
  toJSON (Preferences walking' fitness' distance' time' perceived' accomodation' required' excluded') =
    let
      required'' = S.map locationID required'
      excluded'' = S.map locationID excluded'
    in
      object [ "walking" .= walking', "fitness" .= fitness', "distance" .= distance', "time" .= time', "perceived" .= perceived', "accomodation" .= accomodation', "required" .= required'', "excluded" .= excluded'']

-- | Normalise preferences to the correct locations, based on placeholders
normalisePreferences :: Camino -- ^ The camino that contains the correct locations
  -> Preferences -- ^ The preferences with placeholders
  -> Preferences -- ^ The preferences with locations updated
normalisePreferences camino preferences =
  let
    locs = locations camino
  in
    preferences {
      preferenceRequired = S.map (\l -> locs M.! (locationID l)) (preferenceRequired preferences),
      preferenceExcluded = S.map (\l -> locs M.! (locationID l)) (preferenceExcluded preferences)
    }

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
        preferenceAccommodation = M.fromList [ 
          (MunicipalAlbergue, Penance 0.0), 
          (PrivateAlbergue, Penance 0.5), 
          (GuestHouse, Penance 1.0),
          (House, Penance 1.5),
          (Hotel, Penance 2.0),
          (Camping, Penance 5.0)
        ],
        preferenceRequired = S.empty,
        preferenceExcluded = S.empty
      } 