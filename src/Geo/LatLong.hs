{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-|
Module      : Geo.LatLong
Description : Latitude and longitude data and functions
Copyright   : (c) Doug Palmer, 2026
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Geo.LatLong (
    LatLong(..)
  , SRS(..)
  , withoutElevation
  , withElevation
  -- * Distances
  , haversineDistance
  , euclidianDistance2
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Aeson.Types
import Data.Default.Class
import Data.Text (Text)
import Data.Util (nothingIfDef)

-- | Spatial reference system
--
--   This is just a text reference to a standard SRS name or EPSG code.
--   Really, everything is assumed to be @WGS84@.
data SRS = SRS Text
  deriving (Eq, Ord, Show, Generic)

srsID :: SRS -> Text
srsID (SRS sid) = sid

instance Default SRS where
  def = SRS "WGS84"

instance FromJSON SRS where
  parseJSON Null = return $ def
  parseJSON (String v) = return $ SRS v
  parseJSON v = typeMismatch "String" v

instance ToJSON SRS where
  toJSON srs' = if srs' == def then Null else String (srsID srs')

instance NFData SRS

data LatLong = LatLong {
    latitude :: Double
  , longitude :: Double
  , elevation :: Maybe Double
  , srs :: SRS
} deriving (Eq, Ord, Show, Generic)

instance FromJSON LatLong where
  -- Named object
  parseJSON (Object v) = do
    latitude' <- v .: "latitude"
    longitude' <- v .: "longitude"
    elevation' <- v .:? "elevation"
    srs' <- v .:? "srs" .!= def
    return LatLong {
        latitude = latitude'
      , longitude = longitude'
      , elevation = elevation'
      , srs = srs'
      }
  parseJSON v = typeMismatch "Object" v

instance ToJSON LatLong where
  toJSON (LatLong latitude' longitude' elevation' srs') =
    object [
        "latitude" .= latitude'
      , "longitude" .= longitude'
      , "elevation" .= elevation'
      , "srs" .= nothingIfDef srs'
    ]
  toEncoding (LatLong latitude' longitude' elevation' srs') =
    pairs $
         "latitude" .= latitude'
      <> "longitude" .= longitude'
      <> "elevation" .?= elevation'
      <> "srs" .?= nothingIfDef srs'

instance NFData LatLong

instance Default LatLong where
  def = LatLong 0.0 0.0 Nothing def

-- | Provide a lat-long without an elevation
withoutElevation :: LatLong -> LatLong
withoutElevation ll@(LatLong _ _ Nothing _) = ll
withoutElevation (LatLong lat' long' _ srs') = LatLong lat' long' Nothing srs'

-- | Provide a lat-long with an (optionally nothing) elevation
withElevation :: LatLong -> Maybe Double -> LatLong
withElevation ll@(LatLong _ _ Nothing _) Nothing = ll
withElevation (LatLong lat' long' _ srs') melev' = LatLong lat' long' melev' srs'

-- | Squared Euclidian distance between two lat longs
--   This is not accurate, but good enough for quick estimation for things like sorts and selection
--
--   @see `haversineDistance`
euclidianDistance2 :: LatLong -- ^ From lat/long
  -> LatLong -- ^ To lat/long
  -> Double -- ^ Distance squared in aribtrary units
euclidianDistance2 (LatLong lat1 long1 _elev1 _srs1) (LatLong lat2 long2 _elev2 _srs2) = (lat2 - lat1) * (lat2 - lat1) + (long2 - long1) * (long2 - long1)

-- | Distance for small angle differences using the Haverisne formula
--
--   https://en.wikipedia.org/wiki/Haversine_formula
haversineDistance :: LatLong -- ^ From lat/long
  -> LatLong -- ^ To lat/long
  -> Double -- ^ Distance in metres
haversineDistance (LatLong lat1 long1 _elev1 _srs1) (LatLong lat2 long2 _elev2 _srs2) = let
  lat1r = lat1 * pi / 180.0
  long1r = long1 * pi / 180.0
  lat2r = lat2 * pi / 180.0
  long2r = long2 * pi / 180.0
  deltalat = lat2r - lat1r
  deltalong = long2r - long1r
  r = 6378137.0
  hav = sqrt (1.0 - cos deltalat + cos lat1r * cos lat2r * (1 - cos deltalong))
  hav' = max (-1.0) (min 1.0 hav)
  in
    2.0 * r * hav'
