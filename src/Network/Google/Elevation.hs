{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Elevation
Description : Google Maps Elevation API
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Network.Google.Elevation (
    LatLngElevation(..)
  , module Network.Google.Maps

  , getElevations
) where

import GHC.Generics
import Conduit
import Data.Aeson
import Data.List.Split (chunksOf)
import Data.Text (intercalate)
import Network.Google.Maps
import Web.PathPieces

data LatLngElevation = LatLngElevation {
    lleLocation :: LatLng
  , lleElevation :: Maybe Double
  , lleResolution :: Maybe Double
} deriving (Eq, Ord, Show, Generic)

instance FromJSON LatLngElevation where
  parseJSON (Object v) = do
    location' <- v .: "location"
    elevation' <- v .:? "elevation"
    resolution' <- v .:? "resolution"
    return $ LatLngElevation {
        lleLocation = location'
      , lleElevation = elevation'
      , lleResolution = resolution'
      }
  parseJSON v = error ("Unable to parse lat/lng/elevation object " ++ show v)

instance ToJSON LatLngElevation where
  toJSON (LatLngElevation location' elevation' resolution') =
    object [
        "location" .= location'
      , "elevation" .= elevation'
      , "resolution" .= resolution'
    ]

elevationChunkSize = 100

getElevations' :: (MonadIO m, MonadThrow m) => MapApi -> [LatLng] -> m [LatLngElevation]
getElevations' api locations = do
  let locations' = intercalate "|" $ map toPathPiece locations
  callGet api "elevation" [("locations", locations')]

-- | Get the elevations for a series of locations
--   The requests are broken into chunks and then re-assembled at the end
getElevations :: (MonadIO m, MonadThrow m) => MapApi -> [LatLng] -> m [LatLngElevation]
getElevations api locations = do
  let locations' = chunksOf elevationChunkSize locations
  elevations' <- mapM (getElevations' api) locations'
  return $ concat elevations'