{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Maps
Description : Google Maps API
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Network.Google.Maps (
    ApiResponse(..)
  , LatLng(..)
  , MapApi(..)
  , MapException(..)

  , callGet
) where

import GHC.Generics (Generic)
import Conduit
import Control.Exception (Exception)
import Data.Aeson
import Data.Default.Class
import Data.Text (Text, breakOn, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Text.Read
import Web.PathPieces
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest, setRequestQueryString)

-- | A latitude and longitude pair
--   These lat/longs are used by the Google Map API and are assumed to use the [WGS84 SRS](https://en.wikipedia.org/wiki/World_Geodetic_System)
data LatLng = LatLng {
    lat :: Double
  , lng :: Double
} deriving (Eq, Ord, Show, Generic)

instance FromJSON LatLng
instance ToJSON LatLng

instance PathPiece LatLng where
  fromPathPiece v = LatLng <$> readMaybe (unpack $ fst ll) <*> readMaybe (unpack $ snd ll)
    where ll = breakOn "," v
  toPathPiece ll = (pack $ show $ lat ll) <> "," <> (pack $ show $ lng ll)

data ApiResponse a = ApiResponse {
    responseStatus :: Text
  , responseResults :: a
}

instance (FromJSON a) => FromJSON (ApiResponse a) where
  parseJSON (Object v) = do
    status' <- v .: "status"
    results' <- v .: "results"
    return $ ApiResponse {
        responseStatus = status'
      , responseResults = results'
    }
  parseJSON v = error ("Unable to parse api response object " ++ show v)

instance (ToJSON a) => ToJSON (ApiResponse a) where
  toJSON (ApiResponse status' results') =
    object [
        "status" .= status'
      , "results" .= results'
    ]

data MapApi = MapApi {
    apiBase :: Text -- ^ The base map API Url
  , apiKey :: Text -- ^ The API key to use when making requests
}

instance Default MapApi where
  def = MapApi "https://maps.googleapis.com/maps/api" "API_KEY"


-- | An domain exception arising from the map API
data MapException = MapException Text
  deriving (Show)

instance Exception MapException

-- | Make an GET API call
callGet :: (FromJSON a, MonadIO m, MonadThrow m) => MapApi -- ^ The map API context
  -> Text -- ^ The API call
  -> [(Text, Text)]-- ^ The arguments
  -> m a
callGet api method args = do
  let args' = map (\(k, v) -> (encodeUtf8 k, Just $ encodeUtf8 v)) $ ("key", apiKey api):args
  req <- parseRequest $ unpack $ apiBase api <> "/" <> method <> "/json"
  let req' = setRequestQueryString args' req
  response <- httpJSON req'
  let response' = getResponseBody response
  if responseStatus response' /= "OK" then
    throwM $ MapException ("Bad response " <> responseStatus response' <> " for " <> (pack $ show $ req'))
  else
    return $ responseResults response'
