{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE InstanceSigs #-}
{-|
Module      : Geo.Feature
Description : A feature is some sort of geographical entity with some meaning attached to it.
Copyright   : (c) Doug Palmer, 2026
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}

module Geo.Feature (
    Feature(..)
  , SimpleFeature
  -- * GeoJSON
  , parseGeoJSONFeature
  , toGeoJSONFeature
  , toGeoJSONFeatureEncoding
  , readGeoJSONFeature
) where

import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Encoding (list, pair)
import Data.Aeson.Types (Parser, parseEither, parseFail, unexpected)
import Data.Attributes
import qualified Data.ByteString.Lazy as LB
import Data.Description
import Data.Localised
import qualified Data.Set as S
import Data.Text (Text)
import Geo.Geometry

data (Geo a) => Feature a = Feature {
    featureID :: Text -- ^ The feature identifier
  , featureName :: Maybe (Localised TaggedText) -- ^ An optional (localised) name for the feature
  , featureDescription :: Maybe Description -- ^ Additional descriptive information
  , featureAttributes :: Maybe Attributes -- ^ Feature attributes
  , featureBoundingBox :: Maybe BoundingBox -- ^ An explicit bounding box
  , featureGeometry :: Maybe a -- ^ The geometry associated with the feature (Nothing if this feature just consists of subfeatures)
  , featureSubFeatures :: [Feature a] -- ^ Any sub-features
} deriving (Show)

instance (Geo a, FromJSON a) => FromJSON (Feature a) where
  parseJSON (Object v) = do
    id' <- v .: "id"
    name' <- v .:? "name"
    description' <- v .:? "description"
    attributes' <- v .:? "attributes"
    bbox' <- v .:? "bbox"
    geometry' <- v .:? "geometry"
    features' <-  v .:? "features" .!= []
    return $ Feature id' name' description' attributes' bbox' geometry' features'
  parseJSON v = unexpected v

instance (Geo a, ToJSON a) => ToJSON (Feature a) where
  toJSON (Feature id' name' description' attributes' bbox' geometry' features') =
    object [
        "id" .= id'
      , "name" .= name'
      , "description" .= description'
      , "attributes" .= attributes'
      , "bbox" .= bbox'
      , "geometry" .= geometry'
      , "features" .= (if null features' then Nothing else Just features')
      ]
  toEncoding (Feature id' name' description' attributes' bbox' geometry' features') =
    pairs $
        "id" .= id'
      <> "name" .?= name'
      <> "description" .?= description'
      <> "attributes" .?= attributes'
      <> "bbox" .?= bbox'
      <> "geometry" .?= geometry'
      <> "features" .?= (if null features' then Nothing else Just features')

instance (Geo a, NFData a) => NFData (Feature a) where
  rnf feature =
    featureID feature `deepseq`
    featureName feature `deepseq`
    featureDescription feature `deepseq`
    featureAttributes feature `deepseq`
    featureBoundingBox feature `deepseq`
    featureGeometry feature `deepseq`
    featureSubFeatures feature `deepseq` ()

instance  {-# OVERLAPPING #-} (Geo a) => Geo (Feature a) where
  centroid f = centroidFromGeometries (points f)
  points f = foldl' (\pts -> \f' -> pts `S.union` points f') (maybe S.empty points (featureGeometry f)) (featureSubFeatures f)
  remap f feature = feature {
    featureBoundingBox = (remap f) <$> featureBoundingBox feature
    , featureGeometry = (remap f) <$> featureGeometry feature
    , featureSubFeatures = map (remap f) (featureSubFeatures feature)
    }
  isMultiGeometry f = maybe ((not $ null fs) && all isMultiGeometry fs) (\g -> all isMultiGeometry fs && isMultiGeometry g) (featureGeometry f) where fs = featureSubFeatures f
  isClosedGeometry f = maybe ((not $ null fs) && all isClosedGeometry fs) (\g -> all isClosedGeometry fs && isClosedGeometry g) (featureGeometry f) where fs = featureSubFeatures f

-- | Common feature type used by GeoJSON
type SimpleFeature = Feature SimpleGeometry

-- | Aeson JSON parser for a GeoJSON feature
parseGeoJSONFeature :: Value -> Parser SimpleFeature
parseGeoJSONFeature (Object v) = do
  type' <- (v .: "type") :: (Parser Text)
  id' <- v .: "id"
  name' <- v .:? "name"
  description' <- v .:? "description"
  attributes' <- v .:? "properties"
  bbox' <- v .:? "bbox"
  geometry' <- v .:? "geometry"
  features' <- v .:? "features" .!= []
  features'' <- mapM parseGeoJSONFeature features'
  case (type', geometry', features'') of
    ("Feature", Just _, []) -> return $ Feature id' name'  description' attributes' bbox' geometry' []
    ("FeatureCollection", Nothing, features''') -> return $ Feature id' name' description' attributes' bbox' Nothing features'''
    _ -> parseFail "Invalid GeoJSON feature"
parseGeoJSONFeature v = unexpected v

toGeoJSONFeature :: SimpleFeature -> Value
toGeoJSONFeature (Feature id' name' description' attributes' bbox' (Just geometry') []) = object [
    "type" .= ("Feature" :: Text)
  , "id" .= id'
  , "name" .= name'
  , "description" .= description'
  , "properties" .= attributes'
  , "bbox" .= (toGeoJSONBoundingBox <$> bbox')
  , "geometry" .= toGeoJSONSimpleGeometry geometry'
  ]
toGeoJSONFeature (Feature id' name' description' attributes' bbox' Nothing features') = object [
    "type" .= ("FeatureCollection" :: Text)
  , "id" .= id'
  , "name" .= name'
  , "description" .= description'
  , "properties" .= attributes'
  , "bbox" .= (toGeoJSONBoundingBox <$> bbox')
  , "features" .= (toJSONList $ map toGeoJSONFeature features')
  ]
toGeoJSONFeature v = error ("Can't encode feature as GeoJSON " ++ show v)

toGeoJSONFeatureEncoding :: Feature SimpleGeometry -> Encoding
toGeoJSONFeatureEncoding (Feature id' name' description' attributes' bbox' (Just geometry') []) = pairs $
     "type" .= ("Feature" :: Text)
  <> "id" .= id'
  <> "name" .?= name'
  <> "description" .?= description'
  <> "properties" .?= attributes'
  <> maybe mempty (\bb -> pair "bbox" (toGeoJSONBoundingBoxEncoding bb)) bbox'
  <> pair "geometry" (toGeoJSONSimpleGeometryEncoding geometry')
toGeoJSONFeatureEncoding (Feature id' name' description' attributes' bbox' Nothing features') = pairs $
     "type" .= ("FeatureCollection" :: Text)
  <> "id" .= id'
  <> "name" .?= name'
  <> "description" .?= description'
  <> "properties" .?= attributes'
  <> maybe mempty (\bb -> pair "bbox" (toGeoJSONBoundingBoxEncoding bb)) bbox'
  <> pair "features" (list toGeoJSONFeatureEncoding features')
toGeoJSONFeatureEncoding v = error ("Can't encode feature as GeoJSON " ++ show v)

-- | Read a GeoJSON feature from bytes
readGeoJSONFeature :: LB.ByteString -> Either String SimpleFeature
readGeoJSONFeature bytes' = either Left (parseEither parseGeoJSONFeature) (eitherDecode bytes')
