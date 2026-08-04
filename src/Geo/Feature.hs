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
) where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Types (unexpected)
import Data.Attributes
import Data.Description
import Data.Localised
import qualified Data.Set as S
import Data.Text (Text)
import Geo.Geometry
import Geo.LatLong

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
  centroid :: Geo a => Feature a -> LatLong
  centroid f = centroidFromGeometries (points f)
  points f = foldl' (\pts -> \f' -> pts `S.union` points f') (maybe S.empty points (featureGeometry f)) (featureSubFeatures f)
  remap f feature = feature {
    featureBoundingBox = (remap f) <$> featureBoundingBox feature
    , featureGeometry = (remap f) <$> featureGeometry feature
    , featureSubFeatures = map (remap f) (featureSubFeatures feature)
    }
  isMultiGeometry f = maybe ((not $ null fs) && all isMultiGeometry fs) (\g -> all isMultiGeometry fs && isMultiGeometry g) (featureGeometry f) where fs = featureSubFeatures f
  isClosedGeometry :: Geo a => Feature a -> Bool
  isClosedGeometry f = maybe ((not $ null fs) && all isClosedGeometry fs) (\g -> all isClosedGeometry fs && isClosedGeometry g) (featureGeometry f) where fs = featureSubFeatures f
  