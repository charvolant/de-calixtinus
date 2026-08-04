{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Data.Geo.GeoJSON
Description : Parse GeoJSON data
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

See https://datatracker.ietf.org/doc/html/rfc7946
-}
module Geo.GeoJSON where

import Data.Aeson
import Data.Aeson.Encoding (list, pair)
import Data.Aeson.Types (Parser, parseFail, unexpected, parseEither)
import qualified Data.ByteString.Lazy as LB
import Data.Default.Class
import Data.Text (Text)
import Geo.Feature
import Geo.Geometry
import Geo.LatLong

--   GeoJSON ponts are arrays of either [long, lat] or [long, lat, elevation] with an assumed WGS84 SRS.
parseGJPoint :: Value -> Parser LatLong
parseGJPoint v@(Array _) = do
  v' <- parseJSON v
  case v' of
    [long', lat'] -> return $ LatLong lat' long' Nothing def
    [long', lat', elev'] -> return $ LatLong lat' long' (Just elev') def
    _ -> parseFail ("Invalid GeoJSON point " ++ show v)
parseGJPoint v = unexpected v

-- Outpuit a GeoJSON point
toGJPoint :: LatLong -> Value
toGJPoint (LatLong lat' long' Nothing _) = toJSONList [long', lat']
toGJPoint (LatLong lat' long' (Just elev') _) = toJSONList [long', lat', elev']

-- GeoJSON bounding boxes are either 4- or 6-digit bounding arrays
parseGJBoundingBox :: Value -> Parser BoundingBox
parseGJBoundingBox v@(Array _) = do
  v' <- parseJSON v
  case v' of
    [swlong', swlat', nelong', nelat'] -> return $ BoundingBox
      (LatLong swlat' swlong' Nothing def)
      (LatLong nelat' nelong' Nothing def)
    [swlong', swlat', swelev', nelong', nelat', neelev'] -> return $ BoundingBox
      (LatLong swlat' swlong' (Just swelev') def)
      (LatLong nelat' nelong' (Just neelev') def)
    _ -> parseFail ("Invalid GeoJSON bounding box " ++ show v)
parseGJBoundingBox v = unexpected v

maybeParse :: (Value -> Parser a) -> Maybe Value ->  Parser (Maybe a)
maybeParse _ Nothing = return Nothing
maybeParse p (Just v) = Just <$> (p v)

maybeEncodePair :: Key -> (a -> Encoding) -> Maybe a -> Series
maybeEncodePair _field _enc Nothing = mempty
maybeEncodePair field enc (Just v) = pair field (enc v)

toGJBoundingBox :: BoundingBox -> Value
toGJBoundingBox (BoundingBox (LatLong swlat' swlong' Nothing _) (LatLong nelat' nelong' Nothing _)) = toJSONList [swlong', swlat', nelong', nelat']
toGJBoundingBox (BoundingBox (LatLong swlat' swlong' (Just swelev') _) (LatLong nelat' nelong' (Just neelev') _)) = toJSONList [swlong', swlat', swelev', nelong', nelat', neelev']
toGJBoundingBox v = error ("Ivalid bounding box " ++ show v)

parseGJGeometry :: Value -> Parser SimpleGeometry
parseGJGeometry (Object v) = do
  type' <- v .: "type"
  bbox' <- v .:? "bbox"
  case type' :: Text of
    "Point" -> do
      cs  <- v .: "coordinates"
      ll <- parseGJPoint cs
      return $ Point bbox' ll
    "MultiPoint" -> do
      cs  <- v .: "coordinates"
      lls <- mapM parseGJPoint cs
      return $ MultiPoint bbox' lls
    "LineString" -> do
      cs  <- v .: "coordinates"
      lls <- mapM parseGJPoint cs
      return $ LineString bbox' lls
    "MultiLineString" -> do
      cs  <- v .: "coordinates"
      llls <- mapM (mapM parseGJPoint) cs
      return $ MultiLineString bbox' llls
    "Polygon" -> do
      cs  <- v .: "coordinates"
      lls <- mapM parseGJPoint cs
      return $ Polygon bbox' lls
    "MultiPolygon" -> do
      cs  <- v .: "coordinates"
      llls <- mapM (mapM parseGJPoint) cs
      return $ MultiPolygon bbox' llls
    "GeometryCollection" -> do
      gs  <- v .: "geometries"
      gs' <- mapM parseGJGeometry gs
      return $ GeometryCollection bbox' gs'
    _ -> parseFail ("Invalid geometry " ++ show v)
parseGJGeometry v = unexpected v

toGJGeometry :: SimpleGeometry -> Value
toGJGeometry  (Point bbox pt) = object [ "type" .= ("Point" :: Text), "bbox" .= bbox, "coordinates" .= toGJPoint pt ]
toGJGeometry  (MultiPoint bbox pts) = object [ "type" .= ("MultiPoint" :: Text), "bbox" .= bbox, "coordinates" .= map toGJPoint pts ]
toGJGeometry  (LineString bbox pts) = object [ "type" .= ("LineString" :: Text), "bbox" .= bbox, "coordinates" .= map toGJPoint pts ]
toGJGeometry  (MultiLineString bbox lns) = object [ "type" .= ("MultiLineString" :: Text), "bbox" .= bbox, "coordinates" .= map (map toGJPoint) lns ]
toGJGeometry  (Polygon bbox pts) = object [ "type" .= ("Polygon" :: Text), "bbox" .= bbox, "coordinates" .=map toGJPoint pts ]
toGJGeometry  (MultiPolygon bbox lns) = object [ "type" .= ("MultiPolygon" :: Text), "bbox" .= bbox, "coordinates" .=  map (map toGJPoint) lns ]
toGJGeometry  (GeometryCollection bbox gs) = object [ "type" .= ("GeometryCollection" :: Text), "bbox" .= bbox, "geometries" .= map toGJGeometry gs ]

toGJEncoding :: SimpleGeometry -> Encoding
toGJEncoding  (Point bbox pt) = pairs $ "type" .= ("Point" :: Text) <> "bbox" .?= bbox <> "coordinates" .= toGJPoint pt
toGJEncoding  (MultiPoint bbox pts) = pairs $ "type" .= ("MultiPoint" :: Text) <> "bbox" .?= bbox <> "coordinates" .= map toGJPoint pts
toGJEncoding  (LineString bbox pts) = pairs $ "type" .= ("LineString" :: Text) <> "bbox" .?= bbox <> "coordinates" .= map toGJPoint pts
toGJEncoding  (MultiLineString bbox lns) = pairs $ "type" .= ("MultiLineString" :: Text) <> "bbox" .?= bbox <> "coordinates" .= map (map toGJPoint) lns
toGJEncoding  (Polygon bbox pts) = pairs $ "type" .= ("Polygon" :: Text) <> "bbox" .?= bbox <> "coordinates" .= map toGJPoint pts
toGJEncoding  (MultiPolygon bbox lns) = pairs $ "type" .= ("MultiPolygon" :: Text) <> "bbox" .?= bbox <> "coordinates" .=  map (map toGJPoint) lns
toGJEncoding  (GeometryCollection bbox gs) = pairs $ "type" .= ("GeometryCollection" :: Text) <> "bbox" .?= bbox <> pair "geometries" (list toGJEncoding gs)

parseGJFeature :: Value -> Parser (Feature SimpleGeometry)
parseGJFeature (Object v) = do
  type' <- (v .: "type") :: (Parser Text)
  id' <- v .: "id"
  name' <- v .:? "name"
  description' <- v .:? "description"
  attributes' <- v .:? "properties"
  bbox' <- (v .:? "bbox") >>= (maybeParse parseGJBoundingBox)
  geometry' <- v .:? "geometry" >>= (maybeParse parseGJGeometry)
  features' <- v .:? "features" .!= []
  features'' <- mapM parseGJFeature features'
  case (type', geometry', features'') of
    ("Feature", Just _, []) -> return $ Feature id' name'  description' attributes' bbox' geometry' []
    ("FeatureCollection", Nothing, features''') -> return $ Feature id' name' description' attributes' bbox' Nothing features'''
    _ -> parseFail "Invalid GeoJSON feature"
parseGJFeature v = unexpected v

toGJFeature :: Feature SimpleGeometry -> Value
toGJFeature (Feature id' name' description' attributes' bbox' (Just geometry') []) = object [
    "type" .= ("Feature" :: Text)
  , "id" .= id'
  , "name" .= name'
  , "description" .= description'
  , "properties" .= attributes'
  , "bbox" .= (toGJBoundingBox <$> bbox')
  , "geometry" .= toGJGeometry geometry'
  ]
toGJFeature (Feature id' name' description' attributes' bbox' Nothing features') = object [
    "type" .= ("FeatureCollection" :: Text)
  , "id" .= id'
  , "name" .= name'
  , "description" .= description'
  , "properties" .= attributes'
  , "bbox" .= (toGJBoundingBox <$> bbox')
  , "features" .= (toJSONList $ map toGJFeature features')
  ]
toGJFeature v = error ("Can't encode feature as GeoJSON " ++ show v)

toGJFeatureEncoding :: Feature SimpleGeometry -> Encoding
toGJFeatureEncoding (Feature id' name' description' attributes' bbox' (Just geometry') []) = pairs $
     "type" .= ("Feature" :: Text)
  <> "id" .= id'
  <> "name" .?= name'
  <> "description" .?= description'
  <> "properties" .?= attributes'
  <> maybeEncodePair "bbox" (toEncoding . toGJBoundingBox) bbox'
  <> pair "geometry" (toGJEncoding geometry')
toGJFeatureEncoding (Feature id' name' description' attributes' bbox' Nothing features') = pairs $
     "type" .= ("FeatureCollection" :: Text)
  <> "id" .= id'
  <> "name" .?= name'
  <> "description" .?= description'
  <> "properties" .?= attributes'
  <> maybeEncodePair "bbox" (toEncoding . toGJBoundingBox) bbox'
  <> pair "features" (list toGJFeatureEncoding features')
toGJFeatureEncoding v = error ("Can't encode feature as GeoJSON " ++ show v)

-- | Read a GeoJSON feature from bytes
readGeoJSONFeature :: LB.ByteString -> Either String (Feature SimpleGeometry)
readGeoJSONFeature bytes' = either Left (parseEither parseGJFeature) (eitherDecode bytes')