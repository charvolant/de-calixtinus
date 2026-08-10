{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-|
Module      : Geo.Geometry
Description : Points, lines and polygons constructed from latitude and longitudes
Copyright   : (c) Doug Palmer, 2026
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}

module Geo.Geometry (
  -- * Geometries
    Geo(..)
  , SimpleGeometry(..)
  -- * Bounding boxes
  , BoundedGeo(..)
  , BoundingBox(..)
  , unionBoundingBox
  -- * Utilities
  , centroidFromGeometries
  , unionMaybeBoundingBox
  -- * GeoJSON
  , toGeoJSONBoundingBox
  , toGeoJSONBoundingBoxEncoding
  , toGeoJSONSimpleGeometry
  , toGeoJSONSimpleGeometryEncoding
) where

import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Aeson
import Data.Aeson.Encoding (list, pair)
import Data.Aeson.Types (parseFail, unexpected)
import Data.Default.Class
import qualified Data.Set as S
import Data.Text (Text)
import Data.Util (maybeMax, maybeMin)
import Geo.LatLong

-- | A geo-locatable entity
class Geo a where
  -- | Compute the centroid of a the entity
  --
  --  This is the mean of the latitudes, longitudes and elevations (if present) of the entity, according to a common SRS.
  --  If there is no elevation data, then the centroid elevation is also @Nothing@
  centroid :: a -> LatLong
  -- | The points that make up the geometry
  --
  points :: a -> S.Set LatLong
  -- | Remap the lat-longs of this geometry without changing the shape of the geometry
  --
  --   Remap is intended to keep the rough shape of whatever is being remapped.
  --   If you twist things or do other horrible transformations to the geometry, then don't be surprised if things
  --   turn out poorly.
  remap :: (LatLong -> LatLong) -- ^ The remapping function
    -> a -- ^ The source geometry
    -> a -- ^ The remapped geometry
  -- | Is this a multi-geometry?
  --
  --   Multi-geometries are collections of lines or polygons of a similar type 
  isMultiGeometry :: a -> Bool
  -- | Is this a closed object?
  --
  --   Closed objects are polygons or collections of polygons
  isClosedGeometry :: a -> Bool

instance Geo LatLong where
  centroid :: LatLong -> LatLong
  centroid ll = ll
  points ll = S.singleton ll
  remap :: (LatLong -> LatLong) -> LatLong -> LatLong
  remap f ll = f ll
  isMultiGeometry _ = False
  isClosedGeometry _ = False

-- | Get a centoid from some sort of collection geometries
centroidFromGeometries :: (Geo a, Foldable t) => t a -> LatLong
centroidFromGeometries gs = let
      (slats, slongs, selevs, len, len', srs'') = foldl'
        (\(lats, longs, elevs, slen, slen', _srs) -> \g' ->
          let
            (LatLong lat lon elev srs') = centroid g'
          in
            (lats + lat, longs + lon, elevs + maybe 0.0 id elev, slen + 1, slen' + maybe 0 (const 1) elev, srs')
        )
        (0.0, 0.0, 0.0, 0, 0, def)
        gs
    in
      LatLong
        (if len == 0 then 0.0 else (slats / len))
        (if len == 0 then 0.0 else (slongs / len))
        (if len' == 0 then Nothing else Just (selevs / len'))
        srs''

instance {-# OVERLAPPING #-} (Foldable t, Functor t, Geo a) => Geo (t a) where
  centroid gl = centroidFromGeometries gl
  points gl = foldl' (\pts -> \g -> pts `S.union` points g) S.empty gl
  remap f gl = fmap (remap f) gl
  isMultiGeometry gl = (not $ null gl) && all isMultiGeometry gl
  isClosedGeometry gl = (not $ null gl) && all isClosedGeometry gl
  
-- | The bounding box for a geometry or feature.
--
--   A bounding box consists of two lat/longs showing the South-West and North-East corners, respectively, with
--   an optional minimum/maximum elevation
data BoundingBox = BoundingBox LatLong LatLong
  deriving (Eq, Show, Generic)

-- | A class indicating that an geolocated object has a bounding box.
--
--   Bounding boxes may be explcitly stated or computed from the underlying object
class BoundedGeo a where
  -- | Get the object's bounding box.
  --
  --   If there object has an explicit bounding box, then use that, otherwise compute it
  --   If the geometry is empty, then there may not be a bounding box
  --
  --   By default, this simply computes the bounding box using `computeBoundingBox`.
  --   Instances may override this to
  boundingBox :: a -> Maybe BoundingBox
  boundingBox v = computeBoundingBox v
  -- | Compute the object's bounding box
  --
  --   Ignore any supplied bounding box and compute the bounding box based on the geometry of the underlying object
  --    If the geometry is empty, then there may not be a bounding box
  computeBoundingBox :: a -> Maybe BoundingBox
  -- | Add an explicit bounding box to the object
  --
  --   By default, this does nothing.
  withBoundingBox :: a -> Maybe BoundingBox -> a
  withBoundingBox v _ = v
  -- | Add an computed bounding box to the object
  --
  --   By default, this does nothing.
  withComputedBoundingBox :: a -> a
  withComputedBoundingBox v = withBoundingBox v (computeBoundingBox v)

instance FromJSON BoundingBox where
  -- Named object style
  parseJSON (Object v) = do
    sw' <- v .: "sw"
    ne' <- v .: "ne"
    return $ BoundingBox sw' ne'
  -- GeoJSON style
  parseJSON v@(Array _) = do
    v' <- parseJSON v
    case v' of
      [swlong', swlat', nelong', nelat'] -> return $ BoundingBox
        (LatLong swlat' swlong' Nothing def)
        (LatLong nelat' nelong' Nothing def)
      [swlong', swlat', swelev', nelong', nelat', neelev'] -> return $ BoundingBox
        (LatLong swlat' swlong' (Just swelev') def)
        (LatLong nelat' nelong' (Just neelev') def)
      _ -> parseFail ("Invalid GeoJSON bounding box " ++ show v)
  parseJSON v = unexpected v

instance ToJSON BoundingBox where
  toJSON (BoundingBox sw ne) = object [ "sw" .= sw, "ne" .= ne ]
  toEncoding (BoundingBox sw ne) = pairs $  "sw" .= sw <> "ne" .= ne

-- | Convert a Bounding into GeoJSON format
toGeoJSONBoundingBox :: BoundingBox -> Value
toGeoJSONBoundingBox (BoundingBox (LatLong swlat' swlong' Nothing _) (LatLong nelat' nelong' Nothing _)) = toJSONList [swlong', swlat', nelong', nelat']
toGeoJSONBoundingBox (BoundingBox (LatLong swlat' swlong' (Just swelev') _) (LatLong nelat' nelong' (Just neelev') _)) = toJSONList [swlong', swlat', swelev', nelong', nelat', neelev']
toGeoJSONBoundingBox v = error ("Ivalid bounding box " ++ show v)

-- | Convert a Bounding into GeoJSON format
toGeoJSONBoundingBoxEncoding :: BoundingBox -> Encoding
toGeoJSONBoundingBoxEncoding bbox = toEncoding $ toGeoJSONBoundingBox bbox

instance NFData BoundingBox

instance Geo BoundingBox where
  centroid (BoundingBox sw ne) = centroid [sw, ne]
  points (BoundingBox (LatLong swlat' swlong' swelev' swsrs') (LatLong nelat' nelong' neelev' nesrs')) =
    S.fromList [
        (LatLong swlat' swlong' swelev' swsrs')
      , (LatLong nelat' swlong' swelev' swsrs')
      , (LatLong nelat' nelong' neelev' nesrs')
      , (LatLong swlat' nelong' neelev' nesrs')
      ]
  remap f (BoundingBox sw' ne') = BoundingBox (f sw') (f ne')
  isMultiGeometry _ = False 
  isClosedGeometry _ = True

-- | Create the union of two bounding boxes.
--
--   The union is the bounding box that covers both
unionBoundingBox :: BoundingBox -> BoundingBox -> BoundingBox
unionBoundingBox (BoundingBox sw1 ne1) (BoundingBox sw2 ne2) =
  BoundingBox
    (LatLong
      (min (latitude sw1) (latitude sw2))
      (min (longitude sw1) (longitude sw2))
      (maybeMin (elevation sw1) (elevation sw2))
      (srs sw1)
      )
    (LatLong
      (max (latitude ne1) (latitude ne2))
      (max (longitude ne1) (longitude ne2))
      (maybeMax (elevation ne1) (elevation ne2))
      (srs ne1)
      )

-- | Create the union of two optional bounding boxes.
--
--   The union is the bounding box that covers both, if both are available, otherwise any existing bounding box is used
unionMaybeBoundingBox :: Maybe BoundingBox -> Maybe BoundingBox -> Maybe BoundingBox
unionMaybeBoundingBox Nothing Nothing = Nothing
unionMaybeBoundingBox Nothing v@(Just _) = v
unionMaybeBoundingBox v@(Just _) Nothing = v
unionMaybeBoundingBox (Just bb1) (Just bb2) = Just (unionBoundingBox bb1 bb2)

instance BoundedGeo LatLong where
  computeBoundingBox ll = Just $ BoundingBox ll ll

instance {-# OVERLAPPING #-} (BoundedGeo a, Foldable t) => BoundedGeo (t a) where
  boundingBox gs = foldr (\v -> \bb -> unionMaybeBoundingBox bb (boundingBox v)) Nothing gs
  computeBoundingBox gs = foldr (\v -> \bb -> unionMaybeBoundingBox bb (computeBoundingBox v)) Nothing gs


-- | Simple geometry primitives with an optional explicit bounding box
data SimpleGeometry =
  -- | A single point
    Point (Maybe BoundingBox) LatLong
  -- | A collection of unconnected points
  | MultiPoint (Maybe BoundingBox) [LatLong]
  -- | A collection of connected points, with each point connected to the previous point
  | LineString (Maybe BoundingBox) [LatLong]
  -- | A collection of line strings
  | MultiLineString (Maybe BoundingBox) [[LatLong]]
  -- | A collection of connected points, with each point connected to the previous point and the last point connected to the first point
  | Polygon (Maybe BoundingBox) [LatLong]
  -- | A collection of polygons
  | MultiPolygon (Maybe BoundingBox) [[LatLong]]
  -- | A collection of arbitrary geometries
  | GeometryCollection (Maybe BoundingBox) [SimpleGeometry]
  deriving (Eq, Show)

instance BoundedGeo SimpleGeometry where
  boundingBox g@(Point bbox _pt) = bbox <|> computeBoundingBox g
  boundingBox g@(MultiPoint bbox _pts) = bbox <|> computeBoundingBox g
  boundingBox g@(LineString bbox _pts) = bbox <|> computeBoundingBox g
  boundingBox g@(MultiLineString bbox _pts) = bbox <|> computeBoundingBox g
  boundingBox g@(Polygon bbox _pts) = bbox <|> computeBoundingBox g
  boundingBox g@(MultiPolygon bbox _pts) = bbox <|> computeBoundingBox g
  boundingBox g@(GeometryCollection bbox _gs) = bbox <|> computeBoundingBox g

  computeBoundingBox (Point _bbox pt) = computeBoundingBox pt
  computeBoundingBox (MultiPoint _bbox pts) = computeBoundingBox pts
  computeBoundingBox (LineString _bbox pts) = computeBoundingBox pts
  computeBoundingBox (MultiLineString _bbox pts) = computeBoundingBox pts
  computeBoundingBox (Polygon _bbox pts) = computeBoundingBox pts
  computeBoundingBox (MultiPolygon _bbox pts) = computeBoundingBox pts
  computeBoundingBox (GeometryCollection _bbox gs) = computeBoundingBox gs
  
  withBoundingBox (Point _bbox pt) bbox = Point bbox pt
  withBoundingBox (MultiPoint _bbox pts) bbox = MultiPoint bbox pts
  withBoundingBox (LineString _bbox pts) bbox = LineString bbox pts
  withBoundingBox (MultiLineString _bbox pts) bbox = MultiLineString bbox pts
  withBoundingBox (Polygon _bbox pts) bbox = Polygon bbox pts
  withBoundingBox (MultiPolygon _bbox pts) bbox = MultiPolygon bbox pts
  withBoundingBox (GeometryCollection _bbox gs) bbox = GeometryCollection bbox gs
  

instance FromJSON SimpleGeometry where
  parseJSON (Object v) = do
    type' <- v .: "type"
    bbox' <- v .:? "bbox"
    case type' :: Text of
      "Point" -> do
        pt <- v .: "coordinates"
        return $ Point bbox' pt
      "MultiPoint" -> do
        pts <- v .: "coordinates"
        return $ MultiPoint bbox' pts
      "LineString" -> do
        ln <- v .: "coordinates"
        return $ LineString bbox' ln
      "MultiLineString" -> do
        lns <- v .: "coordinates"
        return $ MultiLineString bbox' lns
      "Polygon" -> do
        pg <- v .: "coordinates"
        return $ Polygon bbox' pg
      "MultiPolygon" -> do
        pgs <- v .: "coordinates"
        return $ MultiPolygon bbox' pgs
      "GeometryCollection" -> do
        gs <- v .: "geometries"
        return $ GeometryCollection bbox' gs
      t -> parseFail ("Unexpected geometry type " ++ show t)
  parseJSON v = unexpected v

toJSONGeometry :: (LatLong -> Value) -> (BoundingBox -> Value) -> (SimpleGeometry -> Value) -> SimpleGeometry -> Value
toJSONGeometry llv bbv _gv (Point bbox pt) = object [ "type" .= ("Point" :: Text), "bbox" .=  (bbv <$> bbox), "coordinates" .= llv pt ]
toJSONGeometry llv bbv _gv (MultiPoint bbox pts) = object [ "type" .= ("MultiPoint" :: Text), "bbox" .= (bbv <$> bbox), "coordinates" .= map llv pts ]
toJSONGeometry llv bbv _gv (LineString bbox ln) = object [ "type" .= ("LineString" :: Text), "bbox" .= (bbv <$> bbox), "coordinates" .= map llv ln ]
toJSONGeometry llv bbv _gv (MultiLineString bbox lns) = object [ "type" .= ("MultiLineString" :: Text), "bbox" .= (bbv <$> bbox), "coordinates" .= map (map llv) lns ]
toJSONGeometry llv bbv _gv (Polygon bbox pg) = object [ "type" .= ("Polygon" :: Text), "bbox" .= (bbv <$> bbox), "coordinates" .= map llv pg ]
toJSONGeometry llv bbv _gv (MultiPolygon bbox pgs) = object [ "type" .= ("MultiPolygon" :: Text) , "bbox" .= (bbv <$> bbox), "coordinates" .=  map (map llv) pgs ]
toJSONGeometry _llv bbv gv (GeometryCollection bbox gs) = object [ "type" .= ("GeometryCollection" :: Text), "bbox" .= (bbv <$> bbox), "geometries" .= map gv gs ]

toJSONGeometryEncoding :: (LatLong -> Encoding) -> (BoundingBox -> Value) -> (SimpleGeometry -> Encoding) -> SimpleGeometry -> Encoding
toJSONGeometryEncoding llv bbv _gv (Point bbox pt) = pairs $ "type" .= ("Point" :: Text) <> "bbox" .?=  (bbv <$> bbox) <> pair "coordinates" (llv pt)
toJSONGeometryEncoding llv bbv _gv (MultiPoint bbox pts) = pairs $ "type" .= ("MultiPoint" :: Text) <> "bbox" .?= (bbv <$> bbox) <> pair "coordinates" (list llv pts)
toJSONGeometryEncoding llv bbv _gv (LineString bbox ln) = pairs $ "type" .= ("LineString" :: Text) <> "bbox" .?= (bbv <$> bbox) <> pair "coordinates" (list llv ln)
toJSONGeometryEncoding llv bbv _gv (MultiLineString bbox lns) = pairs $ "type" .= ("MultiLineString" :: Text) <> "bbox" .?= (bbv <$> bbox) <> pair "coordinates" (list (list llv) lns)
toJSONGeometryEncoding llv bbv _gv (Polygon bbox pg) = pairs $ "type" .= ("Polygon" :: Text) <> "bbox" .?= (bbv <$> bbox) <> pair "coordinates" (list llv pg)
toJSONGeometryEncoding llv bbv _gv (MultiPolygon bbox pgs) = pairs $ "type" .= ("MultiPolygon" :: Text)  <> "bbox" .?= (bbv <$> bbox) <> pair "coordinates" (list (list llv) pgs)
toJSONGeometryEncoding _llv bbv gv (GeometryCollection bbox gs) = pairs $ "type" .= ("GeometryCollection" :: Text) <> "bbox" .?= (bbv <$> bbox) <> pair "geometries" (list gv gs)

instance ToJSON SimpleGeometry where
  toJSON g = toJSONGeometry toJSON toJSON toJSON g
  toEncoding :: SimpleGeometry -> Encoding
  toEncoding g = toJSONGeometryEncoding toEncoding toJSON toEncoding g

toGeoJSONSimpleGeometry :: SimpleGeometry -> Value
toGeoJSONSimpleGeometry g = toJSONGeometry toGeoJSONLatLong toGeoJSONBoundingBox toGeoJSONSimpleGeometry g

toGeoJSONSimpleGeometryEncoding :: SimpleGeometry -> Encoding
toGeoJSONSimpleGeometryEncoding g = toJSONGeometryEncoding (toEncoding . toGeoJSONLatLong) toGeoJSONBoundingBox toGeoJSONSimpleGeometryEncoding g

instance Geo SimpleGeometry where
  centroid (Point _bbox pt) = pt
  centroid (MultiPoint _bbox pts) = centroid pts
  centroid (LineString _bbox ln) = centroid ln
  centroid (MultiLineString _bbox lns) = centroid lns
  centroid (Polygon _bbox pg) = centroid pg
  centroid (MultiPolygon _bbox pgs) = centroid pgs
  centroid (GeometryCollection _bbox gs) = centroid gs

  points (Point _bbox pt) = S.singleton pt
  points (MultiPoint _bbox pts) = S.fromList pts
  points (LineString _bbox ln) = S.fromList ln
  points (MultiLineString _bbox lns) = S.unions $ map S.fromList lns
  points (Polygon _bbox pg) = S.fromList pg
  points (MultiPolygon _bbox pgs) = S.unions $ map S.fromList  pgs
  points (GeometryCollection _bbox gs) = points gs

  remap f (Point bbox pt) = Point ((remap f) <$> bbox) (remap f pt)
  remap f (MultiPoint bbox pts) = MultiPoint ((remap f) <$> bbox) (remap f pts)
  remap f (LineString bbox ln) = LineString ((remap f) <$> bbox) (remap f ln)
  remap f (MultiLineString bbox lns) = MultiLineString ((remap f) <$> bbox) (remap f lns)
  remap f (Polygon bbox pg) = Polygon ((remap f) <$> bbox) (remap f pg)
  remap f (MultiPolygon bbox pgs) = MultiPolygon ((remap f) <$> bbox) (remap f pgs)
  remap f (GeometryCollection bbox gs) = GeometryCollection ((remap f) <$> bbox) (remap f gs)

  isMultiGeometry (Point _bbox _pt) = False
  isMultiGeometry (MultiPoint _bbox _pts) = True
  isMultiGeometry (LineString _bbox _ln) = False
  isMultiGeometry (MultiLineString _bbox _lns) = True
  isMultiGeometry (Polygon _bbox _pg) = False
  isMultiGeometry (MultiPolygon _bbox _pgs) = True
  isMultiGeometry (GeometryCollection _bbox gs) = (not $ null gs) && all isMultiGeometry gs

  isClosedGeometry (Point _bbox _pt) = False
  isClosedGeometry (MultiPoint _bbox _pts) = False
  isClosedGeometry (LineString _bbox _ln) = False
  isClosedGeometry (MultiLineString _bbox _lns) = False
  isClosedGeometry (Polygon _bbox _pg) = True
  isClosedGeometry (MultiPolygon _bbox _pgs) = True
  isClosedGeometry (GeometryCollection _bbox gs) = (not $ null gs) && all isClosedGeometry gs
