{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module GeoJSONSpec(testGeoJSON) where

import Test.HUnit
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Types (parseEither)
import Data.Attributes
import Data.Default.Class
import Data.Description
import Data.Localised
import qualified Data.Map as M
import Data.Maybe
import Geo.Feature
import Geo.GeoJSON
import Geo.Geometry
import Geo.LatLong
import Text.Read (readEither)
import TestUtils
import Text.RawString.QQ

testGeoJSON :: Test
testGeoJSON = TestList [
    TestLabel "LatLong GeoJSON" testLatLongGeoJSON
  , TestLabel "BoundingBox GeoJSON" testBoundingBoxGeoJSON
  , TestLabel "Geometry GeoJSON" testGeometryGeoJSON
  , TestLabel "Feature GeoJSON" testFeatureGeoJSON
  ]


ll1 = LatLong 10.2 12.5 Nothing def

lls1 = [r|[12.5, 10.2]|] :: LB.ByteString

llv1 = maybe (error "Can't decode lls1") id ((decode lls1) :: Maybe Value)

ll2 = LatLong (-54.9) 140.2 (Just 105.0) def

lls2 = [r|[140.2, -54.9, 105]|] :: LB.ByteString

llv2 = maybe (error "Can't decode lls2") id ((decode lls2) :: Maybe Value)

testLatLongGeoJSON = TestList [
  testLatLongGeoJSON1, testLatLongGeoJSON2,  testLatLongGeoJSON3, testLatLongGeoJSON4
  ]

testLatLongGeoJSON1 = TestCase (assertEqualBSStripped "Test LatLong GeoJSON 1" lls1 (encode $ toGJPoint ll1))

testLatLongGeoJSON2 = TestCase (assertEqualBSStripped "Test LatLong GeoJSON 2" lls2 (encode $ toGJPoint ll2))

testLatLongGeoJSON3 = TestCase (assertEqual "Test LatLong GeoJSON 3" (Right ll1) (parseEither parseGJPoint llv1))

testLatLongGeoJSON4 = TestCase (assertEqual "Test LatLong GeoJSON 4" (Right ll2) (parseEither parseGJPoint llv2))

testBoundingBoxGeoJSON = TestList [
  testBoundingBoxGeoJSON1, testBoundingBoxGeoJSON2,  testBoundingBoxGeoJSON3, testBoundingBoxGeoJSON4
  ]

bb1 = BoundingBox (LatLong 12.3 10.2 Nothing def) (LatLong 15.2 12.2 Nothing def)

bbs1 = [r|[10.2, 12.3, 12.2, 15.2]|] :: LB.ByteString

bbv1 = maybe (error "Can't decode bbs1") id ((decode bbs1) :: Maybe Value)

bb2 = BoundingBox (LatLong 12.3 10.2 (Just 20.0) def) (LatLong 15.2 12.2 (Just 40.0) def)

bbs2 = [r|[10.2, 12.3, 20, 12.2, 15.2, 40]|] :: LB.ByteString

bbv2 = maybe (error "Can't decode bbs2") id ((decode bbs2) :: Maybe Value)

testBoundingBoxGeoJSON1 = TestCase (assertEqualBSStripped "Test BoundingBox GeoJSON 1" bbs1 (encode $ toGJBoundingBox bb1))

testBoundingBoxGeoJSON2 = TestCase (assertEqualBSStripped "Test BoundingBox GeoJSON 2" bbs2 (encode $ toGJBoundingBox bb2))

testBoundingBoxGeoJSON3 = TestCase (assertEqual "Test BoundingBox GeoJSON 3" (Right bb1) (parseEither parseGJBoundingBox bbv1))

testBoundingBoxGeoJSON4 = TestCase (assertEqual "Test BoundingBox GeoJSON 4" (Right bb2) (parseEither parseGJBoundingBox bbv2))

testGeometryGeoJSON = TestList [
  testGeometryGeoJSON1, testGeometryGeoJSON2, testGeometryGeoJSON3, testGeometryGeoJSON4,
  testGeometryGeoJSON5, testGeometryGeoJSON6, testGeometryGeoJSON7, testGeometryGeoJSON8,
  testGeometryGeoJSON9, testGeometryGeoJSON10, testGeometryGeoJSON11, testGeometryGeoJSON12,
  testGeometryGeoJSON13, testGeometryGeoJSON14
  ]

g1 = Point Nothing (LatLong 44.0 (-8.56) Nothing def)

gs1 = [r|
{
  "type": "Point",
  "coordinates": [-8.56, 44]
}
|]

gv1 = maybe (error "Can't decode gv1") id ((decode gs1) :: Maybe Value)

g2 = MultiPoint Nothing [LatLong 44.0 (-8.56) Nothing def, LatLong 25.1 (-123.5) Nothing def]

gs2 = [r|
{
  "type": "MultiPoint",
  "coordinates": [[-8.56, 44], [-123.5, 25.1]]
}
|]

gv2 = maybe (error "Can't decode gv2") id ((decode gs2) :: Maybe Value)

g3 = LineString Nothing [LatLong 44.0 (-8.56) Nothing def, LatLong 25.1 (-123.5) Nothing def]

gs3 = [r|
{
  "type": "LineString",
  "coordinates": [[-8.56, 44], [-123.5, 25.1]]
}
|]

gv3 = maybe (error "Can't decode gv3") id ((decode gs3) :: Maybe Value)

g4 = MultiLineString Nothing [
    [LatLong 44.0 (-8.56) Nothing def, LatLong 25.1 (-123.5) Nothing def]
  , [LatLong 12.0 7.56 Nothing def, LatLong 11.6 8.57 (Just 22.0) def]
  ]
  
gs4 = [r|
{
  "type": "MultiLineString",
  "coordinates": [
    [[-8.56, 44], [-123.5, 25.1]],
    [[7.56, 12], [8.57, 11.6, 22]]
  ]
}
|]

gv4 = maybe (error "Can't decode gv4") id ((decode gs4) :: Maybe Value)

g5 = Polygon Nothing [
  LatLong 11.0 6.56 (Just 34) def,
  LatLong 11.6 8.57 (Just 22.0) def
  ]

gs5 = [r|
{
  "type": "Polygon",
  "coordinates": [
     [6.56, 11, 34], [8.57, 11.6, 22]
  ]
}
|]

gv5 = maybe (error "Can't decode gv5") id ((decode gs5) :: Maybe Value)

g6 = MultiPolygon Nothing [
  [
    LatLong 11.0 6.56 (Just 34) def,
    LatLong 11.6 8.57 (Just 22.0) def
  ]
  ]

gs6 = [r|
{
  "type": "MultiPolygon",
  "coordinates": [[
     [6.56, 11, 34], [8.57, 11.6, 22]
  ]]
}
|]

gv6  = maybe (error "Can't decode gv5") id ((decode gs6) :: Maybe Value)

g7 = GeometryCollection Nothing [g1, g2]

gs7 = [r|
{
  "type": "GeometryCollection",
  "geometries": [
    {
      "type": "Point",
      "coordinates": [-8.56, 44]
    },
    {
      "type": "MultiPoint",
      "coordinates": [[-8.56, 44], [-123.5, 25.1]]
    }
  ]
}
|]

gv7 = maybe (error "Can't decode gv7") id ((decode gs7) :: Maybe Value)

testGeometryGeoJSON1 = TestCase (assertEqualBSStripped "Test Geometry GeoJSON 1" gs1 (encodingToLazyByteString $ toGJEncoding g1))

testGeometryGeoJSON2 = TestCase (assertEqual "Test Geometry GeoJSON 2" (Right g1) (parseEither parseGJGeometry gv1))

testGeometryGeoJSON3 = TestCase (assertEqualBSStripped "Test Geometry GeoJSON 3" gs2 (encodingToLazyByteString $ toGJEncoding g2))

testGeometryGeoJSON4 = TestCase (assertEqual "Test Geometry GeoJSON 4" (Right g2) (parseEither parseGJGeometry gv2))

testGeometryGeoJSON5 = TestCase (assertEqualBSStripped "Test Geometry GeoJSON 5" gs3 (encodingToLazyByteString $ toGJEncoding g3))

testGeometryGeoJSON6 = TestCase (assertEqual "Test Geometry GeoJSON 6" (Right g3) (parseEither parseGJGeometry gv3))

testGeometryGeoJSON7 = TestCase (assertEqualBSStripped "Test Geometry GeoJSON 7" gs4 (encodingToLazyByteString $ toGJEncoding g4))

testGeometryGeoJSON8 = TestCase (assertEqual "Test Geometry GeoJSON 8" (Right g4) (parseEither parseGJGeometry gv4))

testGeometryGeoJSON9 = TestCase (assertEqualBSStripped "Test Geometry GeoJSON 9" gs5 (encodingToLazyByteString $ toGJEncoding g5))

testGeometryGeoJSON10 = TestCase (assertEqual "Test Geometry GeoJSON 10" (Right g5) (parseEither parseGJGeometry gv5))

testGeometryGeoJSON11 = TestCase (assertEqualBSStripped "Test Geometry GeoJSON 11" gs6 (encodingToLazyByteString $ toGJEncoding g6))

testGeometryGeoJSON12 = TestCase (assertEqual "Test Geometry GeoJSON 12" (Right g6) (parseEither parseGJGeometry gv6))

testGeometryGeoJSON13 = TestCase (assertEqualBSStripped "Test Geometry GeoJSON 13" gs7 (encodingToLazyByteString $ toGJEncoding g7))

testGeometryGeoJSON14 = TestCase (assertEqual "Test Geometry GeoJSON 14" (Right g7) (parseEither parseGJGeometry gv7))

testFeatureGeoJSON = TestList [
  testFeatureGeoJSON1, testFeatureGeoJSON2, testFeatureGeoJSON3, testFeatureGeoJSON4,
  testFeatureGeoJSON5, testFeatureGeoJSON6
  ]

a1 = Attributes $ M.fromList [("a", IntegerV 34), ("b", StringV "Prop")]

f1 = Feature 
  "F1" 
  (Just $ wildcardText "Feature 1") 
  (Just $ wildcardDescription "Feature 1 description") 
  (Just a1) 
  Nothing 
  (Just $ LineString Nothing [LatLong 13.2 45.445 Nothing def, LatLong 13.21 45.457 Nothing def])
  []
  
fs1 = [r|
{
  "type": "Feature",
  "id": "F1",
  "name": "Feature 1",
  "description": "Feature 1 description",
  "properties": {
    "a": 34,
    "b": "Prop"
  },
  "geometry": {
    "type": "LineString",
    "coordinates": [
      [ 45.445, 13.2 ],
      [ 45.457, 13.21 ]
    ]
  }
}
|] :: LB.ByteString

fv1 = maybe (error "Can't decode fv1") id ((decode fs1) :: Maybe Value)

f2 = Feature 
  "F2" 
  Nothing 
  Nothing 
  Nothing
  (Just $ BoundingBox (LatLong 7.7 (-8.9) Nothing def) (LatLong 8.3 (-8.3) Nothing def))
  (Just $ LineString Nothing [LatLong 8.27 (-8.87) Nothing def, LatLong 7.78 (-8.31) Nothing def])
  []

fs2 = [r|
{
  "type": "Feature",
  "id": "F2",
  "bbox": [ -8.9, 7.7, -8.3, 8.3 ],
  "geometry": {
    "type": "LineString",
    "coordinates": [
      [ -8.87, 8.27 ],
      [ -8.31, 7.78 ]
    ]
  }
}
|] :: LB.ByteString

fv2 = either (\e -> error ("Can't decode fv2 " ++ e)) id ((eitherDecode fs2) :: Either String Value)

f3 =   Feature
  "F3"
   Nothing
   Nothing
   Nothing
   Nothing
   Nothing
   [f1, f2]

fs3 = [r|
{
  "type": "FeatureCollection",
  "id": "F3",
  "features": [
    {
      "type": "Feature",
      "id": "F1",
      "name": "Feature 1",
      "description": "Feature 1 description",
      "properties": {
        "a": 34,
        "b": "Prop"
      },
      "geometry": {
        "type": "LineString",
        "coordinates": [
          [ 45.445, 13.2 ],
          [ 45.457, 13.21 ]
        ]
      }
    },
    {
      "type": "Feature",
      "id": "F2",
      "bbox": [ -8.9, 7.7, -8.3, 8.3 ],
      "geometry": {
        "type": "LineString",
        "coordinates": [
          [ -8.87, 8.27 ],
          [ -8.31, 7.78 ]
        ]
      }
    }
  ]
}
|] :: LB.ByteString

fv3 = either (\e -> error ("Can't decode fv3 " ++ e)) id ((eitherDecode fs3) :: Either String Value)

testFeatureGeoJSON1 = TestCase (assertEqualBSStripped "Test Feature GeoJSON 1" fs1 (encodingToLazyByteString $ toGJFeatureEncoding f1))

testFeatureGeoJSON2 = TestCase (do
  let f = parseEither parseGJFeature fv1
  assertEqual "Test Feature GeoJSON 2 1" (Right "F1") (featureID <$> f)
  assertEqual "Test Feature GeoJSON 2 2" (Right $ True) ((isJust . featureName) <$> f)
  assertEqual "Test Feature GeoJSON 2 3" (Right $ True) ((isJust . featureDescription) <$> f)
  assertEqual "Test Feature GeoJSON 2 4" (Right (featureGeometry f1)) (featureGeometry <$> f)
  assertEqual "Test Feature GeoJSON 2 5" (Right True) (null <$> featureSubFeatures <$> f)
  )

testFeatureGeoJSON3 = TestCase (assertEqualBSStripped "Test Feature GeoJSON 3" fs2 (encodingToLazyByteString $ toGJFeatureEncoding f2))

testFeatureGeoJSON4 = TestCase (do
  let f = parseEither parseGJFeature fv2
  assertEqual "Test Feature GeoJSON 4 1" (Right "F2") (featureID <$> f)
  assertEqual "Test Feature GeoJSON 4 2" (Right (featureGeometry f2)) (featureGeometry <$> f)
  assertEqual "Test Feature GeoJSON 4 3" (Right True) (null <$> featureSubFeatures <$> f)
  )

testFeatureGeoJSON5 = TestCase (assertEqualBSStripped "Test Feature GeoJSON 5" fs3 (encodingToLazyByteString $ toGJFeatureEncoding f3))

testFeatureGeoJSON6 :: Test
testFeatureGeoJSON6 = TestCase (do
  let f = parseEither parseGJFeature fv3
  assertEqual "Test Feature GeoJSON 6 1" (Right "F3") (featureID <$> f)
  assertEqual "Test Feature GeoJSON 6 2" (Right 2) (length <$> featureSubFeatures <$> f)
  )

