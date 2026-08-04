{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module GeometrySpec(testGeometry) where

import Test.HUnit
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Data.Default.Class
import Geo.Geometry
import Geo.LatLong
import TestUtils
import Text.RawString.QQ

testGeometry :: Test
testGeometry = TestList [
    TestLabel "BoundingBox" testBoundingBox
  , TestLabel "Centroid" testCentroid
  ]

ll1 :: LatLong
ll1 = LatLong 12.3657 (-8.1229) (Just 34) def

ll3 :: LatLong
ll3 = LatLong (-12.9) 148.7 Nothing def

ll4 :: LatLong
ll4 = LatLong 30.56 148.7 (Just 70.0) def

ll5 :: LatLong
ll5 = LatLong 12.6 150.5 Nothing def

bb1 = BoundingBox ll3 ll5

bbs1 = [r|
{
  "sw": {
    "latitude": -12.9,
    "longitude": 148.7
  },
  "ne": {
    "latitude": 12.6,
    "longitude": 150.5
  }
}
|] :: LB.ByteString

bb2 = BoundingBox ll1 ll4

bbs2 = [r|
{
  "sw": {
    "latitude": 12.3657,
    "longitude": -8.1229,
    "elevation": 34.0
  },
  "ne": {
    "latitude": 30.56,
    "longitude": 148.7,
    "elevation": 70.0
  }
}
|] :: LB.ByteString

testBoundingBox :: Test
testBoundingBox = TestList [
    TestLabel "JSON" testBoundingBoxJSON
  , TestLabel "Union" testBoundingBoxUnion
  , TestLabel "BoundedGeometry" testBoundedGeometry
  ]

testBoundingBoxJSON = TestList [
  testBoundingBoxJSON1, testBoundingBoxJSON2, testBoundingBoxJSON3, testBoundingBoxJSON4

  ]

testBoundingBoxJSON1 = TestCase (assertEqualBSStripped "Test BoundingBox JSON 1" bbs1 (encode bb1))

testBoundingBoxJSON2 = TestCase (assertEqualBSStripped "Test BoundingBox JSON 2" bbs2 (encode bb2))

testBoundingBoxJSON3 = TestCase (assertEqual "Test BoundingBox JSON 3" (Just bb1) (decode bbs1))

testBoundingBoxJSON4 = TestCase (assertEqual "Test BoundingBox JSON 4" (Just bb2) (decode bbs2))

testBoundingBoxUnion = TestList [
  testBoundingBoxUnion1
  ]

testBoundingBoxUnion1 = TestCase (assertEqual "Test BoundingBox Union 1" (BoundingBox (LatLong (-12.9) (-8.1229) (Just 34.0) def) (LatLong 30.56 150.5 (Just 70.0) def)) (unionBoundingBox bb1 bb2))

testBoundedGeometry = TestList [
  testBoundedGeometry1, testBoundedGeometry2, testBoundedGeometry3, testBoundedGeometry4
  ]

testBoundedGeometry1 = TestCase (assertEqual "Test Bounded Geometry 1" (Just $ BoundingBox ll1 ll1) (computeBoundingBox ll1))

testBoundedGeometry2 = TestCase (assertEqual "Test Bounded Geometry 2" (Just $ BoundingBox ll1 ll1) (computeBoundingBox [ll1, ll1]))

testBoundedGeometry3 = TestCase (assertEqual "Test Bounded Geometry 3" (Just $ BoundingBox ll1 ll1) (computeBoundingBox [[ll1, ll1]]))

testBoundedGeometry4 = TestCase (assertEqual "Test Bounded Geometry 3" Nothing (computeBoundingBox ([] :: [LatLong])))



testCentroid = TestList [
  testCentroid1
  ]

testCentroid1 = TestCase (assertEqual "Test Centroid 1" (LatLong (-0.26715) 70.28855 (Just 34.0) def) (centroid [ll1, ll3]))
