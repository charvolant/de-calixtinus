{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LatLongSpec(testLatLong) where

import Test.HUnit
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Data.Default.Class
import Geo.LatLong
import Text.Read (readEither)
import TestUtils
import Text.RawString.QQ
import Data.Aeson.Text (encodeToLazyText)

testLatLong :: Test
testLatLong = TestList [
    TestLabel "LatLong" testLatLongElev
  ]

testLatLongElev :: Test
testLatLongElev = TestList [
    TestLabel "JSON" testLatLongJSON
  ]

ll1 :: LatLong
ll1 = LatLong 12.3657 (-8.1229) (Just 34) def

lls1 = [r|
{
  "latitude": 12.3657,
  "longitude": -8.1229,
  "elevation": 34.0
}
|] :: LB.ByteString

llg1 = [-8.1229, 12.3657, 34.0]

ll2 :: LatLong
ll2 = LatLong (-12.9) 148.7 Nothing (SRS "EPSG:7844")

lls2 = [r|
{
  "latitude": -12.9,
  "longitude": 148.7,
  "srs": "EPSG:7844"
}
|] ::LB.ByteString

testLatLongJSON = TestList [
  testLatLongJSON1, testLatLongJSON2,  testLatLongJSON3, testLatLongJSON4
  ]

ll3 :: LatLong
ll3 = LatLong (-12.9) 148.7 Nothing def

llg3 = [148.7, -12.9]


ll4 :: LatLong
ll4 = LatLong 30.56 148.7 (Just 70.0) def

ll5 :: LatLong
ll5 = LatLong 12.6 150.5 Nothing def

testLatLongJSON1 = TestCase (assertEqualBSStripped "Test LatLong JSON 1" lls1 (encode ll1))

testLatLongJSON2 = TestCase (assertEqualBSStripped "Test LatLong JSON 2" lls2 (encode ll2))

testLatLongJSON3 = TestCase (assertEqual "Test LatLong JSON 3" (Just ll1) (decode lls1))

testLatLongJSON4 = TestCase (assertEqual "Test LatLong JSON 4" (Just ll2) (decode lls2))
