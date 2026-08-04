{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module FeatureSpec(testFeature) where

import Test.HUnit
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Data.Attributes
import Data.Default.Class
import Data.Description
import Data.Localised
import qualified Data.Map as M
import Data.Maybe
import Geo.Feature
import Geo.Geometry
import Geo.LatLong
import TestUtils
import Text.RawString.QQ

testFeature :: Test
testFeature = TestList [
    TestLabel "JSON" testFeatureJSON
  ]

f1 :: Feature SimpleGeometry
f1 = Feature
  "F1"
  Nothing
  Nothing
  Nothing
  Nothing
  (Just (Point Nothing (LatLong 12.5 55.2 Nothing def)))
  []

fs1 = [r|
{
  "id": "F1",
  "geometry": {
    "type": "Point",
    "point": { "latitude": 12.5, "longitude": 55.2 }
  }
}
|] :: LB.ByteString

f2 :: Feature SimpleGeometry
f2 = Feature
  "F2"
  (Just $ wildcardText "Feature 2")
  (Just $ wildcardDescription "A description of feature 2")
  (Just $ Attributes $ M.fromList [("a", IntegerV 1), ("b", StringV "foo")])
  Nothing
  (Just (LineString Nothing [LatLong 12.5 55.2 Nothing def, (LatLong 12.52 55.3 Nothing def)]))
  []

fs2 = [r|
{
  "id": "F2",
  "name": "Feature 2",
  "description": "A description of feature 2",
  "attributes": {
    "a": 1,
    "b": "foo"
  },
  "geometry": {
    "type": "LineString",
    "line": [
      { "latitude": 12.5, "longitude": 55.2 },
      { "latitude": 12.52, "longitude": 55.3 }
    ]
  }
}
|] :: LB.ByteString

testFeatureJSON = TestList [
  testFeatureJSON1, testFeatureJSON2,  testFeatureJSON3, testFeatureJSON4

 ]

testFeatureJSON1 = TestCase (assertEqualBSStripped "Test Feature JSON 1" fs1 (encode f1))

testFeatureJSON2 = TestCase (do
    let f = eitherDecode fs1
    assertEqual"Test Feature JSON 1 1" (Right "F1") (featureID <$> f)
    assertEqual"Test Feature JSON 1 2" (Right Nothing) (featureAttributes  <$> f)
    assertEqual"Test Feature JSON 1 3" (Right $ featureGeometry f1) (featureGeometry <$> f)
    assertEqual"Test Feature JSON 1 4" (Right $ True) (null <$> featureSubFeatures <$> f)
  )

testFeatureJSON3 = TestCase (assertEqualBSStripped "Test Feature JSON 3" fs2 (encode f2))

testFeatureJSON4 = TestCase (do
    let f = eitherDecode fs2
    assertEqual"Test Feature JSON 4 1" (Right "F2") (featureID <$> f)
    assertEqual"Test Feature JSON 4 2" (Right $ localiseDefault $ fromJust $ featureName f2) (localiseDefault <$> fromJust <$> featureName <$> f)
    assertEqual"Test Feature JSON 4 2" (Right $ featureAttributes f2) (featureAttributes  <$> f)
    assertEqual"Test Feature JSON 4 3" (Right $ featureGeometry f2) (featureGeometry <$> f)
    assertEqual"Test Feature JSON 4 4" (Right $ True) (null <$> featureSubFeatures <$> f)
  )
