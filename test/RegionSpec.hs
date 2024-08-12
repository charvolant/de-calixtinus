{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module RegionSpec(testRegion) where

import Test.HUnit
import Data.Aeson
import Data.Description
import Data.Event
import Data.Localised
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Region
import qualified Data.Set as S
import Text.RawString.QQ


testRegion :: Test
testRegion = TestList [
   TestLabel "Region" testRegions
 , TestLabel "RegionConfig" testRegionConfig
 ]

testRegions = TestList [
    TestLabel "JSON" testRegionJSON
  ]
  
testRegionJSON = TestList [
  testRegionToJSON1, testRegionToJSON2, testRegionToJSON3, 
  testRegionFromJSON1
  ]

testRegionToJSON1 =
  let
    region = Region "World" (wildcardText "World") Planet Nothing Nothing S.empty (Just rootLocale) []
    ec = encode region
  in
    TestCase (do
      assertEqual "Region ToJSON 1 1" "{\"description\":null,\"holidays\":[],\"id\":\"World\",\"locale\":\"*\",\"member\":[],\"name\":\"World\",\"parent\":null,\"type\":\"Planet\"}" ec
      )

testRegionToJSON2 =
  let
    description1 = Description Nothing (Just $ wildcardText "The French Republic") [] Nothing Nothing
    region1 = Region "World" (wildcardText "World") Planet Nothing Nothing S.empty (Just rootLocale) []
    region2 = Region "FR" (wildcardText "France") Country (Just description1) (Just region1) S.empty (localeFromID "fr") []
    ec = encode region2
  in
    TestCase (do
      assertEqual "Region ToJSON 2 1" "{\"description\":\"The French Republic\",\"holidays\":[],\"id\":\"FR\",\"locale\":\"fr\",\"member\":[],\"name\":\"France\",\"parent\":\"World\",\"type\":\"Country\"}" ec
      )

testRegionToJSON3 =
  let
    region1 = Region "World" (wildcardText "World") Planet Nothing Nothing S.empty (Just rootLocale) []
    region2 = Region "FR" (wildcardText "France") Country Nothing (Just region1) S.empty Nothing [NamedCalendar "Easter"]
    ec = encode region2
  in
    TestCase (do
      assertEqual "Region ToJSON 2 1" "{\"description\":null,\"holidays\":[{\"key\":\"Easter\",\"type\":\"named\"}],\"id\":\"FR\",\"locale\":null,\"member\":[],\"name\":\"France\",\"parent\":\"World\",\"type\":\"Country\"}" ec
      )

testRegionFromJSON1 =
  let
    decoded = decode "{\"description\":null,\"holidays\":[],\"id\":\"FR\",\"locale\":\"fr\",\"member\":[],\"name\":\"France\",\"parent\":\"World\",\"type\":\"Country\"}" :: Maybe Region
  in
    TestCase (do
      assertBool "Region FromJSON 1 1" (isJust decoded)
      let decoded' = fromJust decoded
      assertEqual "Region FromJSON 1 2" "FR" (regionID decoded')
      assertEqual "Region FromJSON 1 3" (Just $ TaggedText rootLocale "France") (localise [] $ regionName decoded')
      assertEqual "Region FromJSON 1 4" Country (regionType decoded')
      assertBool "Region FromJSON 1 5" (isNothing $ regionDescription decoded')
      assertEqual "Region FromJSON 1 6" (Just "World") (regionID <$> regionParent decoded')
      assertEqual "Region FromJSON 1 7" S.empty (regionMember decoded')
      assertEqual "Region FromJSON 1 8" (Just $ localeFromIDOrError "fr") (regionLocale decoded')
      assertEqual "Region FromJSON 1 9" [] (regionHolidays decoded')
      )

testRegionConfig = TestList [
    TestLabel "JSON" testRegionConfigJSON
  ]

testRegionConfigJSON = TestList [
  testRegionConfigToJSON1,
  testRegionConfigFromJSON1
  ]
  
regionConfigJSON1 = [r|
  [
    {
      "id":"World",
      "name":"The Planet Earth",
      "description":null,
      "locale":"*",
      "type":"Planet"
    },
    {
      "id":"Europe",
      "name":"Europe",
      "description":null,
      "parent":"World",
      "type":"Continent"
    }
  ]
|]

testRegionConfigToJSON1 =
  let
    region1 = Region "World" (wildcardText "World") Planet Nothing Nothing S.empty (Just rootLocale) []
    region2 = Region "Europe" (wildcardText "Europe") Continent Nothing (Just region1) S.empty Nothing []
    config = createRegionConfig [region1, region2]
    ec = encode config
  in
    TestCase (do
      assertEqual "RegionConfig ToJSON 1 1" "[{\"description\":null,\"holidays\":[],\"id\":\"Europe\",\"locale\":null,\"member\":[],\"name\":\"Europe\",\"parent\":\"World\",\"type\":\"Continent\"},{\"description\":null,\"holidays\":[],\"id\":\"World\",\"locale\":\"*\",\"member\":[],\"name\":\"World\",\"parent\":null,\"type\":\"Planet\"}]" ec
      )

testRegionConfigFromJSON1 =
  let
    decoded = decode regionConfigJSON1 :: Maybe RegionConfig
  in
    TestCase (do
      assertBool "RegionConfig FromJSON 1 1" (isJust decoded)
      let decoded' = fromJust decoded
      assertEqual "RegionConfig FromJSON 1 2" 2 (length $ regionConfigRegions decoded')
      let world = (regionConfigLookup decoded') "World"
      assertBool "RegionConfig FromJSON 1 3" (isJust world)
      let world' = fromJust world
      assertEqual "RegionConfig FromJSON 1 4" "World" (regionID world')
      let europe = (regionConfigLookup decoded') "Europe"
      assertBool "RegionConfig FromJSON 1 5" (isJust europe)
      let europe' = fromJust europe
      assertEqual "RegionConfig FromJSON 1 6" "Europe" (regionID europe')
      assertEqual "RegionConfig FromJSON 1 6" Continent (regionType europe')
      assertEqual "RegionConfig FromJSON 1 7" "Europe" (plainText $ fromJust $ localise [] $ regionName europe')
      assertBool "RegionConfig FromJSON 1 7" (isNothing $ regionLocale europe')
      assertBool "RegionConfig FromJSON 1 8" (isJust $ regionParent europe')
      let parent' = fromJust $ regionParent europe'
      assertEqual "RegionConfig FromJSON 1 9" "World" (regionID parent')
      assertEqual "RegionConfig FromJSON 1 10" "The Planet Earth" (plainText $ fromJust $ localise [] $ regionName parent')
      assertEqual "RegionConfig FromJSON 1 11" Planet (regionType parent')
   )
