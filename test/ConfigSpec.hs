{-# LANGUAGE OverloadedStrings #-}
module ConfigSpec(testConfig) where

import Test.HUnit
import Camino.Config
import Data.Default.Class
import Data.Localised
import Data.Maybe (fromJust, isJust)

testConfig1 = Config {
  configParent = Just def,
  configWeb = Web {
    webRoot = Just "https:/nowhere1.com",
    webAssets = [
      Asset {
        assetId = "foo",
        assetType = Css,
        assetPath = "bar/baz.css",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      }
    ],
    webLinks = [],
    webMaps = []
  },
  configCaminos = [
    Asset {
      assetId = "C1",
      assetType = CaminoDefinition,
      assetPath = "c1.json",
      assetIntegrity = Nothing,
      assetCrossOrigin = Unused
    }
  ],
  configCalendars = Nothing,
  configRegions = Nothing,
  configCaches = [],
  configNotice = Nothing,
  configDebug = Just False
}

testConfig2 = Config {
  configParent = Just def,
  configWeb = Web {
    webRoot = Just "https:/nowhere2.com",
    webAssets = [],
    webLinks = [
      Link {
        linkId = "foo",
        linkType = Header,
        links = Localised [TaggedURL (localeFromIDOrError "fr") (Hyperlink (textToUri "foo-fr.html") (Just "Feu"))]
      }
    ],
    webMaps = []
  },
  configCaminos = [],
  configCalendars = Nothing,
  configRegions = Nothing,
  configCaches = [],
  configNotice = Nothing,
  configDebug = Just False
}

testConfig :: Test
testConfig = TestList [
  TestLabel "GetAssets" testGetAssets,
  TestLabel "GetAsset" testGetAsset,
  TestLabel "GetLinks" testGetLinks
  ]
  
testGetAssets = TestList [
  testGetAssets1, testGetAssets2, testGetAssets3
  ]

testGetAssets1 = TestCase (assertEqual "getAssets 1" 0 (length $ getAssets JavaScript testConfig1))

testGetAssets2 = TestCase (assertEqual "getAssets 2" 5 (length $ getAssets Css testConfig1))

testGetAssets3 = TestCase (assertEqual "getAssets 3" 4 (length $ getAssets Css def))

  
testGetAsset = TestList [
  testGetAsset1, testGetAsset2, testGetAsset3
  ]

testGetAsset1 = TestCase (do
    assertEqual "getAsset 1 1" True (isJust $ getAsset "leaflet-js" testConfig1)
    assertEqual "getAsset 1 2" "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js" (assetPath $ fromJust $ getAsset "leaflet-js" testConfig1)
    )

testGetAsset2 = TestCase (do
    assertEqual "getAsset 2 1" True (isJust $ getAsset "foo" testConfig1)
    assertEqual "getAsset 2 2" "bar/baz.css" (assetPath $ fromJust $ getAsset "foo" testConfig1)
    )

testGetAsset3 = TestCase (
    assertEqual "getAsset 3" False (isJust $ getAsset "foo" def)
    )


  
testGetLinks = TestList [
  testGetLinks1, testGetLinks2
  ]

testGetLinks1 = TestCase (assertEqual "getLinks 1" 0 (length $ getLinks Header testConfig1))

testGetLinks2 = TestCase (assertEqual "getLinks 2" 1 (length $ getLinks Header testConfig2))

