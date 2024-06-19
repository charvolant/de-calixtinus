{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module DescriptionSpec(testDescription) where

import Test.HUnit
import Data.Aeson
import Data.Description
import Data.Localised
import Data.Maybe (fromJust, isJust, isNothing)
import Network.URI (parseURI)


testDescription = TestList [
        TestLabel "JSON" testDescriptionJSON
      , TestLabel "Localised" testLocalisedJSON
  ]

testDescriptionJSON = TestList [
  testDescriptionFromJSON1, testDescriptionFromJSON2, testDescriptionFromJSON3, testDescriptionFromJSON4,
  testDescriptionToJSON1, testDescriptionToJSON2, testDescriptionToJSON3
  ]

testDescriptionFromJSON1 =
  let
    desc = decode "\"Hello There\"" :: Maybe Description
  in
    TestCase (do
      assertBool "Description FromJSON 1 1" (isJust desc)
      let desc' = fromJust desc
      assertEqual "Description FromJSON 1 2" (Just "Hello There") (plainText <$> localise [] (descriptionSummary desc'))
      assertBool "Description FromJSON 1 3" (null $ descNotes desc')
      assertBool "Description FromJSON 1 4" (isNothing $ descAbout desc')
      assertBool "Description FromJSON 1 5" (isNothing $ descImage desc')
      )

testDescriptionFromJSON2 =
  let
    desc = decode "\"Hello There@en\"" :: Maybe Description
  in
    TestCase (do
      assertBool "Description FromJSON 2 1" (isJust desc)
      let desc' = fromJust desc
      assertEqual "Description FromJSON 2 2" (Just "Hello There") (plainText <$> localise [] (descriptionSummary desc'))
      assertBool "Description FromJSON 2 3" (null $ descNotes desc')
      assertBool "Description FromJSON 1 4" (isNothing $ descAbout desc')
      assertBool "Description FromJSON 1 5" (isNothing $ descImage desc')
      )

testDescriptionFromJSON3 =
  let
    desc = decode "{ \"locale\": \"en-US\", \"text\": \"Hello@There\" }" :: Maybe Description
  in
    TestCase (do
      assertBool "Description FromJSON 3 1" (isJust desc)
      let desc' = fromJust desc
      assertEqual "Description FromJSON 3 2" (Just "Hello@There") (plainText <$> localise [] (descriptionSummary desc'))
      assertBool "Description FromJSON 3 3" (null $ descNotes desc')
      assertBool "Description FromJSON 3 4" (isNothing $ descAbout desc')
      assertBool "Description FromJSON 3 5" (isNothing $ descImage desc')
      )

testDescriptionFromJSON4 =
  let
    desc = decode "{ \"text\": \"Hello@There@en-US\", \"about\": \"https://nowhere.com\", \"image\": { \"source\": \"https://somewhere.com\", \"title\": \"Hello\" } }" :: Maybe Description
  in
    TestCase (do
      assertBool "Description FromJSON 4 1" (isJust desc)
      let desc' = fromJust desc
      assertEqual "Description FromJSON 4 2" (Just "Hello@There") (plainText <$> localise [] (descriptionSummary desc'))
      assertEqual "Description FromJSON 4 3" (Just "en-US") (localeID <$> locale <$> localise [] (descriptionSummary desc'))
      assertEqual "Description FromJSON 4 4" (Just "https://nowhere.com") (linkText <$> localise [] (fromJust $ descAbout desc'))
      assertEqual "Description FromJSON 4 5" "https://somewhere.com" (linkText $ fromJust $ descImage desc')
      )

testDescriptionToJSON1 =
  let
    desc = Description Nothing (Just $ wildcardText "Hello There") [] Nothing Nothing
    et = encode desc
  in
    TestCase (do
      assertEqual "Description ToJSON 1 1" "\"Hello There\"" et
      )

testDescriptionToJSON2 =
  let
    loc = localeFromIDOrError "en"
    desc = Description Nothing (Just $ Localised [TaggedText loc "Hello There"]) [] Nothing Nothing
    et = encode desc
  in
    TestCase (do
      assertEqual "Description ToJSON 1 1" "\"Hello There@en\"" et
      )


testDescriptionToJSON3 =
  let
    loc = localeFromIDOrError "en-US" 
    desc = Description Nothing (Just $ Localised [TaggedText loc "Hello There"]) [] (Just $ Localised [TaggedURL loc (Hyperlink (fromJust $ parseURI "https://www.fish.com") (Just "Fish"))]) Nothing
    et = encode desc
  in
    TestCase (do
      assertEqual "Description ToJSON 3 1" "{\"about\":{\"locale\":\"en-US\",\"title\":\"Fish\",\"url\":\"https://www.fish.com\"},\"image\":null,\"notes\":null,\"summary\":null,\"text\":\"Hello There@en-US\"}" et
      )


testLocalisedJSON = TestList [
  testLocalisedToJSON1, testLocalisedToJSON2
  ]


testLocalisedToJSON1 =
  let
    loc =  localeFromIDOrError "en"
    desc = Description Nothing (Just $ Localised [TaggedText loc "Hello There"]) [] Nothing Nothing
    et = encode desc
  in
    TestCase (do
      assertEqual "Localised ToJSON 1 1" "\"Hello There@en\"" et
      )

testLocalisedToJSON2 =
  let
    loc =  localeFromIDOrError "en"
    desc = Description Nothing (Just $ Localised [TaggedText loc "Hello There"]) [] (Just $ Localised [TaggedURL loc (Hyperlink (fromJust $ parseURI "urn:x-y:z") Nothing)]) Nothing
    et = encode desc
  in
    TestCase (do
      assertEqual "Localised ToJSON 2 1" "{\"about\":\"urn:x-y:z@en\",\"image\":null,\"notes\":null,\"summary\":null,\"text\":\"Hello There@en\"}" et
      )

