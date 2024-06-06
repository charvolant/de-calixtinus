{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LocalisedSpec(testLocalised) where

import Test.HUnit
import Data.Aeson
import Data.Localised
import Data.Maybe (fromJust, isJust)

testLocalised :: Test
testLocalised = TestList [
      TestLabel "Locale" testLocale
    , TestLabel "TaggedText" testTaggedText
    , TestLabel "LocalisedText" testLocalisedText
  ]

testLocale = TestList [
      TestLabel "testLocaleFromID" testLocaleFromID
  ]

testLocaleFromID = TestList [
    testLocaleFromID1, testLocaleFromID2, testLocaleFromID3, testLocaleFromID4, testLocaleFromID5
  ]

testLocaleFromID1 = TestCase $ assertEqual "Locale localeFromID 1" "*" (localeID $ localeFromID "*")

testLocaleFromID2 = TestCase $ assertEqual "Locale localeFromID 2" "fr" (localeID $ localeFromID "fre")

testLocaleFromID3 = TestCase $ assertEqual "Locale localeFromID 3" "fr" (localeID $ localeFromID "fr-FR")

testLocaleFromID4 = TestCase $ assertEqual "Locale localeFromID 3" "en-UK" (localeID $ localeFromID "eng_GB")

testLocaleFromID5 = TestCase $ assertEqual "Locale localeFromID 3" "pt" (localeID $ localeFromID "pt")

testTaggedText = TestList [
      TestLabel "JSON" testTaggedTextJSON
  ]

testTaggedTextJSON = TestList [
  testTaggedTextFromJSON1, testTaggedTextFromJSON2, testTaggedTextFromJSON3,
  testTaggedTextToJSON1, testTaggedTextToJSON2
  ]

testTaggedTextFromJSON1 =
  let
    tt = decode "\"Hello There\""
  in
    TestCase (do
      assertBool "TaggedTest FromJSON 1 1" (isJust tt)
      let tt' = fromJust tt
      assertEqual "TaggedTest FromJSON 1 2" "Hello There" (ttText tt')
      assertEqual "TaggedTest FromJSON 1 3" rootLocale (ttLocale tt')
      )

testTaggedTextFromJSON2 =
  let
    tt = decode "\"Hello There@en\""
  in
    TestCase (do
      assertBool "TaggedTest FromJSON 2 1" (isJust tt)
      let tt' = fromJust tt
      assertEqual "TaggedTest FromJSON 2 2" "Hello There" (ttText tt')
      assertEqual "TaggedTest FromJSON 2 3" "en" (localeID $ ttLocale tt')
      )

testTaggedTextFromJSON3 =
  let
    tt = decode "\"Hello@There@en-US\""
  in
    TestCase (do
      assertBool "TaggedTest FromJSON 3 1" (isJust tt)
      let tt' = fromJust tt
      assertEqual "TaggedTest FromJSON 3 2" "Hello@There" (ttText tt')
      assertEqual "TaggedTest FromJSON 3 3" "en-US" (localeID $ ttLocale tt')
      )

testTaggedTextToJSON1 =
  let
    tt = TaggedText "Hello There" rootLocale
    et = encode tt
  in
    TestCase (do
      assertEqual "TaggedTest ToJSON 1 1" "\"Hello There\"" et
      )

testTaggedTextToJSON2 =
  let
    tt = TaggedText "Hello There" (localeFromID "en")
    et = encode tt
  in
    TestCase (do
      assertEqual "TaggedTest ToJSON 1 1" "\"Hello There@en\"" et
      )

testLocalisedText = TestList [
         TestLabel "JSON" testLocalisedTextJSON
       , TestLabel "Localise" testLocalise
  ]

testLocalisedTextJSON = TestList [
  testLocalisedTextFromJSON1, testLocalisedTextFromJSON2, testLocalisedTextFromJSON3,
  testLocalisedTextToJSON1, testLocalisedTextToJSON2, testLocalisedTextToJSON3
  ]

testLocalisedTextFromJSON1 =
  let
    lt = decode "\"Hello There\""
  in TestCase (do
      assertBool "LocalisedText FromJSON 1 1" (isJust lt)
      let lt' = fromJust lt
      assertEqual "LocalisedText FromJSON 1 2" 1 (length $ ltTexts lt')
      let tt' = (ltTexts lt') !! 0
      assertEqual "LocalisedText FromJSON 1 3" "Hello There" (ttText tt')
      assertEqual "LocalisedTextt FromJSON 1 4" rootLocale (ttLocale tt')
    )


testLocalisedTextFromJSON2 =
  let
    lt = decode "\"Hello There@en\""
  in TestCase (do
      assertBool "LocalisedText FromJSON 2 1" (isJust lt)
      let lt' = fromJust lt
      assertEqual "LocalisedText FromJSON 2 2" 1 (length $ ltTexts lt')
      let tt' = (ltTexts lt') !! 0
      assertEqual "LocalisedText FromJSON 2 3" "Hello There" (ttText tt')
      assertEqual "LocalisedTextt FromJSON 2 4" "en" (localeID $ ttLocale tt')
    )

testLocalisedTextFromJSON3 =
  let
    lt = decode "[ \"Hello There@en\", \"Bonjour@fr\" ]"
  in TestCase (do
      assertBool "LocalisedText FromJSON 2 1" (isJust lt)
      let lt' = fromJust lt
      assertEqual "LocalisedText FromJSON 2 2" 2 (length $ ltTexts lt')
      let tt1 = (ltTexts lt') !! 0
      assertEqual "LocalisedText FromJSON 2 3" "Hello There" (ttText tt1)
      assertEqual "LocalisedTextt FromJSON 2 4" "en" (localeID $ ttLocale tt1)
      let tt2 = (ltTexts lt') !! 1
      assertEqual "LocalisedText FromJSON 2 3" "Bonjour" (ttText tt2)
      assertEqual "LocalisedText FromJSON 2 4" "fr" (localeID $ ttLocale tt2)
    )

testLocalisedTextToJSON1 =
  let
    lt = LocalisedText [TaggedText "Hello There" rootLocale]
    et = encode lt
  in
    TestCase (do
      assertEqual "LocalisedText ToJSON 1 1" "\"Hello There\"" et
      )


testLocalisedTextToJSON2 =
  let
    lt = LocalisedText [TaggedText "Hello There" (localeFromID "en")]
    et = encode lt
  in
    TestCase (do
      assertEqual "LocalisedText ToJSON 1 1" "\"Hello There@en\"" et
      )


testLocalisedTextToJSON3 =
  let
    lt = LocalisedText [TaggedText "Hello There" (localeFromID "en"), TaggedText "Bonjour" (localeFromID "fr")]
    et = encode lt
  in
    TestCase (do
      assertEqual "LocalisedText ToJSON 1 1" "[\"Hello There@en\",\"Bonjour@fr\"]" et
      )

testLocalise = TestList [
    testLocalise1, testLocalise2, testLocalise3, testLocalise4, testLocalise5
  ]

testLocalise1 = 
  let
    en = localeFromID "en"
    fr = localeFromID "fr"
    locales = [en, fr]
    lt = LocalisedText [TaggedText "Hello There" rootLocale]
    localised = localise locales lt
  in TestCase (do
      assertEqual "Localise 1 1" "Hello There" (ttText localised)
      assertEqual "Localise 1 2" rootLocale (ttLocale localised)
    )
 
testLocalise2 = 
  let
    en = localeFromID "en"
    fr = localeFromID "fr"
    gl = localeFromID "ga"
    locales = [en, fr]
    lt = LocalisedText [TaggedText "Ola" gl, TaggedText "Hello There" rootLocale]
    localised = localise locales lt
  in TestCase (do
      assertEqual "Localise 2 1" "Hello There" (ttText localised)
      assertEqual "Localise 2 2" rootLocale (ttLocale localised)
    )

 
testLocalise3 = 
  let
    en = localeFromID "en"
    fr = localeFromID "fr"
    gl = localeFromID "ga"
    locales = [en, fr]
    lt = LocalisedText [TaggedText "Ola" gl, TaggedText "Hello There" rootLocale, TaggedText "Bonjour" fr]
    localised = localise locales lt
  in TestCase (do
      assertEqual "Localise 2 1" "Bonjour" (ttText localised)
      assertEqual "Localise 2 2" fr (ttLocale localised)
    )


testLocalise4 = 
  let
    en = localeFromID "en"
    fr = localeFromID "fr"
    fr_fr = localeFromID "fr-FR"
    gl = localeFromID "ga"
    locales = [en, fr_fr]
    lt = LocalisedText [TaggedText "Ola" gl, TaggedText "Hello There" rootLocale, TaggedText "Bonjour" fr]
    localised = localise locales lt
  in TestCase (do
      assertEqual "Localise 2 1" "Bonjour" (ttText localised)
      assertEqual "Localise 2 2" fr (ttLocale localised)
    )


testLocalise5 = 
  let
    en = localeFromID "en"
    fr_fr = localeFromID "fr-FR"
    gl = localeFromID "ga"
    es = localeFromID "es"
    locales = [en, fr_fr]
    lt = LocalisedText [TaggedText "Ola" gl, TaggedText "Hola" es]
    localised = localise locales lt
  in TestCase (do
      assertEqual "Localise 2 1" "Ola" (ttText localised)
      assertEqual "Localise 2 2" gl (ttLocale localised)
    )
