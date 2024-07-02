{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module LocalisedSpec(testLocalised) where

import Test.HUnit
import Data.Aeson
import Data.Localised
import Data.Maybe (fromJust, isJust)
import Network.URI (uriIsAbsolute)

testLocalised :: Test
testLocalised = TestList [
      TestLabel "Locale" testLocale
    , TestLabel "TaggedText" testTaggedText
    , TestLabel "TaggedURL" testTaggedURL
    , TestLabel "Localised" testLocalisedText
  ]

testLocale = TestList [
      TestLabel "testLocaleFromID" testLocaleFromID
  ]

testLocaleFromID = TestList [
    testLocaleFromID1, testLocaleFromID2, testLocaleFromID3, testLocaleFromID4, testLocaleFromID5,
    testLocaleFromID6
  ]

testLocaleFromID1 = TestCase $ assertEqual "Locale localeFromID 1" (Just "*") (localeID <$> localeFromID "*")

testLocaleFromID2 = TestCase $ assertEqual "Locale localeFromID 2" (Just "fr") (localeID <$> localeFromID "fre")

testLocaleFromID3 = TestCase $ assertEqual "Locale localeFromID 3" (Just "fr") (localeID <$> localeFromID "fr-FR")

testLocaleFromID4 = TestCase $ assertEqual "Locale localeFromID 4" (Just "en-UK") (localeID <$> localeFromID "eng_GB")

testLocaleFromID5 = TestCase $ assertEqual "Locale localeFromID 5" (Just "pt") (localeID <$> localeFromID "pt")

testLocaleFromID6 = TestCase $ assertEqual "Locale localeFromID 6" Nothing (localeID <$> localeFromID "se")

testTaggedText = TestList [
      TestLabel "JSON" testTaggedTextJSON
  ]

testTaggedTextJSON = TestList [
  testTaggedTextFromJSON1, testTaggedTextFromJSON2, testTaggedTextFromJSON3, testTaggedTextFromJSON4, testTaggedTextFromJSON5,
  testTaggedTextToJSON1, testTaggedTextToJSON2, testTaggedTextToJSON3
  ]

testTaggedTextFromJSON1 =
  let
    tt = decode "\"Hello There\"" :: Maybe TaggedText
  in
    TestCase (do
      assertBool "TaggedTest FromJSON 1 1" (isJust tt)
      let tt' = fromJust tt
      assertEqual "TaggedTest FromJSON 1 2" "Hello There" (plainText tt')
      assertEqual "TaggedTest FromJSON 1 3" rootLocale (locale tt')
      )

testTaggedTextFromJSON2 =
  let
    tt = decode "\"Hello There@en\"" :: Maybe TaggedText
  in
    TestCase (do
      assertBool "TaggedTest FromJSON 2 1" (isJust tt)
      let tt' = fromJust tt
      assertEqual "TaggedTest FromJSON 2 2" "Hello There" (plainText tt')
      assertEqual "TaggedTest FromJSON 2 3" "en" (localeID $ locale tt')
      )

testTaggedTextFromJSON3 =
  let
    tt = decode "\"Hello@There@en-US\"" :: Maybe TaggedText
  in
    TestCase (do
      assertBool "TaggedTest FromJSON 3 1" (isJust tt)
      let tt' = fromJust tt
      assertEqual "TaggedTest FromJSON 3 2" "Hello@There" (plainText tt')
      assertEqual "TaggedTest FromJSON 3 3" "en-US" (localeID $ locale tt')
      )

testTaggedTextFromJSON4 =
  let
    tt = decode "[\"en\",\"Hello\",\"There\"]" :: Maybe TaggedText
  in
    TestCase (do
      assertBool "TaggedTest FromJSON 4 1" (isJust tt)
      let tt' = fromJust tt
      assertEqual "TaggedTest FromJSON 4 2" "Hello\nThere" (plainText tt')
      assertEqual "TaggedTest FromJSON 4 3" "en" (localeID $ locale tt')
      )

testTaggedTextFromJSON5 =
  let
    tt = decode "[\"Hello\",\"There\"]" :: Maybe TaggedText
  in
    TestCase (do
      assertBool "TaggedTest FromJSON 5 1" (isJust tt)
      let tt' = fromJust tt
      assertEqual "TaggedTest FromJSON 5 2" "Hello\nThere" (plainText tt')
      assertEqual "TaggedTest FromJSON 5 3" "*" (localeID $ locale tt')
      )

testTaggedTextToJSON1 =
  let
    tt = TaggedText rootLocale "Hello There"
    et = encode tt
  in
    TestCase (do
      assertEqual "TaggedTest ToJSON 1 1" "\"Hello There\"" et
      )

testTaggedTextToJSON2 =
  let
    tt = TaggedText (localeFromIDOrError "en") "Hello There"
    et = encode tt
  in
    TestCase (do
      assertEqual "TaggedTest ToJSON 2 1" "\"Hello There@en\"" et
      )

testTaggedTextToJSON3 =
  let
    tt = TaggedText (localeFromIDOrError "en") "Hello\nThere"
    et = encode tt
  in
    TestCase (do
      assertEqual "TaggedTest ToJSON 3 1" "[\"en\",\"Hello\",\"There\"]" et
      )


testTaggedURL = TestList [
         TestLabel "Resolve" testResolveTaggedURL
  ]

testResolveTaggedURL = TestList [
         testResolveTaggedURL1, testResolveTaggedURL2
  ]

testResolveTaggedURL1 =
  let
    turl = TaggedURL rootLocale (Hyperlink (textToUri "https://nothing.com/image1.jpg") Nothing)
  in TestCase (do
      assertBool "Resolve Tagged URL 1 0" (uriIsAbsolute $ link turl)
      assertEqual "Resolve Tagged URL 1 1" "https://nothing.com/image1.jpg" (resolveLink "https://somewhere.com" turl)
      assertEqual "Resolve Tagged URL 1 2" "https://nothing.com/image1.jpg" (resolveLink "" turl)
    )

testResolveTaggedURL2 =
  let
    turl = TaggedURL rootLocale (Hyperlink (textToUri "image1.jpg") Nothing)
  in TestCase (do
      assertBool "Resolve Tagged URL 2 0" (not $ uriIsAbsolute $ link turl)
      assertEqual "Resolve Tagged URL 2 1" "https://somewhere.com/image1.jpg" (resolveLink "https://somewhere.com" turl)
      assertEqual "Resolve Tagged URL 2 2" "/image1.jpg" (resolveLink "" turl)
    )


testLocalisedText = TestList [
         TestLabel "JSON" testLocalisedJSON
       , TestLabel "Localise" testLocalise
  ]

testLocalisedJSON = TestList [
  testLocalisedFromJSON1, testLocalisedFromJSON2, testLocalisedFromJSON3,
  testLocalisedToJSON1, testLocalisedToJSON2, testLocalisedToJSON3
  ]

testLocalisedFromJSON1 =
  let
    lt = decode "\"Hello There\"" :: Maybe (Localised TaggedText)
  in TestCase (do
      assertBool "Localised FromJSON 1 1" (isJust lt)
      let lt' = fromJust lt
      assertEqual "Localised FromJSON 1 2" 1 (length $ elements lt')
      let tt' = (elements lt') !! 0
      assertEqual "Localised FromJSON 1 3" "Hello There" (plainText tt')
      assertEqual "Localisedt FromJSON 1 4" rootLocale (locale tt')
    )


testLocalisedFromJSON2 =
  let
    lt = decode "\"Hello There@en\""  :: Maybe (Localised TaggedText)
  in TestCase (do
      assertBool "Localised FromJSON 2 1" (isJust lt)
      let lt' = fromJust lt
      assertEqual "Localised FromJSON 2 2" 1 (length $ elements lt')
      let tt' = (elements lt') !! 0
      assertEqual "Localised FromJSON 2 3" "Hello There" (plainText tt')
      assertEqual "Localisedt FromJSON 2 4" "en" (localeID $ locale tt')
    )

testLocalisedFromJSON3 =
  let
    lt = decode "[ \"Hello There@en\", \"Bonjour@fr\" ]"  :: Maybe (Localised TaggedText)
  in TestCase (do
      assertBool "Localised FromJSON 2 1" (isJust lt)
      let lt' = fromJust lt
      assertEqual "Localised FromJSON 2 2" 2 (length $ elements lt')
      let tt1 = (elements lt') !! 0
      assertEqual "Localised FromJSON 2 3" "Hello There" (plainText tt1)
      assertEqual "Localisedt FromJSON 2 4" "en" (localeID $ locale tt1)
      let tt2 = (elements lt') !! 1
      assertEqual "Localised FromJSON 2 3" "Bonjour" (plainText tt2)
      assertEqual "Localised FromJSON 2 4" "fr" (localeID $ locale tt2)
    )

testLocalisedToJSON1 =
  let
    lt = Localised [TaggedText rootLocale "Hello There" ]
    et = encode lt
  in
    TestCase (do
      assertEqual "Localised ToJSON 1 1" "\"Hello There\"" et
      )


testLocalisedToJSON2 =
  let
    lt = Localised [TaggedText (localeFromIDOrError "en") "Hello There" ]
    et = encode lt
  in
    TestCase (do
      assertEqual "Localised ToJSON 1 1" "\"Hello There@en\"" et
      )


testLocalisedToJSON3 =
  let
    lt = Localised [TaggedText  (localeFromIDOrError "en") "Hello There", TaggedText (localeFromIDOrError "fr") "Bonjour"]
    et = encode lt
  in
    TestCase (do
      assertEqual "Localised ToJSON 1 1" "[\"Hello There@en\",\"Bonjour@fr\"]" et
      )

testLocalise = TestList [
    testLocalise1, testLocalise2, testLocalise3, testLocalise4, testLocalise5
  ]

testLocalise1 =
  let
    en = localeFromIDOrError "en"
    fr = localeFromIDOrError "fr"
    locales = [en, fr]
    lt = Localised [TaggedText rootLocale "Hello There"]
    localised = localise locales lt
  in TestCase (do
      assertEqual "Localise 1 1" (Just "Hello There") (plainText <$> localised)
      assertEqual "Localise 1 2" (Just rootLocale) (locale <$> localised)
    )

testLocalise2 =
  let
    en = localeFromIDOrError "en"
    fr = localeFromIDOrError "fr"
    gl = localeFromIDOrError "ga"
    locales = [en, fr]
    lt = Localised [TaggedText gl "Ola", TaggedText rootLocale "Hello There"]
    localised = localise locales lt
  in TestCase (do
      assertEqual "Localise 2 1" (Just "Hello There") (plainText <$> localised)
      assertEqual "Localise 2 2" (Just rootLocale) (locale <$> localised)
    )


testLocalise3 =
  let
    en = localeFromIDOrError "en"
    fr = localeFromIDOrError "fr"
    gl = localeFromIDOrError "ga"
    locales = [en, fr]
    lt = Localised [TaggedText gl "Ola", TaggedText rootLocale "Hello There", TaggedText fr "Bonjour"]
    localised = localise locales lt
  in TestCase (do
      assertEqual "Localise 2 1" (Just "Bonjour") (plainText <$> localised)
      assertEqual "Localise 2 2" (Just fr) (locale <$> localised)
    )


testLocalise4 =
  let
    en = localeFromIDOrError "en"
    fr = localeFromIDOrError "fr"
    fr_fr = localeFromIDOrError "fr-FR"
    gl = localeFromIDOrError "ga"
    locales = [en, fr_fr]
    lt = Localised [TaggedText gl "Ola", TaggedText rootLocale "Hello There", TaggedText fr "Bonjour"]
    localised = localise locales lt
  in TestCase (do
      assertEqual "Localise 2 1" (Just "Bonjour") (plainText <$> localised)
      assertEqual "Localise 2 2" (Just fr) (locale <$> localised)
    )


testLocalise5 =
  let
    en = localeFromIDOrError "en"
    fr_fr = localeFromIDOrError "fr-FR"
    gl = localeFromIDOrError "ga"
    es = localeFromIDOrError "es"
    locales = [en, fr_fr]
    lt = Localised [TaggedText gl "Ola", TaggedText es "Hola"]
    localised = localise locales lt
  in TestCase (do
      assertEqual "Localise 2 1" (Just "Ola") (plainText <$> localised)
      assertEqual "Localise 2 2" (Just gl) (locale <$> localised)
    )
