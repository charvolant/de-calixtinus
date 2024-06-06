{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MetadataSpec(testMetadata) where

import Test.HUnit
import Data.Aeson
import Data.Either
import Data.Localised
import Data.Maybe (fromJust)
import Data.Metadata
import Network.URI
import TestUtils
import Text.RawString.QQ
import Data.ByteString.Lazy

dcElements = fromJust $ parseURI "http://purl.org/dc/elements/1.1/"

dcTitle = fromJust $ parseURI "http://purl.org/dc/elements/1.1/title"

dcCreated = fromJust $ parseURI "http://purl.org/dc/elements/1.1/created"

dctermsDescription = fromJust $ parseURI "http://purl.org/dc/terms/source"

namespace1 = Namespace "dc:" "http://purl.org/dc/elements/1.1/"

statement1 = Statement dcTitle (TaggedText "A test title" (localeFromID "en"))

statement2 = Statement dcCreated (TaggedText "2024-02-10" rootLocale)

statement3 = Statement dcCreated (TaggedText "2024-02-11" rootLocale)

testMetadata1 = Metadata {
  metadataNamespaces = [ namespace1 ],
  metadataStatements = [ statement1, statement2, statement3 ]
}


testMetadata = TestList [
  TestLabel "Encode Term" testEncodeTerm,
  TestLabel "Decode Termt" testDecodeTerm,
  TestLabel "Read" testRead
  ]
  
testEncodeTerm = TestList [
  testEncodeTerm1, testEncodeTerm2
  ]

testEncodeTerm1 = TestCase (assertEqual "encodeTerm 1" "http://purl.org/dc/elements/1.1/title" (encodeTerm [] dcTitle))

testEncodeTerm2 = TestCase (assertEqual "encodeTerm 2" "dc:title" (encodeTerm [namespace1] dcTitle))

  
testDecodeTerm = TestList [
  testDecodeTerm1, testDecodeTerm2
  ]

testDecodeTerm1 = TestCase (assertEqual "decodeTerm 1" (Just dcTitle) (decodeTerm [] "http://purl.org/dc/elements/1.1/title"))

testDecodeTerm2 = TestCase (assertEqual "decodeTerm 2" (Just dcTitle) (decodeTerm [namespace1] "dc:title"))


testRead = TestList [
  testRead1
  ]

json1 = [r|
{
  "namespaces": [
    {
      "prefix": "dc",
      "namespace": "http://purl.org/dc/elements/1.1/"
    }
  ],
  "statements": [
    {
      "term": "dc:title",
      "value": "A something title@en"
    },
    {
      "term": "dc:title",
      "value": "Um titulo de algo@pt"
    },
    {
      "term": "dc:created",
      "value": "2024-01-01"
    },
    {
      "term": "http://purl.org/dc/terms/source",
      "value": "Nowhere special"
    }
  ]
}
|] :: ByteString

testRead1 =
  let
    mmetadata = eitherDecode json1 :: Either String Metadata
    metadata = either error id mmetadata
    getTerm (Statement term _value) = term
    getValue (Statement _term value) = ttText value
    getLang (Statement _term value) = localeLanguageTag $ ttLocale value
  in
    TestCase (do
      assertEqual "Read 1 1" 1 (Prelude.length $ metadataNamespaces metadata)
      assertEqual "Read 1 2" dcTitle (getTerm $ metadataStatements metadata !! 0)
      assertEqual "Read 1 3" "A something title" (getValue $ metadataStatements metadata !! 0)
      assertEqual "Read 1 4" "en" (getLang $ metadataStatements metadata !! 0)
      assertEqual "Read 1 5" dcTitle (getTerm $ metadataStatements metadata !! 1)
      assertEqual "Read 1 6" "Um titulo de algo" (getValue $ metadataStatements metadata !! 1)
      assertEqual "Read 1 7" "pt" (getLang $ metadataStatements metadata !! 1)
      assertEqual "Read 1 8" dctermsDescription (getTerm $ metadataStatements metadata !! 3)
      assertEqual "Read 1 9" "Nowhere special" (getValue $ metadataStatements metadata !! 3)
      assertEqual "Read 1 10" "" (getLang $ metadataStatements metadata !! 2)
    )
