{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MetadataSpec(testMetadata) where

import Test.HUnit
import Data.Aeson
import Data.Localised
import Data.Maybe (fromJust)
import Data.Metadata
import Network.URI
import Text.RawString.QQ
import Data.ByteString.Lazy

-- dcElements = fromJust $ parseURI "http://purl.org/dc/elements/1.1/"

dcTitle = fromJust $ parseURI "http://purl.org/dc/elements/1.1/title"

dcCreated = fromJust $ parseURI "http://purl.org/dc/elements/1.1/created"

dctermsDescription = fromJust $ parseURI "http://purl.org/dc/terms/source"

namespace1 = Namespace "dc:" "http://purl.org/dc/elements/1.1/"

statement1 = Statement dcTitle (TaggedText (localeFromIDOrError "en") "A test title")

statement2 = Statement dcCreated (TaggedText rootLocale "2024-02-10")

statement3 = Statement dcCreated (TaggedText rootLocale "2024-02-11")

testMetadata1 = Metadata {
  metadataNamespaces = [ namespace1 ],
  metadataStatements = [ statement1, statement2, statement3 ]
}


testMetadata :: Test
testMetadata = TestList [
    TestLabel "Encode Term" testEncodeTerm
  , TestLabel "Decode Term" testDecodeTerm
  , TestLabel "Read" testRead
  , TestLabel "Write" testWrite
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
  testRead1, testRead2
  ]

json1 = [r|
{
  "namespaces": [
    {
      "prefix": "dc",
      "namespace": "http://purl.org/dc/elements/1.1/"
    },
    {
      "prefix": "skos",
      "namespace": "http://www.w3.org/2004/02/skos/core#"
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
    },
    {
      "term": "skos:example",
      "value": "What? Me?"
    }
  ]
}
|] :: ByteString


json2 = [r|
{
  "statements": [
    {
      "term": "dc:title",
      "value": "A something title@en"
    }
  ]
}
|] :: ByteString

testRead1 =
  let
    mmetadata = eitherDecode json1 :: Either String Metadata
    metadata = either error id mmetadata
    getTerm (Statement term _value) = term
    getTerm _ = error "Invalid term"
    getValue (Statement _term value) = plainText value
    getValue _ = error "Invalid value"
    getLang (Statement _term value) = localeLanguageTag $ locale value
    getLang _ = error "Invalid language"
  in
    TestCase (do
      assertEqual "Read 1 1" 3 (Prelude.length $ metadataNamespaces metadata)
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

testRead2 =
  let
    mmetadata = eitherDecode json2 :: Either String Metadata
    metadata = either error id mmetadata
    getTerm (Statement term _value) = term
    getTerm _ = error "Invalid term"
    getValue (Statement _term value) = plainText value
    getValue _ = error "Invalid value"
    getLang (Statement _term value) = localeLanguageTag $ locale value
    getLang _ = error "Invalid language"
  in
    TestCase (do
      assertEqual "Read 2 1" 2 (Prelude.length $ metadataNamespaces metadata)
      assertEqual "Read 2 2" dcTitle (getTerm $ metadataStatements metadata !! 0)
      assertEqual "Read 2 3" "A something title" (getValue $ metadataStatements metadata !! 0)
    )

testWrite = TestList [
  testWrite1
  ]

testWrite1 =
  let
    em = encode testMetadata1
  in
    TestCase (do
        assertEqual "Write 1 1" "{\"namespaces\":[{\"namespace\":\"http://purl.org/dc/elements/1.1/\",\"prefix\":\"dc\"}],\"statements\":[{\"term\":\"dc:title\",\"value\":\"A test title@en\"},{\"term\":\"dc:created\",\"value\":\"2024-02-10\"},{\"term\":\"dc:created\",\"value\":\"2024-02-11\"}]}" em
      )