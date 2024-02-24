{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module MetadataSpec(testMetadata) where

import Test.HUnit
import Data.Aeson
import Data.Either
import Data.Maybe (fromJust, isJust)
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

statement1 = Statement dcTitle "A test title"

statement2 = Statement dcCreated "2024-02-10"

statement3 = Statement dcCreated "2024-02-11"

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
      "value": "A something title"
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
    getValue (Statement _term value) = value
  in
    TestCase (do
      assertEqual "Read 1 1" 1 (Prelude.length $ metadataNamespaces metadata)
      assertEqual "Read 1 2" dcTitle (getTerm $ metadataStatements metadata !! 0)
      assertEqual "Read 1 3" "A something title" (getValue $ metadataStatements metadata !! 0)
      assertEqual "Read 1 4" dctermsDescription (getTerm $ metadataStatements metadata !! 2)
      assertEqual "Read 1 5" "Nowhere special" (getValue $ metadataStatements metadata !! 2)
    )
