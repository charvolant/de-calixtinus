{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module FormattingSpec(testFormatting) where

import Test.HUnit
import Data.Aeson
import Data.Aeson.Formatting
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Set as S

testFormatting :: Test
testFormatting = TestList [
    TestLabel "JPath" testJPath
  ]

testJPath = TestList [
    TestLabel "JPath Parse" testJPathParse
  , TestLabel "JPath Match" testJPathMatch
  ]

testJPathParse = TestList [
    testJPathParse1
  , testJPathParse2
  , testJPathParse3
  , testJPathParse4
  , testJPathParse5
  , testJPathParse6
  , testJPathParse7
  ]

testJPathParse1 =
    TestCase (do
      assertEqual "JPath Parse 1 1" JPAny (parseJPath "*")
      assertEqual "JPath Parse 1 2" JPAny (parseJPath " * ")
      assertEqual "JPath Parse 1 3" JPAny (parseJPath "* ")
      assertEqual "JPath Parse 1 4" JPAny (parseJPath " *")
    )

testJPathParse2 =
    TestCase (do
      assertEqual "JPath Parse 2 1" JPArray (parseJPath "[]")
      assertEqual "JPath Parse 2 2" JPArray (parseJPath " [ ] ")
      assertEqual "JPath Parse 2 3" JPArray (parseJPath "[ ] ")
      assertEqual "JPath Parse 2 4" JPArray (parseJPath " [   ]")
    )

testJPathParse3 =
    TestCase (do
      assertEqual "JPath Parse 3 1" (JPArrayElement (S.fromList [0])) (parseJPath "[0]")
      assertEqual "JPath Parse 3 2" (JPArrayElement (S.fromList [0])) (parseJPath " [0] ")
      assertEqual "JPath Parse 3 3" (JPArrayElement (S.fromList [0])) (parseJPath "[ 0] ")
      assertEqual "JPath Parse 3 4" (JPArrayElement (S.fromList [0])) (parseJPath " [ 0  ]")
    )

testJPathParse4 =
    TestCase (do
      assertEqual "JPath Parse 4 1" (JPArrayElement (S.fromList [0, 1])) (parseJPath "[0, 1]")
      assertEqual "JPath Parse 4 2" (JPArrayElement (S.fromList [0, 1])) (parseJPath " [1, 0] ")
      assertEqual "JPath Parse 4 3" (JPArrayElement (S.fromList [0, 1, 2])) (parseJPath "[ 0, 1, 2] ")
      assertEqual "JPath Parse 4 4" (JPArrayElement (S.fromList [0, 2, 4, 6])) (parseJPath " [ 0, 4, 2, 6  ]")
    )

testJPathParse5 =
    TestCase (do
      assertEqual "JPath Parse 5 1" (JPArrayElement (S.fromList [0, 1, 2])) (parseJPath "[0 .. 2]")
      assertEqual "JPath Parse 5 2" (JPArrayElement (S.fromList [2, 3, 4])) (parseJPath " [2 ..4] ")
      assertEqual "JPath Parse 5 3" (JPArrayElement (S.fromList [0, 2, 3, 4])) (parseJPath "[0,2..4]")
      assertEqual "JPath Parse 5 4" (JPArrayElement (S.fromList [0 .. 6])) (parseJPath " [ 0..3, 4, 5, 6  ]")
    )

testJPathParse6 =
    TestCase (do
      assertEqual "JPath Parse 5 1" (JPField "foo") (parseJPath "foo")
      assertEqual "JPath Parse 5 2" (JPField "0foo") (parseJPath " 0foo ")
      assertEqual "JPath Parse 5 3" (JPField "foo/bar") (parseJPath "foo/bar ")
      assertEqual "JPath Parse 5 4" (JPField "0.1") (parseJPath "0.1")
    )

testJPathParse7 =
    TestCase (do
      assertEqual "JPath Parse 7 1" JPObject (parseJPath "{}}")
      assertEqual "JPath Parse 7 2" JPObject (parseJPath " { } ")
      assertEqual "JPath Parse 7 3" JPObject (parseJPath "{ } ")
      assertEqual "JPath Parse 7 4" JPObject (parseJPath " {   }")
    )

toJson :: String -> Value
toJson txt = maybe (error ("Unable to decode text " ++ txt)) id $ (decode (LB.pack txt) :: Maybe Value)

testJPathMatch = TestList [
    testJPathMatch1
  , testJPathMatch2
  , testJPathMatch3
  , testJPathMatch4
  , testJPathMatch5
  , testJPathMatch6
  , testJPathMatch7
  , testJPathMatch8
  , testJPathMatch9
  , testJPathMatch10
  , testJPathMatch11
  , testJPathMatch12
  , testJPathMatch13
  , testJPathMatch14
  , testJPathMatch15
  , testJPathMatch16
  , testJPathMatch17
  , testJPathMatch18
  ]

testJPathMatch1 = TestCase $ assertEqual "JPath Match 1" True (matchJPath JPAny json Nothing Nothing)
  where
    json = toJson "12"

testJPathMatch2 = TestCase $ assertEqual "JPath Match 2" True (matchJPath JPAny json Nothing Nothing)
  where
    json = toJson "\"Hello\""


testJPathMatch3 = TestCase $ assertEqual "JPath Match 3" True (matchJPath JPAny json Nothing Nothing)
  where
    json = toJson "[1, 2, 3]"

testJPathMatch4 = TestCase $ assertEqual "JPath Match 4" True (matchJPath JPAny json Nothing Nothing)
  where
    json = toJson "{ \"foo\": \"bar\"}"

testJPathMatch5 = TestCase $ assertEqual "JPath Match 5" True (matchJPath JPObject json Nothing Nothing)
  where
    json = toJson "{ \"foo\": \"bar\"}"

testJPathMatch6 = TestCase $ assertEqual "JPath Match 6" False (matchJPath (JPObject) json Nothing Nothing)
  where
    json = toJson "[ \"foo\", \"bar\" ]"

testJPathMatch7 = TestCase $ assertEqual "JPath Match 7" False (matchJPath (JPObject) json Nothing Nothing)
  where
    json = toJson "false"

testJPathMatch8 = TestCase $ assertEqual "JPath Match 8" True (matchJPath (JPField "foo") json (Just "foo") Nothing)
  where
    json = toJson "{ \"foo\": \"bar\"}"

testJPathMatch9 = TestCase $ assertEqual "JPath Match 9" False (matchJPath (JPField "bar") json (Just "foo") Nothing)
  where
    json = toJson "{ \"foo\": \"bar\"}"

testJPathMatch10 = TestCase $ assertEqual "JPath Match 10" False (matchJPath (JPField "foo") json (Just "foo") Nothing)
  where
    json = toJson "[ \"foo\", \"bar\" ]"

testJPathMatch11 = TestCase $ assertEqual "JPath Match 11" False (matchJPath (JPField "foo") json (Just "foo") Nothing)
  where
    json = toJson "124"

testJPathMatch12 = TestCase $ assertEqual "JPath Match 12" True (matchJPath JPArray json Nothing (Just 0))
  where
    json = toJson "[ true, false ]"

testJPathMatch13 = TestCase $ assertEqual "JPath Match 13" False (matchJPath JPArray json (Just "foo") Nothing)
  where
    json = toJson "{ \"foo\": \"bar\"}"

testJPathMatch14 = TestCase $ assertEqual "JPath Match 14" False (matchJPath JPArray json Nothing (Just 0))
  where
    json = toJson "\"Hello\""

testJPathMatch15 = TestCase $ assertEqual "JPath Match 15" True (matchJPath (JPArrayElement $ S.fromList [0 .. 5]) json Nothing (Just 1))
  where
    json = toJson "[ \"foo\", \"bar\" ]"

testJPathMatch16 = TestCase $ assertEqual "JPath Match 16" False (matchJPath (JPArrayElement $ S.fromList [0 .. 5]) json Nothing (Just 6))
  where
    json = toJson "[ \"foo\", \"bar\" ]"

testJPathMatch17 = TestCase $ assertEqual "JPath Match 17" False (matchJPath (JPArrayElement $ S.fromList [0, 2, 4]) json Nothing (Just 1))
  where
    json = toJson "[ \"foo\", \"bar\" ]"

testJPathMatch18 = TestCase $ assertEqual "JPath Match 18" False (matchJPath (JPArrayElement $ S.fromList [0, 2, 4]) json Nothing (Just 2))
  where
    json = toJson "{ \"foo\": \"bar\" }"

