{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
module UtilSpec(testUtils) where

import Test.HUnit
import Data.Util

testUtils :: Test
testUtils = TestList [
    TestLabel "Canonicalise" testCanonicalise
  , TestLabel "Partition" testPartition
  , TestLabel "Categorise" testCategorise
  , TestLabel "ToFileName" testToFileName
  , TestLabel "MaybeSum" testMaybeSum
  , TestLabel "MaybeMax" testMaybeMin
  , TestLabel "MaybeMin" testMaybeMax
  , TestLabel "Unique" testUnique
  , TestLabel "CommaJoin" testCommaJoin
  ]
  
testCanonicalise = TestList [
  testCanonicalise1, testCanonicalise2, testCanonicalise3, testCanonicalise4,
  testCanonicalise5, testCanonicalise6, testCanonicalise7, testCanonicalise8,
  testCanonicalise9, testCanonicalise10, testCanonicalise11, testCanonicalise12,
  testCanonicalise13, testCanonicalise14, testCanonicalise15, testCanonicalise16,
  testCanonicalise17, testCanonicalise18, testCanonicalise19, testCanonicalise20,
  testCanonicalise21, testCanonicalise22, testCanonicalise23, testCanonicalise24,
  testCanonicalise25, testCanonicalise26, testCanonicalise27, testCanonicalise28,
  testCanonicalise29, testCanonicalise30, testCanonicalise31, testCanonicalise32
  ]

testCanonicalise1 = TestCase (assertEqual "Canonicalise 1" "Hello" (canonicalise "Hello"))

testCanonicalise2 = TestCase (assertEqual "Canonicalise 2" "Rabcal" (canonicalise "Rabçal"))

testCanonicalise3 = TestCase (assertEqual "Canonicalise 3" "AAAAAAAAA" (canonicalise "ÀÁÂÄǍÆÃÅĀ"))

testCanonicalise4 = TestCase (assertEqual "Canonicalise 4" "aaaaaaaaa" (canonicalise "àáâäǎæãåā"))

testCanonicalise5 = TestCase (assertEqual "Canonicalise 5" "CCCC" (canonicalise "ÇĆČĊ"))

testCanonicalise6 = TestCase (assertEqual "Canonicalise 6" "cccc" (canonicalise "çćčċ"))

testCanonicalise7 = TestCase (assertEqual "Canonicalise 7" "DDD" (canonicalise "ḎĎÐ"))

testCanonicalise8 = TestCase (assertEqual "Canonicalise 8" "ddd" (canonicalise "ḏďð"))

testCanonicalise9 = TestCase (assertEqual "Canonicalise 9" "EEEEEEEEE" (canonicalise "ÈÉÊËĚẼĒĖĘ"))

testCanonicalise10 = TestCase (assertEqual "Canonicalise 10" "eeeeeeeee" (canonicalise "èéêëěẽēėę"))

testCanonicalise11 = TestCase (assertEqual "Canonicalise 11" "IIIIIIIII" (canonicalise "ÌÍÎÏǏĨĪİĮ"))

testCanonicalise12 = TestCase (assertEqual "Canonicalise 12" "iiiiiiii" (canonicalise "ìíîïǐĩīį"))

testCanonicalise13 = TestCase (assertEqual "Canonicalise 13" "LLL" (canonicalise "ḺŁĻ"))

testCanonicalise14 = TestCase (assertEqual "Canonicalise 14" "lll" (canonicalise "ḻłļ"))

testCanonicalise15 = TestCase (assertEqual "Canonicalise 15" "NNNNN" (canonicalise "ṈŊÑŃŅ"))

testCanonicalise16 = TestCase (assertEqual "Canonicalise 16" "nnnnn" (canonicalise "ṉŋñńņ"))

testCanonicalise17 = TestCase (assertEqual "Canonicalise 17" "OOOOOOOOO" (canonicalise "ÒÓÔÖǑŒØÕŌ"))

testCanonicalise18 = TestCase (assertEqual "Canonicalise 18" "ooooooooo" (canonicalise "òóôöǒœøõō"))

testCanonicalise19 = TestCase (assertEqual "Canonicalise 19" "RR" (canonicalise "ṞŘ"))

testCanonicalise20 = TestCase (assertEqual "Canonicalise 19" "rr" (canonicalise "ṟř"))

testCanonicalise21 = TestCase (assertEqual "Canonicalise 21" "SSSSS" (canonicalise "ẞŚŠŞȘ"))

testCanonicalise22 = TestCase (assertEqual "Canonicalise 22" "sssss" (canonicalise "ßśšşș"))

testCanonicalise23 = TestCase (assertEqual "Canonicalise 23" "TTTT" (canonicalise "ṮȚŤÞ"))

testCanonicalise24 = TestCase (assertEqual "Canonicalise 24" "tttt" (canonicalise "ṯțťþ"))

testCanonicalise25 = TestCase (assertEqual "Canonicalise 25" "UUUUUUUUU" (canonicalise "ÙÚÛÜǓŨŪŰŮ"))

testCanonicalise26 = TestCase (assertEqual "Canonicalise 27" "uuuuuuuuu" (canonicalise "ùúûüǔũūűů"))

testCanonicalise27 = TestCase (assertEqual "Canonicalise 27" "W" (canonicalise "Ŵ"))

testCanonicalise28 = TestCase (assertEqual "Canonicalise 28" "w" (canonicalise "ŵ"))

testCanonicalise29 = TestCase (assertEqual "Canonicalise 29" "YYY" (canonicalise "ÝŶŸ"))

testCanonicalise30 = TestCase (assertEqual "Canonicalise 30" "yyy" (canonicalise "ýŷÿ"))

testCanonicalise31 = TestCase (assertEqual "Canonicalise 31" "ZZZ" (canonicalise "ŹŽŻ"))

testCanonicalise32 = TestCase (assertEqual "Canonicalise 32" "zzz" (canonicalise "źžż"))

testPartition = TestList [
  testPartition1, testPartition2, testPartition3, testPartition4,
  testPartition5, testPartition6
  ]

part1 v = v `mod` 3

testPartition1 = TestCase (assertEqual "Partition 1" []  (partition part1 []))

testPartition2 = TestCase (assertEqual "Partition 2" [(1, [1])]  (partition part1 [1]))

testPartition3 = TestCase (assertEqual "Partition 3" [(1, [1]), (2, [2])]  (partition part1 [1, 2]))

testPartition4 = TestCase (assertEqual "Partition 4" [(1, [1]), (2, [2]), (0, [3])]  (partition part1 [1, 2, 3]))

testPartition5 = TestCase (assertEqual "Partition 5" [(1, [1, 4]), (2, [2]), (0, [3]), (2, [5])]  (partition part1 [1, 4, 2, 3, 5]))

testPartition6 = TestCase (assertEqual "Partition 6" [('a', ["animal"]), ('d', ["dog", "dingo"]), ('a', ["aardvark"])]  (partition head ["animal", "dog", "dingo", "aardvark"]))

testCategorise = TestList [
  testCategorise1, testCategorise2, testCategorise3, testCategorise4,
  testCategorise5, testCategorise6, testCategorise7
  ]

testCategorise1 = TestCase (assertEqual "Categorise 1" "A-E" (categorise "AARDVARK"))

testCategorise2 = TestCase (assertEqual "Categorise 1" "A-E" (categorise "aardvark"))

testCategorise3 = TestCase (assertEqual "Categorise 3" "N-T" (categorise "Tango"))

testCategorise4 = TestCase (assertEqual "Categorise 4" "U-Z" (categorise "yak"))

testCategorise5 = TestCase (assertEqual "Categorise 5" "..." (categorise "99"))

testCategorise6 = TestCase (assertEqual "Categorise 6" "..." (categorise "'t Hooft'"))

testCategorise7 = TestCase (assertEqual "Categorise 7" "..." (categorise ""))

testToFileName = TestList [
  testToFileName1, testToFileName2, testToFileName3, testToFileName4
  ]

testToFileName1 = TestCase (assertEqual "ToFileName 1" "simple" (toFileName "simple"))

testToFileName2 = TestCase (assertEqual "ToFileName 2" "SiMpLe" (toFileName "SiMpLe"))

testToFileName3 = TestCase (assertEqual "ToFileName 3" "LongName" (toFileName "Long Name"))

testToFileName4 = TestCase (assertEqual "ToFileName 4" "FromHeretoThere" (toFileName "From 'Here' to -There-"))

testMaybeSum = TestList [
  testMaybeSum1, testMaybeSum2, testMaybeSum3, testMaybeSum4
  ]

testMaybeSum1 = TestCase (assertEqual "MaybeSum 1" Nothing (maybeSum Nothing Nothing))

testMaybeSum2 = TestCase (assertEqual "MaybeSum 2" (Just 5) (maybeSum Nothing (Just 5)))

testMaybeSum3 = TestCase (assertEqual "MaybeSum 3" (Just 7) (maybeSum (Just 7) Nothing))

testMaybeSum4 = TestCase (assertEqual "MaybeSum 4" (Just 12) (maybeSum (Just 7) (Just 5)))

testMaybeMin = TestList [
  testMaybeMin1, testMaybeMin2, testMaybeMin3, testMaybeMin4
  ]

testMaybeMin1 = TestCase (assertEqual "MaybeMin 1" Nothing (maybeMin (Nothing :: Maybe Int) Nothing))

testMaybeMin2 = TestCase (assertEqual "MaybeMin 2" (Just 5) (maybeMin Nothing (Just 5 :: Maybe Int)))

testMaybeMin3 = TestCase (assertEqual "MaybeMin 3" (Just 7) (maybeMin (Just 7 :: Maybe Int) Nothing))

testMaybeMin4 = TestCase (assertEqual "MaybeMin 4" (Just 5) (maybeMin (Just 7 :: Maybe Int) (Just 5)))

testMaybeMax = TestList [
  testMaybeMax1, testMaybeMax2, testMaybeMax3, testMaybeMax4
  ]

testMaybeMax1 = TestCase (assertEqual "MaybeMax 1" Nothing (maybeMax (Nothing :: Maybe Int) Nothing))

testMaybeMax2 = TestCase (assertEqual "MaybeMax 2" (Just 5) (maybeMax Nothing (Just 5 :: Maybe Int)))

testMaybeMax3 = TestCase (assertEqual "MaybeMax 3" (Just 7) (maybeMax (Just 7 :: Maybe Int) Nothing))

testMaybeMax4 = TestCase (assertEqual "MaybeMax 4" (Just 7) (maybeMax (Just 7 :: Maybe Int) (Just 5)))

testUnique = TestList [
  testUnique1, testUnique2, testUnique3, testUnique4
  ]


testUnique1 = TestCase (assertEqual "Unique 1" [] (unique ([] :: [Int])))

testUnique2 = TestCase (assertEqual "Unique 2" [1, 2, 3] (unique [1, 2, 3]))

testUnique3 = TestCase (assertEqual "Unique 3" [1, 2, 3] (unique [1, 2, 3, 2, 1]))

testUnique4 = TestCase (assertEqual "Unique 4" [3, 2, 1] (unique [3, 3, 2, 3, 1, 2, 3, 2, 1]))

testCommaJoin = TestList [
  testCommaJoin1, testCommaJoin2, testCommaJoin3, testCommaJoin4,
  testCommaJoin5, testCommaJoin6
  ]


testCommaJoin1 = TestCase (assertEqual "CommaJoin 1" "" (commaJoin []))

testCommaJoin2 = TestCase (assertEqual "CommaJoin 2" "Single" (commaJoin ["Single"]))

testCommaJoin3 = TestCase (assertEqual "CommaJoin 3" "One, Two, Three" (commaJoin ["One", "Two", "Three"]))

testCommaJoin4 = TestCase (assertEqual "CommaJoin 4" "One, Two, Three" (commaJoin [" One ", "Two ", " Three"]))

testCommaJoin5 = TestCase (assertEqual "CommaJoin 5" "Sentence. Clause; Other" (commaJoin ["Sentence.", "Clause;", "Other"]))

testCommaJoin6 = TestCase (assertEqual "CommaJoin 6" "Sentence! Clause, Other." (commaJoin ["Sentence!", "Clause", "Other. "]))
