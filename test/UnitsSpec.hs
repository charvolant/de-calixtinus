{-# LANGUAGE OverloadedStrings #-}
module UnitsSpec(testUnits) where

import Test.HUnit
import Camino.Units
import Text.Read (readEither)
import TestUtils

testUnits :: Test
testUnits = TestList [
      TestLabel "Show" testShow
    , TestLabel "Read" testRead
    , TestLabel "Convert" testConvert
  ]

testShow :: Test
testShow = TestList [
    testShow1, testShow2, testShow3, testShow4
  , testShow5, testShow6
  ]
  
testShow1 = TestCase (assertEqual "Units Show 1" "" (show Unit))

testShow2 = TestCase (assertEqual "Units Show 2" "m" (show Metre))

testShow3 = TestCase (assertEqual "Units Show 3" "km" (show Kilometre))

testShow4 = TestCase (assertEqual "Units Show 4" "mi" (show Mile))

testShow5 = TestCase (assertEqual "Units Show 5" "ft" (show Foot))

testShow6 = TestCase (assertEqual "Units Show 6" "hr" (show Hour))


testRead :: Test
testRead = TestList [
    testRead1, testRead2, testRead3, testRead4
  , testRead5, testRead6
  ]
  
testRead1 = TestCase (assertEqual "Units Read 1" Unit (read ""))

testRead2 = TestCase (assertEqual "Units Read 2" Metre (read " m"))

testRead3 = TestCase (assertEqual "Units Read 3" Kilometre (read "km "))

testRead4 = TestCase (assertEqual "Units Read 4" Mile (read " mi "))

testRead5 = TestCase (assertEqual "Units Read 5" Foot (read "   ft   "))

testRead6 = TestCase (assertEqual "Units Read 6" Hour (read "hr"))

testRead7 = TestCase (assertEqual "Units Read 7" (Left "blah") (readEither "blah" :: Either String Unit))


testConvert :: Test
testConvert = TestList [
    testConvert1, testConvert2, testConvert3, testConvert4
  ]

testConvert1 = TestCase (assertFloatEqual "Units Convert 1" 1.0 (convertAmount Metre Metre 1.0) 0.001)

testConvert2 = TestCase (assertFloatEqual "Units Convert 2" 6.562 (convertAmount Metre Foot 2.0) 0.001)

testConvert3 = TestCase (assertFloatEqual "Units Convert 3" 3.480 (convertAmount Kilometre Mile 5.6) 0.001)

testConvert4 = TestCase (assertFloatEqual "Units Convert 4" 5.600 (convertAmount Mile Kilometre 3.480) 0.001)
