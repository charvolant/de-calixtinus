{-# LANGUAGE MultiParamTypeClasses #-}
module CaminoSpec(testCamino) where

import Test.HUnit
import Camino.Camino

testCamino :: Test
testCamino = TestList [
  TestLabel "Penance" testPenance,
  TestLabel "Leg" testLeg
  ]
  
testPenance = TestList [
  TestLabel "PenanceAppend" testPenanceAppend
  ]
  
simplePenance1 = Penance 1.0

testPenanceAppend = TestList [
  testPenanceAppend1, testPenanceAppend2, testPenanceAppend3, testPenanceAppend4
  ]

testPenanceAppend1 = TestCase (assertEqual "Penance Plus 1" Reject (Reject <> Reject))

testPenanceAppend2 = TestCase (assertEqual "Penance Plus 2" Reject (Reject <> simplePenance1))

testPenanceAppend3 = TestCase (assertEqual "Penance Plus 3" Reject (simplePenance1 <> Reject))

testPenanceAppend4 = TestCase (assertEqual "Penance Plus 8" (Penance 2.0) (simplePenance1 <> simplePenance1))

testLeg = TestList [
  TestLabel "LegEqual" testLegEqual,
  TestLabel "LegCompare" testLegCompare
  ]

location1 = placeholderLocation "L1"

location2 = placeholderLocation "L2"

location3 = placeholderLocation "L3"

leg1 = Leg Road location1 location2 1.2 Nothing 20.0 10.0 Nothing Nothing

leg2 = Leg Trail location1 location2 1.2 Nothing 20.0 10.0 Nothing Nothing

leg3 = Leg Road location1 location3 1.2 Nothing 20.0 10.0 Nothing Nothing

leg4 = Leg Road location2 location3 1.2 Nothing 20.0 10.0 Nothing Nothing

leg5 = Leg Road location1 location3 1.3 Nothing 20.0 10.0 Nothing Nothing

leg6 = Leg Road location1 location3 1.3 Nothing 25.0 0.0 Nothing Nothing

testLegEqual = TestList [
  testLegEqual1, testLegEqual2, testLegEqual3, testLegEqual4, testLegEqual5, testLegEqual6
  ]

testLegEqual1 = TestCase (assertEqual "Leg Equals 1" True (leg1 == leg1))

testLegEqual2 = TestCase (assertEqual "Leg Equals 2" False (leg1 == leg2))

testLegEqual3 = TestCase (assertEqual "Leg Equals 3" False (leg1 == leg3))

testLegEqual4 = TestCase (assertEqual "Leg Equals 4" False (leg3 == leg4))

testLegEqual5 = TestCase (assertEqual "Leg Equals 5" False (leg3 == leg5))

testLegEqual6 = TestCase (assertEqual "Leg Equals 6" True (leg5 == leg6))


testLegCompare = TestList [
  testLegCompare1, testLegCompare2, testLegCompare3, testLegCompare4, testLegCompare5, testLegCompare6,
  testLegCompare7, testLegCompare8, testLegCompare9, testLegCompare10
  ]

testLegCompare1 = TestCase (assertEqual "Leg Compare 1" EQ (leg1 `compare` leg1))

testLegCompare2 = TestCase (assertEqual "Leg Compare 2" LT (leg1 `compare` leg2))

testLegCompare3 = TestCase (assertEqual "Leg Compare 3" LT (leg1 `compare` leg3))

testLegCompare4 = TestCase (assertEqual "Leg Compare 4" LT (leg3 `compare` leg4))

testLegCompare5 = TestCase (assertEqual "Leg Compare 5" LT (leg3 `compare` leg5))

testLegCompare6 = TestCase (assertEqual "Leg Compare 6" EQ (leg5 `compare` leg6))

testLegCompare7 = TestCase (assertEqual "Leg Compare 7" GT (leg2 `compare` leg1))

testLegCompare8 = TestCase (assertEqual "Leg Compare 8" GT (leg3 `compare` leg1))

testLegCompare9 = TestCase (assertEqual "Leg Compare 9" GT (leg4 `compare` leg1))

testLegCompare10 = TestCase (assertEqual "Leg Compare 10" EQ (leg6 `compare` leg5))
