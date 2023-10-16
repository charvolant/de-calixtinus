module WalkingSpec(testWalking) where

import Test.HUnit
import Camino.Walking
import Camino.Camino
import TestUtils

testWalking :: Test
testWalking = TestList [
  TestLabel "Naismith" testNaismith, 
  TestLabel "Tobler" testTobler, 
  TestLabel "Tranter" testTranter
  ]

testNaismith = TestList [testNaismith1, testNaismith2, testNaismith3]

testNaismith1 = TestCase (assertFloatEqual "Flat distance" 1.0 (naismith 5.0 0.0 0.0) 0.001)

testNaismith2 = TestCase (assertFloatEqual "Ascent" 1.5 (naismith 5.0 300 0.0) 0.001)

testNaismith3 = TestCase (assertFloatEqual "Descent" 1.0 (naismith 5.0 0.0 300.0) 0.001)

testTobler = TestList [testTobler1, testTobler2, testTobler3, testTobler4, testTobler5, testTobler6]

testTobler1 = TestCase $ assertFloatEqual "Flat distance" 0.993 (tobler 5.0 0.0 0.0) 0.001

testTobler2 = TestCase $ assertFloatEqual "Small rise" 1.065 (tobler 5.0 100.0 0.0) 0.001

testTobler3 = TestCase $ assertFloatEqual "Large rise" 1.409 (tobler 5.0 500.0 0.0) 0.001

testTobler4 = TestCase $ assertFloatEqual "Small fall" 0.926 (tobler 5.0 0.0 100.0) 0.001

testTobler5 = TestCase $ assertFloatEqual "Large fall" 0.993 (tobler 5.0 0.0 500.0) 0.001

testTobler6 = TestCase $ assertFloatEqual "Mixed" 0.995 (tobler 5.0 50.0 50.0) 0.001

testTranter = TestList [
  testTranter1, testTranter2, testTranter3, testTranter4, testTranter5, testTranter6, testTranter7
  ]

testTranter1 = TestCase $ assertMaybeFloatEqual "Tranter 15 1" (Just 0.5) (tranter SuperFit 1.0) 0.001

testTranter2 = TestCase $ assertMaybeFloatEqual "Tranter 15 1.5" (Just 0.75) (tranter SuperFit 1.5) 0.001

testTranter3 = TestCase $ assertMaybeFloatEqual "Tranter 15 7.5" (Just 5.0) (tranter SuperFit 7.5) 0.001

testTranter4 = TestCase $ assertMaybeFloatEqual "Tranter 20 1.5" (Just 0.9375) (tranter VeryFit 1.5) 0.001

testTranter5 = TestCase $ assertMaybeFloatEqual "Tranter 25 9.0" (Just 11.5) (tranter Fit 9.0) 0.001

testTranter6 = TestCase $ assertMaybeFloatEqual "Tranter 25 9.0" Nothing (tranter Fit (-1.0)) 0.001

testTranter7 = TestCase $ assertMaybeFloatEqual "Tranter 25 9.0" Nothing (tranter Unfit 15.0) 0.001
