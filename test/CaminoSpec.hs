{-# LANGUAGE MultiParamTypeClasses #-}
module CaminoSpec(testCamino) where

import Test.HUnit
import Camino.Camino
import Data.Map as M

testCamino = TestList [
  TestLabel "Penance" testPenance
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

