{-# LANGUAGE MultiParamTypeClasses #-}
module CaminoSpec(testCamino) where

import Test.HUnit
import Camino.Camino
import Data.Map as M

testCamino = TestList [
  TestLabel "Penance" testPenance
  ]
  
testPenance = TestList [
  TestLabel "SimplePenance" testSimplePenance,
  TestLabel "PenanceAppend" testPenanceAppend
  ]
  
simplePenance1 = SimplePenance 1.0

labelledPenance1 = LabeledPenance "One" simplePenance1
 
labelledPenance2 = LabeledPenance "Two" (SimplePenance 3.0)
 
compundPenance1 = CompoundPenance  (M.fromList [("a", (SimplePenance 0.5)), ("b", (SimplePenance 0.5))]) simplePenance1

compundPenance2 = CompoundPenance  (M.fromList [("c", (SimplePenance 0.5)), ("d", Reject)]) Reject

compundPenance3 = CompoundPenance  (M.fromList [("c", (SimplePenance 0.6)), ("d", (SimplePenance 0.7))]) (SimplePenance 1.3)

testSimplePenance = TestList [ testSimplePenance1, testSimplePenance2, testSimplePenance3, testSimplePenance4, testSimplePenance5, testSimplePenance6 ]

testSimplePenance1 = TestCase (assertEqual "Simple penance 1" Reject (simplePenance Reject))

testSimplePenance2 = TestCase (assertEqual "Simple penance 2" simplePenance1 (simplePenance simplePenance1))

testSimplePenance3 = TestCase (assertEqual "Simple penance 3" simplePenance1 (simplePenance labelledPenance1))

testSimplePenance4 = TestCase (assertEqual "Simple penance 4" Reject (simplePenance (LabeledPenance "Hello" Reject)))

testSimplePenance5 = TestCase (assertEqual "Simple penance 5" simplePenance1 (simplePenance compundPenance1))

testSimplePenance6 = TestCase (assertEqual "Simple penance 6" Reject (simplePenance compundPenance2))

testPenanceAppend = TestList [
  testPenanceAppend1, testPenanceAppend2, testPenanceAppend3, testPenanceAppend4, testPenanceAppend5, testPenanceAppend6,
  testPenanceAppend7, testPenanceAppend8, testPenanceAppend9, testPenanceAppend10, testPenanceAppend11, testPenanceAppend11,
  testPenanceAppend12, testPenanceAppend13, testPenanceAppend14, testPenanceAppend15, testPenanceAppend16, testPenanceAppend17,
  testPenanceAppend18, testPenanceAppend19, testPenanceAppend20
  ]

testPenanceAppend1 = TestCase (assertEqual "Penance Plus 1" Reject (Reject <> Reject))

testPenanceAppend2 = TestCase (assertEqual "Penance Plus 2" Reject (Reject <> simplePenance1))

testPenanceAppend3 = TestCase (assertEqual "Penance Plus 3" Reject (simplePenance1 <> Reject))

testPenanceAppend4 = TestCase (assertEqual "Penance Plus 4" Reject (Reject <> labelledPenance1))

testPenanceAppend5 = TestCase (assertEqual "Penance Plus 5" Reject (labelledPenance1 <> Reject))

testPenanceAppend6 = TestCase (assertEqual "Penance Plus 6" Reject (Reject <> compundPenance2))

testPenanceAppend7 = TestCase (assertEqual "Penance Plus 7" Reject (compundPenance2 <> Reject))

testPenanceAppend8 = TestCase (assertEqual "Penance Plus 8" (SimplePenance 2.0) (simplePenance1 <> simplePenance1))

testPenanceAppend9' label penance = TestCase (do
    assertEqual "Penance Plus 9 1" "One" label
    assertEqual "Penance Plus 9 2" (SimplePenance 2.0) penance
  )

testPenanceAppend9 = let result = simplePenance1 <> labelledPenance1 in
  case result of
    LabeledPenance label penance -> testPenanceAppend9' label penance
    _ -> TestCase (assertFailure ("Invalid type " ++ show result))


testPenanceAppend10' label penance = TestCase (do
    assertEqual "Penance Plus 10 1" "One" label
    assertEqual "Penance Plus 10 2" (SimplePenance 2.0) penance
  )

testPenanceAppend10 = let result = labelledPenance1 <> simplePenance1 in
  case result of
    LabeledPenance label penance -> testPenanceAppend10' label penance
    _ -> TestCase (assertFailure ("Invalid type " ++ show result))
 

testPenanceAppend11' components penance = TestCase (do
    assertEqual "Penance Plus 11 1" 2 (length components)
    assertEqual "Penance Plus 11 2" (SimplePenance 4.0) penance
    assertEqual "Penance Plus 11 3" (Just labelledPenance1) (components M.!? "One")
    assertEqual "Penance Plus 11 4" (Just labelledPenance2) (components M.!? "Two")
  )

testPenanceAppend11 = let result = labelledPenance1 <> labelledPenance2 in
  case result of
    CompoundPenance components penance -> testPenanceAppend11' components penance
    _ -> TestCase (assertFailure ("Invalid type " ++ show result))


testPenanceAppend12' components penance = TestCase (do
    assertEqual "Penance Plus 12/13 1" 3 (length components)
    assertEqual "Penance Plus 12/13 2" (SimplePenance 2.0) penance
    assertEqual "Penance Plus 12/13 3" (Just (SimplePenance 0.5)) (components M.!? "a")
    assertEqual "Penance Plus 12/13 4" (Just (SimplePenance 0.5)) (components M.!? "b")
    assertEqual "Penance Plus 12/13 5" (Just simplePenance1) (components M.!? "unknown")
  )

testPenanceAppend12 = let result = compundPenance1 <>simplePenance1 in
  case result of
    CompoundPenance components penance -> testPenanceAppend12' components penance
    _ -> TestCase (assertFailure ("Invalid type " ++ show result))

testPenanceAppend13 = let result = simplePenance1 <> compundPenance1 in
  case result of
    CompoundPenance components penance -> testPenanceAppend12' components penance
    _ -> TestCase (assertFailure ("Invalid type " ++ show result))

testPenanceAppend14 = let result = simplePenance1 <> compundPenance2 in
  case result of
    CompoundPenance _components penance -> TestCase (assertEqual "Penance Plus 14" Reject penance)
    _ -> TestCase (assertFailure ("Invalid penance " ++ show result))


testPenanceAppend15' components penance = TestCase (do
    assertEqual "Penance Plus 15/16 1" 3 (length components)
    assertEqual "Penance Plus 15/16 2" (SimplePenance 2.0) penance
    assertEqual "Penance Plus 15/16 3" (Just (SimplePenance 0.5)) (components M.!? "a")
    assertEqual "Penance Plus 15/16 4" (Just (SimplePenance 0.5)) (components M.!? "b")
    assertEqual "Penance Plus 15/16 5" (Just simplePenance1) (components M.!? "One")
  )

testPenanceAppend15 = let result = compundPenance1 <> labelledPenance1 in
  case result of
    CompoundPenance components penance -> testPenanceAppend15' components penance
    _ -> TestCase (assertFailure ("Invalid type " ++ show result))

testPenanceAppend16 = let result = labelledPenance1 <> compundPenance1 in
  case result of
    CompoundPenance components penance -> testPenanceAppend15' components penance
    _ -> TestCase (assertFailure ("Invalid type " ++ show result))

testPenanceAppend17' components penance = TestCase (do
    assertEqual "Penance Plus 17/18 1" 4 (length components)
    assertEqual "Penance Plus 17/18 2" Reject penance
    assertEqual "Penance Plus 17/18 3" (Just (SimplePenance 0.5)) (components M.!? "a")
    assertEqual "Penance Plus 17/18 4" (Just (SimplePenance 0.5)) (components M.!? "b")
    assertEqual "Penance Plus 17/18 5" (Just (SimplePenance 0.5)) (components M.!? "c")
    assertEqual "Penance Plus 17/18 6" (Just Reject) (components M.!? "d")
  )

testPenanceAppend17 = let result = compundPenance1 <> compundPenance2 in
  case result of
    CompoundPenance components penance -> testPenanceAppend17' components penance
    _ -> TestCase (assertFailure ("Invalid type " ++ show result))

testPenanceAppend18 = let result = compundPenance2 <> compundPenance1 in
  case result of
    CompoundPenance components penance -> testPenanceAppend17' components penance
    _ -> TestCase (assertFailure ("Invalid type " ++ show result))


testPenanceAppend19' components penance = TestCase (do
    assertEqual "Penance Plus 17/18 1" 4 (length components)
    assertEqual "Penance Plus 17/18 2" (SimplePenance 2.3) penance
    assertEqual "Penance Plus 17/18 3" (Just (SimplePenance 0.5)) (components M.!? "a")
    assertEqual "Penance Plus 17/18 4" (Just (SimplePenance 0.5)) (components M.!? "b")
    assertEqual "Penance Plus 17/18 5" (Just (SimplePenance 0.6)) (components M.!? "c")
    assertEqual "Penance Plus 17/18 6" (Just (SimplePenance 0.7)) (components M.!? "d")
  )

testPenanceAppend19 = let result = compundPenance1 <> compundPenance3 in
  case result of
    CompoundPenance components penance -> testPenanceAppend19' components penance
    _ -> TestCase (assertFailure ("Invalid type " ++ show result))

testPenanceAppend20 = let result = compundPenance3 <> compundPenance1 in
  case result of
    CompoundPenance components penance -> testPenanceAppend19' components penance
    _ -> TestCase (assertFailure ("Invalid type " ++ show result))
