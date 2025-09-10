{-# LANGUAGE OverloadedStrings #-}
module SplineSpec(testSpline) where

import Test.HUnit
import Control.Monad
import Data.Default.Class
import Data.Placeholder
import Data.Spline
import Camino.Camino
import Camino.Display.SVG
import TestUtils

testSpline :: Test
testSpline = TestList [
    TestLabel "SplineAt" testSplineAt
  , TestLabel "SplineSlopeAt" testSplineSlopeAt
  , TestLabel "SplineSecondDevs" testSplineSecondDevs
  , TestLabel "MakeSpline" testSplineMakeSpline
  , TestLabel "ToBezier" testToBezier
  , TestLabel "SVG" testSVGSpline
  ]

spline1 = Spline 0.0 1.0 1.0 2.0 3.0 4.0 :: Spline Float

testSplineAt = TestList [testSplineAt1, testSplineAt2, testSplineAt3]

testSplineAt1 = TestCase (assertFloatEqual "Spline At 1" 4.0 (splineAt spline1 0.0) 0.001)

testSplineAt2 = TestCase (assertFloatEqual "Spline At 2" 10.0 (splineAt spline1 1.0) 0.001)

testSplineAt3 = TestCase (assertFloatEqual "Spline At 3" 6.125  (splineAt spline1 0.5) 0.001)

testSplineSlopeAt = TestList [testSplineSlopeAt1, testSplineSlopeAt2, testSplineSlopeAt3]

testSplineSlopeAt1 = TestCase (assertFloatEqual "Spline Slope At 1" 3.0 (splineSlopeAt spline1 0.0) 0.001)

testSplineSlopeAt2 = TestCase (assertFloatEqual "Spline Slope At 2" 10.0 (splineSlopeAt spline1 1.0) 0.001)

testSplineSlopeAt3 = TestCase (assertFloatEqual "Spline Slope At 3" 5.75  (splineSlopeAt spline1 0.5) 0.001)

testSplineSecondDevs = TestList [
  testSplineSecondDevs1, testSplineSecondDevs2, testSplineSecondDevs3, testSplineSecondDevs4,
  testSplineSecondDevs5, testSplineSecondDevs6, testSplineSecondDevs7
  ]

testSplineSecondDevs1 = TestCase (do
    let sds = spline2ndDevs NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0)]
    assertEqual "Spline Second Devs 1 1" 2 (length sds)
    assertFloatEqual "Spline Second Devs 1 2" 0.0 (sds !! 0) 0.001
    assertFloatEqual "Spline Second Devs 1 3" 0.0 (sds !! 1) 0.001
  )

testSplineSecondDevs2 = TestCase (do
    let sds = spline2ndDevs NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 2.0)]
    assertEqual "Spline Second Devs 2 1" 2 (length sds)
    assertFloatEqual "Spline Second Devs 2 2" 0.0 (sds !! 0) 0.001
    assertFloatEqual "Spline Second Devs 2 3" 0.0 (sds !! 1) 0.001
  )

testSplineSecondDevs3 = TestCase (do
    let sds = spline2ndDevs NaturalBoundary (ClampBoundary 0.0) [(0.0, 1.0), (1.0, 2.0)]
    assertEqual "Spline Second Devs 3 1" 2 (length sds)
    assertFloatEqual "Spline Second Devs 3 2" 0.0 (sds !! 0) 0.001
    assertFloatEqual "Spline Second Devs 3 3" (-3.0) (sds !! 1) 0.001
  )

testSplineSecondDevs4 = TestCase (do
    let sds = spline2ndDevs (ClampBoundary 0.0) NaturalBoundary [(0.0, 1.0), (1.0, 2.0)]
    assertEqual "Spline Second Devs 4 1" 2 (length sds)
    assertFloatEqual "Spline Second Devs 4 2" 3.0 (sds !! 0) 0.001
    assertFloatEqual "Spline Second Devs 4 3" 0.0 (sds !! 1) 0.001
  )

testSplineSecondDevs5 = TestCase (do
    let sds = spline2ndDevs NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0), (2.0, 1.0)]
    assertEqual "Spline Second Devs 5 1" 3 (length sds)
    assertFloatEqual "Spline Second Devs 5 2" 0.0 (sds !! 0) 0.001
    assertFloatEqual "Spline Second Devs 5 3" 0.0 (sds !! 1) 0.001
    assertFloatEqual "Spline Second Devs 5 4" 0.0 (sds !! 2) 0.001
  )

testSplineSecondDevs6 = TestCase (do
    let sds = spline2ndDevs NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0), (2.0, 2.0)]
    assertEqual "Spline Second Devs 6 1" 3 (length sds)
    assertFloatEqual "Spline Second Devs 6 2" 0.0 (sds !! 0) 0.001
    assertFloatEqual "Spline Second Devs 6 3" 1.5 (sds !! 1) 0.001
    assertFloatEqual "Spline Second Devs 6 4" 0.0 (sds !! 2) 0.001
  )

testSplineSecondDevs7 = TestCase (do
    let sds = spline2ndDevs NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 2.0), (2.0, 1.0)]
    assertEqual "Spline Second Devs 6 1" 3 (length sds)
    assertFloatEqual "Spline Second Devs 7 2" 0.0 (sds !! 0) 0.001
    assertFloatEqual "Spline Second Devs 7 3" (-3.0) (sds !! 1) 0.001
    assertFloatEqual "Spline Second Devs 7 4" 0.0 (sds !! 2) 0.001
  )



testSplineMakeSpline = TestList [
  testSplineMakeSpline1, testSplineMakeSpline2, testSplineMakeSpline3, testSplineMakeSpline4,
  testSplineMakeSpline5, testSplineMakeSpline6, testSplineMakeSpline7
  ]

testSplineMakeSpline1 = TestCase (do
    let sps = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0)] :: [Spline Float]
    assertEqual "Make Spline 1 1" 1 (length sps)
    let sp1 = sps !! 0
    assertFloatEqual "Make Spline 1 2" 1.0 (splineAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 1 3" 1.0 (splineAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 1 4" 1.0 (splineAt sp1 1.0) 0.001
    assertFloatEqual "Make Spline 1 5" 0.0 (splineSlopeAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 1 5" 0.0 (splineSlopeAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 1 7" 0.0 (splineSlopeAt sp1 1.0) 0.001
  )

testSplineMakeSpline2 = TestCase (do
    let sps = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 2.0)] :: [Spline Float]
    assertEqual "Make Spline 2 1" 1 (length sps)
    let sp1 = sps !! 0
    assertFloatEqual "Make Spline 2 2" 1.0 (splineAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 2 3" 1.5 (splineAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 2 4" 2.0 (splineAt sp1 1.0) 0.001
    assertFloatEqual "Make Spline 2 5" 1.0 (splineSlopeAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 2 5" 1.0 (splineSlopeAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 2 7" 1.0 (splineSlopeAt sp1 1.0) 0.001
  )

testSplineMakeSpline3 = TestCase (do
    let sps = makeSpline NaturalBoundary (ClampBoundary 0.0) [(0.0, 1.0), (1.0, 2.0)] :: [Spline Float]
    assertEqual "Make Spline 3 1" 1 (length sps)
    let sp1 = sps !! 0
    assertFloatEqual "Make Spline 3 2" 1.0 (splineAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 3 3" 1.6875 (splineAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 3 4" 2.0 (splineAt sp1 1.0) 0.001
    assertFloatEqual "Make Spline 3 5" 1.5 (splineSlopeAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 3 5" 1.125 (splineSlopeAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 3 7" 0.0 (splineSlopeAt sp1 1.0) 0.001
  )

testSplineMakeSpline4 = TestCase (do
    let sps = makeSpline (ClampBoundary 0.0) NaturalBoundary [(0.0, 1.0), (1.0, 2.0)] :: [Spline Float]
    assertEqual "Make Spline 4 1" 1 (length sps)
    let sp1 = sps !! 0
    assertFloatEqual "Make Spline 4 2" 1.0 (splineAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 4 3" 1.3125 (splineAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 4 4" 2.0 (splineAt sp1 1.0) 0.001
    assertFloatEqual "Make Spline 4 5" 0.0 (splineSlopeAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 4 6" 1.125 (splineSlopeAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 4 7" 1.5 (splineSlopeAt sp1 1.0) 0.001
  )

testSplineMakeSpline5 = TestCase (do
    let sps = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0), (2.0, 1.0)] :: [Spline Float]
    assertEqual "Make Spline 5 1" 2 (length sps)
    let sp1 = sps !! 0
    let sp2 = sps !! 1
    assertFloatEqual "Make Spline 5 2" 1.0 (splineAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 5 3" 1.0 (splineAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 5 4" 1.0 (splineAt sp1 1.0) 0.001
    assertFloatEqual "Make Spline 5 5" 1.0 (splineAt sp2 1.0) 0.001
    assertFloatEqual "Make Spline 5 6" 1.0 (splineAt sp2 1.5) 0.001
    assertFloatEqual "Make Spline 5 7" 1.0 (splineAt sp2 2.0) 0.001
    assertFloatEqual "Make Spline 5 8" 0.0 (splineSlopeAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 5 9" 0.0 (splineSlopeAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 5 10" 0.0 (splineSlopeAt sp1 1.0) 0.001
    assertFloatEqual "Make Spline 5 11" 0.0 (splineSlopeAt sp2 1.0) 0.001
    assertFloatEqual "Make Spline 5 12" 0.0 (splineSlopeAt sp2 1.5) 0.001
    assertFloatEqual "Make Spline 5 13" 0.0 (splineSlopeAt sp2 1.0) 0.001
  )

testSplineMakeSpline6 = TestCase (do
    let sps = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0), (2.0, 2.0)] :: [Spline Float]
    assertEqual "Make Spline 6 1" 2 (length sps)
    let sp1 = sps !! 0
    let sp2 = sps !! 1
    assertFloatEqual "Make Spline 6 2" (splineAt sp1 1.0) (splineAt sp2 1.0) 0.001
    assertFloatEqual "Make Spline 6 3" (splineSlopeAt sp1 1.0) (splineSlopeAt sp2 1.0) 0.001
    assertFloatEqual "Make Spline 6 4" 1.0 (splineAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 6 5" 0.90625 (splineAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 6 6" 1.0 (splineAt sp1 1.0) 0.001
    assertFloatEqual "Make Spline 6 7" 1.0 (splineAt sp2 1.0) 0.001
    assertFloatEqual "Make Spline 6 8" 1.4063 (splineAt sp2 1.5) 0.001
    assertFloatEqual "Make Spline 6 9" 2.0 (splineAt sp2 2.0) 0.001
    assertFloatEqual "Make Spline 6 10" (-0.25) (splineSlopeAt sp1 0.0) 0.001
    assertFloatEqual "Make Spline 6 11" (-0.0625) (splineSlopeAt sp1 0.5) 0.001
    assertFloatEqual "Make Spline 6 12" 0.5 (splineSlopeAt sp1 1.0) 0.001
    assertFloatEqual "Make Spline 6 13" 0.5 (splineSlopeAt sp2 1.0) 0.001
    assertFloatEqual "Make Spline 6 14" 1.0625 (splineSlopeAt sp2 1.5) 0.001
    assertFloatEqual "Make Spline 6 15" 1.25 (splineSlopeAt sp2 2.0) 0.001
  )

testSplineMakeSpline7 = TestCase (do
    let sps = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 2.0), (2.0, 1.0)] :: [Spline Float]
    assertEqual "Make Spline 7 1" 2 (length sps)
    let sp1 = sps !! 0
    let sp2 = sps !! 1
    assertFloatEqual "Make Spline 7 2" (splineAt sp1 1.0) (splineAt sp2 1.0) 0.001
    assertFloatEqual "Make Spline 7 3" (splineSlopeAt sp1 1.0) (splineSlopeAt sp2 1.0) 0.001
  )
  

testToBezier = TestList [
  testToBezier1, testToBezier2, testToBezier3, testToBezier4,
  testToBezier5, testToBezier6, testToBezier7
  ]

testToBezier1 = TestCase (do
  let [spline] = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0)] :: [Spline Float]
  let bezier = toBezier spline
  assertFloatEqual "To Bezier 1 1" (splineAt spline 0.0) (snd $ bezierAt bezier 0.0) 0.001
  assertFloatEqual "To Bezier 1 2" (splineAt spline 0.5) (snd $ bezierAt bezier 0.5) 0.001
  assertFloatEqual "To Bezier 1 2" (splineAt spline 1.0) (snd $ bezierAt bezier 1.0) 0.001
  let (sx1, sy1) = bezierSlopeAt bezier 0.0
  assertFloatEqual "To Bezier 1 4" (splineSlopeAt spline 0.0) (sy1 / sx1) 0.001
  let (sx2, sy2) = bezierSlopeAt bezier 0.5
  assertFloatEqual "To Bezier 1 5" (splineSlopeAt spline 0.5) (sy2 / sx2) 0.001
  let (sx3, sy3) = bezierSlopeAt bezier 1.0
  assertFloatEqual "To Bezier 1 6" (splineSlopeAt spline 1.0) (sy3 / sx3) 0.001
 )

testToBezier2 = TestCase (do
  let [spline] = makeSpline NaturalBoundary (ClampBoundary 0.0) [(0.0, 1.0), (1.0, 1.0)] :: [Spline Float]
  let bezier = toBezier spline
  assertFloatEqual "To Bezier 2 1" (splineAt spline 0.0) (snd $ bezierAt bezier 0.0) 0.001
  assertFloatEqual "To Bezier 2 2" (splineAt spline 0.5) (snd $ bezierAt bezier 0.5) 0.001
  assertFloatEqual "To Bezier 2 2" (splineAt spline 1.0) (snd $ bezierAt bezier 1.0) 0.001
  let (sx1, sy1) = bezierSlopeAt bezier 0.0
  assertFloatEqual "To Bezier 2 4" (splineSlopeAt spline 0.0) (sy1 / sx1) 0.001
  let (sx2, sy2) = bezierSlopeAt bezier 0.5
  assertFloatEqual "To Bezier 2 5" (splineSlopeAt spline 0.5) (sy2 / sx2) 0.001
  let (sx3, sy3) = bezierSlopeAt bezier 1.0
  assertFloatEqual "To Bezier 2 6" (splineSlopeAt spline 1.0) (sy3 / sx3) 0.001
  )

testToBezier3 = TestCase (do
  let [spline] = makeSpline (ClampBoundary 0.0) NaturalBoundary [(0.0, 1.0), (1.0, 1.0)] :: [Spline Float]
  let bezier = toBezier spline
  assertFloatEqual "To Bezier 3 1" (splineAt spline 0.0) (snd $ bezierAt bezier 0.0) 0.001
  assertFloatEqual "To Bezier 3 2" (splineAt spline 0.5) (snd $ bezierAt bezier 0.5) 0.001
  assertFloatEqual "To Bezier 3 2" (splineAt spline 1.0) (snd $ bezierAt bezier 1.0) 0.001
  let (sx1, sy1) = bezierSlopeAt bezier 0.0
  assertFloatEqual "To Bezier 3 4" (splineSlopeAt spline 0.0) (sy1 / sx1) 0.001
  let (sx2, sy2) = bezierSlopeAt bezier 0.5
  assertFloatEqual "To Bezier 3 5" (splineSlopeAt spline 0.5) (sy2 / sx2) 0.001
  let (sx3, sy3) = bezierSlopeAt bezier 1.0
  assertFloatEqual "To Bezier 3 6" (splineSlopeAt spline 1.0) (sy3 / sx3) 0.001
  )

testToBezier4 = TestCase (do
  let [sp1, sp2] = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0), (2.0, 3.0)] :: [Spline Float]
  let bz1 = toBezier sp1
  assertFloatEqual "To Bezier 4 1" (splineAt sp1 0.0) (snd $ bezierAt bz1 0.0) 0.001
  assertFloatEqual "To Bezier 4 2" (splineAt sp1 0.5) (snd $ bezierAt bz1 0.5) 0.001
  assertFloatEqual "To Bezier 4 2" (splineAt sp1 1.0) (snd $ bezierAt bz1 1.0) 0.001
  let bz2 = toBezier sp2
  assertFloatEqual "To Bezier 4 4" (splineAt sp2 1.0) (snd $ bezierAt bz2 1.0) 0.001
  assertFloatEqual "To Bezier 4 5" (splineAt sp2 1.5) (snd $ bezierAt bz2 1.5) 0.001
  assertFloatEqual "To Bezier 4 6" (splineAt sp2 2.0) (snd $ bezierAt bz2 2.0) 0.001
  let (sx1, sy1) = bezierSlopeAt bz1 0.0
  assertFloatEqual "To Bezier 4 7" (splineSlopeAt sp1 0.0) (sy1 / sx1) 0.001
  let (sx2, sy2) = bezierSlopeAt bz1 0.5
  assertFloatEqual "To Bezier 4 8" (splineSlopeAt sp1 0.5) (sy2 / sx2) 0.001
  let (sx3, sy3) = bezierSlopeAt bz1 1.0
  assertFloatEqual "To Bezier 4 9" (splineSlopeAt sp1 1.0) (sy3 / sx3) 0.001
  let (sx4, sy4) = bezierSlopeAt bz2 1.0
  assertFloatEqual "To Bezier 4 10" (splineSlopeAt sp2 1.0) (sy4 / sx4) 0.001
  let (sx5, sy5) = bezierSlopeAt bz2 1.5
  assertFloatEqual "To Bezier 4 11" (splineSlopeAt sp2 1.5) (sy5 / sx5) 0.001
  let (sx6, sy6) = bezierSlopeAt bz2 2.0
  assertFloatEqual "To Bezier 4 12" (splineSlopeAt sp2 2.0) (sy6 / sx6) 0.001
  )

testToBezier5 = TestCase (do
  let [sp1, sp2] = makeSpline NaturalBoundary NaturalBoundary [(0.0, 2.0), (1.0, 1.0), (2.0, 3.0)] :: [Spline Float]
  let bz1 = toBezier sp1
  assertFloatEqual "To Bezier 5 1" (splineAt sp1 0.0) (snd $ bezierAt bz1 0.0) 0.001
  assertFloatEqual "To Bezier 5 2" (splineAt sp1 0.5) (snd $ bezierAt bz1 0.5) 0.001
  assertFloatEqual "To Bezier 5 2" (splineAt sp1 1.0) (snd $ bezierAt bz1 1.0) 0.001
  let bz2 = toBezier sp2
  assertFloatEqual "To Bezier 5 4" (splineAt sp2 1.0) (snd $ bezierAt bz2 1.0) 0.001
  assertFloatEqual "To Bezier 5 5" (splineAt sp2 1.5) (snd $ bezierAt bz2 1.5) 0.001
  assertFloatEqual "To Bezier 5 6" (splineAt sp2 2.0) (snd $ bezierAt bz2 2.0) 0.001
  )

testToBezier6 = TestCase (do
  let [sp1, sp2, sp3] = makeSpline NaturalBoundary NaturalBoundary [(0.0, 2.0), (1.0, 1.0), (2.0, 3.0), (4.0, 1.0)] :: [Spline Float]
  let bz1 = toBezier sp1
  assertFloatEqual "To Bezier 6 1" (splineAt sp1 0.0) (snd $ bezierAt bz1 0.0) 0.001
  assertFloatEqual "To Bezier 6 2" (splineAt sp1 0.5) (snd $ bezierAt bz1 0.5) 0.001
  assertFloatEqual "To Bezier 6 2" (splineAt sp1 1.0) (snd $ bezierAt bz1 1.0) 0.001
  let bz2 = toBezier sp2
  assertFloatEqual "To Bezier 6 4" (splineAt sp2 1.0) (snd $ bezierAt bz2 1.0) 0.001
  assertFloatEqual "To Bezier 6 5" (splineAt sp2 1.5) (snd $ bezierAt bz2 1.5) 0.001
  assertFloatEqual "To Bezier 6 6" (splineAt sp2 2.0) (snd $ bezierAt bz2 2.0) 0.001
  let bz3 = toBezier sp3
  assertFloatEqual "To Bezier 6 7" (splineAt sp3 2.0) (snd $ bezierAt bz3 2.0) 0.001
  assertFloatEqual "To Bezier 6 8" (splineAt sp3 3.5) (snd $ bezierAt bz3 3.5) 0.001
  assertFloatEqual "To Bezier 6 9" (splineAt sp3 4.0) (snd $ bezierAt bz3 4.0) 0.001
  let (sx1, sy1) = bezierSlopeAt bz1 0.5
  assertFloatEqual "To Bezier 6 10" (splineSlopeAt sp1 0.5) (sy1 / sx1) 0.001
  let (sx2, sy2) = bezierSlopeAt bz2 1.5
  assertFloatEqual "To Bezier 6 11" (splineSlopeAt sp2 1.5) (sy2 / sx2) 0.001
  let (sx3, sy3) = bezierSlopeAt bz3 3.5
  assertFloatEqual "To Bezier 6 12" (splineSlopeAt sp3 3.5) (sy3 / sx3) 0.001
  )

testToBezier7 = TestCase (do
  let bz1 = toBezier spline1
  assertFloatEqual "To Bezier 6 1" (splineAt spline1 0.0) (snd $ bezierAt bz1 0.0) 0.001
  assertFloatEqual "To Bezier 6 2" (splineAt spline1 0.5) (snd $ bezierAt bz1 0.5) 0.001
  assertFloatEqual "To Bezier 6 2" (splineAt spline1 1.0) (snd $ bezierAt bz1 1.0) 0.001
  let (sx1, sy1) = bezierSlopeAt bz1 0.0
  assertFloatEqual "To Bezier 3 4" (splineSlopeAt spline1 0.0) (sy1 / sx1) 0.001
  let (sx2, sy2) = bezierSlopeAt bz1 0.5
  assertFloatEqual "To Bezier 3 5" (splineSlopeAt spline1 0.5) (sy2 / sx2) 0.001
  let (sx3, sy3) = bezierSlopeAt bz1 1.0
  assertFloatEqual "To Bezier 3 6" (splineSlopeAt spline1 1.0) (sy3 / sx3) 0.001
  )

-- Test against difficult cases
testSVGSpline = TestList [
    testSVGSpline1
  ]

location1 :: Location
location1 = (placeholder "P-P275") { locationPosition = LatLong 42.82827 (-8.60971) (Just 128) def }

location2 :: Location
location2 = (placeholder "P-P276") { locationPosition = LatLong 42.83347 (-8.60106) (Just 164) def }

location3 :: Location
location3 = (placeholder "P-P277") { locationPosition = LatLong 42.84746 (-8.58026) (Just 244) def }

location4 :: Location
location4 = (placeholder "P-P278") { locationPosition = LatLong 42.85097 (-8.57719) (Just 220) def }

location5 :: Location
location5 = (placeholder "P-P279") { locationPosition = LatLong 42.86016 (-8.57696) (Just 206) def }

withSegments :: Leg -> Leg
withSegments leg = leg {
    legSegments = buildLegSegments (legFrom leg) (legTo leg) (legWaypoints leg) (legDistance leg) (legAscent leg) (legDescent leg)
  }

leg1 = withSegments $ Leg Road location1 location2 Nothing 1.19 Nothing 35 0 Nothing [] []

leg2 = withSegments $ Leg Road location2 location3 Nothing 2.40 Nothing 85 5 Nothing [] []

leg3 = withSegments $ Leg Road location3 location4 Nothing 1.22 Nothing 5 30 Nothing waypoints []
  where
    waypoints = [
        LatLong 42.84846 (-8.58008) (Just 246) def
      , LatLong 42.84816 (-8.57802) (Just 230) def
      ]

leg4 = withSegments $ Leg Road location4 location4 Nothing 1.58 Nothing 30 40 Nothing [] []

testSVGSpline1 = TestCase (do
  let legs = [leg1, leg2, leg3, leg4]
  let coords = buildCoordinates 1.0 1.0 legs
  let coords' = map (\(d, me, _, _) -> (d, maybe 0.0 id me)) coords
  let sps = makeSpline NaturalBoundary NaturalBoundary coords'
  assertEqual "SVG Spline 1 1" 6 (length sps)
  let sp1 = sps !! 0
  let sp2 = sps !! 1
  let sp3 = sps !! 2
  let sp4 = sps !! 3
  let sp5 = sps !! 4
  let sp6 = sps !! 5
  assertFloatEqual "SVG Spline 1 2" 128.0 (splineAt sp1 0.000) 0.1
  assertFloatEqual "SVG Spline 1 3" 164.0 (splineAt sp2 1.190) 0.1
  assertFloatEqual "SVG Spline 1 4" 244.0 (splineAt sp3 3.590) 0.1
  assertFloatEqual "SVG Spline 1 5" 246.0 (splineAt sp4 3.817) 0.1
  assertFloatEqual "SVG Spline 1 6" 230.0 (splineAt sp5 4.163) 0.1
  assertFloatEqual "SVG Spline 1 7" 220.0 (splineAt sp6 6.390) 0.1
  assertBool "SVG Spline 1 8" (all (\x -> splineAt sp1 x >= 128.0 && splineAt sp1 x <= 164.0) [0.000, 0.001 .. 1.190])
  assertBool "SVG Spline 1 9" (all (\x -> splineAt sp2 x >= 164.0 && splineAt sp2 x <= 244.0) [1.190, 0.001 .. 3.590])
  forM [3.590, 0.001 .. 3.817] (\x -> assertBool ("SVG Spline 1 10 " ++ (show x) ++ "=" ++ (show $ splineAt sp3 x)) (splineAt sp3 x >= 243.5 && splineAt sp3 x <= 246.5))
  forM [3.817, 0.001 .. 4.163] (\x -> assertBool ("SVG Spline 1 11 " ++ (show x) ++ "=" ++ (show $ splineAt sp4 x)) (splineAt sp4 x >= 229.5 && splineAt sp4 x <= 246.5))
  forM [4.163, 0.001 .. 4.810] (\x -> assertBool ("SVG Spline 1 12 " ++ (show x) ++ "=" ++ (show $ splineAt sp5 x)) (splineAt sp5 x >= 219.5 && splineAt sp5 x <= 230.5))
  assertBool "SVG Spline 1 13" (all (\x -> splineAt sp6 x >= 206.0 && splineAt sp6 x <= 220.0) [4.163, 0.001 .. 6.390])
  )