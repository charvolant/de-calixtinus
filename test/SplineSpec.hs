module SplineSpec(testSpline) where

import Test.HUnit
import Data.Spline
import TestUtils

testSpline :: Test
testSpline = TestList [
    TestLabel "SplineAt" testSplineAt
  , TestLabel "SplineSlopeAt" testSplineSlopeAt
  , TestLabel "SplineSecondDevs" testSplineSecondDevs
  , TestLabel "MakeSpline" testSplineMakeSpline
  , TestLabel "ToBezier" testToBezier
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
    let sps = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0)]
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
    let sps = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 2.0)]
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
    let sps = makeSpline NaturalBoundary (ClampBoundary 0.0) [(0.0, 1.0), (1.0, 2.0)]
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
    let sps = makeSpline (ClampBoundary 0.0) NaturalBoundary [(0.0, 1.0), (1.0, 2.0)]
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
    let sps = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0), (2.0, 1.0)]
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
    let sps = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0), (2.0, 2.0)]
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
    let sps = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 2.0), (2.0, 1.0)]
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
  let [spline] = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0)]
  let bezier = toBezier spline
  assertFloatEqual "To Bezier 1 1" (splineAt spline 0.0) (snd $ bezierAt bezier 0.0) 0.001
  assertFloatEqual "To Bezier 1 2" (splineAt spline 0.5) (snd $ bezierAt bezier 0.5) 0.001
  assertFloatEqual "To Bezier 1 2" (splineAt spline 1.0) (snd $ bezierAt bezier 1.0) 0.001
  )

testToBezier2 = TestCase (do
  let [spline] = makeSpline NaturalBoundary (ClampBoundary 0.0) [(0.0, 1.0), (1.0, 1.0)]
  let bezier = toBezier spline
  assertFloatEqual "To Bezier 2 1" (splineAt spline 0.0) (snd $ bezierAt bezier 0.0) 0.001
  assertFloatEqual "To Bezier 2 2" (splineAt spline 0.5) (snd $ bezierAt bezier 0.5) 0.001
  assertFloatEqual "To Bezier 2 2" (splineAt spline 1.0) (snd $ bezierAt bezier 1.0) 0.001
  )

testToBezier3 = TestCase (do
  let [spline] = makeSpline (ClampBoundary 0.0) NaturalBoundary [(0.0, 1.0), (1.0, 1.0)]
  let bezier = toBezier spline
  assertFloatEqual "To Bezier 3 1" (splineAt spline 0.0) (snd $ bezierAt bezier 0.0) 0.001
  assertFloatEqual "To Bezier 3 2" (splineAt spline 0.5) (snd $ bezierAt bezier 0.5) 0.001
  assertFloatEqual "To Bezier 3 2" (splineAt spline 1.0) (snd $ bezierAt bezier 1.0) 0.001
  )

testToBezier4 = TestCase (do
  let [sp1, sp2] = makeSpline NaturalBoundary NaturalBoundary [(0.0, 1.0), (1.0, 1.0), (2.0, 3.0)]
  let bz1 = toBezier sp1
  assertFloatEqual "To Bezier 4 1" (splineAt sp1 0.0) (snd $ bezierAt bz1 0.0) 0.001
  assertFloatEqual "To Bezier 4 2" (splineAt sp1 0.5) (snd $ bezierAt bz1 0.5) 0.001
  assertFloatEqual "To Bezier 4 2" (splineAt sp1 1.0) (snd $ bezierAt bz1 1.0) 0.001
  let bz2 = toBezier sp2
  assertFloatEqual "To Bezier 4 4" (splineAt sp2 1.0) (snd $ bezierAt bz2 1.0) 0.001
  assertFloatEqual "To Bezier 4 5" (splineAt sp2 1.5) (snd $ bezierAt bz2 1.5) 0.001
  assertFloatEqual "To Bezier 4 6" (splineAt sp2 2.0) (snd $ bezierAt bz2 2.0) 0.001
  )

testToBezier5 = TestCase (do
  let [sp1, sp2] = makeSpline NaturalBoundary NaturalBoundary [(0.0, 2.0), (1.0, 1.0), (2.0, 3.0)]
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
  let [sp1, sp2, sp3] = makeSpline NaturalBoundary NaturalBoundary [(0.0, 2.0), (1.0, 1.0), (2.0, 3.0), (4.0, 1.0)]
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
  )

testToBezier7 = TestCase (do
  let bz1 = toBezier spline1
  assertFloatEqual "To Bezier 6 1" (splineAt spline1 0.0) (snd $ bezierAt bz1 0.0) 0.001
  assertFloatEqual "To Bezier 6 2" (splineAt spline1 0.5) (snd $ bezierAt bz1 0.5) 0.001
  assertFloatEqual "To Bezier 6 2" (splineAt spline1 1.0) (snd $ bezierAt bz1 1.0) 0.001
  )

