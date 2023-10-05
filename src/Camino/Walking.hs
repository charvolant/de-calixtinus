{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Walking
Description : Models of time taken to cover a distance
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Derived from various models of time taken to walk a distance.
The Naismith and Tobler rules assume a fit person who isn't slowed down by such minor 
inconveniences as steep slopes, uneven ground, fatigue, heavy packs and the like.
The Tranter corrections help take care of such issues.

* <https://en.wikipedia.org/wiki/Naismith%27s_rule>
* <https://en.wikipedia.org/wiki/Tobler%27s_hiking_function>

@
let t = tobler 4.5 100 0 in
  tranter Normal t
@
-}
module Camino.Walking (
  naismith, 
  tobler, 
  tranter,
  nominalSpeed
) where

import Numeric.Tools.Interpolation
import Data.Vector (fromList)
import Camino.Camino

-- | Calculate the time taken to walk a distance using simple Naismith's rule
naismith :: (Floating a) => a -- ^ The distance in km
  -> a -- ^ The ascent in metres
  -> a -- ^ The descent in metres
  -> a -- ^ The time taken in hours
naismith distance ascent _ = (distance / 5) + (ascent / 600)

-- | Calculate the time taken using Toblers's function
tobler :: (Ord a, Floating a) => a -- ^ The distance in km
  -> a -- ^ The ascent in metres
  -> a -- ^ The descent in metres
  -> a -- ^ The time taken in hours
tobler distance ascent descent =
  let
    da = if ascent == 0.0 then 0.0 else distance * ascent / (descent + ascent)
    dd = if descent == 0.0 then 0.0 else distance * descent / (descent + ascent)
    df = distance - da - dd
    as = if da == 0.0 then 0 else ascent / (da * 1000)
    ds = if dd == 0.0 then 0 else descent / (dd * 1000)
    sa = 6.0 * exp (-3.5 * (as + 0.05))
    sd = 6.0 * exp (-3.5 * abs (ds - 0.05))
    sf = 5.036742
   in
     da / sa + dd / sd + df / sf

-- | The expected points for interpolation for Tranter's corrections
tranterPoints :: UniformMesh
tranterPoints = uniformMesh (0, 24) 25

-- | Value indicating impossible according to the corrections
tranterImpossible :: Double
tranterImpossible = 1000.0

-- | Tranter corrections for fit people (15 mins for 1000ft climb over 1/2 mile)
tranter15 = linearInterp $ tabulate tranterPoints tranter15' where
  tranter15' = fromList [
    0.0, -- 0 hours expected via naismith/tobler
    0.5, -- 1 hours
    1.0, -- 2
    1.5, -- 3
    2.0, -- 4
    2.75, -- 5
    3.5, -- 6
    4.5, -- 7
    5.5, -- 8
    6.75, --9
    7.75, -- 10
    8.875, -- 11 (odd inferred from table)
    10, -- 12
    11.25, -- 13
    12.5, -- 14
    13.5, -- 15
    14.5, -- 16
    15.75, -- 17
    17, -- 18
    18.25, -- 19
    19.5, -- 20
    20.75, -- 21
    22, -- 22
    23, -- 23
    24 -- 24
    ]

tranter20 = linearInterp $ tabulate tranterPoints tranter20' where 
  tranter20' = fromList [
    0.0, -- 0 hours expected via naismith/tobler
    0.625, -- 1 hours
    1.25, -- 2
    2.25, -- 3
    3.25, -- 4
    4.5, -- 5
    5.5, -- 6
    6.5, -- 7
    7.75, -- 8
    8.75, --9
    10, -- 10
    11.25, -- 11 (odd inferred from table)
    12.5, -- 12
    13.75, -- 13
    15, -- 14
    16.25, -- 15
    17.5, -- 16
    18.25, -- 17
    20, -- 18
    21.5, -- 19
    23, -- 20
    tranterImpossible, -- 21
    tranterImpossible, -- 22
    tranterImpossible, -- 23
    tranterImpossible -- 24
    ]

tranter25 = linearInterp $ tabulate tranterPoints tranter25' where
  tranter25' = fromList [
    0.0, -- 0 hours expected via naismith/tobler
    0.725, -- 1 hours
    1.5, -- 2
    3.0, -- 3
    4.25, -- 4
    5.5, -- 5
    7, -- 6
    8.5, -- 7
    10, -- 8
    11.5, --9
    13.25, -- 10
    14.125, -- 11 (odd inferred from table)
    15.0, -- 12
    16.25, -- 13
    17.5, -- 14
    tranterImpossible, -- 15
    tranterImpossible, -- 16
    tranterImpossible, -- 17
    tranterImpossible, -- 18
    tranterImpossible, -- 19
    tranterImpossible, -- 20
    tranterImpossible, -- 21
    tranterImpossible, -- 22
    tranterImpossible, -- 23
    tranterImpossible -- 24
    ]

tranter30 = linearInterp $ tabulate tranterPoints tranter30' where
  tranter30' = fromList [
    0.0, -- 0 hours expected via naismith/tobler
    1.0, -- 1 hours
    2.0, -- 2
    3.5, -- 3
    5, -- 4
    6.25, -- 5
    8.5, -- 6
    10.5, -- 7
    12.5, -- 8
    14.5, -- 9
    tranterImpossible, -- 10
    tranterImpossible, -- 11
    tranterImpossible, -- 12
    tranterImpossible, -- 13
    tranterImpossible, -- 14
    tranterImpossible, -- 15
    tranterImpossible, -- 16
    tranterImpossible, -- 17
    tranterImpossible, -- 18
    tranterImpossible, -- 19
    tranterImpossible, -- 20
    tranterImpossible, -- 21
    tranterImpossible, -- 22
    tranterImpossible, -- 23
    tranterImpossible -- 24
    ]

tranter40 = linearInterp $ tabulate tranterPoints tranter40' where
  tranter40' = fromList [
    0.0, -- 0 hours expected via naismith/tobler
    1.25, -- 1 hours
    2.75, -- 2
    4.25, -- 3
    5.75, -- 4
    7.5, -- 5
    9.5, -- 6
    11.5, -- 7
    tranterImpossible, -- 8
    tranterImpossible, -- 9
    tranterImpossible, -- 10
    tranterImpossible, -- 11
    tranterImpossible, -- 12
    tranterImpossible, -- 13
    tranterImpossible, -- 14
    tranterImpossible, -- 15
    tranterImpossible, -- 16
    tranterImpossible, -- 17
    tranterImpossible, -- 18
    tranterImpossible, -- 19
    tranterImpossible, -- 20
    tranterImpossible, -- 21
    tranterImpossible, -- 22
    tranterImpossible, -- 23
    tranterImpossible -- 24
    ]

tranter50 = linearInterp $ tabulate tranterPoints tranter50' where
  tranter50' = fromList [
    0.0, -- 0 hours expected via naismith/tobler
    1.625, -- 1 hours
    3.25, -- 2
    4.75, -- 3
    6.5, -- 4
    8.5, -- 5
    tranterImpossible, -- 6
    tranterImpossible, -- 7
    tranterImpossible, -- 8
    tranterImpossible, -- 9
    tranterImpossible, -- 10
    tranterImpossible, -- 11
    tranterImpossible, -- 12
    tranterImpossible, -- 13
    tranterImpossible, -- 14
    tranterImpossible, -- 15
    tranterImpossible, -- 16
    tranterImpossible, -- 17
    tranterImpossible, -- 18
    tranterImpossible, -- 19
    tranterImpossible, -- 20
    tranterImpossible, -- 21
    tranterImpossible, -- 22
    tranterImpossible, -- 23
    tranterImpossible -- 24
    ]

tranter' SuperFit = tranter15
tranter' VeryFit = tranter20
tranter' Fit = tranter25
tranter' Normal = tranter30
tranter' Unfit = tranter40
tranter' VeryUnfit = tranter50

-- | Tranter corrections based on fitness.
-- | These corrections take account of fitness levels and fatigue
tranter :: Fitness -- ^ The level of fitness of the person walking
  -> Float -- ^ The hours walked according to the base formula
  -> Float -- ^ The actual hours corrected for fitness and fatigue
tranter fitness hours = realToFrac $ (tranter' fitness) `at` (realToFrac hours)

-- | Normal walking speed, based on fitness level
nominalSpeed :: Fitness -- ^ The level of fitness of the person walking
  -> Float -- ^ Nominal speed on level ground in km/hr
nominalSpeed SuperFit = 6.0
nominalSpeed VeryFit = 5.0
nominalSpeed Fit = 4.5
nominalSpeed Normal = 4.0
nominalSpeed Unfit = 3.5
nominalSpeed VeryUnfit = 3.0