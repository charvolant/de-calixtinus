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

is measured by VAM - velocità ascensionale media - with the assumption that each additional 1% of slope
reduces ascension speed by 50m/hr

* https://en.wikipedia.org/wiki/VAM_(bicycling)


@
let t = tobler 4.5 100 0 in
  tranter Normal t
@
-}
module Camino.Walking (
  cycling,
  naismith, 
  nominalSpeed,
  perceivedDistance,
  tobler, 
  tranter
) where

import Numeric.Tools.Interpolation
import qualified Data.Vector as V (Vector, fromList, length)
import Camino.Camino

-- | Base cycling speed for time estimates
cyclingSpeed :: (Floating a) => Fitness -> a
cyclingSpeed SuperFit = 40.0
cyclingSpeed VeryFit = 32.0
cyclingSpeed Fit = 28.0
cyclingSpeed Normal = 24.0
cyclingSpeed Unfit = 20.0
cyclingSpeed VeryUnfit = 16.0

-- | Base cycling ascent rates for time estimates, using a 5% slope as a rough guide
cyclingAscent :: (Floating a) => Fitness -> a
cyclingAscent SuperFit = 1600.0 -- Finishing mountain stage in Tour de France
cyclingAscent VeryFit = 1300.0
cyclingAscent Fit = 1100.0
cyclingAscent Normal = 900.0
cyclingAscent Unfit = 700.0
cyclingAscent VeryUnfit = 400.0

cycling :: (Ord a, Floating a) => Fitness  -- ^ The broad level of fitness
  -> a -- ^ The distance in km
  -> a -- ^ The ascent in metres
  -> a -- ^ The descent in metres
  -> a -- ^ The time taken in hourse
cycling fitness distance ascent descent = 
  let
    da = if ascent == 0.0 then 0.0 else distance * ascent / (descent + ascent)
    as = if da == 0.0 then 0 else ascent / (da * 1000)
    speed = cyclingSpeed fitness
    ascentRate = cyclingAscent fitness
    modifiedAscentRate = max 200.0 (ascentRate - (as - 0.05) * 5000.0) -- Lose 50m/s per 1% of slope
  in
    distance / speed + ascent / modifiedAscentRate
    
      
-- | Calculate the time taken to walk a distance using simple Naismith's rule
naismith :: (Floating a) => Fitness -- ^ The broad level of fitness
  -> a -- ^ The distance in km
  -> a -- ^ The ascent in metres
  -> a -- ^ The descent in metres
  -> a -- ^ The time taken in hours
naismith _ distance ascent _ = (distance / 5) + (ascent / 600)

-- | Calculate the time taken using Toblers's function
tobler :: (Ord a, Floating a) => Fitness -- ^ The broad level of fitness
  -> a -- ^ The distance in km
  -> a -- ^ The ascent in metres
  -> a -- ^ The descent in metres
  -> a -- ^ The time taken in hours
tobler _ distance ascent descent =
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

makeInterpolation' :: V.Vector Double -> LinearInterp UniformMesh
makeInterpolation' points = linearInterp $ tabulate mesh points 
  where 
    l = V.length points
    mesh = uniformMesh (0.0, fromIntegral (l - 1)) l

-- | Tranter corrections for fit people (15 mins for 1000ft climb over 1/2 mile)
tranter15 :: LinearInterp UniformMesh
tranter15 = makeInterpolation' tranter15' where
  tranter15' = V.fromList [
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

tranter20 :: LinearInterp UniformMesh
tranter20 = makeInterpolation' tranter20' where 
  tranter20' = V.fromList [
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
    23 -- 20
    ]

tranter25 :: LinearInterp UniformMesh
tranter25 = makeInterpolation' tranter25' where
  tranter25' = V.fromList [
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
    17.5 -- 14
    ]

tranter30 :: LinearInterp UniformMesh
tranter30 = makeInterpolation' tranter30' where
  tranter30' = V.fromList [
    0.0, -- 0 hours expected via naismith/tobler
    1.0, -- 1 hours
    2.0, -- 2
    3.5, -- 3
    5, -- 4
    6.25, -- 5
    8.5, -- 6
    10.5, -- 7
    12.5, -- 8
    14.5 -- 9
    ]

tranter40 :: LinearInterp UniformMesh
tranter40 = makeInterpolation' tranter40' where
  tranter40' = V.fromList [
    0.0, -- 0 hours expected via naismith/tobler
    1.25, -- 1 hours
    2.75, -- 2
    4.25, -- 3
    5.75, -- 4
    7.5, -- 5
    9.5, -- 6
    11.5 -- 7
    ]

tranter50 :: LinearInterp UniformMesh
tranter50 = makeInterpolation' tranter50' where
  tranter50' = V.fromList [
    0.0, -- 0 hours expected via naismith/tobler
    1.625, -- 1 hours
    3.25, -- 2
    4.75, -- 3
    6.5, -- 4
    8.5 -- 5
    ]

tranter' :: Fitness -> LinearInterp UniformMesh
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
  -> Maybe Float -- ^ Either Just the actual hours corrected for fitness and fatigue or Nothing for out of range
tranter fitness hours = let
    interp = tranter' fitness
    outRange = hours < tranterLower fitness || hours > tranterUpper fitness
  in
    if outRange then Nothing else Just (realToFrac $ interp `at` (realToFrac hours))

-- | The minimum input value for a fitness
tranterLower :: Fitness -> Float 
tranterLower fitness = realToFrac $ meshLowerBound $ interpolationMesh $ tranter' fitness

-- | The maximum input value for a fitness
tranterUpper :: Fitness -> Float 
tranterUpper fitness = realToFrac $ meshUpperBound $ interpolationMesh $ tranter' fitness

-- | Normal walking speed, based on fitness level and a nomimal five hours of walking/riding, adjusted slightly for sanity
nominalSpeed :: Travel -- ^ The type of travele
  -> Fitness -- ^ The level of fitness of the person walking
  -> Float -- ^ Nominal speed on level ground in km/hr
nominalSpeed Walking SuperFit = 6.0
nominalSpeed Walking VeryFit = 5.5
nominalSpeed Walking Fit = 4.5
nominalSpeed Walking Normal = 4.0
nominalSpeed Walking Unfit = 3.5
nominalSpeed Walking VeryUnfit = 3.0
nominalSpeed Cycling SuperFit = 40.0
nominalSpeed Cycling VeryFit = 32.0
nominalSpeed Cycling Fit = 25.0
nominalSpeed Cycling Normal = 20.0
nominalSpeed Cycling Unfit = 15.0
nominalSpeed Cycling VeryUnfit = 10.0

-- | The perceived distance compared to the actual distance when walking over flat ground
perceivedDistance :: Travel -- ^ The travel type
 -> Fitness -- ^ The fitness level
 -> Float -- ^ The actual distance
 -> Bool -- ^ If out of range, choose the upper boundary, otherwise choose the lower one
 -> Float -- ^ 
perceivedDistance travel fitness distance upper = let
    normalSpeed = nominalSpeed Walking Normal
    baseHours = if travel == Cycling then cycling fitness distance 0.0 0.0 else tobler fitness distance 0.0 0.0
    mlower = tranterLower fitness
    mupper = tranterUpper fitness
    baseHours' = if baseHours >= mlower && baseHours <= mupper then baseHours else if upper then mupper else mlower
    modifiedHours = tranter fitness baseHours'
  in
    case modifiedHours of
      Nothing -> error "Expecting min/max available"
      Just hours -> if distance > pd then distance else pd where pd = hours * normalSpeed
