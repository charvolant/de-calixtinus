{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Spline
Description : Create and use splines
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Create and use cubic splines.

Cubic splines can be used to take a sequence of points an draw a "natural-looking" curve that smoothly passes
through each point.
A series of cubic polynomials, one for each interval then connects the points.
If the points are the sequence \( (x_0, y_0), (x_1, y_1) \ldots (x_n, y_n) \) then the splines,
 \( S_{i}(x) \) will have the properties that \( S_{i - 1}(x_i) = S_{i}(x_i) = y_i \) and
 \( S_{i-1}^{\prime}(x_i) = S_{i}^{\prime}(x_i) \).
This is enough to make everything look smooth and good.

Graphics systems, such as SVG, do not generally use splines.
Instead, splines can be convered into `Bezier` curves.
-}

module Data.Spline (
  -- * Splines
    Spline(..)
  , SplineBoundary(..)
  , makeSpline
  , spline2ndDevs
  , splineAt
  , splineSlopeAt
  -- * Bezier Curves
  , Bezier(..)
  , bezierAt
  , bezierSlopeAt
  , toBezier
) where

-- | A spline piece
--
--   The spline piece runs from \(x_0\) to \(x_1\), \( x_0 < x_1 \) with the equation
--   \( y(x) = a t^3 + b t^2 + c  t + d \) where \( t = x - x_0 \)
data (RealFrac a) => Spline a = Spline
  a -- ^ x0
  a -- ^ x1
  a -- ^ a
  a -- ^ b
  a -- ^ c
  a -- ^ d
  deriving (Eq, Ord, Show)

-- | Evaluate a spline at a specific point
--
--   It is possible to evaluate the spline for x-values outside the spline limits, resulting in an extrapolated value
splineAt :: (RealFrac a) => Spline a -- ^ The spline
  -> a -- The x value
  -> a -- The resulting interpolated y value
splineAt (Spline x0 _ a b c d) x =
  ((a * x' + b) * x' + c) * x' + d where x' = x - x0

-- | Evaluate the slope of a spline at a specific point
splineSlopeAt :: (RealFrac a) => Spline a -- ^ The spline
  -> a -- The x value
  -> a -- The resulting interpolated slope (y') value
splineSlopeAt (Spline x0 _ a b c _d) x =
  (3.0 * a * x' + 2.0 * b) * x' + c where x' = x - x0

-- | A cubic Bezier curve.
--
--   A cubic bezier curve runs from \(0 \leq t \leq 1\) with
--   \( x(t) = x_0 (1 - t^3) + x_1 (1 - t)^2 t + x_2 (1 - t) t^2 + x_3 t^3 \) and
--   \( y(t) = y_0 (1 - t^3) + y_1 (1 - t)^2 t + y_2 (1 - t) t^2 + y_3 t^3 \)
--   The \( (x_0, y_0), (x_1, y_1), (x_2, y_2) \) and \( (x_3, y_3) \) values are the control points,
--   with the curve starting at \((x_0, y_0)\) and ending at \((x_3, y_3)\) and the \((x_1, y_1)\) and \((x_2, y_2)\) acting as handles,
--   pulling the curve towards each point.
data (RealFrac a) => Bezier a = Bezier
  (a, a) -- ^ (x0, y0)
  (a, a) -- ^ (x1, y1)
  (a, a) -- ^ (x2, y2)
  (a, a) -- ^ (x3, y3)
  deriving (Eq, Ord, Show)

-- | Evaluate a Bezier curve at a given point
bezierAt :: (RealFrac a) => Bezier a -> a -> (a, a)
bezierAt (Bezier (x0, y0) (x1, y1) (x2, y2) (x3, y3)) x = let
    t = (x - x0) / (x3 - x0)
    t1 = 1.0 - t
    x' = x0 * t1 * t1 * t1 + 3.0 * x1 * t * t1 * t1 + 3.0 * x2 * t * t * t1 + x3 * t * t * t
    y' = y0 * t1 * t1 * t1 + 3.0 * y1 * t * t1 * t1 + 3.0 * y2 * t * t * t1 + y3 * t * t * t
  in
    (x', y')

-- | Compute the rate of change of both x and y on the Bezier curve with respect to t.
--
--   If you want the actual slope at the point, divide the y-slope by the x-slope, keeping in mind that the
--   curve may be veritcal.
bezierSlopeAt :: (RealFrac a) => Bezier a -> a -> (a, a)
bezierSlopeAt (Bezier (x0, y0) (x1, y1) (x2, y2) (x3, y3)) x = let
    t = (x - x0) / (x3 - x0)
    t1 = 1.0 - t
    x' = (-3.0) * x0 * t1 * t1 + 3.0 * x1 * (t1 * t1 - 2.0 * t * t1) + 3.0 * x2 * (2 * t * t1 - t * t) + 3.0 * x3 * t * t
    y' = (-3.0) * y0 * t1 * t1 + 3.0 * y1 * (t1 * t1 - 2.0 * t * t1) + 3.0 * y2 * (2 * t * t1 - t * t) + 3.0 * y3 * t * t
  in
    (x', y')

-- | Spline boundary conditions
data (RealFrac a) => SplineBoundary a =
  NaturalBoundary -- ^ Natural boundary conditions, where \( y^{\prime \prime} = 0 \) and the spline continues on its natural incoming slope
  | ClampBoundary a -- ^ Clamp the slope of the spline to a specific value
  deriving (Eq, Ord, Show)

-- | Compute the second dervivatives of the spline
--
--   These form the basis of the spline calculations, linking up the spline curves by ensuring that the
--   first derivatives match.
--
--   Based on /Numerical Recipes in C/, 2nd ed, p115 (but Haskellised)
--   In theory, this involves solving a tridiagonal matrix of simultaneous equations.
--   In practice, there's a sneaky way of solving things.
spline2ndDevs :: (RealFrac a, Show a) => SplineBoundary a -> SplineBoundary a -> [(a, a)] -> [a]
spline2ndDevs _sb0 _sbn [] = error "No points for spline"
spline2ndDevs _sb0 _sbn [_] = error "Require at least two points for spline"
spline2ndDevs NaturalBoundary sbn (p1:rest@(_:_)) = let
    u1 = 0.0
    y21 = 0.0
    (y2'2, y2s) = spline2ndDevs' sbn u1 y21 p1 rest
    y21' = y21 * y2'2 + u1
  in
    y21':y2s
spline2ndDevs (ClampBoundary s) sbn (p1@(x1, y1):rest@((x2, y2):_)) = let
    u1 = (3.0/(x2 - x1)) * ((y2 - y1)/(x2 - x1) - s)
    y21 = -0.5
    (y2'2, y2s) = spline2ndDevs' sbn u1 y21 p1 rest
    y21' = y21 * y2'2 + u1
  in
    y21':y2s

spline2ndDevs' :: (RealFrac a, Show a) => SplineBoundary a -> a -> a -> (a, a) -> [(a, a)] -> (a, [a])
spline2ndDevs' _sb _ui1 _y2i1 _pi1 [] = error "Empty spline coordinates"
spline2ndDevs' NaturalBoundary _ui1 _y2i1 _pi1 [_pi] = (y2i, [y2i])
  where
    y2i = 0.0 -- (ui - qi * ui1) / (qi * y2i1 +1.0) where ui = qi = 0.0
spline2ndDevs' (ClampBoundary s) ui1 y2i1 (xi1, yi1) [(xi, yi)] = (y2i, [y2i])
  where
    qi = 0.5
    ui = (3.0 / (xi -xi1)) * (s - (yi - yi1) / (xi - xi1))
    y2i = (ui - qi * ui1) / (qi * y2i1 + 1.0)
spline2ndDevs' sbn ui1 y2i1 (xi1, yi1) ((pi'@(xi, yi)):rest@((xi'1, yi'1):_)) = let
    sig = (xi - xi1) / (xi'1 - xi1)
    p = sig * y2i1 + 2.0
    y2i = (sig - 1.0) / p;
    ui = (yi'1 - yi) / (xi'1 - xi) - (yi - yi1) / (xi - xi1)
    ui'= (6.0 * ui / (xi'1 - xi1) - sig * ui1) / p
    (y2'i, y2s) = spline2ndDevs' sbn ui' y2i pi' rest
    y2i'=y2i * y2'i + ui'
  in
    (y2i', y2i':y2s)

-- Create a piecewise spline, with each interval between \( (x_i, y_i) \) and \( (x_{i + 1}, y_{i + 1} |) modelled
-- by a cubic equation \( y = a  (x - x_i)^3 + b  (x - x_i)^2 + c  (x - x_i) + d \)
-- The result is a vector of `Spline`s one less than the original data
makeSpline :: (RealFrac a, Show a) => SplineBoundary a -> SplineBoundary a -> [(a, a)] -> [Spline a]
makeSpline sb1 sbn points = let
    y2s = spline2ndDevs sb1 sbn points
  in
    makeSpline' points y2s

makeSpline' :: (RealFrac a) => [(a, a)] -> [a] -> [Spline a]
makeSpline' [(xi1, yi1), (xi, yi)] [y2i1, y2i] = [makeSpline'' xi1 yi1 xi yi y2i1 y2i]
makeSpline' ((xi1, yi1):rp@((xi, yi):_)) (y2i1:ry2@(y2i:_)) =
  (makeSpline'' xi1 yi1 xi yi y2i1 y2i):(makeSpline' rp ry2)
makeSpline' _ _ = error "Invalid or mismathing spline points"

makeSpline'' :: (RealFrac a) => a -> a -> a -> a -> a -> a -> Spline a
makeSpline'' xi1 yi1 xi yi y2i1 y2i = let
    h = xi - xi1
    a = (y2i - y2i1) / (6.0 * h)
    b = y2i1 / 2.0
    d = yi1
    c = (yi - yi1) / h - (y2i1 / 2.0) * h - ((y2i - y2i1) / 6.0) * h
  in
    Spline xi1 xi a b c d

-- | Convert a cubic spline into an equivalent cubic Bezier curve
toBezier :: (RealFrac a) => Spline a -> Bezier a
toBezier s@(Spline x0 x3 _a b c d) = let
    x1 = (2.0 * x0 + x3) / 3.0
    x2 = (x0 + 2.0 * x3) / 3.0
    y0 = d
    x = x3 - x0
    y3 = splineAt s x3
    y1 = x * c / 3.0 + d
    y2 = x * (b * x + 2.0 * c) / 3.0 + d
  in
    Bezier (x0, y0) (x1, y1) (x2, y2) (x3, y3)
