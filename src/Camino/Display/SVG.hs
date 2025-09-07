{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : SVG
Description : Produce a SVG graphics for various bits and pieces
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}

module Camino.Display.SVG (
    buildCoordinates -- For testing
 ,  svgElevationProfile
) where

import Camino.Camino
import Camino.Config
import Camino.Display.Css
import Camino.Display.I18n
import Camino.Display.Routes
import Data.Description
import Data.Colour (Colour)
import Data.List (partition, uncons, unsnoc)
import Data.Localised
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Set as S
import Data.Spline
import Data.Text (Text)
import qualified Data.Text as T
import Data.Util (ceilingBy, floorBy, headWithError)
import qualified Data.Vector as V
import Text.Hamlet
import Debug.Trace

buildCoordinates'' :: Double -> Double -> Location -> [LegSegment] -> Double ->  [(Double, Maybe Double, Maybe Location)]
buildCoordinates'' _scalex _scaley _lt [] _d = []
buildCoordinates'' scalex scaley lt [seg] d = [(d', maybe Nothing (\e -> Just $ realToFrac (scaley * e)) (elevation $ locationPosition lt), Just lt)]
  where
    d' = d + (realToFrac $ lsDistance seg) * scalex
buildCoordinates'' scalex scaley lt (seg:rest) d = (d', maybe Nothing (\e -> Just $ realToFrac (scaley * e)) (elevation $ lsTo seg), Nothing):(buildCoordinates'' scalex scaley lt rest d')
  where
    d' = d + (realToFrac $ lsDistance seg) * scalex

buildCoordinates' :: Double -> Double -> [Leg] -> Double -> [(Double, Maybe Double, Maybe Location)]
buildCoordinates' _scalex _scaley [] _d = []
buildCoordinates' scalex scaley (leg:rest) d = (buildCoordinates'' scalex scaley lt (legSegments leg) d) ++ (buildCoordinates' scalex scaley rest d')
  where
    lf = legFrom leg
    lt = legTo leg
    ld = legDistance leg
    ld' = if ld > 0.0 then realToFrac ld else haversineDistance (locationPosition lf) (locationPosition lt) / 1000.0
    d' = d + ld' * scalex

buildCoordinates :: Double -> Double -> [Leg] -> [(Double, Maybe Double, Maybe Location)]
buildCoordinates _scalex _scaley [] = []
buildCoordinates scalex scaley legs@(st:_) = (0.0, realToFrac <$> (scaley *) <$> (elevation $ locationPosition sl), Just sl):(buildCoordinates' scalex scaley legs 0.0)
  where
    sl = legFrom st

-- Required boxLeft <= boxRight, boxTop >= boxBotton
data (Num a, Ord a) => Box a = Box {
    boxLeft :: a
  , boxRight :: a
  , boxTop :: a
  , boxBottom :: a
}

reflectX :: (Num a, Ord a) => Box a -> Box a
reflectX (Box bl br bt bb) = Box (negate br) (negate bl) bt bb

reflectY :: (Num a, Ord a) => Box a -> Box a
reflectY (Box bl br bt bb) = Box bl br (negate bb) (negate bt)

addX :: (Num a, Ord a) => a -> Box a -> Box a
addX x (Box bl br bt bb) = Box (bl + x) (br + x) bt bb

addY :: (Num a, Ord a) => a -> Box a -> Box a
addY y (Box bl br bt bb) = Box bl br (bt + y) (bb + y)

boxWidth :: (Num a, Ord a) => Box a ->  a
boxWidth (Box bl br _ _) = br - bl

boxHeight :: (Num a, Ord a) => Box a ->  a
boxHeight (Box _ _ bt bb) = bt - bb

isOverlapping :: (Num a, Ord a) => Box a -> Box a -> Bool
isOverlapping (Box bl1 br1 bt1 bb1) (Box bl2 br2 bt2 bb2) =
  (bl1 >= bl2 && bl1 <= br2 && bb1 >= bb2 && bb1 <= bt2) ||
  (br1 <= br2 && br1 >= bl2 && bb1 >= bb2 && bb1 <= bt2) ||
  (bl1 >= bl2 && bl1 <= br2 && bt1 <= bt2 && bt1 >= bb2) ||
  (br1 <= br2 && br1 >= bl2 && bt1 <= bt2 && bt1 >= bb2) ||
  (bl2 >= bl1 && bl2 <= br1 && bb2 >= bb1 && bb2 <= bt1) ||
  (br2 <= br1 && br2 >= bl1 && bb2 >= bb1 && bb2 <= bt1) ||
  (bl2 >= bl1 && bl2 <= br1 && bt2 <= bt1 && bt2 >= bb1) ||
  (br2 <= br1 && br2 >= bl1 && bt2 <= bt1 && bt2 >= bb1)

makeBox :: Text -> Int -> Int -> Location -> Box Int
makeBox anchor x y location = let
    width = 20 * (T.length $ localiseDefault $ locationName location)
    height = 15
  in
    case anchor of
      "middle" -> Box (x - hwidth) (x + hwidth) y (y - height) where hwidth = width `div` 2
      "end" -> Box (x - width) x  y (y - height)
      _ -> Box x (x + width) y (y - height)

type LabelPosition = (Int, Int, Int, Text, Box Int, Location)

positionLabel :: (Location -> Bool) -> (Location -> Bool) -> (Double -> Text) -> (Double -> Int) -> (Double -> Int) -> [LabelPosition] -> Int -> (Double, Double, Location) -> LabelPosition
positionLabel label important anchor makeX makeY seen offset v@(d, e, l) = let
  anch = anchor d
  x = makeX d
  box = makeBox anch x offset l
  in
    if any (\(_, _, _, _, box', _) -> isOverlapping box box') seen then
      positionLabel label important anchor makeX makeY seen (offset + 20) v
    else
      (x, makeY e, offset, anch, box, l)

positionLabels'' :: (Location -> Bool) -> (Location -> Bool) -> (Double -> Text) -> (Double -> Int) -> (Double -> Int) -> [LabelPosition] -> [(Double, Double, Location)] -> [LabelPosition]
positionLabels'' label important anchor makeX makeY seen [] = []
positionLabels'' label important anchor makeX makeY seen (v:rest) = let
    pos = positionLabel label important anchor makeX makeY seen 20 v
  in
    pos:(positionLabels'' label important anchor makeX makeY (pos:seen) rest)

positionLabels' :: (Location -> Bool) -> (Location -> Bool) -> (Double -> Text) -> (Double -> Int) -> (Double -> Int) -> [(Double, Double, Location)] -> [LabelPosition]
positionLabels' _label _important _anchor _makeX _makeY [] = []
positionLabels' _label _important _anchor makeX makeY [(d, e, l)] = [(makeX d, makeY e, 20, "center", makeBox "center" (makeX d) 10 l, l)]
positionLabels' label important anchor makeX makeY points = let
    (imps, nonimps) = partition (\(_, _, l) -> important l) points
    imps' = positionLabels'' label important anchor makeX makeY [] imps
    nonimps' = positionLabels'' label important anchor makeX makeY imps' nonimps
  in
    imps' ++ nonimps'

positionLabels :: (Location -> Bool) -> (Location -> Bool) -> (Double -> Text) -> (Double -> Int) -> (Double -> Int) -> Double -> [(Double, Maybe Double, Maybe Location)] -> [LabelPosition]
positionLabels label important anchor makeX makeY defElev coordinates = positionLabels' label important' anchor makeX makeY labels
  where
    labels = map (\(d, me, ml) -> (d, maybe defElev id me, fromJust ml)) $ filter (\(_, _, ml) -> maybe False label ml) coordinates
    sloc = \l -> maybe False (\((_, _, hl), _) -> l == hl) (uncons labels)
    lloc = \l -> maybe False (\(_, (_, _, ll)) -> l == ll) (unsnoc labels)
    important' l = (important l) || (sloc l) || (lloc l)

pathBezier :: (RealFrac a) => Bool -> (a -> Int) -> (a -> Int) -> Bezier a -> String
pathBezier move makeX makeY (Bezier (x0, y0) (x1, y1) (x2, y2) (x3, y3)) =
  (if move then "M" ++ (show $ makeX x0) ++ " " ++ show (makeY y0) else "") ++
  "C " ++
  (show $ makeX x1) ++ " " ++ (show $ makeY y1) ++ ", " ++
  (show $ makeX x2) ++ " " ++ (show $ makeY y2) ++ ", " ++
  (show $ makeX x3) ++ " " ++ (show $ makeY y3)

-- For debugging layouts
showTextLayout :: Bool
showTextLayout = False

-- For debugging layouts
showPathLayout :: Bool
showPathLayout = False

showElevation :: Location -> String
showElevation loc = maybe "" show (elevation $ locationPosition loc)

svgElevationProfile :: Config -> Double -> (Location -> Bool) -> (Location -> Bool) -> [Leg] -> HtmlUrlI18n CaminoMsg CaminoRoute
svgElevationProfile _config maxy label important legs = [ihamlet|
  <svg width="100%" height="100%" preserveAspectRatio="none" xmlns="http://www.w3.org/2000/svg">
    <svg width="100%" height="100%" viewBox="0 0 #{viewx} #{viewy}" preserveAspectRatio="none">
      <path d="#{elevationPath}" vector-effect="non-scaling-stroke" fill="#{toCssColour caminoLightGrey}" stroke="#{toCssColour caminoYellow}" stroke-width="1">
      $if showPathLayout
        <path d="#{elevationLines}" vector-effect="non-scaling-stroke" fill="none" stroke="#{toCssColour caminoBlue}" stroke-width="1">
        $forall (d, e) <- coordinates'
          <line x1="#{makeX d - 2}" x2="#{makeX d + 2}" y1="#{makeY e - 2}" y2="#{makeY e + 2}" vector-effect="non-scaling-stroke" stroke="red" stroke-width="1">
          <line x1="#{makeX d - 2}" x2="#{makeX d + 2}" y1="#{makeY e + 2}" y2="#{makeY e - 2}" vector-effect="non-scaling-stroke" stroke="red" stroke-width="1">
      $forall (x, y1, y2, _a, _b, _l) <- labelPos
        <line x1="#{x}" y1="#{y1 - 2}" x2="#{x}" y2="#{y2 + 2}" vector-effect="non-scaling-stroke" stroke="grey" stroke-width="1">
      <line x1="#{makeX 0.0 - 1}" y1="#{makeY 0.0}" x2="#{makeX 0.0 - 1}" y2="#{makeY maxy}" vector-effect="non-scaling-stroke" stroke="grey" stroke-width="1">
      $forall tic <- tics
        <line x1="#{makeX 0.0 - 5}" y1="#{makeY tic}" x2="#{makeX 0.0 - 1}" y2="#{makeY tic}" vector-effect="non-scaling-stroke" stroke="grey" stroke-width="1">
      $forall tic <- ticLabels
        <line x1="#{makeX 0.0} - 10" y1="#{makeY tic}" x2="#{makeX 0.0 - 1}" y2="#{makeY tic}" vector-effect="non-scaling-stroke" stroke="grey" stroke-width="1">
    $forall (x, _y1, y2, a, b, l) <- labelPos
      <text x="#{makeXP x}%" y="#{makeYP y2}%" font-size="8" text-anchor="#{a}" font-weight="#{fontWeight l}" title="#{showElevation l}">_{TxtPlain False False (locationName l)}
      $if showTextLayout
        <rect x="#{makeXP (boxLeft b)}%" y="#{makeYP (boxBottom b)}%" width="#{makeXP (boxWidth b)}%" height="#{makeYP (boxHeight b)}%" vector-effect="non-scaling-stroke" fill="none" stroke="#{toCssColour caminoBlue}" stroke-width="1">
    $forall tic <- ticLabels
      <text x="#{makeXP (makeX 0.0 + 4)}%" y="#{makeYP (makeY tic + 8)}%" font-size="6" text-anchor="start" fill="grey">#{show $ round tic}
  |]
  where
    viewx = 1200 :: Int
    viewy = 200 :: Int
    coordinates = buildCoordinates 1.0 1.0 legs
    coordinates' = map (\(d, me, _) -> (d, maybe 0.0 id me)) $ filter (\(_, me, _) -> isJust me) coordinates
    maxx = max 1.0 (maximum $ map (\(d, _) -> d) coordinates')
    offsetx = 10.0
    scalex = (fromIntegral viewx - offsetx) / maxx
    offsety = 50.0
    scaley = (fromIntegral viewy - offsety) / maxy
    makeX d = (round $ offsetx + scalex * d) :: Int
    makeXP x = 100.0 * fromIntegral x / fromIntegral viewx :: Double
    makeY e = (viewy - (round $ scaley * e)) :: Int
    makeYD e = (round $ scaley * e) :: Int
    makeYP y = 100.0 * fromIntegral y / fromIntegral viewy :: Double
    dropY me = maybe (round offsety) makeY me - 4
    anchor d = if d < maxx / 5.0 then "start" else if d > maxx * 4.0 / 5.0 then "end" else "middle" :: Text
    fontWeight l = if important l then "bold" else "normal" :: Text
    splines = makeSpline NaturalBoundary NaturalBoundary coordinates'
    beziers = map toBezier splines
    (_, es) = headWithError coordinates'
    elevationPath = "M " ++ (show $ makeX 0.0) ++ " " ++ (show $ makeY 0.0) ++ " L " ++ (show $ makeX 0.0) ++ " " ++ (show $ makeY es) ++ " " ++ (concat $ map (\b -> pathBezier False makeX makeY b ++ " ") beziers) ++ " L " ++ (show $ makeX maxx) ++ " " ++ (show $ makeY 0.0) ++ " Z"
    elevationLines = "M " ++ (show $ makeX 0.0) ++ " " ++ (show $ makeY 0.0) ++ " L " ++ (show $ makeX 0.0) ++ " " ++ (show $ makeY es) ++ (concat $ map (\(d, e) -> " L " ++ (show $ makeX d) ++ " " ++ (show $ makeY e)) coordinates')
    labelPos = positionLabels label important anchor makeX makeY offsety coordinates
    ticLabels = [0.0, step .. (floorBy step maxy)] where step = if maxy < 200.0 then 100.0 else 500.0
    tics = filter (\t -> not $ elem t ticLabels) [0.0, step  .. (ceilingBy step maxy)] where step = if maxy < 200.0 then 10.0 else 100.0
