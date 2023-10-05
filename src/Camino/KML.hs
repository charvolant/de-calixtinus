{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : KML
Description : Produce a KML map for caminos and trips
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

A camino consists of a graph of legs that can be assembled in various ways.
The legs run between two locations, each with possible accomodation and service options.

Generally, it is expected that these models will be read from JSON files.
-}

module Camino.KML where
  
import Control.Monad
import Camino.Camino
import Camino.Planner
import Data.Text (Text, pack)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Text.Hamlet.XML
import Text.XML

type Colour = String

kmlStyle :: String -> Double -> Colour -> Colour -> Maybe String -> [Node]
kmlStyle identifier width line fill icon = [xml|
    <Style id="#{pack identifier}">
      <LineStyle>
        <color>#{pack line}
        <width>#{pack $ show width}
      <PolyStyle>
        <color>#{pack fill}
      $maybe ic <- icon
        <IconStyle>
          <Icon>#{pack ic}
  |]
  
caminoStyles :: Camino -> [Node]
caminoStyles camino =
    kmlStyle "waypointUsed" 1 "ffffffff" "ffffffff" (Just "http://maps.google.com/mapfiles/kml/pal3/icon48.png") ++
    kmlStyle "waypointUnused" 1 "ffffffff" "ffffffff" (Just "http://maps.google.com/mapfiles/kml/pal3/icon56.png") ++
    kmlStyle "stopUsed" 1 "ffffffff" "ffffffff" (Just "http://maps.google.com/mapfiles/kml/pal2/icon20.png") ++
    kmlStyle "legUsed" 8 "ffffff00" "ffffff00" Nothing ++
    kmlStyle "legUnused" 4 "ff808080" "ff808080" Nothing

pointKml :: Maybe LatLong -> [Node]
pointKml (Just latlong) = [xml|
    <Point>
      <coordinates>#{pack $ show $ longitude latlong},#{pack $ show $ latitude latlong},0
  |]
pointKml _ = []

lineKml :: Maybe LatLong -> Maybe LatLong -> [Node]
lineKml (Just latlong1) (Just latlong2) = [xml|
      <LineString>
        <coordinates>
          #{cstr}
    |]
  where
    coords latlong = (pack $ show $ longitude latlong) <> "," <> (pack $ show $ latitude latlong) <> ",0 "
    cstr = coords latlong1 <> " " <> coords latlong2 -- Required because the template removes spaces
lineKml _ _ = []

caminoLocationStyle stops waypoints location
  | S.member location stops = "#stopUsed"
  | S.member location waypoints = "#waypointUsed"
  | otherwise = "#waypointUnused"
  
caminoLocationKml :: S.Set Location -> S.Set Location -> Location -> [Node]
caminoLocationKml stops waypoints location = [xml|
    <Placemark id="${pack $ locationID location}">
      <name>#{locationName location}
      <styleUrl>#{caminoLocationStyle stops waypoints location}
      ^{pointKml $ locationPosition location}
  |]

caminoLegStyle stops waypoints leg
  | (S.member (legFrom leg) waypoints) && (S.member (legTo leg) waypoints)  = "#legUsed"
  | otherwise = "#legUnused"

caminoLegKml :: S.Set Location -> S.Set Location -> Leg -> [Node]
caminoLegKml stops waypoints leg = [xml|
    <Placemark>
      <name>#{(pack $ show $ legDistance leg) <> "km"}
      <styleUrl>#{caminoLegStyle stops waypoints leg}
      ^{lineKml (locationPosition $ legFrom leg) (locationPosition $ legTo leg)}
  |]

-- Create a KML document of a camino
createCaminoDoc :: Camino -> Maybe Trip -> Document
createCaminoDoc camino trip = Document (Prologue [] Nothing []) kml []
  where
    stops = maybe S.empty (S.fromList . tripStops) trip
    waypoints = maybe S.empty (S.fromList . tripWaypoints) trip    
    ns = M.fromList [ ("xmlns", "http://www.opengis.net/kml/2.2"), ("xmlns:gx", "http://www.google.com/kml/ext/2.2") ]
    kml = Element "kml" ns
      [xml|
        <Document>
          ^{caminoStyles camino}
          $forall location <- caminoLocations camino
            ^{caminoLocationKml stops waypoints location}
          $forall leg <- legs camino
            ^{caminoLegKml stops waypoints leg}
      |]



