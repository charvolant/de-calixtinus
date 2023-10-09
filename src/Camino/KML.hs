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
import Camino.Html
import Data.Text (Text, pack)
import Data.Colour (Colour, AlphaColour)
import Data.Colour.SRGB (sRGB24show, toSRGB24, RGB(..))
import Data.Colour.Names (white)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Text.Hamlet.XML
import Text.XML
import Text.Blaze.Html.Renderer.Text
import Data.Text.Lazy (toStrict)
import Numeric (showHex)

showHex2 x 
  | x <= 0xf = ("0"++) . showHex x
  | otherwise = showHex x

-- | Convert am opaque colour into an RGB triple
--   KML is hex aabbggrr - alpha, blue, green, red for some reason
toARGB :: Double -> Colour Double -> Text
toARGB alpha colour = pack $ (showHex2 a' . showHex2 b' . showHex2 g' . showHex2 r') ""
  where
    a' = floor (alpha * 255)
    RGB r' g' b' = toSRGB24 colour


kmlStyle :: String -> Double -> Double -> Colour Double -> Maybe String -> [Node]
kmlStyle identifier width alpha color icon = [xml|
    <Style id="#{pack identifier}">
      <LineStyle>
        <color>#{toARGB alpha color}
        <width>#{pack $ show width}
      <PolyStyle>
        <color>#{toARGB alpha color}
      $maybe ic <- icon
        <IconStyle>
          <Icon>#{pack ic}
  |]

caminoStyles :: Camino -> [Node]
caminoStyles camino =
    kmlStyle "waypointUsed" 1 1 white (Just "https://camino-planner.s3.ap-southeast-2.amazonaws.com/icons/location-waypoint-used.png") ++
    kmlStyle "waypointUnused" 1 0.5 white (Just "https://camino-planner.s3.ap-southeast-2.amazonaws.com/icons/location-waypoint-unused.png") ++
    kmlStyle "stopUsed" 1 1 white (Just "https://camino-planner.s3.ap-southeast-2.amazonaws.com/icons/location-stop.png") ++
    foldr (\r -> \k -> k ++ kmlStyle (routeID r ++ "Used") 8 1 (paletteColour $ routePalette r) Nothing) [] (routes camino) ++
    foldr (\r -> \k -> k ++ kmlStyle (routeID r ++ "Unused") 4 0.5 (paletteColour $ routePalette r) Nothing) [] (routes camino) ++
    kmlStyle "defaultUsed" 8 1 (paletteColour $ palette camino) Nothing ++
    kmlStyle "defaultUnused" 4 0.5 (paletteColour $ palette camino) Nothing


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
  
caminoLocationKml :: Preferences -> Camino -> Maybe Trip -> S.Set Location -> S.Set Location -> Location -> [Node]
caminoLocationKml preferences camino trip stops waypoints location = [xml|
    <Placemark id="#{pack $ locationID location}">
      <name>#{locationName location}
      <description>
        #{toStrict $ renderHtml $ locationSummary preferences camino location}
        $maybe d <- day
          #{toStrict $ renderHtml $ daySummary preferences camino trip d}
      <styleUrl>#{caminoLocationStyle stops waypoints location}
      ^{pointKml $ locationPosition location}
  |]
  where
    day = maybe Nothing (\t -> find (\d -> start d == location) (path t)) trip

caminoLegStyle camino stops waypoints leg =
  let
    from' = legFrom leg
    to' = legTo leg
    used = if (S.member from' waypoints) && (S.member to' waypoints) then "Used" else "Unused"
    route = find (\r -> S.member from' (routeLocations r) || S.member to' (routeLocations r)) (routes camino)
  in
    pack $ "#" ++ maybe "default" routeID route ++ used

caminoLegKml :: Camino -> S.Set Location -> S.Set Location -> Leg -> [Node]
caminoLegKml camino stops waypoints leg = [xml|
    <Placemark>
      <name>#{(pack $ show $ legDistance leg) <> "km"}
      <styleUrl>#{caminoLegStyle camino stops waypoints leg}
      ^{lineKml (locationPosition $ legFrom leg) (locationPosition $ legTo leg)}
  |]

-- Create a KML document of a camino
createCaminoDoc :: Preferences -> Camino -> Maybe Trip -> Document
createCaminoDoc preferences camino trip = Document (Prologue [] Nothing []) kml []
  where
    stops = maybe S.empty (S.fromList . tripStops) trip
    waypoints = maybe S.empty (S.fromList . tripWaypoints) trip    
    ns = M.fromList [ ("xmlns", "http://www.opengis.net/kml/2.2"), ("xmlns:gx", "http://www.google.com/kml/ext/2.2") ]
    kml = Element "kml" ns
      [xml|
        <Document>
          $maybe t <- trip
            <name>#{locationName $ start t} to #{locationName $ finish t}
           ^{caminoStyles camino}
          $forall location <- caminoLocations camino
            ^{caminoLocationKml preferences camino trip stops waypoints location}
          $forall leg <- legs camino
            ^{caminoLegKml camino stops waypoints leg}
      |]



