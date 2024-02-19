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

Note that XML-Hamlet remvoes path interpolation, so routes are not a thing here.
-}

module Camino.Display.KML (
  createCaminoDoc
) where

import Camino.Camino
import Camino.Config
import Camino.Planner
import Camino.Preferences
import Camino.Display.Html
import Camino.Display.I18n
import Camino.Display.Routes
import Data.Maybe (fromJust)
import Data.Text (Text, pack, toLower)
import Data.Text.Lazy (toStrict)
import Data.Colour (Colour)
import Data.Colour.SRGB (toSRGB24, RGB(..))
import Data.Colour.Names (white)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (find)
import Text.Hamlet.XML
import Text.XML
import Text.Blaze.Html.Renderer.Text
import Data.Word (Word8)
import Numeric (showHex)

showHex2 :: Word8 -> ShowS
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


kmlStyle :: String -> Double -> Double -> Colour Double -> Maybe Text -> [Node]
kmlStyle identifier width alpha color icon = [xml|
    <Style id="#{pack identifier}">
      <LineStyle>
        <color>#{toARGB alpha color}
        <width>#{pack $ show width}
      <PolyStyle>
        <color>#{toARGB alpha color}
      $maybe ic <- icon
        <IconStyle>
          <Icon>#{ic}
  |]

caminoStyles :: Config -> Camino -> [Node]
caminoStyles config camino =
    kmlStyle "stopUsed" 1 1 white (Just (iconBase <> "/location-stop.png")) ++
    kmlStyle "villageUsed" 1 1 white (Just (iconBase <> "/location-village-used.png")) ++
    kmlStyle "villageUnused" 1 0.5 white (Just (iconBase <> "/location-village-unused.png")) ++
    kmlStyle "townUsed" 1 1 white (Just (iconBase <> "/location-town-used.png")) ++
    kmlStyle "townUnused" 1 0.5 white (Just (iconBase <> "/location-town-unused.png")) ++
    kmlStyle "cityUsed" 1 1 white (Just (iconBase <> "/location-city-used.png")) ++
    kmlStyle "cityUnused" 1 0.5 white (Just (iconBase <> "/location-city-unused.png")) ++
    kmlStyle "bridgeUsed" 1 1 white (Just (iconBase <> "/location-bridge-used.png")) ++
    kmlStyle "bridgeUnused" 1 0.5 white (Just (iconBase <> "/location-bridge-unused.png")) ++
    kmlStyle "intersectionUsed" 1 1 white (Just (iconBase <> "/location-intersection-used.png")) ++
    kmlStyle "intersectionUnused" 1 0.5 white (Just (iconBase <> "/location-intersection-unused.png")) ++
    kmlStyle "poiUsed" 1 1 white (Just (iconBase <> "/location-poi-used.png")) ++
    kmlStyle "poiUnused" 1 0.5 white (Just (iconBase <> "/location-poi-unused.png")) ++
    foldr (\r -> \k -> k ++ kmlStyle (routeID r ++ "Used") 8 1 (paletteColour $ routePalette r) Nothing) [] (caminoRoutes camino) ++
    foldr (\r -> \k -> k ++ kmlStyle (routeID r ++ "Unused") 4 0.5 (paletteColour $ routePalette r) Nothing) [] (caminoRoutes camino) ++
    kmlStyle "defaultUsed" 8 1 (paletteColour $ routePalette $ caminoDefaultRoute camino) Nothing ++
    kmlStyle "defaultUnused" 4 0.5 (paletteColour $ routePalette $ caminoDefaultRoute camino) Nothing
  where
    iconBase = assetPath $ fromJust $ getAsset "icons" config



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

caminoLocationStyle :: Camino -> S.Set Location -> S.Set Location -> Location -> Text
caminoLocationStyle _camino stops waypoints location
  | S.member location stops = "#stopUsed"
  | S.member location waypoints = "#" <> (toLower $ pack $ show $ locationType location) <> "Used"
  | otherwise = "#" <> (toLower $ pack $ show $ locationType location) <> "Unused"
  
caminoLocationKml :: Config -> Preferences -> Camino -> Maybe Trip -> S.Set Location -> S.Set Location -> Location -> [Node]
caminoLocationKml config preferences camino trip stops waypoints location = [xml|
    <Placemark id="#{pack $ locationID location}">
      <name>#{locationName location}
      <description>
        #{toStrict $ renderHtml $ (locationSummary preferences camino location) message route}
        $maybe d <- day
          #{toStrict $ renderHtml $ (daySummary preferences camino trip d) message route}
      <styleUrl>#{caminoLocationStyle camino stops waypoints location}
      ^{pointKml $ locationPosition location}
  |]
  where
    message = renderCaminoMsg config
    route = renderCaminoRoute config ["en", ""]
    day = maybe Nothing (\t -> find (\d -> start d == location) (path t)) trip

caminoLegStyle :: Camino -> S.Set Location -> S.Set Location -> Leg -> Text
caminoLegStyle camino _stops waypoints leg =
  let
    from' = legFrom leg
    to' = legTo leg
    used = if (S.member from' waypoints) && (S.member to' waypoints) then "Used" else "Unused"
    route = find (\r -> S.member from' (routeLocations r) || S.member to' (routeLocations r)) (caminoRoutes camino)
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
createCaminoDoc :: Config -> Preferences -> Camino -> Maybe Trip -> Document
createCaminoDoc config preferences camino trip = Document (Prologue [] Nothing []) kml []
  where
    stops = maybe S.empty (S.fromList . tripStops) trip
    waypoints = maybe S.empty (S.fromList . tripWaypoints) trip
    ns = M.fromList [ ("xmlns", "http://www.opengis.net/kml/2.2"), ("xmlns:gx", "http://www.google.com/kml/ext/2.2") ]
    kml = Element "kml" ns
      [xml|
        <Document>
          $maybe t <- trip
            <name>#{locationName $ start t} to #{locationName $ finish t}
           ^{caminoStyles config camino}
          $forall location <- caminoLocationList camino
            ^{caminoLocationKml config preferences camino trip stops waypoints location}
          $forall leg <- caminoLegs camino
            ^{caminoLegKml camino stops waypoints leg}
      |]



