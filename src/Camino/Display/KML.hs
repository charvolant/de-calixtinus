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
The legs run between two locations, each with possible accommodation and service options.

Generally, it is expected that these models will be read from JSON files.

Note that XML-Hamlet remvoes path interpolation, so routes are not a thing here.
-}

module Camino.Display.KML (
    createCaminoDoc
  , useCDATA
) where

import Camino.Camino
import Camino.Config
import Camino.Planner
import Camino.Preferences
import Camino.Display.Html
import Camino.Display.I18n
import Camino.Display.Routes
import Data.Localised (localeFromID, rootLocale)
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text, isPrefixOf, pack, toLower)
import Data.Text.Lazy (toStrict)
import Data.Colour (Colour)
import Data.Colour.SRGB (toSRGB24, RGB(..))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (find, singleton)
import Data.XML.Types (Content(..))
import Text.Hamlet
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

kmlRouteStyle :: Text -> Double -> Double -> Colour Double -> [Node]
kmlRouteStyle identifier width alpha color = [xml|
    <Style id="#{identifier}">
      <LineStyle>
        <color>#{toARGB alpha color}
        <width>#{pack $ show width}
      <PolyStyle>
        <color>#{toARGB alpha color}
  |]


kmlLocationStyle :: Config -> Text -> Text -> [Node]
kmlLocationStyle config identifier icon = [xml|
    <Style id="#{identifier}">
      <BalloonStyle>
        <text>
          ^{layout}
      <IconStyle>
        <Icon>#{icon}
  |]
  where
    router = renderCaminoRoute config [rootLocale]
    css = getAsset "camino-css" config
    layout = singleton $ NodeContent $ toStrict $ renderHtml $ [hamlet|
          <html>
            <head>
              $maybe c <- css
                <link rel="stylesheet" href="#{assetPath c}">
            <body>
              \$[description]    
    |] router

locationStyles :: Config -> LocationType -> Text -> [Node]
locationStyles config locType iconBase = let
    name = toLower $ pack $ show locType
  in
    kmlLocationStyle config (name <> "-stop") (iconBase <> "/location-" <> name <> "-stop.png") ++
    kmlLocationStyle config (name <> "-used") (iconBase <> "/location-" <> name <> "-used.png") ++
    kmlLocationStyle config (name <> "-unused") (iconBase <> "/location-" <> name <> "-unused.png")

caminoStyles :: Config -> CaminoPreferences -> [Node]
caminoStyles config camino =
    foldr (\t -> \k -> k ++ locationStyles config t iconBase) [] locationStopTypeEnumeration ++
    foldr (\r -> \k -> k ++ kmlRouteStyle (pack (routeID r <> "-used")) 8 1 (paletteColour $ routePalette r)) [] (caminoRoutes camino') ++
    foldr (\r -> \k -> k ++ kmlRouteStyle (pack (routeID r <> "-unused")) 4 0.5 (paletteColour $ routePalette r)) [] (caminoRoutes camino') ++
    kmlRouteStyle "default-used" 8 1 (paletteColour $ routePalette $ caminoDefaultRoute camino') ++
    kmlRouteStyle "default-unused" 4 0.5 (paletteColour $ routePalette $ caminoDefaultRoute camino')
  where
    camino' = preferenceCamino camino
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

caminoLocationStyle :: CaminoPreferences -> S.Set Location -> S.Set Location -> Location -> Text
caminoLocationStyle _camino stops waypoints location
  | S.member location stops = "#" <> (toLower $ pack $ show $ locationType location) <> "-stop"
  | S.member location waypoints = "#" <> (toLower $ pack $ show $ locationType location) <> "-used"
  | otherwise = "#" <> (toLower $ pack $ show $ locationType location) <> "-unused"


caminoLocationHtmlForPlacemark :: Config -> TravelPreferences -> CaminoPreferences -> Maybe Trip -> S.Set Location -> S.Set Location -> Location -> [Node]
caminoLocationHtmlForPlacemark config preferences camino trip _stops _waypoints location = singleton $ NodeContent (toStrict $ renderHtml $ [ihamlet|
  <div>
    <h4>#{locationNameLabel location}
    $maybe d <- locationDescription location
      <div>^{descriptionLine d}
    <div>
      <div style="display: inline-block;" .services>
        $forall service <- locationServices location
          ^{caminoServiceIcon service}
      <div style="display: inline-block; margin-left: 2em;" .accommodation-types>
          $forall accommodation <- locationAccommodationTypes location
            ^{caminoAccommodationTypeIcon accommodation}
    $maybe d <- day
      <div style="margin-top: 1ex;">
        ^{daySummary preferences camino trip d}
|] message route)
  where
    langs = ["en", ""]
    locales = catMaybes $ map localeFromID langs
    message = renderCaminoMsg config locales
    route = renderCaminoRoute config locales
    day = maybe Nothing (\t -> find (\d -> start d == location) (path t)) trip

caminoLocationKml :: Config -> TravelPreferences -> CaminoPreferences -> Maybe Trip -> S.Set Location -> S.Set Location -> Location -> [Node]
caminoLocationKml config preferences camino trip stops waypoints location = [xml|
    <Placemark id="#{pack $ locationID location}">
      <name>#{locationNameLabel location}
      <description>^{caminoLocationHtmlForPlacemark config preferences camino trip stops waypoints location}
      <styleUrl>#{caminoLocationStyle camino stops waypoints location}
      ^{pointKml $ locationPosition location}
  |]


caminoLegStyle :: CaminoPreferences -> S.Set Location -> S.Set Location -> Leg -> Text
caminoLegStyle camino _stops waypoints leg =
  let
    camino' = preferenceCamino camino
    from' = legFrom leg
    to' = legTo leg
    used = if (S.member from' waypoints) && (S.member to' waypoints) then "-used" else "-unused"
    route = find (\r -> S.member from' (routeLocations r) || S.member to' (routeLocations r)) (caminoRoutes camino')
  in
    pack $ "#" ++ maybe "default" routeID route ++ used

caminoLegKml :: CaminoPreferences -> S.Set Location -> S.Set Location -> Leg -> [Node]
caminoLegKml camino stops waypoints leg = [xml|
    <Placemark>
      <name>#{(pack $ show $ legDistance leg) <> "km"}
      <styleUrl>#{caminoLegStyle camino stops waypoints leg}
      ^{lineKml (locationPosition $ legFrom leg) (locationPosition $ legTo leg)}
  |]

-- | Use CDATA to rander some text.
--   Used to ensure the inner HTML in the description element is encased in CDATA as KML requires
useCDATA :: Content -> Bool
useCDATA (ContentEntity _) = False
useCDATA (ContentText v) = isPrefixOf "<html>" v || isPrefixOf "<div>" v


-- Create a KML document of a camino
createCaminoDoc :: Config -> TravelPreferences -> CaminoPreferences -> Maybe Trip -> Document
createCaminoDoc config preferences camino trip = Document (Prologue [] Nothing []) kml []
  where
    camino' = preferenceCamino camino
    stops = maybe S.empty (S.fromList . tripStops) trip
    waypoints = maybe S.empty (S.fromList . tripWaypoints) trip
    ns = M.fromList [ ("xmlns", "http://www.opengis.net/kml/2.2"), ("xmlns:gx", "http://www.google.com/kml/ext/2.2") ]
    divider = " - "
    kml = Element "kml" ns
      [xml|
        <Document>
          $maybe t <- trip
            <name>#{locationNameLabel $ start t}#{divider}#{locationNameLabel $ finish t}
          ^{caminoStyles config camino}
          $forall location <- caminoLocationList camino'
            ^{caminoLocationKml config preferences camino trip stops waypoints location}
          $forall leg <- caminoLegs camino'
            ^{caminoLegKml camino stops waypoints leg}
      |]



