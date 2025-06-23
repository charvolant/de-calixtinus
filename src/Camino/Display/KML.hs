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
import Data.Description
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text, intercalate, isPrefixOf, pack, toLower)
import Data.Text.Lazy (toStrict)
import Data.Colour (Colour)
import Data.Colour.SRGB (toSRGB24, RGB(..))
import Data.Localised
import Data.Metadata
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (find, singleton)
import Data.Word (Word8)
import Data.XML.Types (Content(..))
import Graph.Graph (outgoing)
import Numeric (showHex)
import Text.Hamlet
import Text.Hamlet.XML
import Text.Shakespeare.Text
import Text.XML
import Text.Blaze.Html.Renderer.Text
import Debug.Trace

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

htmlToNodes :: Config -> [Locale] -> HtmlUrlI18n CaminoMsg CaminoRoute -> [Node]
htmlToNodes config locales html =
  singleton $ NodeContent $ toStrict $ renderHtml $ html message route
    where
      message = renderCaminoMsg config locales
      route = renderCaminoRoute config locales

kmlRouteStyle :: Text -> Double -> Double -> Colour Double -> [Node]
kmlRouteStyle identifier width alpha color = [xml|
    <Style id="#{identifier}">
      <LineStyle>
        <color>#{toARGB alpha color}
        <width>#{pack $ show width}
      <PolyStyle>
        <color>#{toARGB alpha color}
  |]


kmlLocationStyle :: Config -> [Locale] -> Text -> Text -> [Node]
kmlLocationStyle config locales identifier icon = [xml|
    <Style id="#{identifier}">
      <BalloonStyle>
        <text>
          ^{layout}
      <IconStyle>
        <Icon>#{icon}
  |]
  where
    layout = htmlToNodes config locales [ihamlet|
          <html>
            <head>
              <link rel="stylesheet" href="@{AssetRoute "bootstrap-css"}">
              <link rel="stylesheet" href="@{AssetRoute "camino-css"}">
              <script src="@{AssetRoute "bootstrap-js"}">
            <body>
              \$[description]    
    |]

locationStyles :: Config -> [Locale] -> LocationType -> Text -> [Node]
locationStyles config locales locType iconBase = let
    name = toLower $ pack $ show locType
  in
    kmlLocationStyle config locales (name <> "-stop") (iconBase <> "/location-" <> name <> "-stop.png") ++
    kmlLocationStyle config locales (name <> "-used") (iconBase <> "/location-" <> name <> "-used.png") ++
    kmlLocationStyle config locales (name <> "-unused") (iconBase <> "/location-" <> name <> "-unused.png")

caminoStyles :: Config -> [Locale] -> CaminoPreferences -> [Node]
caminoStyles config locales camino =
    foldr (\t -> \k -> k ++ locationStyles config locales t iconBase) [] locationTypeEnumeration ++
    foldr (\r -> \k -> k ++ kmlRouteStyle (routeID r <> "-used") 8 1 (paletteColour $ routePalette r)) [] (caminoRoutes camino') ++
    foldr (\r -> \k -> k ++ kmlRouteStyle (routeID r <> "-unused") 4 0.5 (paletteColour $ routePalette r)) [] (caminoRoutes camino') ++
    kmlRouteStyle "default-used" 8 1 (paletteColour $ routePalette $ caminoDefaultRoute camino') ++
    kmlRouteStyle "default-unused" 4 0.5 (paletteColour $ routePalette $ caminoDefaultRoute camino')
  where
    camino' = preferenceCamino camino
    iconBase = appendRoot config $ assetPath $ fromJust $ getAsset "icons" config

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


caminoLocationHtmlForPlacemark :: Config -> [Locale] -> TravelPreferences -> CaminoPreferences -> Maybe Pilgrimage -> S.Set Location -> S.Set Location -> Location -> [Node]
caminoLocationHtmlForPlacemark config locales tprefs cprefs pilgrimage _stops waypoints location = htmlToNodes config locales $ [ihamlet|
  <div .container-fluid .p-3>
    <div .row>
      <h3>^{locationLineSimple location}
    ^{locationLegSummary tprefs cprefs usedLegs location}
    $maybe desc <- locationDescription location
      <div .row>
        ^{descriptionBlock True False desc}
    $maybe s <- stage
      <div .row .mt-4>
        <div .card>
          <div .card-body>
            <h4 .card-title>_{StageLabel}
            <div .card-text>
              ^{stageSummary tprefs cprefs pilgrimage s}
    $maybe d <- day
      <div .row .mt-4>
        <div .card>
          <div .card-body>
            <h4 .card-title>_{DayLabel}
            <div .card-text>
              ^{daySummary tprefs cprefs pilgrimage d}
|]
  where
    day = maybe Nothing (\p -> findDay p location) pilgrimage
    stage = maybe Nothing (\p -> findStage p location) pilgrimage
    camino = preferenceCamino cprefs
    legs = outgoing camino location
    usedLegs = S.fromList $ filter (\l -> S.member (legTo l) waypoints) legs

caminoTextForSolution :: Config -> [Locale] -> TravelPreferences -> CaminoPreferences -> Maybe Solution -> Text
caminoTextForSolution config locales _tprefs cprefs msolution =  traceShowId $ intercalate "\n" $ catMaybes (heading ++ notes ++ caminoMd ++ solutionMd)
  where
    message = renderCaminoMsgText config locales
    camino = preferenceCamino cprefs
    mpilgrimage = maybe Nothing solutionPilgrimage msolution
    msid = maybe Nothing solutionID msolution
    heading =  [
        Just $ localiseText locales (caminoName camino)
      , (\p -> localiseText locales (locationName (start p)) <> " - " <> localiseText locales (locationName (finish p))) <$> mpilgrimage
      , Just (getWebRoot config <> maybe "" ("/plan/" <>) msid)
      , Just ""
      , (\d -> localiseText locales d) <$> (descText $ caminoDescription camino)
      ]
    notes = map (\n -> Just $ (message $ descriptionNoteTypeMsg $ noteType n) <> ": " <> (localiseText locales (noteText n))) (descNotes $ caminoDescription camino)
    mapMetadata title metadata = (Just ""):(Just (message title)):map (\s -> Just $ statementLabel s <> " " <> statementValue s) (metadataStatements metadata)
    caminoMd = mapMetadata CaminoLabel (caminoMetadata camino)
    msMetadata = maybe Nothing solutionMetadata msolution
    solutionMd = maybe [] (mapMetadata PlanLabel) msMetadata


caminoLocationKml :: Config -> [Locale] -> TravelPreferences -> CaminoPreferences -> Maybe Pilgrimage -> S.Set Location -> S.Set Location -> Location -> [Node]
caminoLocationKml config locales preferences camino pilgrimage stops waypoints location = [xml|
    <Placemark id="#{locationID location}">
      <name>#{name}
      <description>^{caminoLocationHtmlForPlacemark config locales preferences camino pilgrimage stops waypoints location}
      <styleUrl>#{caminoLocationStyle camino stops waypoints location}
      ^{pointKml $ locationPosition location}
  |]
  where
    name = localiseText locales (locationName location)

caminoLegStyle :: CaminoPreferences -> S.Set Location -> S.Set Location -> Leg -> Text
caminoLegStyle camino _stops waypoints leg =
  let
    camino' = preferenceCamino camino
    from' = legFrom leg
    to' = legTo leg
    used = if (S.member from' waypoints) && (S.member to' waypoints) then "-used" else "-unused"
    route = find (\r -> S.member from' (routeLocations r) || S.member to' (routeLocations r)) (caminoRoutes camino')
  in
    "#" <> maybe "default" routeID route <> used

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
useCDATA (ContentText v) = isPrefixOf "<html" v || isPrefixOf "<div" v

createCaminoTitle :: [Locale] -> Camino -> Maybe Pilgrimage -> Text
createCaminoTitle locales camino Nothing = localiseText locales (caminoName camino)
createCaminoTitle locales camino (Just pilgrimage) =
  localiseText locales (caminoName camino)
   <> " "
   <> localiseText locales (locationName (start pilgrimage))
   <> " - "
   <> localiseText locales (locationName (finish pilgrimage))

-- Create a KML document of a camino
createCaminoDoc :: Config -> [Locale] -> TravelPreferences -> CaminoPreferences -> Maybe Solution -> Document
createCaminoDoc config locales tprefs cprefs msolution = Document (Prologue [] Nothing []) kml []
  where
    camino = preferenceCamino cprefs
    mpilgrimage = maybe Nothing solutionPilgrimage msolution
    stops = maybe S.empty (S.fromList . pilgrimageStops) mpilgrimage
    waypoints = maybe S.empty (S.fromList . pilgrimageWaypoints) mpilgrimage
    ns = M.fromList [ ("xmlns", "http://www.opengis.net/kml/2.2"), ("xmlns:gx", "http://www.google.com/kml/ext/2.2") ]
    kml = Element "kml" ns
      [xml|
        <Document>
          <name>#{createCaminoTitle locales camino mpilgrimage}
          <description>#{caminoTextForSolution config locales tprefs cprefs msolution}
          ^{caminoStyles config locales cprefs}
          $forall location <- caminoLocationList camino
            ^{caminoLocationKml config locales tprefs cprefs mpilgrimage stops waypoints location}
          $forall leg <- caminoLegs camino
            ^{caminoLegKml cprefs stops waypoints leg}
          <ExtendedData>
            $forall stmt <- metadataStatements (caminoMetadata camino)
              <Data name="#{statementLabel stmt}">
                <value>#{statementValue stmt}
            $maybe md <- maybe Nothing solutionMetadata msolution
              $forall stmt <- metadataStatements md
                 <Data name="#{statementLabel stmt}">
                   <value>#{statementValue stmt}

      |]



