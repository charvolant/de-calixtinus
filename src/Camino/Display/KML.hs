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

Use the leg specifications to produce a KML file that describes a camino and planned route.

This does not approach the level of detail avilable on the map.
Detailed features are not used, instead the legs are used for simple links between locations.
Location points contain simple summary HTML.

Icons for locations are taken from the standard map symbology.
-}

module Camino.Display.KML (
    createCaminoDoc
  , useCDATA
) where

import Control.Applicative ((<|>))
import Camino.Camino
import Camino.Colour
import Camino.Config
import Camino.Planner
import Camino.Preferences
import Camino.Display.Css
import Camino.Display.Html
import Camino.Display.I18n
import Camino.Display.Routes
import Data.Attributes
import Data.Description
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text, intercalate, isPrefixOf, pack, toLower)
import Data.Text.Lazy (toStrict)
import Data.Localised
import qualified Data.Map as M
import Data.Metadata
import qualified Data.Set as S
import Data.List (find, singleton)
import qualified Data.Units as U
import Data.XML.Types (Content(..))
import qualified Geo.Feature as F
import Geo.Geometry
import Geo.LatLong
import Graph.Graph (outgoing)
import Text.Hamlet
import Text.Hamlet.XML
import Text.XML
import Text.Blaze.Html.Renderer.Text

htmlToNodes :: U.SystemOfUnits -> Config -> [Locale] -> HtmlUrlI18n CaminoMsg CaminoRoute -> [Node]
htmlToNodes _sou config locales html =
  singleton $ NodeContent $ toStrict $ renderHtml $ html message route
    where
      message = renderCaminoMsg config locales
      route = renderCaminoRoute config locales


kmlTrailDescription :: Config -> U.SystemOfUnits -> [Locale] -> [Node]
kmlTrailDescription config sou locales = htmlToNodes sou config locales $ [ihamlet|
<div .container-fluid>
  <div .row>
    <p>$[description]
    <p>$[source]
    <p>$[license] - $[rightsHolder]
|]

kmlTrailStyle :: Config -> U.SystemOfUnits -> [Locale] -> Route -> Maybe Feature -> Text -> Bool -> [Node]
kmlTrailStyle config sou locales route mfeature identifier used = [xml|
    <Style id="#{identifier}">
      <LineStyle>
        <color>#{toKmlColour falpha rgb}
        <width>#{pack $ show width}
      <PolyStyle>
        <color>#{toKmlColour falpha rgb}
      <BalloonStyle>
        <bgColor>#{toKmlColour 1.0 caminoBackground}
        <text>^{kmlTrailDescription config sou locales}
  |]
  where
    (rgb, width, _dalpha, alpha, _mdashes, _mcap) = featureLineStyle route (null $ routeFeatures route) (maybe False featureDummy mfeature) used (maybe Road featureType mfeature)
    falpha = realToFrac alpha

kmlTrailStyles :: Config -> U.SystemOfUnits -> [Locale] -> Route -> Bool -> S.Set Feature -> [Node]
kmlTrailStyles config sou locales route routeUsed usedFeatures =
  kmlTrailStyle config sou locales route Nothing ("style-route-" <> (routeID route)) routeUsed ++
  foldr (\f -> \s -> kmlTrailStyle config sou locales route (Just f) ("style-feature-" <> (featureID f)) (S.member f usedFeatures) ++ s) [] (concat $ map allFeatures (routeFeatures route))

kmlLegStyle :: Config -> U.SystemOfUnits -> [Locale] -> Route -> Text -> Bool -> [Node]
kmlLegStyle _config _sou _locales route identifier used = [xml|
    <Style id="#{identifier}">
      <LineStyle>
        <color>#{toKmlColour falpha rgb}
        <width>#{pack $ show width}
      <PolyStyle>
        <color>#{toKmlColour falpha rgb}
  |]
  where
      (rgb, width, _dalpha, alpha, _mdashes, _mcap) = featureLineStyle route (null $ routeFeatures route) False used Road
      falpha = realToFrac alpha



kmlLocationStyle :: Config -> U.SystemOfUnits -> [Locale] -> Text -> Text -> [Node]
kmlLocationStyle config sou locales identifier icon = [xml|
    <Style id="#{identifier}">
      <BalloonStyle>
        <text>
          ^{layout}
      <IconStyle>
        <Icon>#{icon}
  |]
  where
    layout = htmlToNodes sou config locales [ihamlet|
          <html>
            <head>
              <link rel="stylesheet" href="@{AssetRoute "bootstrap-css"}">
              <link rel="stylesheet" href="@{AssetRoute "camino-css"}">
              <script src="@{AssetRoute "bootstrap-js"}">
            <body>
              \$[description]    
    |]

locationStyles :: Config -> U.SystemOfUnits -> [Locale] -> LocationType -> Text -> [Node]
locationStyles config sou locales locType iconBase = let
    name = toLower $ pack $ show locType
  in
    kmlLocationStyle config sou locales (name <> "-stop") (iconBase <> "/location-" <> name <> "-stop.png") ++
    kmlLocationStyle config sou locales (name <> "-used") (iconBase <> "/location-" <> name <> "-used.png") ++
    kmlLocationStyle config sou locales (name <> "-unused") (iconBase <> "/location-" <> name <> "-unused.png")

caminoStyles :: Config -> U.SystemOfUnits -> [Locale] -> CaminoPreferences -> S.Set Feature -> [Node]
caminoStyles config sou locales cprefs usedFeatures =
    foldr (\t -> \k -> k ++ locationStyles config sou locales t iconBase) [] locationTypeEnumeration ++
    foldr (\r -> \k -> k ++ kmlTrailStyles config sou locales r (routeUsed r) usedFeatures) [] (caminoRoutes camino) ++
    foldr (\r -> \k -> k ++ kmlTrailStyles config sou locales r (routeUsed r) usedFeatures) [] (caminoRoutes camino) ++
    foldr (\r -> \k -> k ++ kmlLegStyle config sou locales r ("style-leg-used" <> routeID r) True) [] (caminoRoutes camino) ++
    foldr (\r -> \k -> k ++ kmlLegStyle config sou locales r ("style-leg-unused-" <>routeID r) False) [] (caminoRoutes camino) ++
    kmlLegStyle config sou locales (caminoDefaultRoute camino) "style-leg-unused-default" True ++
    kmlLegStyle config sou locales (caminoDefaultRoute camino) "style-leg-unused-default" False
  where
    camino = preferenceCamino cprefs
    iconBase = appendRoot config $ assetPath $ fromJust $ getAsset "icons" config
    routeUsed r = S.member r (preferenceRoutes cprefs)

featureStyleMap'' feature = (fid, "style-feature-" <> fid):(concat $ map featureStyleMap'' (featureFeatures feature))
  where fid = featureID feature

featureStyleMap' :: Route -> [(Text, Text)]
featureStyleMap' route = concat $ map featureStyleMap'' (routeFeatures route)

featureStyleMap :: CaminoPreferences -> M.Map Text Text
featureStyleMap cprefs = let
    camino = preferenceCamino cprefs
    styles = concat $ map (\r -> featureStyleMap' r) (caminoRoutes camino)
  in
    M.fromList styles

pointKml :: LatLong -> [Node]
pointKml latlong = [xml|
<Point>
  <coordinates>#{pack $ show $ longitude latlong},#{pack $ show $ latitude latlong}}
|]

lineKml :: [LatLong] -> [Node]
lineKml latlongs = [xml|
<LineString>
  <coordinates>
    #{cstr}
|]
  where
    coords latlong = (pack $ show $ longitude latlong) <> "," <> (pack $ show $ latitude latlong)
    cstr = intercalate " " $ map coords latlongs -- Required because the template removes spaces

polygonKml :: [LatLong] -> [Node]
polygonKml latlongs = [xml|
  <Polygon>
    <outerBoundaryIs>
      <LinearRing>
        <coordinates>
          #{cstr}
|]
  where
    coords latlong = (pack $ show $ longitude latlong) <> "," <> (pack $ show $ latitude latlong)
    cstr = intercalate " " $ map coords latlongs -- Required because the template removes spaces

caminoLocationStyle :: CaminoPreferences -> S.Set Location -> S.Set Location -> Location -> Text
caminoLocationStyle _camino stops waypoints location
  | S.member location stops = "#" <> (toLower $ pack $ show $ locationType location) <> "-stop"
  | S.member location waypoints = "#" <> (toLower $ pack $ show $ locationType location) <> "-used"
  | otherwise = "#" <> (toLower $ pack $ show $ locationType location) <> "-unused"

_caminoLocationHtmlSnippet :: U.SystemOfUnits -> Config -> [Locale] -> Location -> [Node]
_caminoLocationHtmlSnippet sou config locales location = htmlToNodes sou config locales $ [ihamlet|
<div .container-fluid>
  <div .row>
    ^{locationLineIcons config location}
|]

caminoLocationHtmlForPlacemark :: U.SystemOfUnits -> Config -> [Locale] -> TravelPreferences -> CaminoPreferences -> Maybe Pilgrimage -> S.Set Location -> S.Set Location -> Location -> [Node]
caminoLocationHtmlForPlacemark sou config locales tprefs cprefs pilgrimage _stops waypoints location = htmlToNodes sou config locales $ [ihamlet|
  <div .container-fluid .p-3>
    <div .row>
      <h3>^{locationLineSimple config location}
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

caminoTextForSolution :: U.SystemOfUnits -> Config -> [Locale] -> TravelPreferences -> CaminoPreferences -> Maybe Solution -> Text
caminoTextForSolution _sou config locales _tprefs cprefs msolution =  intercalate "\n" $ catMaybes (heading ++ notes ++ caminoMd ++ solutionMd)
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


caminoLocationKml :: U.SystemOfUnits -> Config -> [Locale] -> TravelPreferences -> CaminoPreferences -> Maybe Pilgrimage -> S.Set Location -> S.Set Location -> Location -> [Node]
caminoLocationKml sou config locales preferences camino pilgrimage stops waypoints location = [xml|
    <Placemark id="#{locationID location}">
      <name>#{name}
      $maybe desc <- locationDescription location
        <Snippet>#{localiseText locales (descriptionSummary desc)}
      $nothing
        <Snippet>
      <description>^{caminoLocationHtmlForPlacemark sou config locales preferences camino pilgrimage stops waypoints location}
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
    used = if (S.member from' waypoints) && (S.member to' waypoints) then "used" else "unused"
    route = find (\r -> let
        allowed = routeLocationSet r
      in
        S.member from' allowed || S.member to' allowed
      )
      (caminoRoutes camino')
  in
    "#style-leg-" <> used <> maybe "default" routeID route

caminoLegKml :: (CaminoMsg -> Text) -> U.SystemOfUnits -> CaminoPreferences -> S.Set Location -> S.Set Location -> Leg -> [Node]
caminoLegKml renderer sou camino stops waypoints leg = [xml|
    <Placemark>
      <name>#{renderer (Txt (locationName (legFrom leg)))} - #{renderer (Txt (locationName (legTo leg)))} (#{renderer (DistanceFormatted sou (legDistance  leg))})
      <description>
      <styleUrl>#{caminoLegStyle camino stops waypoints leg}
      ^{lineKml positions}
  |]
  where
    positions = (locationPosition $ legFrom leg):(legWaypoints leg ++ [locationPosition $ legTo leg])

simpleGeometryKml :: SimpleGeometry -> [Node]
simpleGeometryKml (Point _bbox pt) = [xml|
^{pointKml pt}
|]
simpleGeometryKml (MultiPoint _bbox pts) = [xml|
<MultiGeometry>
  $forall pt <- pts
    ^{pointKml pt}
|]
simpleGeometryKml (LineString _bbox ln) = [xml|
^{lineKml ln}
|]
simpleGeometryKml (MultiLineString _bbox lns) = [xml|
<MultiGeometry>
 $forall ln <- lns
    ^{lineKml ln}
|]
simpleGeometryKml (Polygon _bbox pg) = [xml|
^{polygonKml pg}
|]
simpleGeometryKml (MultiPolygon _bbox pgs) = [xml|
<MultiGeometry>
  $forall pg <- pgs
    ^{polygonKml pg}
|]
simpleGeometryKml (GeometryCollection _bbox gs) = [xml|
<MultiGeometry>
  $forall g <- gs
    ^{simpleGeometryKml g}
|]

featureNameKml :: [Locale] -> F.SimpleFeature -> [Node]
featureNameKml locales feature = [xml|
$maybe n <- name
  <name>#{n}
|]
  where
    name = (localiseText locales <$> F.featureName feature) <|>
      (valueToText <$> (lookupAttribute "title" $ maybe emptyAttributes id (F.featureAttributes feature)))

attributesKml :: [Locale] -> Maybe Attributes -> [Node]
attributesKml _locales Nothing = [xml||]
attributesKml _locales (Just as) = [xml|
<ExtendedData>
  $forall (k, v) <- attributesToTextList as
    <Data name="#{k}">
      <value>#{v}
|]

simpleFeatureKml :: Config -> U.SystemOfUnits -> [Locale] -> Route -> Text -> (Text -> Text -> Text) -> F.SimpleFeature -> [Node]
simpleFeatureKml config sou locales route defaultStyle lookupStyle feature = [xml|
$if null (F.featureSubFeatures feature)
  <Placemark>
    ^{featureNameKml locales feature}
    ^{attributesKml locales mattrs''}
    $maybe g <- F.featureGeometry feature
       <styleUrl>##{styleID}
      ^{simpleGeometryKml g}
$else
  <Folder>
    ^{featureNameKml locales feature}
    ^{attributesKml locales mattrs''}
    $maybe g <- F.featureGeometry feature
      <styleUrl>##{styleID}
      ^{simpleGeometryKml g}
    $forall sf <- F.featureSubFeatures feature
      ^{simpleFeatureKml config sou locales route styleID lookupStyle sf}
|]
  where
    fid = F.featureID feature
    styleID = lookupStyle defaultStyle fid
    mdesc' = F.featureDescription feature
    mattrs' = F.featureAttributes feature
    mattrs'' = case (mdesc', mattrs') of
      (Nothing, Nothing) -> Nothing
      (Nothing, a@(Just _)) -> a
      (Just desc, ma) -> Just $ addAttribute "description" (StringV $ localisedDescriptionText locales desc) $ maybe emptyAttributes id ma

featureKml :: Config -> U.SystemOfUnits -> [Locale] -> Route -> M.Map Text F.SimpleFeature -> M.Map Text Text -> Feature -> [Node]
featureKml config sou locales route features styles feature = [xml|
<Folder>
  <name>#{localiseText locales (featureName feature)}
  $maybe desc <- featureDescription feature
    <Snippet>#{localiseText locales (descriptionSummary desc)}
    <description>#{localisedDescriptionText locales desc}
  $maybe sf <- M.lookup (featureID feature) features
    ^{simpleFeatureKml config sou locales route styleID lookupStyle sf}
|]
  where
    styleID = "style-route-" <> routeID route
    lookupStyle defaultStyle fid = M.findWithDefault defaultStyle fid styles

routeKml :: Config -> U.SystemOfUnits -> [Locale] -> Route -> M.Map Text F.SimpleFeature -> M.Map Text Text -> [Node]
routeKml config sou locales route features styles = [xml|
<Folder>
  <name>#{localiseText locales (routeName route)}
  $with desc <- routeDescription route
    <Snippet>#{localiseText locales (descriptionSummary desc)}
    <description>#{localisedDescriptionText locales desc}
  $forall f <- routeFeatures route
    ^{featureKml config sou locales route features styles f}
|]

-- | Use CDATA to rander some text.
--
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

-- | Create a KML document for a camino and optional plan
createCaminoDoc :: Config -> [Locale] -> TravelPreferences -> CaminoPreferences -> Maybe Solution -> M.Map Text F.SimpleFeature -> Document
createCaminoDoc config locales tprefs cprefs msolution features = Document (Prologue [] Nothing []) kml []
  where
    sou = preferenceUnits tprefs
    camino = preferenceCamino cprefs
    (BoundingBox sw ne) = caminoBbox camino
    mpilgrimage = maybe Nothing solutionPilgrimage msolution
    usedRoutes = maybe (S.fromList $ caminoRoutes camino) (preferenceRoutes . solutionCaminoPreferences) msolution
    stops = maybe S.empty (S.fromList . pilgrimageStops) mpilgrimage
    waypoints = maybe S.empty (S.fromList . pilgrimageWaypoints) mpilgrimage
    usedFeatures = caminoUsedFeatures camino usedRoutes waypoints
    styles = featureStyleMap cprefs
    distance = haversineDistance (locationPosition $ preferenceStart cprefs) (locationPosition $ preferenceFinish cprefs)
    locationsAppear = floor (distance / 1000.0 * 2.0) :: Int
    featuresAppear = floor (distance / 1000.0 * 10.0) :: Int
    renderer = renderCaminoMsgText config locales
    ns = M.fromList [ ("xmlns", "http://www.opengis.net/kml/2.2"), ("xmlns:gx", "http://www.google.com/kml/ext/2.2") ]
    kml = Element "kml" ns
      [xml|
<Document>
  <name>#{createCaminoTitle locales camino mpilgrimage}
  <description>#{caminoTextForSolution sou config locales tprefs cprefs msolution}
  ^{caminoStyles config sou locales cprefs usedFeatures}
  <Folder>
    <name>#{renderer LocationsLabel}
    <Region>
      <LatLonAltBox>
        <north>#{pack $ show $ latitude ne}
        <south>#{pack $ show $ latitude sw}
        <east>#{pack $ show $ longitude ne}
        <west>#{pack $ show $ longitude sw}
      <Lod>
        <minLodPixels>#{pack $ show locationsAppear}
    $forall location <- caminoLocations camino
      ^{caminoLocationKml sou config locales tprefs cprefs mpilgrimage stops waypoints location}
  <Folder>
    <name>#{renderer RoutesLabel}
    <Region>
      <LatLonAltBox>
        <north>#{pack $ show $ latitude ne}
        <south>#{pack $ show $ latitude sw}
        <east>#{pack $ show $ longitude ne}
        <west>#{pack $ show $ longitude sw}
      <Lod>
        <minLodPixels>#{pack $ show featuresAppear}
    $forall route <- caminoRoutes camino
      ^{routeKml config sou locales route features styles}
  <Folder>
    <name>#{renderer StageLabel}
    <Region>
      <LatLonAltBox>
        <north>#{pack $ show $ latitude ne}
        <south>#{pack $ show $ latitude sw}
        <east>#{pack $ show $ longitude ne}
        <west>#{pack $ show $ longitude sw}
      <Lod>
        <maxLodPixels>#{pack $ show featuresAppear}
    $forall leg <- caminoLegs camino
      ^{caminoLegKml renderer sou cprefs stops waypoints leg}
  <ExtendedData>
    $forall stmt <- metadataStatements (caminoMetadata camino)
      <Data name="#{statementLabel stmt}">
        <value>#{statementValue stmt}
    $maybe md <- maybe Nothing solutionMetadata msolution
      $forall stmt <- metadataStatements md
         <Data name="#{statementLabel stmt}">
           <value>#{statementValue stmt}

|]



