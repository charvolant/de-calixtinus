{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : Html
Description : Produce HTML descriptions map of caminos and trips
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Generate HTML descriptions of 
-}
module Camino.Display.Html where

import Camino.Camino
import Camino.Config (Config(..), AssetConfig(..), AssetType(..), MapConfig(..), getAssets, getAsset, getMap)
import Camino.Planner (Trip, Day, Metrics(..), tripStops, tripWaypoints)
import Camino.Preferences
import Camino.Display.Css (toCssColour)
import Camino.Display.Routes
import Text.Hamlet
import qualified Data.Text as T (intercalate, null, pack, Text)
import Formatting
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortBy)
import qualified Data.Text as T (Text, toUpper, take, filter)
import Data.Text.ICU.Char (Bool_(..), property)
import Data.Text.ICU.Normalize2 (NormalizationMode(..), normalize)
import Data.Maybe (isJust, fromJust)
import Text.Blaze.Html (preEscapedToHtml)
import Numeric
import Data.Char (ord)



-- | Canonicalise text, removing accents and diacritics
canonicalise :: T.Text -> T.Text
canonicalise t = T.filter (not . property Diacritic) (normalize NFD t)

partition' :: Eq t => (a -> t) -> [a] -> t -> [a] -> [(t, [a])]
partition' _classifier [] cl segment = [(cl, reverse segment)]
partition' classifier (s:source) cl segment = if cl' == cl then 
    partition' classifier source cl (s:segment) 
  else 
    (cl, reverse segment):(partition' classifier source cl' [s])
  where cl' = classifier s

-- | Split a sorted list into a partition, based on some sort of partition function
partition :: (Eq b) => (a -> b) -- ^ The classifier function, produces the element to split the list on
  -> [a] -- ^ The source list
  -> [(b, [a])] -- ^ A resulting list of category - elements that fit the category pairs
partition _classifier [] = []
partition classifier (s:source) = partition' classifier source (classifier s) [s]

formatPenance :: Penance -> HtmlUrl CaminoRoute
formatPenance Reject = [hamlet| <span .penance .rejected>Rejected |]
formatPenance (Penance p) = [hamlet| <span .penance>#{format (fixed 1) p}km |]

formatDistance :: (Real a) => Maybe a -> HtmlUrl CaminoRoute
formatDistance Nothing = [hamlet| <span .distance .rejected>Rejected |]
formatDistance (Just d) = [hamlet| <span .distance>#{format (fixed 1) d}km |]

formatTime :: (Real a) => Maybe a -> HtmlUrl CaminoRoute
formatTime Nothing = [hamlet| <span .time .rejected>Rejected |]
formatTime (Just t) = [hamlet| <span .time>#{format (fixed 1) t}hrs |]

formatHeight :: (Real a) => Maybe a -> HtmlUrl CaminoRoute
formatHeight Nothing = [hamlet| <span .height .rejected>Rejected |]
formatHeight (Just h) = [hamlet| <span .height>#{format (fixed 0) h}m |]

penanceSummary :: Preferences -> Camino -> Metrics -> HtmlUrl CaminoRoute
penanceSummary _preferences _camino metrics = [hamlet|
   <div .penance-summary .dropdown title="Penance Summary">
     <button .btn .btn-outline-primary .penance-summary .dropdown-toggle data-toggle="dropdown" aria-haspopup="true" aria-expanded="false" data-bs-toggle="dropdown">
       ^{formatPenance $ metricsPenance metrics}
     <div .dropdown-menu>
       <a .dropdown-item>Distance: ^{formatDistance $ metricsPerceivedDistance metrics}
       <a .dropdown-item>Accommodation: ^{formatPenance $ metricsAccommodation metrics}
       <a .dropdown-item>Day: ^{formatPenance $ metricsStop metrics}
       <a .dropdown-item>Distance Penalty: ^{formatPenance $ metricsDistanceAdjust metrics}
       <a .dropdown-item>Time Penality: ^{formatPenance $ metricsTimeAdjust metrics}
       <a .dropdown-item>Other: ^{formatPenance $ metricsMisc metrics}
   |]
    
metricsSummary :: Preferences -> Camino -> Metrics -> HtmlUrl CaminoRoute
metricsSummary _preferences _camino metrics = [hamlet|
    Distance: ^{formatDistance $ Just (metricsDistance metrics)}
    (feels like ^{formatDistance $ metricsPerceivedDistance metrics})
    over ^{formatTime $ metricsTime metrics}
    Ascent: ^{formatHeight $ Just (metricsAscent metrics)}
    Descent: ^{formatHeight $ Just (metricsDescent metrics)}
    Penance: ^{formatPenance $ metricsPenance metrics}
  |]

daySummary :: Preferences -> Camino -> Maybe Trip -> Day -> HtmlUrl CaminoRoute
daySummary _preferences _camino _trip day = [hamlet|
    <p>#{locationName $ start day} to #{locationName $ finish day} #{time}hrs #{distance}km (feels like #{perceivedDistance}km)
    <p>#{T.intercalate ", " (map locationName ((map legFrom $ path day) ++ [finish day]))}
  |]
  where
    time = maybe "*" (format (fixed 1)) (metricsTime $ score day)
    distance = format (fixed 1) (metricsDistance $ score day)
    perceivedDistance = maybe "*" (format (fixed 1)) (metricsPerceivedDistance $ score day)


tripSummary :: Preferences -> Camino -> Trip -> HtmlUrl CaminoRoute
tripSummary _preferences _camino trip = [hamlet|
    <h1>From #{locationName $ start trip} to #{locationName $ finish trip}
    <h2>Stages
    <ul>
    $forall day <- path trip
      <ol>#{locationName $ start day} - #{locationName $ finish day}
  |]

locationSummary :: Preferences -> Camino -> Location -> HtmlUrl CaminoRoute
locationSummary _preferences _camino location = [hamlet|
    $if not $ T.null services
      <p>Services: #{services}
    $if not $ T.null accommodation
      <p>Accomodation: #{accommodation}
  |]
  where 
    services = T.intercalate ", " (map (\s -> T.pack $ show s) (S.toList $ locationServices location))
    accommodation = T.intercalate ", " (map (\a -> T.pack $ show $ accommodationType a) (locationAccommodation location))

caminoLocationTypeIcon :: LocationType -> HtmlUrl CaminoRoute
caminoLocationTypeIcon Village = [hamlet| <span .location-type .ca-village title="Village"> |]
caminoLocationTypeIcon Town = [hamlet| <span .location-type .ca-town title="Town"> |]
caminoLocationTypeIcon City = [hamlet| <span .location-type .ca-city title="City"> |]
caminoLocationTypeIcon Bridge = [hamlet| <span .location-type .ca-bridge title="Bridge"> |]
caminoLocationTypeIcon Intersection = [hamlet| <span .location-type .ca-intersection title="Intersection"> |]
caminoLocationTypeIcon _ = [hamlet| <span .location-type .ca-poi title="Locality"> |]

caminoSleepingIcon :: Sleeping -> HtmlUrl CaminoRoute
caminoSleepingIcon Shared = [hamlet| <span .sleeping .ca-shared title="Shared"> |]
caminoSleepingIcon Single = [hamlet| <span.sleeping .ca-bed-single title="Single"> |]
caminoSleepingIcon Double = [hamlet| <span .sleeping ca-bed-double title="Double"> |]
caminoSleepingIcon DoubleWC = [hamlet| <span .sleeping .ca-bed-double-wc title="Double with WC"> |]
caminoSleepingIcon Triple = [hamlet| <span .sleeping .ca-bed-triple title="Triple"> |]
caminoSleepingIcon TripleWC = [hamlet| <span .sleeping .ca-bed-triple-wc title="Triple with WC"> |]
caminoSleepingIcon Quadruple = [hamlet| <span .sleeping .ca-bed-quadruple title="Quadruple"> |]
caminoSleepingIcon QuadrupleWC = [hamlet| <span .sleeping .bed-quadruple-wc title="Quadruple with WC"> |]
caminoSleepingIcon Mattress = [hamlet| <span .sleeping .ca-mattress title="Mattress"> |]
caminoSleepingIcon SleepingBag = [hamlet| <span .sleeping .ca-sleeping-bag title="SleepingBag"> |]

caminoAccommodationTypeIcon :: AccommodationType -> HtmlUrl CaminoRoute
caminoAccommodationTypeIcon MunicipalAlbergue = [hamlet| <span .accomodation .municipal-albergue .ca-albergue title="Municipal Albergue"> |]
caminoAccommodationTypeIcon PrivateAlbergue = [hamlet| <span .accomodation .private-albergue .ca-albergue title="Private Albergue"> |]
caminoAccommodationTypeIcon GuestHouse = [hamlet| <span .accomodation .guest-house .ca-guesthouse title="Guest House"> |]
caminoAccommodationTypeIcon House = [hamlet| <span .accomodation .house .ca-house title="House"> |]
caminoAccommodationTypeIcon Hotel = [hamlet| <span .accomodation .hotel .ca-hotel title="Hotel"> |]
caminoAccommodationTypeIcon Camping = [hamlet| <span .accomodation .camping .ca-tent title="Camping"> |]

caminoServiceIcon :: Service -> HtmlUrl CaminoRoute
caminoServiceIcon WiFi = [hamlet| <span .service .ca-wifi title="WiFi"> |]
caminoServiceIcon Restaurant = [hamlet| <span .service .ca-restaurant title="Restaurant"> |]
caminoServiceIcon Pharmacy = [hamlet| <span .service .ca-pharmacy title="Pharmacy"> |]
caminoServiceIcon Bank = [hamlet| <span .service .ca-bank title="Bank"> |]
caminoServiceIcon BicycleRepair = [hamlet| <span .service .ca-bicycle-repair title="Bicycle Repair"> |]
caminoServiceIcon Groceries = [hamlet| <span .service .ca-groceries title="Groceries"> |]
caminoServiceIcon Medical = [hamlet| <span .service .ca-medical title="Medical"> |]
caminoServiceIcon WashingMachine = [hamlet| <span .service .ca-washing-machine title="Washing Machine"> |]
caminoServiceIcon Dryer = [hamlet| <span .service .ca-dryer title="Dryer"> |]
caminoServiceIcon Handwash = [hamlet| <span .service .ca-handwash title="Hand Wash"> |]
caminoServiceIcon Kitchen = [hamlet| <span .service .ca-kitchen title="Kitchen"> |]
caminoServiceIcon Breakfast = [hamlet| <span .service .ca-breakfast title="Breakfast"> |]
caminoServiceIcon Dinner = [hamlet| <span .service .ca-dinner title="Dinner"> |]
caminoServiceIcon Lockers = [hamlet| <span .service .ca-lockers title="Lockers"> |]
caminoServiceIcon Accessible = [hamlet| <i .service .ca-accessible title="Accessible"> |]
caminoServiceIcon Stables = [hamlet| <span .service .ca-stables title="Stables"> |]
caminoServiceIcon Pets = [hamlet| <span .service .ca-pets title="Pets Allowed"> |]
caminoServiceIcon BicycleStorage = [hamlet| <span .service .ca-bicycle-storage title="Bicycle Storage"> |]
caminoServiceIcon CampSite = [hamlet| <span .service .ca-camping title="Camp Site"> |]
caminoServiceIcon Bedlinen = [hamlet| <span .service .ca-bedlinen title="Bed-linen"> |]
caminoServiceIcon Towels = [hamlet| <span .service .ca-towels title="Towels"> |]
caminoServiceIcon Pool = [hamlet| <span .service .ca-pool title="Pool"> |]
caminoServiceIcon Heating = [hamlet| <span .service .ca-heating title="Heating"> |]
caminoServiceIcon Prayer = [hamlet| <span .service .ca-prayer title="Prayer"> |]
caminoServiceIcon Train = [hamlet| <span .service .ca-train title="Train"> |]
caminoServiceIcon Bus = [hamlet| <span .service .ca-bus title="Bus"> |]

caminoAccommodationHtml :: Accommodation -> HtmlUrl CaminoRoute
caminoAccommodationHtml (GenericAccommodation _type) = [hamlet| |]
caminoAccommodationHtml (Accommodation name' type' services' sleeping') = [hamlet|
  <div .card .accomodation .m-1 .p-1>
    <h5>
      ^{caminoAccommodationTypeIcon type'}
      #{name'}
    <div .card-body>
      <div .container-fluid>
        <div .row>
          <div .col>
            $forall service <- services'
              ^{caminoServiceIcon service}
          <div .col>
            $forall sleeping <- sleeping'
              ^{caminoSleepingIcon sleeping}
 |]

locationLine :: Preferences -> Camino -> Location -> HtmlUrl CaminoRoute
locationLine _preferences _camino location = [hamlet|
    #{locationName location}
    <span .accomodation-types>
      $forall accomodation <- locationAccommodationTypes location
        ^{caminoAccommodationTypeIcon accomodation}
    <span .services>
      $forall service <- locationServices location
        ^{caminoServiceIcon service}
  |]

caminoLocationHtml :: Preferences -> Camino -> Maybe Trip -> S.Set Location -> S.Set Location -> Location -> HtmlUrl CaminoRoute
caminoLocationHtml _preferences camino _trip stops waypoints location = [hamlet|
  <div id="#{locationID location}" class="location-#{routeID route}" :isStop:.border-primary :isStop:.location-stop :isWaypoint:.border-primary-subtle :isWaypoint:.location-waypoint .location .card .p-2 .mx-1 .mt-4>
    <div .card-title>
      <h4>
        ^{caminoLocationTypeIcon (locationType location)}
        #{locationName location}
    <div .card-body>
      <div .container-fluid>
        <div .row .mb-4>
          <div .col .services>
            $forall service <- locationServices location
              ^{caminoServiceIcon service}
          <div .col .accomodation-types>
            $forall accomodation <- locationAccommodationTypes location
              ^{caminoAccommodationTypeIcon accomodation}
        $forall accomodation <- locationAccommodation location
          ^{caminoAccommodationHtml accomodation}
  |]
  where
    route = caminoRoute camino location
    isStop = S.member location stops
    isWaypoint = (not isStop) && (S.member location waypoints)
    
caminoLocationsHtml :: Preferences -> Camino -> Maybe Trip -> HtmlUrl CaminoRoute
caminoLocationsHtml preferences camino trip = [hamlet|
  <div .container-fluid>
    <div .row>
      <nav .navbar .navbar-expand-md>
        <ul .navbar-nav>
          $forall (initial, locs) <- locationPartition
            <li .dropdown .m-1>
              <a href="#" .dropdown-toggle data-toggle="dropdown" aria-haspopup="true" aria-expanded="false" data-bs-toggle="dropdown">
                #{initial}
              <ul .dropdown-menu>
                $forall loc <- locs
                  <a .dropdown-item href="##{locationID loc}">#{locationName loc}
    <div .row>
      <div .col>
        $forall loc <- locationsSorted
          <div .row>
            ^{caminoLocationHtml preferences camino trip stops waypoints loc}
  |]
  where
    locationOrder a b = compare (canonicalise $ locationName a) (canonicalise $ locationName b)
    locationsSorted = sortBy locationOrder (caminoLocations camino)
    locationPartition = partition (\l -> T.toUpper $ canonicalise $ T.take 1 $ locationName l) locationsSorted
    stops = maybe S.empty (S.fromList . tripStops) trip
    waypoints = maybe S.empty (S.fromList . tripWaypoints) trip
    
preferenceRangeHtml :: (Real a) => PreferenceRange a -> HtmlUrl CaminoRoute
preferenceRangeHtml range = [hamlet|
    <span .text-danger>#{format (fixed 1) (rangeMinimum range)} -
    <span>#{format (fixed 1)( rangeLower range)} -
    <span .text-success .fw-bolder>#{format (fixed 1) (rangeTarget range)} -
    <span>#{format (fixed 1) (rangeUpper range)} -
    <span .text-danger>#{format (fixed 1) (rangeMaximum range)}
    $maybe d <- rangeDerived range
      <p .text-body-tertiary .smaller>#{d}
  |]
  
preferencesHtml :: Preferences -> Camino -> Maybe Trip -> HtmlUrl CaminoRoute
preferencesHtml preferences _camino _trip = [hamlet|
  <div .container-fluid>
    <h2>Preferences</h2>
    <div .row>
      <div .col-3>Walking Estimate
      <div .col .offset-1>#{preferenceWalkingFunction preferences}
    <div .row>
      <div .col-3>Fitness
      <div .col .offset-1>#{show $ preferenceFitness preferences}
    <div .row>
      <div .col-3>Distance Preferences (km)
      <div .col .offset-1>^{preferenceRangeHtml $ preferenceDistance preferences}
    <div .row>
      <div .col-3>Perceived Distance Preferences (km)
      <div .col .offset-1>^{preferenceRangeHtml $ preferencePerceivedDistance preferences}
    <div .row>
      <div .col-3>Time Preferences (hours)
      <div .col. .offset-1>^{preferenceRangeHtml $ preferenceTime preferences}
    <div .row>
      <div .col-3>Accommodation Preferences
      $forall ak <- M.keys $ preferenceAccommodation preferences
        <div .row>
          <div .col-1 .offset-3>^{caminoAccommodationTypeIcon ak}
          <div .col>^{formatPenance $ findAcc preferences ak}
    <div .row>
      <div .col-3>Required Stops
      <div .col .offset-1>
        <ul>
        $forall l <- preferenceRequired preferences
          <li>
            <a href="##{locationID l}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName l}
    <div .row>
      <div .col-3>Excluded Stops
      <div .col .offset-1>
        <ul>
        $forall l <- preferenceExcluded preferences
          <li>
            <a href="##{locationID l}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName l}
  |]
  where findAcc prefs ak = (preferenceAccommodation prefs) M.! ak

caminoTripHtml :: Preferences -> Camino -> Trip -> HtmlUrl CaminoRoute
caminoTripHtml preferences camino trip = [hamlet|
  <div .container-fluid>
    <div .row .trip-summary>
      <div .col-11>
        <p>
          #{locationName $ start trip}
          $forall l <- map finish $ path trip
            \  - #{locationName l}
        <p>
          ^{metricsSummary preferences camino $ score trip}
      <div .col-1>
        <a .btn .btn-info href="@{KMLRoute}">KML
    $forall day <- path trip
      <div .card .day .p-1>
        <h4>
          <a href="@{LocationRoute (start day)}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName $ start day}
          \   -
          <a href="@{LocationRoute (finish day)}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName $ finish day}
          ^{formatDistance $ Just (metricsDistance $ score day)}
          ^{penanceSummary preferences camino $ score day}
        <div .card-body>
         <p>
            ^{metricsSummary preferences camino $ score day}
          <ul>
            <li>
              <div .location-summary>
                ^{locationLine preferences camino (start day)}
            $forall leg <- path day
              <li>
                <div .location-summary>
                  ^{locationLine preferences camino (legTo leg)}
                <div .distance-summary>
                  <span .leg-distance>#{format (fixed 1 % "km") (legDistance leg)}
                  <span .leg-ascent>#{format (fixed 0 % "m") (legAscent leg)}
                  <span .leg-descent>#{format (fixed 0 % "m") (legDescent leg)}
   |]

caminoMapHtml :: Preferences -> Camino -> Maybe Trip -> HtmlUrl CaminoRoute
caminoMapHtml _preferences _camino _trip = [hamlet|
  <div .container-fluid>
    <div .d-flex .justify-content-center>
      <div #map>
  |]

caminoLocationIcon :: Preferences -> Camino -> S.Set Location -> S.Set Location -> Location -> String
caminoLocationIcon _preferences _camino stops waypoints location
  | S.member location stops = "iconStop"
  | otherwise = "icon" ++ (show $ locationType location) ++ used
    where
      used = if S.member location waypoints then "Used" else "Unused"
      
caminoMapScript :: Preferences -> Camino -> Maybe Trip -> HtmlUrl CaminoRoute
caminoMapScript preferences camino trip = [hamlet|
  <script>
    var iconStop = L.icon({
      iconUrl: '@{IconRoute "location-stop.png"}',
      iconSize: [40, 40]
    });
    var iconVillageUsed = L.icon({
      iconUrl: '@{IconRoute "location-village-used.png"}',
      iconSize: [16, 16]
    });
    var iconVillageUnused = L.icon({
      iconUrl: '@{IconRoute "location-village-unused.png"}',
      iconSize: [16, 16]
    });
    var iconTownUsed = L.icon({
      iconUrl: '@{IconRoute "location-town-used.png"}',
      iconSize: [32, 20]
    });
    var iconTownUnused = L.icon({
      iconUrl: '@{IconRoute "location-town-unused.png"}',
      iconSize: [32, 20]
    });
    var iconCityUsed = L.icon({
      iconUrl: '@{IconRoute "location-city-used.png"}',
      iconSize: [32, 25]
    });
    var iconCityUnused = L.icon({
      iconUrl: '@{IconRoute "location-city-unused.png"}',
      iconSize: [32, 25]
    });
    var iconBridgeUsed = L.icon({
      iconUrl: '@{IconRoute "location-bridge-used.png"}',
      iconSize: [24, 9]
    });
    var iconBridgeUnused = L.icon({
      iconUrl: '@{IconRoute "location-bridge-unused.png"}',
      iconSize: [24, 9]
    });
    var iconIntersectionUsed = L.icon({
      iconUrl: '@{IconRoute "location-intersection-used.png"}',
      iconSize: [24, 22]
    });
    var iconIntersectionUnused = L.icon({
      iconUrl: '@{IconRoute "location-intersection-unused.png"}',
      iconSize: [24, 22]
    });
    var iconPoiUsed = L.icon({
      iconUrl: '@{IconRoute "location-poi-used.png"}',
      iconSize: [15, 20]
    });
    var iconPoiUnused = L.icon({
      iconUrl: '@{IconRoute "location-poi-unused.png"}',
      iconSize: [15, 20]
    });
    var map = L.map('map');
    map.fitBounds([ [#{latitude tl}, #{longitude tl}], [#{latitude br}, #{longitude br}] ]);
    L.tileLayer('@{MapTileRoute}', {
        maxZoom: 19,
        attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(map);
    var marker;
    var line;
    $forall location <- M.elems $ locations camino
      $maybe position <- locationPosition location
        marker = L.marker([#{latitude position}, #{longitude position}], { icon: #{caminoLocationIcon preferences camino stops waypoints location} } );
        marker.bindTooltip(`^{locationLine preferences camino location}`);
        marker.addTo(map);
    $forall leg <- legs camino
      $if isJust (locationPosition $ legFrom leg) && isJust (locationPosition $ legTo leg)
        line = L.polyline([
          [#{maybe 0.0 latitude (locationPosition $ legFrom leg)}, #{maybe 0.0 longitude (locationPosition $ legFrom leg)}],
          [#{maybe 0.0 latitude (locationPosition $ legTo leg)}, #{maybe 0.0 longitude (locationPosition $ legTo leg)}]
       ], {
          color: '#{toCssColour $ paletteColour $ routePalette $ caminoLegRoute camino leg}',
          weight: #{chooseWidth leg},
          opacity: #{chooseOpacity leg}
       });
       line.addTo(map);
  |]
  where
    (tl, br) = caminoBbox camino
    stops = maybe S.empty (S.fromList . tripStops) trip
    waypoints = maybe S.empty (S.fromList . tripWaypoints) trip
    chooseWidth leg | S.member (legFrom leg) waypoints && S.member (legTo leg) waypoints = 6 :: Int
      | otherwise = 3 :: Int
    chooseOpacity leg | S.member (legFrom leg) waypoints && S.member (legTo leg) waypoints = 1.0 :: Float
      | otherwise = 0.5 :: Float
  
caminoHtml :: Config -> Preferences -> Camino -> Maybe Trip -> HtmlUrl CaminoRoute
caminoHtml config preferences camino trip = [hamlet|
  $doctype 5
  <html>
    <head>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0, shrink-to-fit=no">
      <title>#{title}
      $forall c <- css
        <link rel="stylesheet" href="#{assetPath c}">
      <link rel="stylesheet" href="camino.css">
    <body>
      <header .p-2>
        <nav .navbar .navbar-expand-md>
          <div .container-fluid>
            <a .navbar-brand href="#">
              <img width="64" height="64" src="@{IconRoute "tile-64.png"}" alt="Camino Planner">
      <main .container-fluid .p-2>
        <h1>#{title}
        <div>
          <ul .nav .nav-tabs role="tablist">
            <li .nav-item role="presentation">
              <a .nav-link .active role="tab" data-bs-toggle="tab" href="#map-tab">Map
            <li .nav-item role="presentation">
              <a .nav-link role="tab" data-bs-toggle="tab" href="#plan-tab">Plan
            <li .nav-item role="presentation">
              <a #locations-toggle .nav-link role="tab" data-bs-toggle="tab" href="#locations-tab">Locations
            <li .nav-item role="presentation">
              <a .nav-link role="tab" data-bs-toggle="tab" href="#preferences-tab">Preferences
          <div .tab-content>
            <div .tab-pane .active role="tabpanel" id="map-tab">
              ^{caminoMapHtml preferences camino trip}
            $maybe t <- trip
              <div .tab-pane role="tabpanel" id="plan-tab">
                ^{caminoTripHtml preferences camino t}
            <div .tab-pane role="tabpanel" id="locations-tab">
              ^{caminoLocationsHtml preferences camino trip}
            <div .tab-pane role="tabpanel" id="preferences-tab">
              ^{preferencesHtml preferences camino trip}
      <footer .text-center .py-4 .px-2>
        <div .row .row-cols-1 .row-cols-lg-3>
          <div .col>
            <p .text-muted .my-2>
              <a href="https://github.com/charvolant/camino-planner">The Camino Planner
          <div .col>
            <p .text-muted .my-2>
          <div .col>
            <p .text-muted .my-2>Example only
      $forall s <- scripts
        <script src="#{assetPath s}">
      ^{caminoMapScript preferences camino trip}
  |]
  where
    title = maybe "Camino" (\t -> (locationName $ start t) <> " - " <> (locationName $ finish t)) trip
    css = getAssets Css config
    scripts = getAssets JavaScript config

