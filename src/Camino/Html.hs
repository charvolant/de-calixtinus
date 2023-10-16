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
module Camino.Html where

import Camino.Camino
import Camino.Config (Config, getConfigValue)
import Camino.Planner (Trip, Day, Metrics(..), tripStops, tripWaypoints)
import Camino.Preferences
import Data.Colour
import Data.Colour.SRGB (sRGB24show)
import Text.Hamlet
import Text.Cassius
import qualified Data.Text as T (intercalate, null, pack, Text)
import Formatting
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortBy)
import qualified Data.Text as T (toUpper, take)
import Data.Maybe (isJust)


-- | Create a CSS-able colour
toCssColour :: Colour Double -- ^ The colour to display
 -> String -- ^ A #rrggbb colour triple
toCssColour colour = sRGB24show colour

partition' :: Eq t => (a -> t) -> [a] -> t -> [a] -> [(t, [a])]
partition' _classifier [] cl segment = [(cl, reverse segment)]
partition' classifier (s:source) cl segment = if cl' == cl then 
    partition' classifier source cl (s:segment) 
  else 
    (cl, reverse segment):(partition' classifier source cl' [s])
  where cl' = classifier s

metricsSummary :: Bool -> Config -> Preferences -> Camino -> Metrics -> Html
metricsSummary long _config _preferences _camino metrics = [shamlet|
    #{distance'}
    (feels like #{perceived'})
    over #{time'}
    Ascent: #{ascent'}
    Descent: #{descent'}
    Penance: #{formatPenance $ metricsPenance metrics}
    $if long
      (#{perceived'}
      + #{formatPenance $ metricsAccomodation metrics}
      + #{formatPenance $ metricsStop metrics}
      + #{formatPenance $ metricsDistanceAdjust metrics}
      + #{formatPenance $ metricsTimeAdjust metrics}
      + #{formatPenance $ metricsMisc metrics})
  |]
 where
   formatPenance penance = case penance of
     Reject -> "Rejected"
     Penance p -> format (fixed 1 % "km") p
   distance' = format (fixed 1 % "km") (metricsDistance metrics)
   time' = maybe "*" (format (fixed 0 % "hrs")) (metricsTime metrics)
   perceived' = maybe "*" (format (fixed 1 % "km")) (metricsPerceivedDistance metrics)
   ascent' = format (fixed 0 % "m") (metricsAscent metrics)
   descent' = format (fixed 0 % "m") (metricsDescent metrics)

-- | Split a sorted list into a partition, based on some sort of partition function
partition :: (Eq b) => (a -> b) -- ^ The classifier function, produces the element to split the list on
  -> [a] -- ^ The source list
  -> [(b, [a])] -- ^ A resulting list of category - elements that fit the category pairs
partition _classifier [] = []
partition classifier (s:source) = partition' classifier source (classifier s) [s]

daySummary :: Preferences -> Camino -> Maybe Trip -> Day -> Html
daySummary _preferences _camino _trip day = [shamlet|
    <p>#{locationName $ start day} to #{locationName $ finish day} #{time}hrs #{distance}km (feels like #{perceivedDistance}km)
    <p>#{T.intercalate ", " (map locationName ((map legFrom $ path day) ++ [finish day]))}
  |]
  where
    time = maybe "*" (format (fixed 1)) (metricsTime $ score day)
    distance = format (fixed 1) (metricsDistance $ score day)
    perceivedDistance = maybe "*" (format (fixed 1)) (metricsPerceivedDistance $ score day)


tripSummary :: Preferences -> Camino -> Trip -> Html
tripSummary _preferences _camino trip = [shamlet|
    <h1>From #{locationName $ start trip} to #{locationName $ finish trip}
    <h2>Stages
    <ul>
    $forall day <- path trip
      <ol>#{locationName $ start day} - #{locationName $ finish day}
  |]

locationSummary :: Preferences -> Camino -> Location -> Html
locationSummary _preferences _camino location = [shamlet|
    $if not $ T.null services
      <p>Services: #{services}
    $if not $ T.null accommodation
      <p>Accomodation: #{accommodation}
  |]
  where 
    services = T.intercalate ", " (map (\s -> T.pack $ show s) (locationServices location))
    accommodation = T.intercalate ", " (map (\a -> T.pack $ show $ accommodationType a) (locationAccommodation location))


caminoSleepingIcon :: Config -> Sleeping -> Html
caminoSleepingIcon _ Shared = [shamlet| <i .sleeping .fa-solid .fa-bed title="Shared"> |]
caminoSleepingIcon _ Single = [shamlet| <i .sleeping .fa-solid .fa-bed title="Single"> |]
caminoSleepingIcon _ Double = [shamlet|
  <span title="Double">
    <i .sleeping .fa-solid .fa-bed>
    <sup>2
  |]
caminoSleepingIcon _ DoubleWC = [shamlet|
  <span .sleeping title="Double with WC">
    <i .fa-solid .fa-bed>
    <sup>2+WC
  |]
caminoSleepingIcon _ Triple = [shamlet|
  <span title="Triple">
    <i .sleeping .fa-solid .fa-bed>
    <sup>3
  |]
caminoSleepingIcon _ TripleWC = [shamlet|
  <span .sleeping title="Triple with WC">
    <i .fa-solid .fa-bed>
    <sup>3+WC
  |]
caminoSleepingIcon _ Quadruple = [shamlet|
  <span title="Quadruple">
    <i .sleeping .fa-solid .fa-bed>
    <sup>4
  |]
caminoSleepingIcon _ QuadrupleWC = [shamlet|
  <span .sleeping title="Quadruple with WC">
    <i .fa-solid .fa-bed>
    <sup>4+WC
  |]
caminoSleepingIcon _ Matress = [shamlet| <i .sleeping .fa-solid .fa-matress-pillow title="Matress"> |]
caminoSleepingIcon _ SleepingBag = [shamlet| <i .sleeping .fa-solid .fa-tarp title="SleepingBag"> |]

caminoAccommodationTypeIcon :: Config -> AccommodationType -> Html
caminoAccommodationTypeIcon _ MunicipalAlbergue = [shamlet| <i  .accomodation .municipal-albergue .fa-solid .fa-house title="Municipal Albergue"> |]
caminoAccommodationTypeIcon _ PrivateAlbergue = [shamlet| <i  .accomodation .private-albergue .fa-solid .fa-house-chimney title="Private Albergue"> |]
caminoAccommodationTypeIcon _ GuestHouse = [shamlet| <i  .accomodation .guest-house .fa-solid .fa-house-chimney-window title="Guest House"> |]
caminoAccommodationTypeIcon _ House = [shamlet| <i  .accomodation .house .fa-solid .fa-house-chimney-window title="House"> |]
caminoAccommodationTypeIcon _ Hotel = [shamlet| <i  .accomodation .hotel .fa-solid .fa-hotel title="Hotel"> |]
caminoAccommodationTypeIcon _ Camping = [shamlet| <i  .accomodation .camping .fa-solid .fa-tent title="Camping"> |]

caminoServiceIcon :: Config -> Service -> Html
caminoServiceIcon _ WiFi = [shamlet| <i  .service .fa-solid .fa-wifi title="WiFi"> |]
caminoServiceIcon _ Restaurant = [shamlet| <i  .service .fa-solid .fa-utensils title="Restaurant"> |]
caminoServiceIcon _ Pharmacy = [shamlet| <i  .service .fa-solid .fa-mortar-pestle title="Pharmacy"> |]
caminoServiceIcon _ Bank = [shamlet| <i  .service .fa-solid .fa-credit-card title="Bank"> |]
caminoServiceIcon _ BicycleRepair = [shamlet| <i  .service .fa-solid .fa-bicycle title="Bicycle Repair"> |]
caminoServiceIcon _ Groceries = [shamlet| <i  .service .fa-solid .fa-cart-shopping title="Groceries"> |]
caminoServiceIcon _ Medical = [shamlet| <i  .service .fa-solid .fa-house-medical title="Medical"> |]
caminoServiceIcon _ WashingMachine = [shamlet| <i  .service .fa-solid .fa-jug-detergent title="Washing Machine"> |]
caminoServiceIcon _ Dryer = [shamlet| <i  .service .fa-solid .fa-fan title="Dryer"> |]
caminoServiceIcon _ Handwash = [shamlet| <i  .service .fa-solid .fa-soap title="Hand Wash"> |]
caminoServiceIcon _ Kitchen = [shamlet| <i  .service .fa-solid .fa-kitchen-set title="Kitchen"> |]
caminoServiceIcon _ Breakfast = [shamlet| <i  .service .fa-solid .fa-mug-saucer title="Breakfast"> |]
caminoServiceIcon _ Dinner = [shamlet| <i  .service .fa-solid .fa-bowl-food title="Dinner"> |]
caminoServiceIcon _ Lockers = [shamlet| <i  .service .fa-solid .fa-toilet-portable title="Lockers"> |]
caminoServiceIcon _ Accessible = [shamlet| <i .service .fa-solid .fa-accessible-icon title="Accessible"> |]
caminoServiceIcon _ Stables = [shamlet| <i  .service .fa-solid .fa-horse-head title="Stables"> |]
caminoServiceIcon _ Pets = [shamlet| <i  .service .fa-solid .fa-dog title="Pets Allowed"> |]
caminoServiceIcon _ BicycleStorage = [shamlet| <i  .service .fa-solid .fa-bicycle title="Bicycle Storage"> |]
caminoServiceIcon _ CampSite = [shamlet| <i  .service .fa-solid .fa-campground title="Camp Site"> |]
caminoServiceIcon _ Bedlinen = [shamlet| <i  .service .fa-solid .fa-rug title="Bed-linen"> |]
caminoServiceIcon _ Towels = [shamlet| <i  .service .fa-solid .fa-scroll title="Towels"> |]
caminoServiceIcon _ Pool = [shamlet| <i  .service .fa-solid .fa-person-swimmingl title="Pool"> |]
caminoServiceIcon _ Heating = [shamlet| <i  .service .fa-solid .fa-temperature-high title="Heating"> |]
caminoServiceIcon _ Prayer = [shamlet| <i  .service .fa-solid .fa-hands-praying title="Prayer"> |]
caminoServiceIcon _ Train = [shamlet| <i  .service .fa-solid .fa-train title="Train"> |]
caminoServiceIcon _ Bus = [shamlet| <i  .service .fa-solid .fa-bus-simple title="Bus"> |]

caminoAccommodationHtml :: Config -> Accommodation -> Html
caminoAccommodationHtml _ (GenericAccommodation _type) = [shamlet| |]
caminoAccommodationHtml config (Accommodation name' type' services' sleeping') = [shamlet|
  <div .card .accomodation>
    <h5>
      #{name'}
      ^{caminoAccommodationTypeIcon config type'}
    <div .card-body>
      <div .row>
        <div .col>
          $forall service <- services'
            ^{caminoServiceIcon config service}
        <div .col>
          $forall sleeping <- sleeping'
            ^{caminoSleepingIcon config sleeping}
 |]

locationLine :: Config -> Preferences -> Camino -> Location -> Html
locationLine config _preferences _camino location = [shamlet|
    #{locationName location}
    <span .accomodation-types>
      $forall accomodation <- locationAccommodationTypes location
        ^{caminoAccommodationTypeIcon config accomodation}
    <span .services>
      $forall service <- locationServices location
        ^{caminoServiceIcon config service}
  |]

caminoLocationHtml :: Config -> Preferences -> Camino -> Maybe Trip -> S.Set Location -> S.Set Location -> Location -> Html
caminoLocationHtml config _preferences camino _trip stops waypoints location = [shamlet|
  <div id="#{locationID location}" class="location-#{routeID route}" :isStop:.border-primary :isStop:.location-stop :isWaypoint:.border-primary-subtle :isWaypoint:.location-waypoint .location .card .m-1>
    <div .row .card-title>
      <div .col>
        <h4>
          #{locationName location}
      <div .col .accomodation-types>
        $forall accomodation <- locationAccommodationTypes location
          ^{caminoAccommodationTypeIcon config accomodation}
      <div .col .services>
        $forall service <- locationServices location
          ^{caminoServiceIcon config service}
    <div .card-body>
      $forall accomodation <- locationAccommodation location
        ^{caminoAccommodationHtml config accomodation}
  |]
  where
    route = caminoRoute camino location
    isStop = S.member location stops
    isWaypoint = (not isStop) && (S.member location waypoints)
    
caminoLocationsHtml :: Config -> Preferences -> Camino -> Maybe Trip -> Html
caminoLocationsHtml config preferences camino trip = [shamlet|
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
            ^{caminoLocationHtml config preferences camino trip stops waypoints loc}
  |]
  where
    locationOrder a b = compare (locationName a) (locationName b)
    locationsSorted = sortBy locationOrder (caminoLocations camino)
    locationPartition = partition (\l -> T.toUpper $ T.take 1 $ locationName l) locationsSorted
    stops = maybe S.empty (S.fromList . tripStops) trip
    waypoints = maybe S.empty (S.fromList . tripWaypoints) trip
    
preferenceRangeHtml :: (Real a) => PreferenceRange a -> Html
preferenceRangeHtml range = [shamlet|
    <span .text-danger>#{format (fixed 1) (rangeMinimum range)} -
    <span>#{format (fixed 1)( rangeLower range)} -
    <span .text-success .fw-bolder>#{format (fixed 1) (rangeTarget range)} -
    <span>#{format (fixed 1) (rangeUpper range)} -
    <span .text-danger>#{format (fixed 1) (rangeMaximum range)}
    $maybe d <- rangeDerived range
      <span .text-body-tertiary>#{d}
  |]
preferencesHtml :: Preferences -> Camino -> Maybe Trip -> Html
preferencesHtml preferences _camino _trip = [shamlet|
  <div .container-fluid>
    <h2>Preferences</h2>
    <div .row>
      <div .col>Walking Estimate
      <div.col>#{preferenceWalkingFunction preferences}
    <div .row>
      <div .col>Fitness
      <div .col>#{show $ preferenceFitness preferences}
    <div .row>
      <div .col>Distance Preferences (km)
      <div .col>^{preferenceRangeHtml $ preferenceDistance preferences}
    <div .row>
      <div .col>Perceived Distance Preferences (km)
      <div .col>^{preferenceRangeHtml $ preferencePerceivedDistance preferences}
    <div .row>
      <div .col>Time Preferences (hours)
      <div .col>^{preferenceRangeHtml $ preferenceTime preferences}
    <div .row>
      <div .col>Accomondation Preferences
      <div .col>
        $forall ak <- M.keys $ preferenceAccommodation preferences
          <div .row>
            <div .col>#{show ak}
            <div .col>#{show $ findAcc preferences ak}
    <div .row>
      <div .col>Required Stops
      <div .col>
        <ul>
        $forall l <- preferenceRequired preferences
          <li>
            <a href="##{locationID l}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName l}
    <div .row>
      <div .col>Excluded Stops
      <div .col>
        <ul>
        $forall l <- preferenceExcluded preferences
          <li>
            <a href="##{locationID l}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName l}
  |]
  where findAcc prefs ak = (preferenceAccommodation prefs) M.! ak

caminoTripHtml :: Config -> Preferences -> Camino -> Trip -> Html
caminoTripHtml config preferences camino trip = [shamlet|
  <div .container-fluid>
    <div .row .trip-summary>
      <div .col>
        <p>
          #{locationName $ start trip}
          $forall l <- map finish $ path trip
            \  - #{locationName l}
        <p>
          ^{metricsSummary False config preferences camino $ score trip}
    $forall day <- path trip
      <div .card>
        <h4>
          <a href="##{locationID $ start day}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName $ start day}
          \   -
          <a href="##{locationID $ finish day}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName $ finish day}
        <div .card-body>
          <ul>
            <li>
              <div .location-summary>
                ^{locationLine config preferences camino (start day)}
            $forall leg <- path day
              <li>
                <div .location-summary>
                  ^{locationLine config preferences camino (legTo leg)}
                <div .distance-summary>
                  <span .leg-distance>#{format (fixed 1 % "km") (legDistance leg)}
                  <span .leg-ascent>#{format (fixed 0 % "m") (legAscent leg)}
                  <span .leg-descent>#{format (fixed 0 % "m") (legDescent leg)}
          <p>
            ^{metricsSummary True config preferences camino $ score day}
  |]

caminoMapHtml :: Config -> Preferences -> Camino -> Maybe Trip -> Html
caminoMapHtml _config _preferences _camino _trip = [shamlet|
  <div .container-fluid>
    <div .d-flex .justify-content-center>
      <div #map>
  |]

caminoMapScript :: Config -> Preferences -> Camino -> Maybe Trip -> Html
caminoMapScript config preferences camino trip = [shamlet|
  <script>
    var stopUsedIcon = L.icon({
      iconUrl: '#{iconBase}/location-stop.png',
      iconSize: [32, 32]
    });
    var waypointUsedIcon = L.icon({
      iconUrl: '#{iconBase}/location-waypoint-used.png',
      iconSize: [24, 24]
    });
    var waypointUnusedIcon = L.icon({
      iconUrl: '#{iconBase}/location-waypoint-unused.png',
      iconSize: [24, 24]
    });
    var map = L.map('map');
    map.fitBounds([ [#{latitude tl}, #{longitude tl}], [#{latitude br}, #{longitude br}] ]);
    L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(map);
    var marker;
    var line;
    $forall location <- M.elems $ locations camino
      $maybe position <- locationPosition location
        marker = L.marker([#{latitude position}, #{longitude position}], { icon: #{chooseIcon location} } );
        marker.bindTooltip(`#{locationLine config preferences camino location}`);
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
    iconBase = getConfigValue "web.icons.base" "icons" config :: String
    stops = maybe S.empty (S.fromList . tripStops) trip
    waypoints = maybe S.empty (S.fromList . tripWaypoints) trip
    chooseWidth leg | S.member (legFrom leg) waypoints && S.member (legTo leg) waypoints = 6 :: Int
      | otherwise = 3 :: Int
    chooseOpacity leg | S.member (legFrom leg) waypoints && S.member (legTo leg) waypoints = 1.0 :: Float
      | otherwise = 0.5 :: Float
    chooseIcon location | S.member location stops = "stopUsedIcon" :: String
      | S.member location waypoints = "waypointUsedIcon" :: String
      | otherwise = "waypointUnusedIcon" :: String

paletteCss :: Config -> String -> Palette -> Css
paletteCss _config ident pal = [cassius|
.#{ident}
  h1
    color: #{toCssColour $ paletteColour pal}
  h2
    color: #{toCssColour $ paletteColour pal}
  h3
    color: #{toCssColour $ paletteColour pal}
  h4
    color: #{toCssColour $ paletteColour pal}

  |] undefined

caminoBaseCss :: Config -> Camino -> Css
caminoBaseCss _config _camino = [cassius|
#map
  width: 80%
  height: 800px
  padding: 1em
.service
  color: #1964c0
.accomodation
  color: #1964c0
.accomodation.municipal-albergue
  color: #f9b34a
.distance-summary
  display: inline-block
  margin-left: 1em
  .leg-distance
    margin-left: 1em
  .leg-ascent
    vertical-align: super
    font-size: smaller
  .leg-ascent::before
    font-family: "Font Awesome 6 Free"
    font-weight: 900
    content: "\f106"
  .leg-descent
    font-size: smaller
    veritcal-align: sub
  .leg-descent::before
    font-family: "Font Awesome 6 Free"
    font-weight: 900
    content: "\f107"
.location 
  .card-title 
    h4
      display: inline-block
    .accomodation-types
      display: inline-block
      margin-left: 2em
    .services
      display: inline-block
      margin-left: 2em
.location-summary
  display: inline-block
  .accomodation-types
    font-size: smaller
    display: inline-block
    margin-left: 1ex
  .services
    font-size: smaller
    display: inline-block
    margin-left: 0,5ex
.location-waypoint
  .card-title
    h4
      font-weight: bolder
.location-stop
  .card-title
    h4
      font-weight: bolder
    h4::after
      margin-left: 1ex
      color: #f9b34a
      font-family: "Font Awesome 6 Free"
      font-weight: 900
      font-size: smaller
      content: "\f236"
  |] undefined

caminoCss :: Config -> Camino -> [Css]
caminoCss config camino = base':default':routes'
  where
    base' = caminoBaseCss config camino
    default' = paletteCss config "location-default" (palette camino)
    routes' = map (\r -> paletteCss config ("location-" ++ (routeID r)) (routePalette r)) (routes camino)
  
caminoHtml :: Config -> Preferences -> Camino -> Maybe Trip -> Html
caminoHtml config preferences camino trip = [shamlet|
  $doctype 5
  <html>
    <head>
      <meta charset="utf-8">
      <meta name="viewport" content="width=device-width, initial-scale=1.0, shrink-to-fit=no">
      <title>#{title}
      <link rel="stylesheet" href="#{bootstrapCss}">
      <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css" integrity="sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY=" crossorigin="">
      <link rel="stylesheet" href="camino.css">
    <body>
      <header>
        <nav .navbar .navbar-expand-md .bg-body>
          <div .container-fluid>
            <a .navbar-brand href="#">
              <img width="64" height="64" src="#{iconBase}/tile-64.png" alt="Camino Planner">
      <main>
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
              ^{caminoMapHtml config preferences camino trip}
            $maybe t <- trip
              <div .tab-pane role="tabpanel" id="plan-tab">
                ^{caminoTripHtml config preferences camino t}
            <div .tab-pane role="tabpanel" id="locations-tab">
              ^{caminoLocationsHtml config preferences camino trip}
            <div .tab-pane role="tabpanel" id="preferences-tab">
              ^{preferencesHtml preferences camino trip}
      <footer .text-ceter .py-4>
        <div .row .row-cols-1 .row-cols-lg-3>
          <div .col>
            <p .text-muted .my-2>Copyright&nbsp;© 2023 Brand
          <div .col>
            <p .text-muted .my-2>...
          <div .col>
            <p .text-muted .my-2>...
      <script src="#{jQueryJs}">
      <script src="#{bootstrapJs}">
      <script src="#{fontAwsomeKit}" crossorigin="anonymous">
      <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js" integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=" crossorigin="">
      ^{caminoMapScript config preferences camino trip}
  |]
  where
    title = maybe "Camino" (\t -> (locationName $ start t) <> " - " <> (locationName $ finish t)) trip
    iconBase = getConfigValue "web.icons.base" "icons" config :: String
    fontAwsomeKit = getConfigValue "web.icons.kit" "kit.js" config :: String
    jQueryJs = getConfigValue "web.assets.jQueryJs" "jquery.js" config :: T.Text
    bootstrapCss = getConfigValue "web.assets.bootstrapCss" "bootstrap.css"  config :: T.Text
    bootstrapJs = getConfigValue "web.assets.bootstrapJs" "bootstrap.js" config :: T.Text