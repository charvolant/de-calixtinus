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
import qualified Data.Text as T (intercalate, null, pack, Text, Text)
import Formatting
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortBy)
import qualified Data.Text as T (Text, toUpper, take, filter)
import Data.Text.ICU.Char (Bool_(..), property)
import Data.Text.ICU.Normalize (NormalizationMode(..), normalize)
import Data.Maybe (isJust)
import Text.Blaze.Html (preEscapedToHtml)
import Numeric
import Data.Char (ord)



-- | Canonicalise text, removing accents and diacritics
canonicalise :: T.Text -> T.Text
canonicalise t = T.filter (not . property Diacritic) (normalize NFD t)

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

-- | Split a sorted list into a partition, based on some sort of partition function
partition :: (Eq b) => (a -> b) -- ^ The classifier function, produces the element to split the list on
  -> [a] -- ^ The source list
  -> [(b, [a])] -- ^ A resulting list of category - elements that fit the category pairs
partition _classifier [] = []
partition classifier (s:source) = partition' classifier source (classifier s) [s]

formatPenance :: Penance -> Html
formatPenance Reject = [shamlet| <span .penance .rejected>Rejected |]
formatPenance (Penance p) = [shamlet| <span .penance>#{format (fixed 1) p}km |]

formatDistance :: (Real a) => Maybe a -> Html
formatDistance Nothing = [shamlet| <span .distance .rejected>Rejected |]
formatDistance (Just d) = [shamlet| <span .distance>#{format (fixed 1) d}km |]

formatTime :: (Real a) => Maybe a -> Html
formatTime Nothing = [shamlet| <span .time .rejected>Rejected |]
formatTime (Just t) = [shamlet| <span .time>#{format (fixed 1) t}hrs |]

formatHeight :: (Real a) => Maybe a -> Html
formatHeight Nothing = [shamlet| <span .height .rejected>Rejected |]
formatHeight (Just h) = [shamlet| <span .height>#{format (fixed 0) h}m |]

penanceSummary :: Config -> Preferences -> Camino -> Metrics -> Html
penanceSummary _config _preferences _camino metrics = [shamlet|
   <div .penance-summary .dropdown title="Penance Summary">
     <button .btn .btn-outline-primary .penance-summary .dropdown-toggle data-toggle="dropdown" aria-haspopup="true" aria-expanded="false" data-bs-toggle="dropdown">
       ^{formatPenance $ metricsPenance metrics}
     <div .dropdown-menu>
       <a .dropdown-item>Distance: ^{formatDistance $ metricsPerceivedDistance metrics}
       <a .dropdown-item>Accomodation: ^{formatPenance $ metricsAccomodation metrics}
       <a .dropdown-item>Day: ^{formatPenance $ metricsStop metrics}
       <a .dropdown-item>Distance Penalty: ^{formatPenance $ metricsDistanceAdjust metrics}
       <a .dropdown-item>Time Penality: ^{formatPenance $ metricsTimeAdjust metrics}
       <a .dropdown-item>Other: ^{formatPenance $ metricsMisc metrics}
   |]
    
metricsSummary :: Config -> Preferences -> Camino -> Metrics -> Html
metricsSummary _config _preferences _camino metrics = [shamlet|
    Distance: ^{formatDistance $ Just (metricsDistance metrics)}
    (feels like ^{formatDistance $ metricsPerceivedDistance metrics})
    over ^{formatTime $ metricsTime metrics}
    Ascent: ^{formatHeight $ Just (metricsAscent metrics)}
    Descent: ^{formatHeight $ Just (metricsDescent metrics)}
    Penance: ^{formatPenance $ metricsPenance metrics}
  |]

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
    services = T.intercalate ", " (map (\s -> T.pack $ show s) (S.toList $ locationServices location))
    accommodation = T.intercalate ", " (map (\a -> T.pack $ show $ accommodationType a) (locationAccommodation location))


caminoSleepingIcon :: Config -> Sleeping -> Html
caminoSleepingIcon _ Shared = [shamlet| <span .sleeping .ca-shared title="Shared"> |]
caminoSleepingIcon _ Single = [shamlet| <span.sleeping .ca-bed-single title="Single"> |]
caminoSleepingIcon _ Double = [shamlet| <span .sleeping ca-bed-double title="Double"> |]
caminoSleepingIcon _ DoubleWC = [shamlet| <span .sleeping .ca-bed-double-wc title="Double with WC"> |]
caminoSleepingIcon _ Triple = [shamlet| <span .sleeping .ca-bed-triple title="Triple"> |]
caminoSleepingIcon _ TripleWC = [shamlet| <span .sleeping .ca-bed-triple-wc title="Triple with WC"> |]
caminoSleepingIcon _ Quadruple = [shamlet| <span .sleeping .ca-bed-quadruple title="Quadruple"> |]
caminoSleepingIcon _ QuadrupleWC = [shamlet| <span .sleeping .bed-quadruple-wc title="Quadruple with WC"> |]
caminoSleepingIcon _ Matress = [shamlet| <span .sleeping .ca-matress title="Matress"> |]
caminoSleepingIcon _ SleepingBag = [shamlet| <span .sleeping .ca-sleeping-bag title="SleepingBag"> |]

caminoAccommodationTypeIcon :: Config -> AccommodationType -> Html
caminoAccommodationTypeIcon _ MunicipalAlbergue = [shamlet| <span .accomodation .municipal-albergue .ca-albergue title="Municipal Albergue"> |]
caminoAccommodationTypeIcon _ PrivateAlbergue = [shamlet| <span .accomodation .private-albergue .ca-albergue title="Private Albergue"> |]
caminoAccommodationTypeIcon _ GuestHouse = [shamlet| <span .accomodation .guest-house .ca-guesthouse title="Guest House"> |]
caminoAccommodationTypeIcon _ House = [shamlet| <span .accomodation .house .ca-house title="House"> |]
caminoAccommodationTypeIcon _ Hotel = [shamlet| <span .accomodation .hotel .ca-hotel title="Hotel"> |]
caminoAccommodationTypeIcon _ Camping = [shamlet| <span .accomodation .camping .ca-tent title="Camping"> |]

caminoServiceIcon :: Config -> Service -> Html
caminoServiceIcon _ WiFi = [shamlet| <span .service .ca-wifi title="WiFi"> |]
caminoServiceIcon _ Restaurant = [shamlet| <span .service .ca-restaurant title="Restaurant"> |]
caminoServiceIcon _ Pharmacy = [shamlet| <span .service .ca-pharmacy title="Pharmacy"> |]
caminoServiceIcon _ Bank = [shamlet| <span .service .ca-bank title="Bank"> |]
caminoServiceIcon _ BicycleRepair = [shamlet| <span .service .ca-bicycle-repair title="Bicycle Repair"> |]
caminoServiceIcon _ Groceries = [shamlet| <span .service .ca-groceries title="Groceries"> |]
caminoServiceIcon _ Medical = [shamlet| <span .service .ca-medical title="Medical"> |]
caminoServiceIcon _ WashingMachine = [shamlet| <span .service .ca-washing-machine title="Washing Machine"> |]
caminoServiceIcon _ Dryer = [shamlet| <span .service .ca-dryer title="Dryer"> |]
caminoServiceIcon _ Handwash = [shamlet| <span .service .ca-handwash title="Hand Wash"> |]
caminoServiceIcon _ Kitchen = [shamlet| <span .service .ca-kitchen title="Kitchen"> |]
caminoServiceIcon _ Breakfast = [shamlet| <span .service .ca-breakfast title="Breakfast"> |]
caminoServiceIcon _ Dinner = [shamlet| <span .service .ca-dinner title="Dinner"> |]
caminoServiceIcon _ Lockers = [shamlet| <span .service .ca-lockers title="Lockers"> |]
caminoServiceIcon _ Accessible = [shamlet| <i .service .ca-accessible title="Accessible"> |]
caminoServiceIcon _ Stables = [shamlet| <span .service .ca-stables title="Stables"> |]
caminoServiceIcon _ Pets = [shamlet| <span .service .ca-pets title="Pets Allowed"> |]
caminoServiceIcon _ BicycleStorage = [shamlet| <span .service .ca-bicycle-storage title="Bicycle Storage"> |]
caminoServiceIcon _ CampSite = [shamlet| <span .service .ca-camping title="Camp Site"> |]
caminoServiceIcon _ Bedlinen = [shamlet| <span .service .ca-bedlinen title="Bed-linen"> |]
caminoServiceIcon _ Towels = [shamlet| <span .service .ca-towels title="Towels"> |]
caminoServiceIcon _ Pool = [shamlet| <span .service .ca-pool title="Pool"> |]
caminoServiceIcon _ Heating = [shamlet| <span .service .ca-heating title="Heating"> |]
caminoServiceIcon _ Prayer = [shamlet| <span .service .ca-prayer title="Prayer"> |]
caminoServiceIcon _ Train = [shamlet| <span .service .ca-train title="Train"> |]
caminoServiceIcon _ Bus = [shamlet| <span .service .ca-bus title="Bus"> |]

caminoAccommodationHtml :: Config -> Accommodation -> Html
caminoAccommodationHtml _ (GenericAccommodation _type) = [shamlet| |]
caminoAccommodationHtml config (Accommodation name' type' services' sleeping') = [shamlet|
  <div .card .accomodation .p-1>
    <h5>
      ^{caminoAccommodationTypeIcon config type'}
      #{name'}
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
  <div id="#{locationID location}" class="location-#{routeID route}" :isStop:.border-primary :isStop:.location-stop :isWaypoint:.border-primary-subtle :isWaypoint:.location-waypoint .location .card .p-1 .m-1>
    <div .row .card-title>
      <div .col>
        <h4>
          #{locationName location}
    <div .card-body>
      <div .row>
        <div .col .services>
          $forall service <- locationServices location
            ^{caminoServiceIcon config service}
      <div .row .mb-5>
        <div .col .accomodation-types>
          $forall accomodation <- locationAccommodationTypes location
            ^{caminoAccommodationTypeIcon config accomodation}
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
    locationOrder a b = compare (canonicalise $ locationName a) (canonicalise $ locationName b)
    locationsSorted = sortBy locationOrder (caminoLocations camino)
    locationPartition = partition (\l -> T.toUpper $ canonicalise $ T.take 1 $ locationName l) locationsSorted
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
          ^{metricsSummary config preferences camino $ score trip}
    $forall day <- path trip
      <div .card .day>
        <h4>
          <a href="##{locationID $ start day}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName $ start day}
          \   -
          <a href="##{locationID $ finish day}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName $ finish day}
          ^{formatDistance $ Just (metricsDistance $ score day)}
          ^{penanceSummary config preferences camino $ score day}
        <div .card-body>
         <p>
            ^{metricsSummary config preferences camino $ score day}
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
   |]

caminoMapHtml :: Config -> Preferences -> Camino -> Maybe Trip -> Html
caminoMapHtml _config _preferences _camino _trip = [shamlet|
  <div .container-fluid>
    <div .d-flex .justify-content-center>
      <div #map>
  |]

caminoLocationIcon :: Config -> Preferences -> Camino -> S.Set Location -> S.Set Location -> Location -> String
caminoLocationIcon _config _preferences _camino stops waypoints location
  | S.member location stops = "iconStop"
  | otherwise = "icon" ++ (show $ locationType location) ++ used
    where
      used = if S.member location waypoints then "Used" else "Unused"
      
caminoMapScript :: Config -> Preferences -> Camino -> Maybe Trip -> Html
caminoMapScript config preferences camino trip = [shamlet|
  <script>
    var iconStop = L.icon({
      iconUrl: '#{iconBase}/location-stop.png',
      iconSize: [40, 40]
    });
    var iconVillageUsed = L.icon({
      iconUrl: '#{iconBase}/location-village-used.png',
      iconSize: [16, 16]
    });
    var iconVillageUnused = L.icon({
      iconUrl: '#{iconBase}/location-village-unused.png',
      iconSize: [16, 16]
    });
    var iconTownUsed = L.icon({
      iconUrl: '#{iconBase}/location-town-used.png',
      iconSize: [32, 20]
    });
    var iconTownUnused = L.icon({
      iconUrl: '#{iconBase}/location-town-unused.png',
      iconSize: [32, 20]
    });
    var iconCityUsed = L.icon({
      iconUrl: '#{iconBase}/location-city-used.png',
      iconSize: [32, 25]
    });
    var iconCityUnused = L.icon({
      iconUrl: '#{iconBase}/location-city-unused.png',
      iconSize: [32, 25]
    });
    var iconBridgeUsed = L.icon({
      iconUrl: '#{iconBase}/location-bridge-used.png',
      iconSize: [24, 9]
    });
    var iconBridgeUnused = L.icon({
      iconUrl: '#{iconBase}/location-bridge-unused.png',
      iconSize: [24, 9]
    });
    var iconIntersectionUsed = L.icon({
      iconUrl: '#{iconBase}/location-intersection-used.png',
      iconSize: [24, 22]
    });
    var iconIntersectionUnused = L.icon({
      iconUrl: '#{iconBase}/location-intersection-unused.png',
      iconSize: [24, 22]
    });
    var iconPoiUsed = L.icon({
      iconUrl: '#{iconBase}/location-poi-used.png',
      iconSize: [15, 20]
    });
    var iconPoiUnused = L.icon({
      iconUrl: '#{iconBase}/location-poi-unused.png',
      iconSize: [15, 20]
    });
    var map = L.map('map');
    map.fitBounds([ [#{latitude tl}, #{longitude tl}], [#{latitude br}, #{longitude br}] ]);
    L.tileLayer('#{tiles}', {
        maxZoom: 19,
        attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(map);
    var marker;
    var line;
    $forall location <- M.elems $ locations camino
      $maybe position <- locationPosition location
        marker = L.marker([#{latitude position}, #{longitude position}], { icon: #{caminoLocationIcon config preferences camino stops waypoints location} } );
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
    tiles = preEscapedToHtml $ (getConfigValue "web.map.tiles" "https://tile.openstreetmap.org/{z}/{x}/{y}.png" config :: String)
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
  
iconCss :: Config -> String -> Char -> Css
iconCss _config ident ch = [cassius|
.#{ident}::before
  font-family: "Camino Icons"
  font-weight: normal
  line-height: 1
  text-rendering: auto
  content: "\#{hex $ ord ch}"
|] undefined
  where
    hex c = showHex c ""

iconList = [
    ("ca-accessible", '\xe067'),
    ("ca-albergue", '\xe010'),
    ("ca-bank", '\xe042'),
    ("ca-bed-double", '\xe022'),
    ("ca-bed-double-wc", '\xe023'),
    ("ca-bed-quadruple", '\xe026'),
    ("ca-bed-quadruple-wc", '\xe027'),
    ("ca-bed-single", '\xe020'),
    ("ca-bed-triple", '\xe024'),
    ("ca-bed-triple-wc", '\xe025'),
    ("ca-bedlinen", '\xe06c'),
    ("ca-breakfast", '\xe064'),
    ("ca-bicycle-repair", '\xe045'),
    ("ca-bicycle-storage", '\xe06a'),
    ("ca-bus", '\xe047'),
    ("ca-camping", '\xe019'),
    ("ca-dinner", '\xe065'),
    ("ca-dryer", '\xe062'),
    ("ca-groceries", '\xe041'),
    ("ca-guesthouse", '\xe012'),
    ("ca-handwash", '\xe063'),
    ("ca-heating", '\xe070'),
    ("ca-hotel", '\xe013'),
    ("ca-house", '\xe011'),
    ("ca-kitchen", '\xe06b'),
    ("ca-locker", '\xe066'),
    ("ca-matress", '\xe028'),
    ("ca-medical", '\xe044'),
    ("ca-pets", '\xe069'),
    ("ca-pharmacy", '\xe043'),
    ("ca-pool", '\xe06e'),
    ("ca-prayer", '\xe06f'),
    ("ca-restaurant", '\xe040'),
    ("ca-shared", '\xe021'),
    ("ca-sleeping-bag", '\xe028'),
    ("ca-stables", '\xe068'),
    ("ca-tent", '\xe018'),
    ("ca-towels", '\xe06d'),
    ("ca-train", '\xe046'),
    ("ca-washing-machine", '\xe061'),
    ("ca-wifi", '\xe060')
  ]
  
caminoIconCss :: Config -> Camino -> [Css]
caminoIconCss config _camino = map (\(ident, ch) -> iconCss config ident ch) iconList

caminoBaseCss :: Config -> Camino -> Css
caminoBaseCss config _camino = [cassius|
@font-face
  font-family: "Camino Icons"
  font-weight: normal
  font-style: normal
  src: url(#{fontBase}/Camino-Icons.woff)
#map
  width: 80%
  height: 800px
  padding: 1em
a
  text-decoration: none
.day
  h4
    .distance
      margin-left: 1em
      font-size: initial
    .penance-summary
      margin-left: 1em
      font-size: initial
      display: inline-block
.service
  color: #1964c0
.accomodation
  color: #1964c0
.accomodation.card
  padding: 1ex
.accomodation.municipal-albergue
  color: #f9b34a
.distance-summary
  display: inline-block
  margin-left: 1em
  .leg-distance
    margin-left: 1em
  .leg-ascent
    font-size: smaller
  .leg-ascent::before
    content: "\2191"
  .leg-descent
    font-size: smaller
  .leg-descent::before
    content: "\2193"
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
    h4::before
      margin-right: 0.5ex
      color: #f9b34a
      font-family: "Camino Icons"
      font-weight: normal
      content: "\e020"
  |] undefined
  where
    fontBase = getConfigValue "web.fonts.base" "fonts" config :: String

caminoCss :: Config -> Camino -> [Css]
caminoCss config camino = (base':default':routes') ++ icons'
  where
    base' = caminoBaseCss config camino
    default' = paletteCss config "location-default" (palette camino)
    routes' = map (\r -> paletteCss config ("location-" ++ (routeID r)) (routePalette r)) (routes camino)
    icons' = caminoIconCss config camino
  
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
            <p .text-muted .my-2>
              <a href="https://github.com/charvolant/camino-planner">The Camino Planner
          <div .col>
            <p .text-muted .my-2>
          <div .col>
            <p .text-muted .my-2>Example only
      <script src="#{jQueryJs}">
      <script src="#{bootstrapJs}">
      <script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js" integrity="sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=" crossorigin="">
      ^{caminoMapScript config preferences camino trip}
  |]
  where
    title = maybe "Camino" (\t -> (locationName $ start t) <> " - " <> (locationName $ finish t)) trip
    iconBase = getConfigValue "web.icons.base" "icons" config :: String
    jQueryJs = getConfigValue "web.assets.jQueryJs" "jquery.js" config :: T.Text
    bootstrapCss = getConfigValue "web.assets.bootstrapCss" "bootstrap.css"  config :: T.Text
    bootstrapJs = getConfigValue "web.assets.bootstrapJs" "bootstrap.js" config :: T.Text