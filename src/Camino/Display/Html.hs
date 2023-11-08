{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Camino.Config (Config(..), AssetConfig(..), AssetType(..), LinkConfig(..), LinkI18n(..), LinkType(..), getAssets, getLinks)
import Camino.Planner (Trip, Day, Metrics(..), tripStops, tripWaypoints)
import Camino.Preferences
import Camino.Display.Css (toCssColour)
import Camino.Display.I18n
import Camino.Display.Routes
import Text.Hamlet
import qualified Data.Text as T (intercalate, null, pack, Text)
import Formatting
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (sortBy)
import qualified Data.Text as T (toUpper, take, filter)
import Data.Text.ICU.Char (Bool_(..), property)
import Data.Text.ICU.Normalize2 (NormalizationMode(..), normalize)
import Data.Maybe (isJust)

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

penanceSummary :: Preferences -> Camino -> Metrics -> HtmlUrlI18n CaminoMsg CaminoRoute
penanceSummary _preferences _camino metrics = [ihamlet|
   <div .penance-summary .dropdown title="_{PenanceSummaryTitle}">
     <button .btn .btn-outline-primary .penance-summary .dropdown-toggle data-toggle="dropdown" aria-haspopup="true" aria-expanded="false" data-bs-toggle="dropdown">
       _{PenanceMsg (metricsPenance metrics)}
     <div .dropdown-menu>
       <a .dropdown-item>_{DistancePenanceMsg (metricsPerceivedDistance metrics)}
       <a .dropdown-item>_{AccommodationPenanceMsg (metricsAccommodation metrics)}
       <a .dropdown-item>_{StopPenanceMsg (metricsStop metrics)}
       <a .dropdown-item>_{StopServicesPenanceMsg (metricsStopServices metrics)}
       <a .dropdown-item>_{DayServicesPenanceMsg (metricsDayServices metrics)}
       <a .dropdown-item>_{DistanceAdjustMsg (metricsDistanceAdjust metrics)}
       <a .dropdown-item>_{TimeAdjustMsg (metricsTimeAdjust metrics)}
       <a .dropdown-item>_{MiscPenanceMsg (metricsMisc metrics)}
   |]
    
metricsSummary :: Preferences -> Camino -> Metrics -> HtmlUrlI18n CaminoMsg CaminoRoute
metricsSummary _preferences _camino metrics = [ihamlet|
    _{DistanceMsg (metricsDistance metrics) (metricsPerceivedDistance metrics)}
    _{TimeMsg (metricsTime metrics)}
    _{AscentMsg (metricsAscent metrics)}
    _{DescentMsg (metricsDescent metrics)}
    _{PenanceMsg (metricsPenance metrics)}
  |]

daySummary :: Preferences -> Camino -> Maybe Trip -> Day -> HtmlUrlI18n CaminoMsg CaminoRoute
daySummary _preferences _camino _trip day = [ihamlet|
    <p>_{DaySummaryMsg day}
    <p>#{T.intercalate ", " (map locationName ((map legFrom $ path day) ++ [finish day]))}
  |]

tripSummary :: Preferences -> Camino -> Trip -> HtmlUrlI18n CaminoMsg CaminoRoute
tripSummary _preferences _camino trip = [ihamlet|
    <h1>From #{locationName $ start trip} to #{locationName $ finish trip}
    <h2>Stages
    <ul>
    $forall day <- path trip
      <ol>#{locationName $ start day} - #{locationName $ finish day}
  |]

locationSummary :: Preferences -> Camino -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
locationSummary _preferences _camino location = [ihamlet|
    $if not $ T.null services
      <p>_{ServicesLabel} #{services}
    $if not $ T.null accommodation
      <p>_{AccommodationLabel} #{accommodation}
  |]
  where 
    services = T.intercalate ", " (map (\s -> T.pack $ show s) (S.toList $ locationServices location))
    accommodation = T.intercalate ", " (map (\a -> T.pack $ show $ accommodationType a) (locationAccommodation location))

caminoLocationTypeIcon :: LocationType -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationTypeIcon Village = [ihamlet| <span .location-type .ca-village title="_{VillageTitle}"> |]
caminoLocationTypeIcon Town = [ihamlet| <span .location-type .ca-town title="_{TownTitle}"> |]
caminoLocationTypeIcon City = [ihamlet| <span .location-type .ca-city title="_{CityTitle}"> |]
caminoLocationTypeIcon Bridge = [ihamlet| <span .location-type .ca-bridge title="_{BridgeTitle}"> |]
caminoLocationTypeIcon Intersection = [ihamlet| <span .location-type .ca-intersection title="_{IntersectionTitle}"> |]
caminoLocationTypeIcon _ = [ihamlet| <span .location-type .ca-poi title="_{PoiTitle}"> |]

caminoSleepingIcon :: Sleeping -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoSleepingIcon Shared = [ihamlet| <span .sleeping .ca-shared title="_{SharedTitle}"> |]
caminoSleepingIcon Single = [ihamlet| <span.sleeping .ca-bed-single title="_{SingleTitle}"> |]
caminoSleepingIcon Double = [ihamlet| <span .sleeping .ca-bed-double title="_{DoubleTitle}"> |]
caminoSleepingIcon DoubleWC = [ihamlet| <span .sleeping .ca-bed-double-wc title="_{DoubleWcTitle}"> |]
caminoSleepingIcon Triple = [ihamlet| <span .sleeping .ca-bed-triple title="_{TripleTitle}"> |]
caminoSleepingIcon TripleWC = [ihamlet| <span .sleeping .ca-bed-triple-wc title="_{TripleWcTitle}"> |]
caminoSleepingIcon Quadruple = [ihamlet| <span .sleeping .ca-bed-quadruple title="_{QuadrupleTitle}"> |]
caminoSleepingIcon QuadrupleWC = [ihamlet| <span .sleeping .ca-bed-quadruple-wc title="_{QuadrupleWcTitle}"> |]
caminoSleepingIcon Mattress = [ihamlet| <span .sleeping .ca-mattress title="_{MattressTitle}"> |]
caminoSleepingIcon SleepingBag = [ihamlet| <span .sleeping .ca-sleeping-bag title="_{SleepingBagTitle}"> |]

caminoAccommodationTypeIcon :: AccommodationType -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoAccommodationTypeIcon MunicipalAlbergue = [ihamlet| <span .accomodation .municipal-albergue .ca-albergue title="_{MunicipalAlbergueTitle}"> |]
caminoAccommodationTypeIcon PrivateAlbergue = [ihamlet| <span .accomodation .private-albergue .ca-albergue title="_{PrivateAlbergueTitle}"> |]
caminoAccommodationTypeIcon GuestHouse = [ihamlet| <span .accomodation .guest-house .ca-guesthouse title="_{GuestHouseTitle}"> |]
caminoAccommodationTypeIcon House = [ihamlet| <span .accomodation .house .ca-house title="_{HouseTitle}"> |]
caminoAccommodationTypeIcon Hotel = [ihamlet| <span .accomodation .hotel .ca-hotel title="_{HotelTitle}"> |]
caminoAccommodationTypeIcon Camping = [ihamlet| <span .accomodation .camping .ca-tent title="_{CampingTitle}"> |]

caminoAccommodationLabel :: Accommodation -> CaminoMsg
caminoAccommodationLabel (GenericAccommodation MunicipalAlbergue) = MunicipalAlbergueTitle
caminoAccommodationLabel (GenericAccommodation PrivateAlbergue) = PrivateAlbergueTitle
caminoAccommodationLabel (GenericAccommodation GuestHouse) = GuestHouseTitle
caminoAccommodationLabel (GenericAccommodation House) = HouseTitle
caminoAccommodationLabel (GenericAccommodation Hotel) = HotelTitle
caminoAccommodationLabel (GenericAccommodation Camping) = CampingTitle
caminoAccommodationLabel (Accommodation _type _name _services _sleeping) = AccommodationLabel -- Generally shouldn'ty be called

caminoServiceIcon :: Service -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoServiceIcon WiFi = [ihamlet| <span .service .ca-wifi title="_{WiFiTitle}"> |]
caminoServiceIcon Restaurant = [ihamlet| <span .service .ca-restaurant title="_{RestaurantTitle}"> |]
caminoServiceIcon Pharmacy = [ihamlet| <span .service .ca-pharmacy title="_{PharmacyTitle}"> |]
caminoServiceIcon Bank = [ihamlet| <span .service .ca-bank title="_{BankTitle}"> |]
caminoServiceIcon BicycleRepair = [ihamlet| <span .service .ca-bicycle-repair title="_{BicycleRepairTitle}"> |]
caminoServiceIcon Groceries = [ihamlet| <span .service .ca-groceries title="_{GroceriesTitle}"> |]
caminoServiceIcon Medical = [ihamlet| <span .service .ca-medical title="_{MedicalTitle}"> |]
caminoServiceIcon WashingMachine = [ihamlet| <span .service .ca-washing-machine title="_{WashingMachineTitle}"> |]
caminoServiceIcon Dryer = [ihamlet| <span .service .ca-dryer title="_{DryerTitle}"> |]
caminoServiceIcon Handwash = [ihamlet| <span .service .ca-handwash title="_{HandwashTitle}"> |]
caminoServiceIcon Kitchen = [ihamlet| <span .service .ca-kitchen title="_{KitchenTitle}"> |]
caminoServiceIcon Breakfast = [ihamlet| <span .service .ca-breakfast title="_{BreakfastTitle}"> |]
caminoServiceIcon Dinner = [ihamlet| <span .service .ca-dinner title="_{DinnerTitle}"> |]
caminoServiceIcon Lockers = [ihamlet| <span .service .ca-lockers title="_{LockersTitle}"> |]
caminoServiceIcon Accessible = [ihamlet| <i .service .ca-accessible title="_{AccessibleTitle}"> |]
caminoServiceIcon Stables = [ihamlet| <span .service .ca-stables title="_{StablesTitle}"> |]
caminoServiceIcon Pets = [ihamlet| <span .service .ca-pets title="_{PetsTitle}"> |]
caminoServiceIcon BicycleStorage = [ihamlet| <span .service .ca-bicycle-storage title="_{BicycleStorageTitle}"> |]
caminoServiceIcon CampSite = [ihamlet| <span .service .ca-camping title="_{CampSiteTitle}"> |]
caminoServiceIcon Bedlinen = [ihamlet| <span .service .ca-bedlinen title="BedlinenTitle"> |]
caminoServiceIcon Towels = [ihamlet| <span .service .ca-towels title="_{TowelsTitle}"> |]
caminoServiceIcon Pool = [ihamlet| <span .service .ca-pool title="_{PoolTitle}"> |]
caminoServiceIcon Heating = [ihamlet| <span .service .ca-heating title="_{HeatingTitle}"> |]
caminoServiceIcon Prayer = [ihamlet| <span .service .ca-prayer title="_{PrayerTitle}"> |]
caminoServiceIcon Train = [ihamlet| <span .service .ca-train title="_{TrainTitle}"> |]
caminoServiceIcon Bus = [ihamlet| <span .service .ca-bus title="_{BusTitle}"> |]

caminoAccommodationSummaryHtml :: Accommodation -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoAccommodationSummaryHtml a@(GenericAccommodation type') = [ihamlet|
    ^{caminoAccommodationTypeIcon type'} _{caminoAccommodationLabel a}
  |]
caminoAccommodationSummaryHtml a@(Accommodation _name type' services' sleeping') = [ihamlet|
    <span .accomodation>
      <span .pr-4>
        ^{caminoAccommodationTypeIcon type'} #{accommodationName a}
      <span .p2-4>
        $forall service <- services'
           ^{caminoServiceIcon service}
      <span>
        $forall sleeping <- sleeping'
          ^{caminoSleepingIcon sleeping}
 |]

caminoAccommodationHtml :: Accommodation -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoAccommodationHtml (GenericAccommodation _type) = [ihamlet| |]
caminoAccommodationHtml (Accommodation name' type' services' sleeping') = [ihamlet|
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

locationLine :: Preferences -> Camino -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
locationLine _preferences _camino location = [ihamlet|
    #{locationName location}
    <span .accomodation-types>
      $forall accomodation <- locationAccommodationTypes location
        ^{caminoAccommodationTypeIcon accomodation}
    <span .services>
      $forall service <- locationServices location
        ^{caminoServiceIcon service}
  |]

caminoLocationHtml :: Preferences -> Camino -> Maybe Trip -> S.Set Location -> S.Set Location -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationHtml _preferences camino _trip stops waypoints location = [ihamlet|
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
    
caminoLocationsHtml :: Preferences -> Camino -> Maybe Trip -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationsHtml preferences camino trip = [ihamlet|
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
    
preferenceRangeHtml :: (Real a) => PreferenceRange a -> HtmlUrlI18n CaminoMsg CaminoRoute
preferenceRangeHtml range = [ihamlet|
    <span .text-danger>#{format (fixed 1) (rangeMinimum range)} -
    <span>#{format (fixed 1)( rangeLower range)} -
    <span .text-success .fw-bolder>#{format (fixed 1) (rangeTarget range)} -
    <span>#{format (fixed 1) (rangeUpper range)} -
    <span .text-danger>#{format (fixed 1) (rangeMaximum range)}
    $maybe d <- rangeDerived range
      <p .text-body-tertiary .smaller>#{d}
  |]
  
preferencesHtml :: Preferences -> Camino -> Maybe Trip -> HtmlUrlI18n CaminoMsg CaminoRoute
preferencesHtml preferences _camino _trip = [ihamlet|
  <div .container-fluid>
    <div .row>
      <div .col-3>_{WalkingFunctionLabel}
      <div .col .offset-1>#{preferenceWalkingFunction preferences}
    <div .row>
      <div .col-3>_{FitnessLabel}
      <div .col .offset-1>#{show $ preferenceFitness preferences}
    <div .row>
      <div .col-3>_{DistancePreferencesLabel}
      <div .col .offset-1>^{preferenceRangeHtml $ preferenceDistance preferences}
    <div .row>
      <div .col-3>_{DistancePreferencesPerceivedLabel}
      <div .col .offset-1>^{preferenceRangeHtml $ preferencePerceivedDistance preferences}
    <div .row>
      <div .col-3>_{TimePreferencesLabel}
      <div .col. .offset-1>^{preferenceRangeHtml $ preferenceTime preferences}
    <div .row>
      <div .col-3>_{AccommodationPreferencesLabel}
      $forall ak <- M.keys $ preferenceAccommodation preferences
        <div .row>
          <div .col-1 .offset-3>^{caminoAccommodationTypeIcon ak}
          <div .col>_{PenanceFormatted (findAcc preferences ak)}
    <div .row>
      <div .col-3>_{StopPreferencesLabel}
      <div .col .offset-1>_{PenanceFormatted (preferenceStop preferences)}
    <div .row>
      <div .col-3>_{StopServicesPreferencesLabel}
      $forall sk <- M.keys $ preferenceStopServices preferences
        <div .row>
          <div .col-1 .offset-3>^{caminoServiceIcon sk}
          <div .col>_{PenanceFormatted (findSs preferences sk)}
    <div .row>
      <div .col-3>_{DayServicesPreferencesLabel}
      $forall sk <- M.keys $ preferenceDayServices preferences
        <div .row>
          <div .col-1 .offset-3>^{caminoServiceIcon sk}
          <div .col>_{PenanceFormatted (findDs preferences sk)}
    <div .row>
      <div .col-3>_{RequiredStopsLabel}
      <div .col .offset-1>
        <ul>
        $forall l <- preferenceRequired preferences
          <li>
            <a href="##{locationID l}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName l}
    <div .row>
      <div .col-3>_{ExcludedStopsLabel}
      <div .col .offset-1>
        <ul>
        $forall l <- preferenceExcluded preferences
          <li>
            <a href="##{locationID l}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">#{locationName l}
  |]
  where
    findAcc prefs ak = (preferenceAccommodation prefs) M.! ak
    findSs prefs sk = (preferenceStopServices prefs) M.! sk
    findDs prefs sk = (preferenceDayServices prefs) M.! sk

caminoTripHtml :: Preferences -> Camino -> Trip -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoTripHtml preferences camino trip = [ihamlet|
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
          _{DistanceFormatted (metricsDistance $ score day)}
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
         $forall accom <- metricsAccommodationChoice $ score day
            <p>
              ^{caminoAccommodationSummaryHtml accom}
   |]

caminoMapHtml :: Preferences -> Camino -> Maybe Trip -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapHtml _preferences _camino _trip = [ihamlet|
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
      
caminoMapScript :: Preferences -> Camino -> Maybe Trip -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapScript preferences camino trip = [ihamlet|
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

layoutHtml :: Config -- ^ The configuration to use when inserting styles, scripts, paths etc.
 ->  T.Text -- ^ The page title
 -> Maybe (HtmlUrlI18n CaminoMsg CaminoRoute) -- ^ Any extra HTML to be added to the head
 -> HtmlUrlI18n CaminoMsg CaminoRoute -- ^ The body HTML
 -> Maybe (HtmlUrlI18n CaminoMsg CaminoRoute) -- ^ Any extra HTML to be added to the foot (after the footer)
 -> HtmlUrlI18n CaminoMsg CaminoRoute -- ^ The laid out HTML
layoutHtml config title header body footer = [ihamlet|
    $doctype 5
     <html>
       <head>
         <meta charset="utf-8">
         <meta name="viewport" content="width=device-width, initial-scale=1.0, shrink-to-fit=no">
         <title>#{title}
         $forall c <- css
           <link rel="stylesheet" href="#{assetPath c}">
         <link rel="stylesheet" href="camino.css">
         $maybe h <- header
           ^{h}
       <body>
         <header .p-2>
           <nav .navbar .navbar-expand-md>
             <div .container-fluid>
               <a .navbar-brand href="#">
                 <img width="64" height="64" src="@{IconRoute "tile-64.png"}" alt="Camino Planner">
               <h1>#{title}
               <div .collapse .navbar-collapse .d-flex .justify-content-end #navcol-links">
                 <ul .navbar-nav>
                   $forall link <- headLinks
                     <li .nav-item">
                       <a .nav-item href="@{LinkRoute  link}">_{LinkLabel link}
         <main .p-2>
           ^{body}
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
         $maybe f <- footer
          ^{f}
     |]
     where
       css = getAssets Css config
       headLinks = getLinks Header config
       scripts = getAssets JavaScript config

caminoHtml :: Config -> Preferences -> Camino -> Maybe Trip -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoHtml config preferences camino trip = let
    title = maybe "Camino" (\t -> (locationName $ start t) <> " - " <> (locationName $ finish t)) trip
    body = [ihamlet|
      <main .container-fluid .p-2>
        <div>
          <ul .nav .nav-tabs role="tablist">
            <li .nav-item role="presentation">
              <a .nav-link .active role="tab" data-bs-toggle="tab" href="#map-tab">_{MapLabel}
            <li .nav-item role="presentation">
              <a .nav-link role="tab" data-bs-toggle="tab" href="#plan-tab">_{PlanLabel}
            <li .nav-item role="presentation">
              <a #locations-toggle .nav-link role="tab" data-bs-toggle="tab" href="#locations-tab">_{LocationsLabel}
            <li .nav-item role="presentation">
              <a .nav-link role="tab" data-bs-toggle="tab" href="#preferences-tab">_{PreferencesLabel}
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
      ^{caminoMapScript preferences camino trip}
    |]
    foot = Just (caminoMapScript preferences camino trip)
  in
    layoutHtml config title Nothing body foot

helpHtml :: Config -> HtmlUrlI18n CaminoMsg CaminoRoute
helpHtml config = let
    title = "Help"
    body = $(ihamletFile "templates/help/help-en.hamlet")
  in
    layoutHtml config title Nothing body Nothing
