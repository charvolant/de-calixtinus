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
import Camino.Planner (Trip, Day, Metrics(..), tripLegs, tripStops, tripWaypoints)
import Camino.Preferences
import Camino.Display.Css (toCssColour)
import Camino.Display.I18n
import Camino.Display.Routes
import Graph.Graph (outgoing)
import Text.Hamlet
import qualified Data.Text as T (concat, filter, intercalate, null, pack, take, Text, toLower, toUpper)
import Formatting
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Text.ICU.Char (Bool_(..), property)
import Data.Text.ICU.Normalize2 (NormalizationMode(..), normalize)
import Data.Maybe (fromJust, isJust)

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

-- | Include a label row if some values are non-empty
conditionalLabel :: CaminoMsg -> [a] -> HtmlUrlI18n CaminoMsg CaminoRoute
conditionalLabel label values = [ihamlet|
  $if not $ null values
    <div .row>
      <div .col>
        <h6>_{label}
  |]

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

caminoLocationTypeMapIcon :: LocationType -> Bool -> Bool -> CaminoRoute
caminoLocationTypeMapIcon lt stop waypoint =
  let
    name = T.toLower $ T.pack $ show lt
    style = if stop then "stop" else if waypoint then "used" else "unused"
  in
    IconRoute (T.concat ["location-", name, "-", style, ".png"])

caminoLocationTypeIcon :: LocationType -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationTypeIcon Village = [ihamlet| <span .location-type .ca-village title="_{VillageTitle}"> |]
caminoLocationTypeIcon Town = [ihamlet| <span .location-type .ca-town title="_{TownTitle}"> |]
caminoLocationTypeIcon City = [ihamlet| <span .location-type .ca-city title="_{CityTitle}"> |]
caminoLocationTypeIcon Bridge = [ihamlet| <span .location-type .ca-bridge title="_{BridgeTitle}"> |]
caminoLocationTypeIcon Intersection = [ihamlet| <span .location-type .ca-intersection title="_{IntersectionTitle}"> |]
caminoLocationTypeIcon Monastery = [ihamlet| <span .location-type .ca-monastery title="_{MonasteryTitle}"> |]
caminoLocationTypeIcon Peak = [ihamlet| <span .location-type .ca-peak title="_{PeakTitle}"> |]
caminoLocationTypeIcon _ = [ihamlet| <span .location-type .ca-poi title="_{PoiTitle}"> |]

caminoLocationTypeLabel :: LocationType -> CaminoMsg
caminoLocationTypeLabel Village = VillageTitle
caminoLocationTypeLabel Town = TownTitle
caminoLocationTypeLabel City = CityTitle
caminoLocationTypeLabel Bridge = BridgeTitle
caminoLocationTypeLabel Intersection = IntersectionTitle
caminoLocationTypeLabel Monastery = MonasteryTitle
caminoLocationTypeLabel Peak = PeakTitle
caminoLocationTypeLabel Poi = PoiTitle
caminoLocationTypeLabel _ = PoiTitle

caminoLegTypeIcon :: LegType -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLegTypeIcon Trail = [ihamlet| <span .leg-type .ca-walking title="_{TrailTitle}"> |]
caminoLegTypeIcon CyclePath = [ihamlet| <span .leg-type .ca-cycling title="_{CyclePathTitle}"> |]
caminoLegTypeIcon Ferry = [ihamlet| <span .leg-type .ca-ferry title="_{FerryTitle}"> |]
caminoLegTypeIcon _ = [ihamlet| <span .leg-type title="_{RoadTitle}"> |]

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
caminoServiceIcon Accessible = [ihamlet| <span .service .ca-accessible title="_{AccessibleTitle}"> |]
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
  <div .row .accomodation>
    <div .offset-1 .col-5>
      ^{caminoAccommodationTypeIcon type'}
      #{name'}
    <div .col-4>
      $forall service <- services'
        ^{caminoServiceIcon service}
    <div .col-2>
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

legLine :: Preferences -> Camino -> Leg -> HtmlUrlI18n CaminoMsg CaminoRoute
legLine _preferences _camino leg = [ihamlet|
    ^{caminoLegTypeIcon (legType leg)}
    $if legDistance leg > 0
      <span .leg-distance>#{format (fixed 1 % "km") (legDistance leg)}
    $if legAscent leg > 0
      <span .leg-ascent>#{format (fixed 0 % "m") (legAscent leg)}
    $if legDescent leg > 0
      <span .leg-descent>#{format (fixed 0 % "m") (legDescent leg)}
    $if isJust $ legTime leg
      <span .leg-time>#{format (fixed 1 % "hrs") (fromJust $ legTime leg)}
    $if isJust $ legPenance leg
      <span .leg-penance>_{LegPenanceMsg (fromJust $ legPenance leg)}
    $if isJust $ legNotes leg
      <span .leg-notes>#{fromJust $ legNotes leg}
  |]

locationLegLine :: Preferences -> Bool -> Camino -> Leg -> HtmlUrlI18n CaminoMsg CaminoRoute
locationLegLine preferences showLink camino leg = [ihamlet|
   $if showLink
     <a href="##{locationID $ legTo leg}">
       #{locationName $ legTo leg}
   $else
     #{locationName $ legTo leg}
   ^{legLine preferences camino leg}
 |]

locationLegs :: Preferences -> Bool -> Camino -> S.Set Leg -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
locationLegs preferences showLink camino used location = [ihamlet|
  $forall leg <- usedLegs
    <div .row>
      <div .col .leg-to .leg-line .leg-used .offset-1>
        ^{locationLegLine preferences showLink camino leg}
  $forall leg <- unusedLegs
      <div .row>
        <div .col .leg-to .leg-line .leg-unused .offset-1>
          ^{locationLegLine preferences showLink camino leg}
 |]
  where
    outgoingLegs = outgoing camino location
    (usedLegs, unusedLegs) = L.partition (\l -> S.member l used) outgoingLegs

caminoLocationHtml :: Preferences -> Camino -> Maybe Trip -> String -> S.Set Location -> S.Set Location -> S.Set Leg -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationHtml preferences camino _trip containerId stops waypoints used location = [ihamlet|
  <div id="#{lid}" .accordion-item .location-#{routeID route} :isStop:.location-stop :isWaypoint:.location-waypoint .location>
    <div .accordion-header>
      <div .row>
        <button .accordion-button .collapsed data-bs-toggle="collapse" data-bs-target="#location-body-#{lid}" aria-expanded="false" aria-controls="location-body-#{lid}">
          <h5 .col-6>
            ^{caminoLocationTypeIcon (locationType location)}
            #{locationName location}
          <div .col .services>
             $forall service <- locationServices location
                ^{caminoServiceIcon service}
          <div .col .accomodation-types>
             $forall accomodation <- locationAccommodationTypes location
                 ^{caminoAccommodationTypeIcon accomodation}
    <div id="location-body-#{lid}" .accordion-collapse .collapse aria-labelledby="location-heading-#{lid}" data-parent="##{containerId}">
      <div .accordion-body .container-fluid>
        $if (isJust $ locationDescription location) || (isJust $ locationHref location)
          <div .row>
            <div .col>
              $maybe d <- locationDescription location
                #{d}
            <div .col-1 .float-end>
              $maybe href <- locationHref location
                  <a href="#{href}">
                    <span .link-out .text-info title="_{LinkOut (locationName location)}">
        ^{conditionalLabel AccommodationLabel (locationAccommodation location)}
        $forall accomodation <- locationAccommodation location
          ^{caminoAccommodationHtml accomodation}
        ^{conditionalLabel RouteLabel (outgoing camino location)}
        ^{locationLegs preferences True camino used location}
  |]
  where
    lid = locationID location
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
        <div .mt-2 .p-2>
          <h2>#{caminoName camino}
          <p>
            #{caminoDescription camino}
    <div .row>
      <div .col>
        <div #locations .accordion .container-fluid>
          $forall loc <- locationsSorted
            <div .row>
              ^{caminoLocationHtml preferences camino trip "locations" stops waypoints usedLegs loc}
  |]
  where
    locationOrder a b = compare (canonicalise $ locationName a) (canonicalise $ locationName b)
    locationsSorted = L.sortBy locationOrder (caminoLocationList camino)
    locationPartition = partition (\l -> T.toUpper $ canonicalise $ T.take 1 $ locationName l) locationsSorted
    stops = maybe S.empty (S.fromList . tripStops) trip
    waypoints = maybe S.empty (S.fromList . tripWaypoints) trip
    usedLegs = maybe (S.fromList $ caminoLegs camino) (S.fromList . tripLegs) trip
    
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
preferencesHtml preferences camino _trip = [ihamlet|
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
      <div .col-3>_{RouteLabel}
      <div .col .offset-1>
        <ul>
          <li>
            #{routeName $ caminoDefaultRoute camino}
          $forall r <- preferenceRoutes preferences
            <li>
              #{routeName r}
    <div .row>
      <div .col-3>_{RequiredStopsLabel}
      <div .col .offset-1>
        <ul>
          $forall l <- preferenceStops preferences
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
                <div .leg-summary .leg-line>
                  ^{legLine preferences camino leg}
         $forall accom <- metricsAccommodationChoice $ score day
            <p>
              ^{caminoAccommodationSummaryHtml accom}
   |]

caminoMapHtml :: Preferences -> Camino -> Maybe Trip -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapHtml _preferences camino _trip = [ihamlet|
  <div .container-fluid>
    <div .row>
      <div .col .d-flex .justify-content-center>
        <div #map>
      <div .col-2>
        <div .card .mt-2 .p-1>
          <div .card-body>
            <table .map-key>
              <thead>
                <tr>
                  <th colspan="4">_{KeyLabel}
              <tbody>
                $forall r <- caminoRoutes camino
                  <tr>
                    <td>
                    <td .border .border-light style="background-color: #{toCssColour $ paletteColour $ routePalette r}">
                    <td>
                    <td>#{routeName r}
                $forall lt <- [Bridge, City, Intersection, Monastery, Peak, Poi, Town, Village]
                  <tr>
                    <td>
                      <img .map-key-icon src="@{caminoLocationTypeMapIcon lt True False}" title="_{StopLabel}">
                    <td>
                      <img .map-key-icon src="@{caminoLocationTypeMapIcon lt False True}" title="_{WaypointLabel}">
                    <td>
                      <img .map-key-icon src="@{caminoLocationTypeMapIcon lt False False}" title="_{UnusedLabel}">
                    <td>
                      _{caminoLocationTypeLabel lt}
  |]

caminoLocationIcon :: Preferences -> Camino -> S.Set Location -> S.Set Location -> Location -> String
caminoLocationIcon _preferences _camino stops waypoints location =
  "icon" ++ (show $ locationType location) ++ (status location)
   where
    status loc
     | S.member loc stops = "Stop"
     | S.member loc waypoints = "Used"
     | otherwise = "Unused"

caminoMapTooltip :: Preferences -> Camino -> Maybe Trip -> S.Set Leg -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapTooltip preferences camino _trip usedLegs location = [ihamlet|
  <div .location-tooltip .container-fluid>
    <div .row>
      <div .col>
        ^{locationLine preferences camino location}
    ^{locationLegs preferences False camino usedLegs location}
  |]

caminoMapScript :: Preferences -> Camino -> Maybe Trip -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapScript preferences camino trip = [ihamlet|
  <script>
    var iconVillageUsed = L.icon({
      iconUrl: '@{IconRoute "location-village-used.png"}',
      iconSize: [16, 16]
    });
    var iconVillageUnused = L.icon({
      iconUrl: '@{IconRoute "location-village-unused.png"}',
      iconSize: [16, 16]
    });
    var iconVillageStop = L.icon({
      iconUrl: '@{IconRoute "location-village-stop.png"}',
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
    var iconTownStop = L.icon({
      iconUrl: '@{IconRoute "location-town-stop.png"}',
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
    var iconCityStop = L.icon({
      iconUrl: '@{IconRoute "location-city-stop.png"}',
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
    var iconBridgeStop = L.icon({
      iconUrl: '@{IconRoute "location-bridge-stop.png"}',
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
    var iconIntersectionStop = L.icon({
      iconUrl: '@{IconRoute "location-intersection-stop.png"}',
      iconSize: [24, 22]
    });
    var iconMonasteryUsed = L.icon({
      iconUrl: '@{IconRoute "location-monastery-used.png"}',
      iconSize: [20, 20]
    });
    var iconMonasteryUnused = L.icon({
      iconUrl: '@{IconRoute "location-monastery-unused.png"}',
      iconSize: [20, 20]
    });
    var iconMonasteryStop = L.icon({
      iconUrl: '@{IconRoute "location-monastery-stop.png"}',
      iconSize: [20, 20]
    });
    var iconPeakUsed = L.icon({
      iconUrl: '@{IconRoute "location-peak-used.png"}',
      iconSize: [20, 18]
    });
    var iconPeakUnused = L.icon({
      iconUrl: '@{IconRoute "location-peak-unused.png"}',
      iconSize: [20, 18]
    });
    var iconPeakStop = L.icon({
      iconUrl: '@{IconRoute "location-peak-stop.png"}',
      iconSize: [20, 18]
    });
    var iconPoiUsed = L.icon({
      iconUrl: '@{IconRoute "location-poi-used.png"}',
      iconSize: [15, 20]
    });
    var iconPoiUnused = L.icon({
      iconUrl: '@{IconRoute "location-poi-unused.png"}',
      iconSize: [15, 20]
    });
    var iconPoiStop = L.icon({
      iconUrl: '@{IconRoute "location-poi-stop.png"}',
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
    $forall location <- M.elems $ caminoLocations camino
      $maybe position <- locationPosition location
        marker = L.marker([#{latitude position}, #{longitude position}], { icon: #{caminoLocationIcon preferences camino stops waypoints location} } );
        marker.bindTooltip(`^{caminoMapTooltip preferences camino trip usedLegs location}`);
        marker.addTo(map);
        marker.on('click', function(e) { $('#locations-toggle').tab('show'); $("##{locationID location}").get(0).scrollIntoView({behavior: 'smooth'}); } );
    $forall leg <- caminoLegs camino
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
    usedLegs = maybe S.empty (S.fromList . tripLegs) trip
    chooseWidth leg | S.member leg usedLegs = 6 :: Int
      | otherwise = 3 :: Int
    chooseOpacity leg | S.member leg usedLegs = 1.0 :: Float
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
