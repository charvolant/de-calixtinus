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
import Camino.Config (Config(..), AssetConfig(..), AssetType(..), getAssets)
import Camino.Planner (TripChoice(..), Solution(..), Day, Metrics(..), Trip, tripLegs, tripStops, tripWaypoints)
import Camino.Preferences
import Camino.Util
import Camino.Display.Css (caminoCss, toCssColour)
import Camino.Display.I18n
import Camino.Display.Routes
import Data.Description
import Data.Localised
import Data.Maybe (fromJust, isJust)
import Data.Metadata
import Graph.Graph (incoming, outgoing)
import Text.Cassius (renderCss)
import Text.Hamlet
import qualified Data.Text as T (concat, intercalate, null, pack, take, toLower, toUpper)
import Formatting
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

-- | Include a label row if some values are non-empty
conditionalLabel :: CaminoMsg -> [a] -> HtmlUrlI18n CaminoMsg CaminoRoute
conditionalLabel label values = [ihamlet|
  $if not $ null values
    <div .row>
      <div .col>
        <h6 .pt-3>_{label}
  |]

penanceSummary :: TravelPreferences -> CaminoPreferences -> Bool -> Bool -> Metrics -> HtmlUrlI18n CaminoMsg CaminoRoute
penanceSummary _preferences _camino accommodationDetail serviceDetail metrics = [ihamlet|
   _{PenanceMsg (metricsPenance metrics)} =
   _{DistancePenanceMsg (metricsPerceivedDistance metrics)}
   $if metricsDistanceAdjust metrics /= mempty
     \ + _{DistanceAdjustMsg (metricsDistanceAdjust metrics)}
   $if metricsTimeAdjust metrics /= mempty
     \ + _{TimeAdjustMsg (metricsTimeAdjust metrics)}
   $if metricsStop metrics /= mempty
      \ + _{StopPenanceMsg (metricsStop metrics)}
   $if metricsLocation metrics /= mempty
     \ + _{LocationPenanceMsg (metricsLocation metrics)}
   $if showAccommodation
     \ + _{AccommodationPenanceMsg (metricsAccommodation metrics)}
     $if hasAccommodationServices && accommodationDetail
       \ (
       $forall service <- acServices
          ^{caminoServiceIcon service}
       )
   $if metricsStopServices metrics /= mempty
     \ + _{StopServicesPenanceMsg (metricsStopServices metrics)}
     $if serviceDetail
       \ (
       $forall service <- metricsMissingStopServices metrics
          ^{caminoServiceIcon service}
       )
   $if metricsDayServices metrics /= mempty
     \ + _{DayServicesPenanceMsg (metricsDayServices metrics)}
     if $serviceDetail
       \ (
       $forall service <- metricsMissingDayServices metrics
          ^{caminoServiceIcon service}
       )
   $if metricsMisc metrics /= mempty
     \ + _{MiscPenanceMsg (metricsMisc metrics)}
   |]
  where
    acServices = maybe S.empty (tripChoiceServices . fst) (L.uncons $ metricsAccommodationChoice metrics)
    hasAccommodationServices = accommodationDetail && not (S.null acServices)
    showAccommodation = metricsLocation metrics /= mempty || hasAccommodationServices

metricsSummary :: TravelPreferences -> CaminoPreferences -> Metrics -> Maybe Int -> HtmlUrlI18n CaminoMsg CaminoRoute
metricsSummary _preferences _camino metrics days = [ihamlet|
    _{DistanceMsg (metricsDistance metrics) (metricsPerceivedDistance metrics)}
    _{TimeMsg (metricsTime metrics)}
    $maybe d <- days
      (_{DaysMsg d}) #
    _{AscentMsg (metricsAscent metrics)}
    _{DescentMsg (metricsDescent metrics)}
  |]

daySummary :: TravelPreferences -> CaminoPreferences -> Maybe Trip -> Day -> HtmlUrlI18n CaminoMsg CaminoRoute
daySummary _preferences _camino _trip day = [ihamlet|
    <p>_{DaySummaryMsg day}
    <ol>
      $forall loc <- (map legFrom $ path day) ++ [finish day]
        <li>_{Txt (locationName loc)}
  |]

tripSummary :: TravelPreferences -> CaminoPreferences -> Trip -> HtmlUrlI18n CaminoMsg CaminoRoute
tripSummary _preferences _camino trip = [ihamlet|
    <h1>From _{Txt (locationName $ start trip)} to _{Txt (locationName $ finish trip)}
    <h2>Stages
    <ol>
      $forall day <- path trip
        <li>_{Txt (locationName $ start day)} - _{Txt (locationName $ finish day)}
  |]

locationSummary :: TravelPreferences -> CaminoPreferences -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
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

caminoPoiTypeMapIcon :: LocationType -> CaminoRoute
caminoPoiTypeMapIcon lt =
  let
    name = T.toLower $ T.pack $ show lt
  in
    IconRoute (T.concat ["poi-", name, ".png"])

caminoLocationTypeIcon :: LocationType -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationTypeIcon Village = [ihamlet| <span .location-type .ca-village title="_{VillageTitle}"> |]
caminoLocationTypeIcon Town = [ihamlet| <span .location-type .ca-town title="_{TownTitle}"> |]
caminoLocationTypeIcon City = [ihamlet| <span .location-type .ca-city title="_{CityTitle}"> |]
caminoLocationTypeIcon Bridge = [ihamlet| <span .location-type .ca-bridge title="_{BridgeTitle}"> |]
caminoLocationTypeIcon Intersection = [ihamlet| <span .location-type .ca-intersection title="_{IntersectionTitle}"> |]
caminoLocationTypeIcon Monastery = [ihamlet| <span .location-type .ca-monastery title="_{MonasteryTitle}"> |]
caminoLocationTypeIcon Peak = [ihamlet| <span .location-type .ca-peak title="_{PeakTitle}"> |]
caminoLocationTypeIcon Church = [ihamlet| <span .location-type .ca-church title="_{ChurchTitle}"> |]
caminoLocationTypeIcon Cathedral = [ihamlet| <span .location-type .ca-cathedral title="_{CathedralTitle}"> |]
caminoLocationTypeIcon Cross = [ihamlet| <span .location-type .ca-cross title="_{CrossTitle}"> |]
caminoLocationTypeIcon Fountain = [ihamlet| <span .location-type .ca-fountain title="_{FountainTitle}"> |]
caminoLocationTypeIcon Municipal = [ihamlet| <span .location-type .ca-municipal title="_{MunicipalTitle}"> |]
caminoLocationTypeIcon Museum = [ihamlet| <span .location-type .ca-museum title="_{MuseumTitle}"> |]
caminoLocationTypeIcon Historical = [ihamlet| <span .location-type .ca-historical title="_{HistoricalTitle}"> |]
caminoLocationTypeIcon Park = [ihamlet| <span .location-type .ca-park title="_{ParkTitle}"> |]
caminoLocationTypeIcon Natural = [ihamlet| <span .location-type .ca-natural title="_{NaturalTitle}"> |]
caminoLocationTypeIcon Hazard = [ihamlet| <span .location-type .ca-hazard title="_{HazardTitle}"> |]
caminoLocationTypeIcon _ = [ihamlet| <span .location-type .ca-poi title="_{PoiTitle}"> |]

caminoLocationTypeLabel :: LocationType -> CaminoMsg
caminoLocationTypeLabel Village = VillageTitle
caminoLocationTypeLabel Town = TownTitle
caminoLocationTypeLabel City = CityTitle
caminoLocationTypeLabel Bridge = BridgeTitle
caminoLocationTypeLabel Intersection = IntersectionTitle
caminoLocationTypeLabel Monastery = MonasteryTitle
caminoLocationTypeLabel Peak = PeakTitle
caminoLocationTypeLabel Church = ChurchTitle
caminoLocationTypeLabel Cathedral = CathedralTitle
caminoLocationTypeLabel Cross = CrossTitle
caminoLocationTypeLabel Fountain = FountainTitle
caminoLocationTypeLabel Municipal = MunicipalTitle
caminoLocationTypeLabel Museum = MuseumTitle
caminoLocationTypeLabel Historical = HistoricalTitle
caminoLocationTypeLabel Park = ParkTitle
caminoLocationTypeLabel Natural = NaturalTitle
caminoLocationTypeLabel Hazard = HazardTitle
caminoLocationTypeLabel Poi = PoiTitle

caminoLegTypeIcon :: LegType -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLegTypeIcon Trail = [ihamlet| <span .leg-type .ca-walking title="_{TrailTitle}"> |]
caminoLegTypeIcon CyclePath = [ihamlet| <span .leg-type .ca-cycling title="_{CyclePathTitle}"> |]
caminoLegTypeIcon Ferry = [ihamlet| <span .leg-type .ca-ferry title="_{FerryTitle}"> |]
caminoLegTypeIcon Boat = [ihamlet| <span .leg-type .ca-rowing title="_{BoatTitle}"> |]
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
caminoAccommodationTypeIcon PilgrimAlbergue = [ihamlet| <span .accommodation .municipal-albergue .ca-albergue title="_{PilgrimAlbergueTitle}"> |]
caminoAccommodationTypeIcon PrivateAlbergue = [ihamlet| <span .accommodation .private-albergue .ca-albergue title="_{PrivateAlbergueTitle}"> |]
caminoAccommodationTypeIcon Hostel = [ihamlet| <span .accommodation .hostel .ca-hostel title="_{HostelTitle}"> |]
caminoAccommodationTypeIcon GuestHouse = [ihamlet| <span .accommodation .guest-house .ca-guesthouse title="_{GuestHouseTitle}"> |]
caminoAccommodationTypeIcon HomeStay = [ihamlet| <span .accommodation .home-stay .ca-homestay title="_{HomeStayTitle}"> |]
caminoAccommodationTypeIcon House = [ihamlet| <span .accommodation .house .ca-house title="_{HouseTitle}"> |]
caminoAccommodationTypeIcon Hotel = [ihamlet| <span .accommodation .hotel .ca-hotel title="_{HotelTitle}"> |]
caminoAccommodationTypeIcon Gite = [ihamlet| <span .accommodation .gite .ca-gite title="_{GiteTitle}"> |]
caminoAccommodationTypeIcon CampGround = [ihamlet| <span .accommodation .camp-ground .ca-campground title="_{CampGroundTitle}"> |]
caminoAccommodationTypeIcon Camping = [ihamlet| <span .accommodation .camping .ca-tent title="_{CampingTitle}"> |]

caminoAccommodationTypeMsg :: AccommodationType -> CaminoMsg
caminoAccommodationTypeMsg PilgrimAlbergue = PilgrimAlbergueTitle
caminoAccommodationTypeMsg PrivateAlbergue = PrivateAlbergueTitle
caminoAccommodationTypeMsg Hostel = HostelTitle
caminoAccommodationTypeMsg GuestHouse = GuestHouseTitle
caminoAccommodationTypeMsg HomeStay = HomeStayTitle
caminoAccommodationTypeMsg House = HouseTitle
caminoAccommodationTypeMsg Hotel = HotelTitle
caminoAccommodationTypeMsg Gite = GiteTitle
caminoAccommodationTypeMsg CampGround = CampGroundTitle
caminoAccommodationTypeMsg Camping = CampingTitle

caminoAccommodationLabel :: Accommodation -> CaminoMsg
caminoAccommodationLabel (GenericAccommodation PilgrimAlbergue) = PilgrimAlbergueTitle
caminoAccommodationLabel (GenericAccommodation PrivateAlbergue) = PrivateAlbergueTitle
caminoAccommodationLabel (GenericAccommodation Hostel) = HostelTitle
caminoAccommodationLabel (GenericAccommodation GuestHouse) = GuestHouseTitle
caminoAccommodationLabel (GenericAccommodation HomeStay) = HomeStayTitle
caminoAccommodationLabel (GenericAccommodation House) = HouseTitle
caminoAccommodationLabel (GenericAccommodation Hotel) = HotelTitle
caminoAccommodationLabel (GenericAccommodation Gite) = GiteTitle
caminoAccommodationLabel (GenericAccommodation CampGround) = CampGroundTitle
caminoAccommodationLabel (GenericAccommodation Camping) = CampingTitle
caminoAccommodationLabel _ = AccommodationLabel

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

caminoServiceMsg :: Service -> CaminoMsg
caminoServiceMsg WiFi = WiFiTitle
caminoServiceMsg Restaurant = RestaurantTitle
caminoServiceMsg Pharmacy = PharmacyTitle
caminoServiceMsg Bank = BankTitle
caminoServiceMsg BicycleRepair = BicycleRepairTitle
caminoServiceMsg Groceries = GroceriesTitle
caminoServiceMsg Medical = MedicalTitle
caminoServiceMsg WashingMachine = WashingMachineTitle
caminoServiceMsg Dryer = DryerTitle
caminoServiceMsg Handwash = HandwashTitle
caminoServiceMsg Kitchen = KitchenTitle
caminoServiceMsg Breakfast = BreakfastTitle
caminoServiceMsg Dinner = DinnerTitle
caminoServiceMsg Lockers = LockersTitle
caminoServiceMsg Accessible = AccessibleTitle
caminoServiceMsg Stables = StablesTitle
caminoServiceMsg Pets = PetsTitle
caminoServiceMsg BicycleStorage = BicycleStorageTitle
caminoServiceMsg CampSite = CampSiteTitle
caminoServiceMsg Bedlinen = BedlinenTitle
caminoServiceMsg Towels = TowelsTitle
caminoServiceMsg Pool = PoolTitle
caminoServiceMsg Heating = HeatingTitle
caminoServiceMsg Prayer = PrayerTitle
caminoServiceMsg Train = TrainTitle
caminoServiceMsg Bus = BusTitle

caminoTravelMsg :: Travel -> CaminoMsg
caminoTravelMsg Walking = WalkingTitle
caminoTravelMsg Cycling = CyclingTitle

caminoTravelIcon :: Travel -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoTravelIcon Walking = [ihamlet| <span .travel .travel-walking .ca-walking title="_{WalkingTitle}"> |]
caminoTravelIcon Cycling = [ihamlet| <span .travel .travel-cycling .ca-cycling title="_{CyclingTitle}"> |]

caminoFitnessMsg :: Fitness -> CaminoMsg
caminoFitnessMsg SuperFit = SuperFitTitle
caminoFitnessMsg VeryFit = VeryFitTitle
caminoFitnessMsg Fit = FitTitle
caminoFitnessMsg Normal = NormalTitle
caminoFitnessMsg Unfit = UnfitTitle
caminoFitnessMsg VeryUnfit = VeryUnfitTitle

caminoFitnessLabel :: Fitness -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoFitnessLabel fitness = [ihamlet| <span .fitness>_{caminoFitnessMsg fitness} |]

caminoComfortMsg :: Comfort -> CaminoMsg
caminoComfortMsg Austere = AustereTitle
caminoComfortMsg Frugal = FrugalTitle
caminoComfortMsg Pilgrim = PilgrimTitle
caminoComfortMsg Comfortable = ComfortableTitle
caminoComfortMsg Luxurious = LuxuriousTitle

caminoComfortLabel :: Comfort -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoComfortLabel comfort = [ihamlet| <span .comfort>_{caminoComfortMsg comfort} |]

caminoAccommodationSummaryHtml :: Accommodation -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoAccommodationSummaryHtml a@(GenericAccommodation type') = [ihamlet|
    ^{caminoAccommodationTypeIcon type'} _{caminoAccommodationLabel a}
  |]
caminoAccommodationSummaryHtml a@(Accommodation _name type' services' sleeping') = [ihamlet|
    <span .accommodation>
      <span .pr-4>
        ^{caminoAccommodationTypeIcon type'} _{Txt (accommodationName a)}
      <span .p2-4>
        $forall service <- services'
           ^{caminoServiceIcon service}
      <span>
        $forall sleeping <- sleeping'
          ^{caminoSleepingIcon sleeping}
 |]


caminoAccommodationChoiceSummaryHtml :: Accommodation -> Metrics -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoAccommodationChoiceSummaryHtml accommodation metrics = [ihamlet|
    <span .accommodation>
      <span .pr-4>
        ^{caminoAccommodationTypeIcon type'} _{caminoAccommodationTypeMsg type'}
      $if hasServices
        <span .p2-4>
          \(
          $forall service <- services'
            ^{caminoServiceIcon service}
          )
  |]
  where
      type' = accommodationType accommodation
      services' = maybe S.empty (tripChoiceServices . fst) (L.uncons $ metricsAccommodationChoice metrics)
      hasServices = not (S.null services')

caminoAccommodationNameHtml :: Accommodation -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoAccommodationNameHtml (GenericAccommodation type') = [ihamlet|_{caminoAccommodationTypeMsg type'}|]
caminoAccommodationNameHtml (Accommodation name' _type  _services _sleeping) = [ihamlet|_{Txt name'}|]

caminoAccommodationHtml :: Accommodation -> Maybe (TripChoice Accommodation) ->HtmlUrlI18n CaminoMsg CaminoRoute
caminoAccommodationHtml accommodation choice = [ihamlet|
  <div .row .accommodation>
    <div .offset-1 .col-5>
      ^{caminoAccommodationTypeIcon type'}
      ^{caminoAccommodationNameHtml accommodation}
      $maybe ac <- choice'
        <span .best-accommodation>
          $if tripChoicePenance ac /= mempty
            <span .best-accommodation-penance>+_{PenanceFormatted (tripChoicePenance ac)}
    <div .col-4>
      $forall service <- accommodationServices accommodation
        ^{caminoServiceIcon service}
    <div .col-2>
      $forall sleeping <- accommodationSleeping accommodation
        ^{caminoSleepingIcon sleeping}
 |]
   where
     name' = accommodationNameLabel accommodation
     type' = accommodationType accommodation
     cp' = maybe Reject tripChoicePenance choice
     cn' = maybe "" (accommodationNameLabel . tripChoice) choice
     ct' = maybe Camping (accommodationType . tripChoice) choice
     choice' = if cp' /= Reject && type' == ct'  && name' == cn' then choice else Nothing

-- | Get elements of a possible solution
solutionElements :: Camino -> Maybe Solution -> (Either Location Trip, S.Set Location, S.Set Location, S.Set Leg)
solutionElements camino Nothing = (
    Left (head $ routeStarts $ caminoDefaultRoute camino),
    S.empty,
    S.fromList $ caminoLocationList camino,
    S.fromList $ caminoLegs camino
  )
solutionElements _camino (Just solution) = (
    trip',
    either (const S.empty) (S.fromList . tripStops) trip',
    either (const S.empty) (S.fromList . tripWaypoints) trip',
    either (const S.empty) (S.fromList . tripLegs) trip'
  ) where
    trip' = solutionTrip solution

descriptionNoteTypeIcon :: NoteType -> HtmlUrlI18n CaminoMsg CaminoRoute
descriptionNoteTypeIcon Information = [ihamlet| <span .note-type .ca-information title="_{InformationTitle}">|]
descriptionNoteTypeIcon Warning = [ihamlet| <span .note-type .ca-warning title="_{WarningTitle}">|]
descriptionNoteTypeIcon Calendar = [ihamlet| <span .note-type .ca-calendar title="_{CalendarTitle}">|]
descriptionNoteTypeIcon Hours = [ihamlet| <span .note-type .ca-clock title="_{HoursTitle}">|]
descriptionNoteTypeIcon Address = [ihamlet| <span .note-type .ca-globe title="_{AddressTitle}">|]

descriptionNote :: Note -> HtmlUrlI18n CaminoMsg CaminoRoute
descriptionNote note = [ihamlet|
  <div .note .#{nc}>
    <div .description-icon .float-start>^{descriptionNoteTypeIcon nt}
    _{TxtFormatted (noteText note)}
  |]
  where
    nt = noteType note
    nc = "note-" <> (T.toLower $ T.pack $ show nt)

descriptionLine :: Description -> HtmlUrlI18n CaminoMsg CaminoRoute
descriptionLine description = [ihamlet|
  _{Txt (descriptionSummary description)}
  |]

-- Only partial. This should be enclosed in a .row since it allows extra stuff to be added
descriptionBlock :: Bool -> Description -> HtmlUrlI18n CaminoMsg CaminoRoute
descriptionBlock showAbout description = [ihamlet|
    $maybe img <- mimg
      <div .description-thumbnail .card .float-end>
        <div .card-body>
          <img .rounded .img-fluid src="@{ImgRoute img}" alt="_{TxtPlain True False (imageTitle img)}" title="_{TxtPlain True False (imageTitle img)}" dc:rights="#{attribution}" onclick="showImagePopup('@{ImgRoute img}', '_{TxtPlain True True (imageTitle img)}', '#{attribution}')"">
    $if showAbout
      $maybe about <- descAbout description
        <div .float-start .description-icon>
          <a .about href="@{LinkRoute about}" title="_{LinkTitle about}">
            <span .ca-link>
    $maybe txt <- descText description
      _{TxtFormatted txt}
    $forall note <- descNotes description
      ^{descriptionNote note}
  |]
  where
    mimg = descImage description
    attribution = maybe "" imageAttribution mimg

locationLine :: TravelPreferences -> CaminoPreferences -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
locationLine _preferences _camino location = [ihamlet|
    _{Txt (locationName location)}
    <span .accommodation-types>
      $forall accommodation <- locationAccommodationTypes location
        ^{caminoAccommodationTypeIcon accommodation}
    <span .services>
      $forall service <- locationServices location
        ^{caminoServiceIcon service}
  |]

poiLine :: TravelPreferences -> CaminoPreferences -> PointOfInterest -> HtmlUrlI18n CaminoMsg CaminoRoute
poiLine _preferences _camino poi = [ihamlet|
    _{Txt (poiName poi)}
  |]

legLine :: TravelPreferences -> CaminoPreferences -> Leg -> HtmlUrlI18n CaminoMsg CaminoRoute
legLine _preferences _camino leg = [ihamlet|
    <div .d-inline-block>
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
      $if isJust $ legDescription leg
        <span .leg-description>^{descriptionLine (fromJust $ legDescription leg)}
  |]

locationLegLine :: TravelPreferences -> Bool -> Bool -> CaminoPreferences -> Leg -> HtmlUrlI18n CaminoMsg CaminoRoute
locationLegLine preferences showLink showTo camino leg = [ihamlet|
   $if showLink
     <a href="##{locid}" onclick="showLocationDescription('#{locid}');">
       _{Txt (locationName $ direction leg)}
   $else
     _{Txt (locationName $ direction leg)}
   ^{legLine preferences camino leg}
 |]
 where
   direction = if showTo then legTo else legFrom
   locid = locationID $ direction leg

locationLegSummary :: TravelPreferences ->CaminoPreferences -> S.Set Leg -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
locationLegSummary preferences camino used location = [ihamlet|
  $forall leg <- usedLegs
    <div .row>
      <div .col .leg-to .leg-line .leg-used .offset-1>
        ^{locationLegLine preferences False True camino leg}
  $forall leg <- unusedLegs
      <div .row>
        <div .col .leg-to .leg-line .leg-unused .offset-1>
          ^{locationLegLine preferences False True camino leg}
 |]
  where
    camino' = preferenceCamino camino
    outgoingLegs = outgoing camino' location
    (usedLegs, unusedLegs) = L.partition (\l -> S.member l used) outgoingLegs


locationLegs :: TravelPreferences -> CaminoPreferences -> S.Set Leg -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
locationLegs preferences camino used location = [ihamlet|
  <div .row>
    <div .col .text-start .my-auto>
      <ul>
        $forall leg <- usedIncomingLegs
          <li .leg-to .leg-line .leg-used>
            ^{locationLegLine preferences True False camino leg}
        $forall leg <- unusedIncomingLegs
          <li .leg-to .leg-line .leg-unused>
            ^{locationLegLine preferences True False camino leg}
    <div .col .text-end .my-auto>
      <ul>
        $forall leg <- usedOutgoingLegs
          <li .leg-to .leg-line .leg-used>
            ^{locationLegLine preferences True True camino leg}
        $forall leg <- unusedOutgoingLegs
          <li .leg-to .leg-line .leg-unused>
            ^{locationLegLine preferences True True camino leg}
 |]
  where
    camino' = preferenceCamino camino
    outgoingLegs = outgoing camino' location
    (usedOutgoingLegs, unusedOutgoingLegs) = L.partition (\l -> S.member l used) outgoingLegs
    incomingLegs = incoming camino' location
    (usedIncomingLegs, unusedIncomingLegs) = L.partition (\l -> S.member l used) incomingLegs

caminoPointOfInterestHtml :: PointOfInterest -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoPointOfInterestHtml poi = [ihamlet|
  <div .row>
    <div .card .g-0>
      <div .card-header>
        <span .poi-types>
          ^{caminoLocationTypeIcon (poiType poi)}
        _{Txt (poiName poi)}
        $maybe pos <- poiPosition poi
          <a .description-icon .float-end onclick="showLocationOnMap(#{latitude pos}, #{longitude pos})">
            <span .ca-globe title="_{ShowOnMapTitle}">
        $maybe d <- poiDescription poi
          $maybe about <- descAbout d
            <a .description-icon .about .float-end href="@{LinkRoute about}" title="_{LinkTitle about}">
              <span .ca-link>
      <div .card-body>
          $maybe d <- poiDescription poi
            ^{descriptionBlock False d}
  |]

caminoLocationHtml :: TravelPreferences -> CaminoPreferences -> Maybe Solution -> String -> S.Set Location -> S.Set Location -> S.Set Leg -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationHtml preferences camino solution containerId stops waypoints used location = [ihamlet|
  <div id="#{lid}" .accordion-item .location-#{routeID route} :isStop:.location-stop :isWaypoint:.location-waypoint .location>
    <div .accordion-header>
      <div .row>
        <button .accordion-button .collapsed data-bs-toggle="collapse" data-bs-target="#location-body-#{lid}" aria-expanded="false" aria-controls="location-body-#{lid}">
          <h5 .col-5>
            ^{caminoLocationTypeIcon (locationType location)}
            _{Txt (locationName location)}
          <div .col-2 .services>
            $forall service <- locationServices location
              ^{caminoServiceIcon service}
          <div .col-2 .accommodation-types>
            $forall accommodation <- locationAccommodationTypes location
              ^{caminoAccommodationTypeIcon accommodation}
          <div .col-2 .poi-types>
            $forall poi <- locationPoiTypes location
              ^{caminoLocationTypeIcon poi}
          <div .col-1>
            $maybe pos <- locationPosition location
              <a .show-on-map onclick="showLocationOnMap(#{latitude pos}, #{longitude pos})">
                <span .ca-globe title="_{ShowOnMapTitle}">
    <div id="location-body-#{lid}" .accordion-collapse .collapse aria-labelledby="location-heading-#{lid}" data-parent="##{containerId}">
      <div .accordion-body .container-fluid>
        ^{locationLegs preferences camino used location}
        $maybe d <- locationDescription location
          <div .row>
            <div .col>
              ^{descriptionBlock True d}
        ^{conditionalLabel AccommodationLabel (locationAccommodation location)}
        $forall accommodation <- locationAccommodation location
          ^{caminoAccommodationHtml accommodation accChoice}
        ^{conditionalLabel PoisLabel (locationPois location)}
        $forall poi <- locationPois location
          ^{caminoPointOfInterestHtml poi}
  |]
  where
    camino' = preferenceCamino camino
    lid = locationID location
    route = caminoRoute camino' (preferenceRoutes camino) location
    isStop = S.member location stops
    isWaypoint = (not isStop) && (S.member location waypoints)
    accChoice = maybe Nothing (\s -> M.lookup location (solutionAccommodation s)) solution
    
caminoLocationsHtml :: TravelPreferences -> CaminoPreferences -> Maybe Solution -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationsHtml preferences camino solution = [ihamlet|
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
                  <a .dropdown-item href="##{locationID loc}">_{Txt (locationName loc)}
    <div .row>
      <div .col>
        <div #locations .accordion .container-fluid>
          $forall loc <- locationsSorted
            <div .row>
              ^{caminoLocationHtml preferences camino solution "locations" stops waypoints usedLegs loc}
  |]
  where
    camino' = preferenceCamino camino
    locationOrder a b = compare (canonicalise $ locationNameLabel a) (canonicalise $ locationNameLabel b)
    locationsSorted = L.sortBy locationOrder (caminoLocationList camino')
    locationPartition = partition (\l -> T.toUpper $ canonicalise $ T.take 1 $ locationNameLabel l) locationsSorted
    (_trip, stops, waypoints, usedLegs) = solutionElements camino' solution

preferenceRangeHtml :: (Real a) => PreferenceRange a -> HtmlUrlI18n CaminoMsg CaminoRoute
preferenceRangeHtml range = [ihamlet|
    <span .text-danger>#{maybe "." (format (fixed 1)) (rangeMinimum range)} -
    <span>#{format (fixed 1)( rangeLower range)} -
    <span .text-success .fw-bolder>#{format (fixed 1) (rangeTarget range)} -
    <span>#{format (fixed 1) (rangeUpper range)} -
    <span .text-danger>#{maybe "." (format (fixed 1)) (rangeMaximum range)}
    $maybe d <- rangeDerived range
      <p .text-body-tertiary .smaller>#{d}
  |]
  
preferencesHtml :: Bool -> TravelPreferences -> CaminoPreferences -> HtmlUrlI18n CaminoMsg CaminoRoute
preferencesHtml showLink preferences camino = [ihamlet|
  <div .container-fluid>
    <div .row>
      <div .col-4>_{TravelLabel}
      <div .col>^{caminoTravelIcon $ preferenceTravel preferences} _{caminoTravelMsg $ preferenceTravel preferences}
    <div .row>
      <div .col-4>_{FitnessLabel}
      <div .col>^{caminoFitnessLabel $ preferenceFitness preferences}
    <div .row>
      <div .col-4>_{ComfortLabel}
      <div .col>^{caminoComfortLabel $ preferenceComfort preferences}
    <div .row>
      <div .col-4>_{DistancePreferencesLabel}
      <div .col>^{preferenceRangeHtml $ preferenceDistance preferences}
    <div .row>
      <div .col-4>_{TimePreferencesLabel}
      <div .col>^{preferenceRangeHtml $ preferenceTime preferences}
    <div .row>
      <div .col-4>_{LocationPreferencesLabel}
    $forall lk <- locationTypes
      <div .row>
        <div .col-3 .offset-1>
          <span .location-type-sample>^{caminoLocationTypeIcon lk}
          \ _{caminoLocationTypeLabel lk}
        <div .col>_{PenanceFormatted (findLoc preferences lk)}
    <div .row>
      <div .col-4>_{AccommodationPreferencesLabel}
    $forall ak <- accommodationTypes
      <div .row>
        <div .col-3 .offset-1>
          ^{caminoAccommodationTypeIcon ak}
          \ _{caminoAccommodationTypeMsg ak}
        <div .col>_{PenanceFormatted (findAcc preferences ak)}
    <div .row>
      <div .col-4>_{StopServicesPreferencesLabel}
    $forall sk <- M.keys $ preferenceStopServices preferences
      <div .row>
        <div .col-3 .offset-1>
          ^{caminoServiceIcon sk}
          \ _{caminoServiceMsg sk}
        <div .col>_{PenanceFormatted (findSs preferences sk)}
    <div .row>
      <div .col-4>_{DayServicesPreferencesLabel}
    $forall sk <- M.keys $ preferenceDayServices preferences
      <div .row>
        <div .col-3 .offset-1>
          ^{caminoServiceIcon sk}
          \ _{caminoServiceMsg sk}
        <div .col>_{PenanceFormatted (findDs preferences sk)}
    <div .row>
      <div .col-4>_{RouteLabel}
      <div .col>
        <ul .bar-separated-list>
          $forall r <- selectedRoutes camino
            <li>
              $if showLink
                <a href="##{routeID r}" data-toggle="tab" onclick="showRouteDescription('#{routeID r}')">_{Txt (routeName r)}
              $else
                _{Txt (routeName r)}
    <div .row>
      <div .col-4>_{TripStartLabel}
      <div .col>
        $with start <- preferenceStart camino
          $if showLink
            <a href="##{locationID start}" data-toggle="tab" onclick="showLocationDescription('#{locationID start}')">_{Txt (locationName start)}
          $else
            _{Txt (locationName start)}
    <div .row>
      <div .col-4>_{TripFinishLabel}
      <div .col>
        $with finish <- preferenceFinish camino
          $if showLink
            <a href="##{locationID finish}" data-toggle="tab" onclick="showLocationDescription('#{locationID finish}')">_{Txt (locationName finish)}
          $else
            _{Txt (locationName finish)}
    <div .row>
      <div .col-4>_{RequiredStopsLabel}
      <div .col>
        <ul .bar-separated-list>
          $forall l <- preferenceStops camino
            <li>
              $if showLink
                <a href="##{locationID l}" data-toggle="tab" onclick="showLocationDescription('#{locationID l}')">_{Txt (locationName l)}
              $else
                _{Txt (locationName l)}
    <div .row>
      <div .col-4>_{ExcludedStopsLabel}
      <div .col>
        <ul .bar-separated-list>
          $forall l <- preferenceExcluded camino
            <li>
              $if showLink
                <a href="##{locationID l}" data-toggle="tab" onclick="showLocationDescription('#{locationID l}')">_{Txt (locationName l)}
              $else
                _{Txt (locationName l)}
  |]
  where
    locationTypes = locationStopTypeEnumeration
    accommodationTypes = accommodationTypeEnumeration
    findAcc prefs ak = M.findWithDefault mempty ak (preferenceAccommodation prefs)
    findLoc prefs lk = M.findWithDefault mempty lk (preferenceLocation prefs)
    findSs prefs sk = (preferenceStopServices prefs) M.! sk
    findDs prefs sk = (preferenceDayServices prefs) M.! sk

caminoTripHtml :: TravelPreferences -> CaminoPreferences -> Trip -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoTripHtml preferences camino trip = [ihamlet|
  <div .container-fluid>
    <div .row .trip-summary>
      <div .col>
        <p>
          _{Txt (locationName $ start trip)}
          $forall l <- map finish $ path trip
            \  - <a href="#leg-#{locationID l}">_{Txt (locationName l)}
        <p>
          ^{metricsSummary preferences camino (score trip) (Just $ length $ path trip)}
          _{PenanceMsg (metricsPenance (score trip))}
    $forall day <- path trip
      <div .card .day .p-1>
        <h4 id="leg-#{locationID (finish day)}">
          <a href="@{LocationRoute (start day)}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">_{Txt (locationName $ start day)}
          \   -
          <a href="@{LocationRoute (finish day)}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">_{Txt (locationName $ finish day)}
          _{DistanceFormatted (metricsDistance $ score day)}
        <div .card-body>
         <p>
            ^{metricsSummary preferences camino (score day) Nothing}
            <br>
            ^{penanceSummary preferences camino False True $ score day}
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
              ^{caminoAccommodationChoiceSummaryHtml (tripChoice accom) (score day)}
   |]

caminoMapHtml :: TravelPreferences -> CaminoPreferences -> Maybe Solution -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapHtml _preferences _camino _solution = [ihamlet|
  <div .container-fluid>
    <div .row>
      <div .col .d-flex .justify-content-center>
        <div #map>
  |]

caminoMapKeyHtml :: TravelPreferences -> CaminoPreferences -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapKeyHtml _preferences camino = [ihamlet|
<div #map-key .card>
  <div .card-body>
    <h2 .card-title>_{MapLabel}
    <table .table-striped>
      <tbody>
        $forall r <- caminoRoutes camino'
          <tr>
            <td>
            <td .border .border-light style="background-color: #{toCssColour $ paletteColour $ routePalette r}">
            <td>
            <td>
            <td>_{Txt (routeName r)}
        $forall lt <- locationTypeEnumeration
          <tr>
            <td>
              <img .map-key-icon src="@{caminoLocationTypeMapIcon lt True False}" title="_{StopLabel}">
            <td>
              <img .map-key-icon src="@{caminoLocationTypeMapIcon lt False True}" title="_{WaypointLabel}">
            <td>
              <img .map-key-icon src="@{caminoLocationTypeMapIcon lt False False}" title="_{UnusedLabel}">
            <td>
              <img .map-key-icon src="@{caminoPoiTypeMapIcon lt}" title="_{PoiLabel}">
            <td>
              _{caminoLocationTypeLabel lt}
  |]
  where
    camino' = preferenceCamino camino

caminoLocationIcon :: TravelPreferences -> CaminoPreferences -> S.Set Location -> S.Set Location -> Location -> String
caminoLocationIcon _preferences _camino stops waypoints location =
  "icon" ++ (show $ locationType location) ++ (status location)
   where
    status loc
     | S.member loc stops = "Stop"
     | S.member loc waypoints = "Used"
     | otherwise = "Unused"

caminoPoiIcon :: TravelPreferences -> CaminoPreferences -> PointOfInterest -> String
caminoPoiIcon _preferences _camino poi =
  "icon" ++ (show $ poiType poi) ++ "Poi"

caminoLocationTooltip :: TravelPreferences -> CaminoPreferences -> Maybe Solution -> S.Set Leg -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationTooltip preferences camino _solution usedLegs location = [ihamlet|
  <div .location-tooltip .container-fluid>
    <div .row>
      <div .col>
        ^{locationLine preferences camino location}
    ^{locationLegSummary preferences camino usedLegs location}
  |]

caminoPoiTooltip :: TravelPreferences -> CaminoPreferences -> PointOfInterest -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoPoiTooltip preferences camino poi = [ihamlet|
  <div .location-tooltip .container-fluid>
    <div .row>
      <div .col>
        ^{poiLine preferences camino poi}
  |]

caminoMapScript :: TravelPreferences -> CaminoPreferences -> Maybe Solution -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapScript preferences camino solution = [ihamlet|
  <script>
    var map = L.map('map');

    function showRouteDescription(id) {
      \$('#about-toggle').tab('show');
      \$('#' + id).get(0).scrollIntoView({behavior: 'smooth'});
    }
    
    function openLocationDescription(id) {
      \$('#location-body-' + id).addClass('show');
    }

    function showLocationDescription(id) {
      \$('#locations-toggle').tab('show');
      \$('#' + id).get(0).scrollIntoView({behavior: 'smooth'});
      openLocationDescription(id);
    }

    function showLocationOnMap(lat, lng) {
      \$('#map-toggle').tab('show');
      map.fitBounds([ [lat + 0.01, lng - 0.01], [lat - 0.01, lng + 0.01] ]);
    }

    function selectZoom(map, locations, legs, pois) {
      var zoom = map.getZoom();
      var showLocations = zoom > 8;
      var showLegs = true;
      var showPois = zoom > 12;
      if (showPois && !map.hasLayer(pois))
          map.addLayer(pois);
      if (!showPois && map.hasLayer(pois))
          map.removeLayer(pois);
      if (showLegs && !map.hasLayer(legs))
          map.addLayer(legs);
      if (!showLegs && map.hasLayer(legs))
          map.removeLayer(legs);
      if (showLocations && !map.hasLayer(locations))
          map.addLayer(locations);
      if (!showLocations && map.hasLayer(locations))
          map.removeLayer(locations);
    }

    $forall icon <- icons
      $with name <- T.pack $ show $ fst icon
        var icon#{name}Used = L.icon({
          iconUrl: '@{caminoLocationTypeMapIcon (fst icon) False True}',
          iconSize: [#{fst (snd icon)}, #{snd (snd icon)}]
        });
        var icon#{name}Unused = L.icon({
          iconUrl: '@{caminoLocationTypeMapIcon (fst icon) False False}',
          iconSize: [#{fst (snd icon)}, #{snd (snd icon)}]
        });
        var icon#{name}Stop = L.icon({
          iconUrl: '@{caminoLocationTypeMapIcon (fst icon) True True}',
          iconSize: [#{fst (snd icon)}, #{snd (snd icon)}]
        });
        var icon#{name}Poi = L.icon({
          iconUrl: '@{caminoPoiTypeMapIcon (fst icon)}',
          iconSize: [#{fst (snd icon)}, #{snd (snd icon)}]
        });
    map.fitBounds([ [#{latitude tl}, #{longitude tl}], [#{latitude br}, #{longitude br}] ]);
    L.tileLayer('@{MapTileRoute}', {
        maxZoom: 19,
        attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
    }).addTo(map);
    var locations = L.layerGroup();
    var legs = L.layerGroup();
    var pois = L.layerGroup();
    var marker;
    var line;
    $forall location <- M.elems $ caminoLocations camino'
      $maybe position <- locationPosition location
        marker = L.marker([#{latitude position}, #{longitude position}], { icon: #{caminoLocationIcon preferences camino stops waypoints location} } );
        marker.bindTooltip(`^{caminoLocationTooltip preferences camino solution usedLegs location}`);
        marker.addTo(locations);
        marker.on('click', function(e) { showLocationDescription("#{locationID location}"); } );
      $forall poi <- locationPois location
        $maybe position <- poiPosition poi
          marker = L.marker([#{latitude position}, #{longitude position}], { icon: #{caminoPoiIcon preferences camino poi} } );
          marker.bindTooltip(`^{caminoPoiTooltip preferences camino poi}`);
          marker.addTo(pois);
          marker.on('click', function(e) { showLocationDescription("#{locationID location}"); } );
    $forall leg <- caminoLegs camino'
      $if isJust (locationPosition $ legFrom leg) && isJust (locationPosition $ legTo leg)
        line = L.polyline([
          [#{maybe 0.0 latitude (locationPosition $ legFrom leg)}, #{maybe 0.0 longitude (locationPosition $ legFrom leg)}],
          [#{maybe 0.0 latitude (locationPosition $ legTo leg)}, #{maybe 0.0 longitude (locationPosition $ legTo leg)}]
        ], {
           color: '#{toCssColour $ paletteColour $ routePalette $ caminoLegRoute camino' leg}',
           weight: #{chooseWidth leg},
           opacity: #{chooseOpacity leg}
        });
        line.addTo(legs);
    map.on("zoomend", function() {
      selectZoom(map, locations, legs, pois);
    });
    locations.setZIndex(30);
    legs.setZIndex(20);
    pois.setZIndex(10);
    selectZoom(map, locations, legs, pois);
  |]
  where
    camino' = preferenceCamino camino
    (tl, br) = caminoBbox camino'
    (_trip, stops, waypoints, usedLegs) = solutionElements camino' solution
    chooseWidth leg | S.member leg usedLegs = 7 :: Int
      | otherwise = 5 :: Int
    chooseOpacity leg | S.member leg usedLegs = 1.0 :: Float
      | otherwise = 0.5 :: Float
    icons = [
        (Village, (16, 16))
      , (Town, (32, 20))
      , (City, (32, 25))
      , (Monastery, (24, 22))
      , (Bridge, (24, 9))
      , (Intersection, (24, 22))
      , (Peak, (20, 18))
      , (Church, (24, 21))
      , (Cathedral, (19, 24))
      , (Cross, (15, 24))
      , (Fountain, (19, 24))
      , (Municipal, (24, 21))
      , (Museum, (24, 22))
      , (Historical, (24, 17))
      , (Park, (24, 25))
      , (Natural, (24, 25))
      , (Hazard, (20, 18))
      , (Poi, (15, 24))
      ] :: [(LocationType, (Int, Int))]


aboutHtml :: TravelPreferences -> CaminoPreferences -> HtmlUrlI18n CaminoMsg CaminoRoute
aboutHtml _preferences camino = [ihamlet|
  <div .container-fluid>
    <h2>_{Txt (caminoName camino')}
    <div .row>
      <div .col>
        ^{descriptionBlock True (caminoDescription camino')}
    <h3>_{RoutesLabel}
    $forall route <- caminoRoutes camino'
      <div ##{routeID route} .row>
        <div .col>
          <span style="color: #{toCssColour $ paletteColour $ routePalette route}">_{Txt (routeName route)}
          ^{descriptionBlock True (routeDescription route)}
    <h2>_{InformationLabel}
    <p>_{InformationDescription}
    $with metadata <- caminoMetadata camino'
      $forall statement <- metadataStatements metadata
        <div .row>
          <div .col-1>
            <a href="#{show $ statementTerm statement}">#{statementLabel statement}
            $maybe l <- statementLang statement
              <span>#{l}
          <div .col>
            #{statementValue statement}
  |]
  where
    camino' = preferenceCamino camino


layoutHtml :: Config -- ^ The configuration to use when inserting styles, scripts, paths etc.
 -> Localised TaggedText -- ^ The page title
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
         <title>_{Txt title}
         <link rel="icon" type="image/x-icon" href="@{IconRoute "favicon.ico"}">
         $forall c <- css
           <link rel="stylesheet" href="#{assetPath c}">
         $forall s <- scriptsHeader
           <script src="#{assetPath s}">
         $maybe h <- header
           ^{h}
       <body>
         <header .p-2>
           <nav .navbar .navbar-expand-md>
             <div .container-fluid>
               <a .navbar-brand href="#">
                 <img width="64" height="64" src="@{IconRoute "tile-64.png"}" alt="Camino Planner">
               <h1>_{Txt title}
               <div .collapse .navbar-collapse .d-flex .justify-content-end #navcol-links">
                 <ul .navbar-nav>
         <main .container-fluid .p-2>
           ^{body}
         <footer .text-center .py-4 .px-2>
           <div .row .row-cols-1 .row-cols-lg-3>
             <div .col>
               <p .text-muted .my-2>
                 <a href="https://github.com/charvolant/de-calixtinus">De Calixtinus
             <div .col>
               <p .text-muted .my-2>
             <div .col>
               <p .text-muted .my-2>Example only
         $forall s <- scriptsFooter
           <script src="#{assetPath s}">
         $maybe f <- footer
          ^{f}
     |]
     where
       css = getAssets Css config
       scriptsHeader = getAssets JavaScriptEarly config
       scriptsFooter = getAssets JavaScript config


keyHtml :: Config -> TravelPreferences -> CaminoPreferences -> HtmlUrlI18n CaminoMsg CaminoRoute
keyHtml _config preferences camino = $(ihamletFile "templates/help/key-en.hamlet")

helpHtml :: Config -> HtmlUrlI18n CaminoMsg CaminoRoute
helpHtml _config = $(ihamletFile "templates/help/help-en.hamlet")

caminoHtmlBase :: Config -> TravelPreferences -> CaminoPreferences -> Maybe Solution -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoHtmlBase config preferences camino solution =
  [ihamlet|
      <style>
        $forall css <- caminoCss config (preferenceCamino camino)
          #{renderCss css }
      <div>
        <ul .nav .nav-tabs role="tablist">
          <li .nav-item role="presentation">
            <a #map-toggle .nav-link .active role="tab" data-bs-toggle="tab" href="#map-tab">_{MapLabel}
          $maybe _t <- trip
            <li .nav-item role="presentation">
              <a .nav-link role="tab" data-bs-toggle="tab" href="#plan-tab">_{PlanLabel}
          <li .nav-item role="presentation">
            <a #locations-toggle .nav-link role="tab" data-bs-toggle="tab" href="#locations-tab">_{LocationsLabel}
          <li .nav-item role="presentation">
            <a #preferences-toggle .nav-link role="tab" data-bs-toggle="tab" href="#preferences-tab">_{PreferencesLabel}
          <li .nav-item role="presentation">
            <a #about-toggle .nav-link role="tab" data-bs-toggle="tab" href="#about-tab">_{AboutLabel}
          <li .nav-item role="presentation">
            <a #key-toggle .nav-link role="tab" data-bs-toggle="tab" href="#key-tab">_{KeyLabel}
        <div .tab-content>
          <div .tab-pane .active role="tabpanel" id="map-tab">
            ^{caminoMapHtml preferences camino solution}
          $maybe t <- trip
            <div .tab-pane role="tabpanel" id="plan-tab">
              ^{caminoTripHtml preferences camino t}
          <div .tab-pane role="tabpanel" id="locations-tab">
            ^{caminoLocationsHtml preferences camino solution}
          <div .tab-pane role="tabpanel" id="preferences-tab">
            ^{preferencesHtml True preferences camino}
          <div .tab-pane role="tabpanel" id="about-tab">
            ^{aboutHtml preferences camino}
          <div .tab-pane role="tabpanel" id="key-tab">
            ^{keyHtml config preferences camino}
    ^{caminoMapScript preferences camino solution}
  |]
  where
    trip = maybe Nothing (\s -> either (const Nothing) Just (solutionTrip s)) solution

-- | Display a camino wihout a chosen route
caminoHtmlSimple :: Config -> CaminoPreferences -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoHtmlSimple config camino =
  [ihamlet|
      <style>
        $forall css <- caminoCss config (preferenceCamino camino)
          #{renderCss css }
      <div .row>
        <div .col .m-1>
          ^{descriptionBlock True (caminoDescription camino')}
      <div .row>
        <div .col>
          <div>
            <ul .nav .nav-tabs role="tablist">
              <li .nav-item role="presentation">
                <a #map-toggle .nav-link .active role="tab" data-bs-toggle="tab" href="#map-tab">_{MapLabel}
              <li .nav-item role="presentation">
                <a #locations-toggle .nav-link role="tab" data-bs-toggle="tab" href="#locations-tab">_{LocationsLabel}
              <li .nav-item role="presentation">
                <a #about-toggle .nav-link role="tab" data-bs-toggle="tab" href="#about-tab">_{AboutLabel}
              <li .nav-item role="presentation">
                <a #key-toggle .nav-link role="tab" data-bs-toggle="tab" href="#key-tab">_{KeyLabel}
            <div .tab-content>
              <div .tab-pane .active role="tabpanel" id="map-tab">
                ^{caminoMapHtml preferences camino Nothing}
              <div .tab-pane role="tabpanel" id="locations-tab">
                ^{caminoLocationsHtml preferences camino Nothing}
              <div .tab-pane role="tabpanel" id="about-tab">
                ^{aboutHtml preferences camino}
              <div .tab-pane role="tabpanel" id="key-tab">
                ^{keyHtml config preferences camino}
        ^{caminoMapScript preferences camino Nothing}
  |]
  where
    camino' = preferenceCamino camino
    preferences = defaultTravelPreferences Walking Normal Pilgrim


caminoHtml :: Config -> TravelPreferences -> CaminoPreferences -> Solution -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoHtml config preferences camino solution = let
    title = caminoName $ preferenceCamino camino
  in
    layoutHtml config title Nothing (caminoHtmlBase config preferences camino (Just solution)) Nothing
