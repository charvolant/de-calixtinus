{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Control.Monad.Reader
import Camino.Camino
import Camino.Config (Config(..), AssetConfig(..), AssetType(..), getAssets, getDebug)
import Camino.Planner (TripChoice(..), TripChoiceMap, TripChoices(..), Solution(..), Day, Journey, Metrics(..), Pilgrimage, dayLegs, journeyLegs, pilgrimageLegs, pilgrimageRests, pilgrimageStockpoints, pilgrimageStops, pilgrimageWaypoints)
import Camino.Preferences
import Camino.Display.Css (caminoCss, toCssColour)
import Camino.Display.I18n
import Camino.Display.Routes
import Camino.Display.SVG
import Data.Colour (blend)
import Data.Colour.Names (lightgrey)
import Data.Description
import Data.Event
import Data.Event.Date
import qualified Data.List as L
import Data.Localised
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Metadata
import Data.Region
import qualified Data.Set as S
import Data.Summary
import Data.Util
import Graph.Graph (Edge(..), incoming, outgoing)
import Text.Cassius (renderCss)
import Text.Hamlet
import qualified Data.Text as T (Text, concat, intercalate, null, pack, replace, take, toLower, toUpper)
import Formatting

-- A useful separator
endash :: Char
endash = '\x2013'

-- Make a javascript string out of possible text
javascriptString :: Maybe T.Text -> T.Text
javascriptString mtxt = maybe "null" (\t -> "'" <> (T.replace "\'" "\\'" $ T.replace "\"" "\'" t) <> "'") mtxt

-- Show a checkmark if something is true
checkMark :: Bool -> T.Text
checkMark True = "\x2714"
checkMark False = ""

-- | Include a label row if some values are non-empty
conditionalLabel :: CaminoMsg -> [a] -> HtmlUrlI18n CaminoMsg CaminoRoute
conditionalLabel label values = [ihamlet|
  $if not $ null values
    <div .row>
      <div .col>
        <h6 .pt-3>_{label}
  |]

penanceTable :: TravelPreferences -> CaminoPreferences -> Bool -> Bool -> Metrics -> HtmlUrlI18n CaminoMsg CaminoRoute
penanceTable _preferences _camino accommodationDetail serviceDetail metrics = [ihamlet|
  <div .detail .border .border-info-subtle .rounded .penance-table .float-end .ms-1 .p-1>
   <table .table .table-sm>
     <tr>
      <td>_{DistancePenanceLabel}
      <td .text-end>#{maybe "-" formatDistance (metricsPerceivedDistance metrics)}
      <td>
     $if metricsDistanceAdjust metrics /= mempty
      <tr>
        <td>_{DistanceAdjustLabel}
        <td .text-end>_{PenanceFormatted True (metricsDistanceAdjust metrics)}
        <td>
     $if metricsTimeAdjust metrics /= mempty
      <tr>
        <td>_{TimeAdjustLabel}
        <td .text-end>_{PenanceFormatted True (metricsTimeAdjust metrics)}
        <td>
     $if metricsStop metrics /= mempty
      <tr>
        <td>_{StopPenanceLabel}
        <td .text-end>_{PenanceFormatted True (metricsStop metrics)}
        <td>
     $if metricsLocation metrics /= mempty
      <tr>
        <td>_{LocationPenanceLabel}
        <td .text-end>_{PenanceFormatted True (metricsLocation metrics)}
        <td>
     $if showAccommodation
      <tr>
        <td>_{AccommodationPenanceLabel}
        <td .text-end>_{PenanceFormatted True (metricsAccommodation metrics)}
        <td>
          $if hasAccommodationServices && accommodationDetail
            $forall service <- acServices
              ^{caminoServiceIcon service}
     $if metricsStopServices metrics /= mempty
       <tr>
         <td>_{StopServicesPenanceLabel}
         <td .text-end>_{PenanceFormatted True (metricsStopServices metrics)}
         <td>
           $if serviceDetail
             $forall service <- metricsMissingStopServices metrics
               ^{caminoServiceIcon service}
     $if metricsDayServices metrics /= mempty
       <tr>
         <td>_{DayServicesPenanceLabel}
         <td .text-end>_{PenanceFormatted True (metricsDayServices metrics)}
         <td>
           $if serviceDetail
             $forall service <- metricsMissingDayServices metrics
               ^{caminoServiceIcon service}
     $if metricsRestPressure metrics /= mempty
       <tr>
        <td>_{RestPressurePenanceLabel}
        <td .text-end>_{PenanceFormatted True (metricsRestPressure metrics)}
        <td>
     $if metricsFatigue metrics /= mempty
      <tr>
        <td>_{FatiguePenanceLabel}
        <td .text-end>_{PenanceFormatted True (metricsFatigue metrics)}
        <td>
     $if metricsRest metrics /= mempty
      <tr>
        <td>_{RestPenanceLabel}
        <td .text-end>_{PenanceFormatted True (metricsRest metrics)}
        <td>
     $if metricsRestPoints metrics /= mempty
      <tr>
        <td>_{RestPointsPenanceLabel}
        <td .text-end>_{PenanceFormatted True (metricsRestPoints metrics)}
        <td>
     $if metricsMisc metrics /= mempty
      <tr>
        <td>_{MiscPenanceLabel}
        <td .text-end>_{PenanceFormatted True (metricsMisc metrics)}
        <td>
   |]
  where
    acServices = maybe S.empty (tripChoiceFeatures . fst) (L.uncons $ metricsAccommodationChoice metrics)
    hasAccommodationServices = accommodationDetail && not (S.null acServices)
    showAccommodation = metricsLocation metrics /= mempty || hasAccommodationServices

metricsSummary :: TravelPreferences -> CaminoPreferences -> Metrics -> Maybe Int -> Maybe Int -> HtmlUrlI18n CaminoMsg CaminoRoute
metricsSummary _preferences _camino metrics days stages = [ihamlet|
    _{DistanceMsg (metricsDistance metrics) (metricsPerceivedDistance metrics)}
    _{TimeMsg (metricsTime metrics)}
    _{PoiTime (metricsPoiTime metrics)} #
    $maybe d <- days
      \ _{DaysMsg d} #
    $maybe s <- stages
      \ _{StagesMsg s} #
    _{AscentMsg (metricsAscent metrics)}
    _{DescentMsg (metricsDescent metrics)}
    _{PenanceMsg (metricsPenance metrics)}
  |]

daySummary :: TravelPreferences -> CaminoPreferences -> Maybe Pilgrimage -> Day -> HtmlUrlI18n CaminoMsg CaminoRoute
daySummary _preferences _camino _pilgrimage day = [ihamlet|
    <p>_{DaySummaryMsg day}
    <ol .bar-separated-list>
      $forall loc <- (map legFrom $ path day) ++ [finish day]
        <li>_{Txt (locationName loc)}
  |]

stageSummary :: TravelPreferences -> CaminoPreferences -> Maybe Pilgrimage -> Journey -> HtmlUrlI18n CaminoMsg CaminoRoute
stageSummary _preferences _camino _pilgrimage stage = [ihamlet|
    <p>_{JourneySummaryMsg stage}
    <ol .bar-separated-list>
      $forall loc <- (map start $ path stage) ++ [finish stage]
        <li>_{Txt (locationName loc)}
  |]

tripSummary :: TravelPreferences -> CaminoPreferences -> Pilgrimage -> HtmlUrlI18n CaminoMsg CaminoRoute
tripSummary _preferences _camino pilgrimage = [ihamlet|
    <h1>From _{Txt (locationName $ start pilgrimage)} to _{Txt (locationName $ finish pilgrimage)}
    <h2>Stages
    <ol>
      $forall stage <- path pilgrimage
        $forall day <- path stage
          <li>_{Txt (locationName $ start day)} - _{Txt (locationName $ finish day)}
        <li>_{RestpointLabel}
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

caminoEventTypeIcon :: EventType -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoEventTypeIcon Religious = [ihamlet| <span .event-type .event-religious .ca-bishop title="_{ReligiousEventTitle}"> |]
caminoEventTypeIcon Food = [ihamlet| <span .event-type .event-food .ca-food title="_{FoodEventTitle}"> |]
caminoEventTypeIcon Music = [ihamlet| <span .event-type .event-music .ca-music title="_{MusicEventTitle}"> |]
caminoEventTypeIcon Performance = [ihamlet| <span .event-type .event-performance .ca-theatre title="_{PerformanceEventTitle}"> |]
caminoEventTypeIcon Festival = [ihamlet| <span .event-type .event-festival .ca-festival  title="_{FestivalEventTitle}"> |]
caminoEventTypeIcon Holiday = [ihamlet| <span .event-type .event-holiday .ca-holiday title="_{HolidayEventTitle}"> |]
caminoEventTypeIcon PilgrimMass= [ihamlet| <span .event-type .event-pilgrimmass .ca-altar title="_{PilgrimMassEventTitle}"> |]
caminoEventTypeIcon Mass = [ihamlet| <span .event-type .event-mass .ca-altar title="_{MassEventTitle}"> |]

caminoLocationTypeIcon :: LocationType -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationTypeIcon Village = [ihamlet| <span .location-type .ca-village title="_{VillageTitle}"> |]
caminoLocationTypeIcon Town = [ihamlet| <span .location-type .ca-town title="_{TownTitle}"> |]
caminoLocationTypeIcon City = [ihamlet| <span .location-type .ca-city title="_{CityTitle}"> |]
caminoLocationTypeIcon Bridge = [ihamlet| <span .location-type .ca-bridge title="_{BridgeTitle}"> |]
caminoLocationTypeIcon Intersection = [ihamlet| <span .location-type .ca-intersection title="_{IntersectionTitle}"> |]
caminoLocationTypeIcon Monastery = [ihamlet| <span .location-type .ca-monastery title="_{MonasteryTitle}"> |]
caminoLocationTypeIcon Peak = [ihamlet| <span .location-type .ca-peak title="_{PeakTitle}"> |]
caminoLocationTypeIcon Lookout = [ihamlet| <span .location-type .ca-lookout title="_{LookoutTitle}"> |]
caminoLocationTypeIcon Promontory = [ihamlet| <span .location-type .ca-promontory title="_{PromontoryTitle}"> |]
caminoLocationTypeIcon Church = [ihamlet| <span .location-type .ca-church title="_{ChurchTitle}"> |]
caminoLocationTypeIcon Cathedral = [ihamlet| <span .location-type .ca-cathedral title="_{CathedralTitle}"> |]
caminoLocationTypeIcon Cross = [ihamlet| <span .location-type .ca-cross title="_{CrossTitle}"> |]
caminoLocationTypeIcon Fountain = [ihamlet| <span .location-type .ca-fountain title="_{FountainTitle}"> |]
caminoLocationTypeIcon Statue = [ihamlet| <span .location-type .ca-statue title="_{StatueTitle}"> |]
caminoLocationTypeIcon Artwork = [ihamlet| <span .location-type .ca-artwork title="_{ArtworkTitle}"> |]
caminoLocationTypeIcon Municipal = [ihamlet| <span .location-type .ca-municipal title="_{MunicipalTitle}"> |]
caminoLocationTypeIcon InformationPoint = [ihamlet| <span .location-type .ca-information-point title="_{InformationPointTitle}"> |]
caminoLocationTypeIcon PilgrimResource = [ihamlet| <span .location-type .ca-pilgrim title="_{PilgrimResourceTitle}"> |]
caminoLocationTypeIcon Junction = [ihamlet| <span .location-type .ca-junction title="_{JunctionTitle}"> |]
caminoLocationTypeIcon Shop = [ihamlet| <span .location-type .ca-shop title="_{ShopTitle}"> |]
caminoLocationTypeIcon Winery = [ihamlet| <span .location-type .ca-winery title="_{WineryTitle}"> |]
caminoLocationTypeIcon Museum = [ihamlet| <span .location-type .ca-museum title="_{MuseumTitle}"> |]
caminoLocationTypeIcon Theatre = [ihamlet| <span .location-type .ca-theatre title="_{TheatreTitle}"> |]
caminoLocationTypeIcon Historical = [ihamlet| <span .location-type .ca-historical title="_{HistoricalTitle}"> |]
caminoLocationTypeIcon Park = [ihamlet| <span .location-type .ca-park title="_{ParkTitle}"> |]
caminoLocationTypeIcon Beach = [ihamlet| <span .location-type .ca-beach title="_{BeachTitle}"> |]
caminoLocationTypeIcon Natural = [ihamlet| <span .location-type .ca-natural title="_{NaturalTitle}"> |]
caminoLocationTypeIcon Hazard = [ihamlet| <span .location-type .ca-hazard title="_{HazardTitle}"> |]
caminoLocationTypeIcon PlaceholderLocation = [ihamlet| <span .text-warning .location-type .ca-poi title="_{PlaceholderLabel}"> |]
caminoLocationTypeIcon _ = [ihamlet| <span .location-type .ca-poi title="_{PoiTitle}"> |]

caminoLocationTypeLabel :: LocationType -> CaminoMsg
caminoLocationTypeLabel Village = VillageTitle
caminoLocationTypeLabel Town = TownTitle
caminoLocationTypeLabel City = CityTitle
caminoLocationTypeLabel Bridge = BridgeTitle
caminoLocationTypeLabel Intersection = IntersectionTitle
caminoLocationTypeLabel Monastery = MonasteryTitle
caminoLocationTypeLabel Peak = PeakTitle
caminoLocationTypeLabel Lookout = LookoutTitle
caminoLocationTypeLabel Promontory = PromontoryTitle
caminoLocationTypeLabel Church = ChurchTitle
caminoLocationTypeLabel Cathedral = CathedralTitle
caminoLocationTypeLabel Cross = CrossTitle
caminoLocationTypeLabel Fountain = FountainTitle
caminoLocationTypeLabel Statue = StatueTitle
caminoLocationTypeLabel Artwork = ArtworkTitle
caminoLocationTypeLabel Municipal = MunicipalTitle
caminoLocationTypeLabel InformationPoint = InformationPointTitle
caminoLocationTypeLabel PilgrimResource = PilgrimResourceTitle
caminoLocationTypeLabel Junction = JunctionTitle
caminoLocationTypeLabel Shop = ShopTitle
caminoLocationTypeLabel Winery = WineryTitle
caminoLocationTypeLabel Museum = MuseumTitle
caminoLocationTypeLabel Theatre = TheatreTitle
caminoLocationTypeLabel Historical = HistoricalTitle
caminoLocationTypeLabel Park = ParkTitle
caminoLocationTypeLabel Beach = BeachTitle
caminoLocationTypeLabel Natural = NaturalTitle
caminoLocationTypeLabel Hazard = HazardTitle
caminoLocationTypeLabel Poi = PoiTitle
caminoLocationTypeLabel PlaceholderLocation = PlaceholderLabel

caminoPoiCategoryLabel :: PoiCategory -> CaminoMsg
caminoPoiCategoryLabel ReligiousPoi  = ReligiousPoiTitle
caminoPoiCategoryLabel HistoricalPoi  = HistoricalPoiTitle
caminoPoiCategoryLabel CulturalPoi  = CulturalPoiTitle
caminoPoiCategoryLabel NaturalPoi  = NaturalPoiTitle
caminoPoiCategoryLabel PilgrimPoi  = PilgrimPoiTitle
caminoPoiCategoryLabel RecreationPoi  = RecreationPoiTitle

caminoLegTypeIcon :: LegType -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLegTypeIcon Trail = [ihamlet| <span .leg-type .ca-walking title="_{TrailTitle}"> |]
caminoLegTypeIcon CyclePath = [ihamlet| <span .leg-type .ca-cycling title="_{CyclePathTitle}"> |]
caminoLegTypeIcon FerryLink = [ihamlet| <span .leg-type .ca-ferry title="_{FerryTitle}"> |]
caminoLegTypeIcon BoatLink = [ihamlet| <span .leg-type .ca-rowing title="_{BoatTitle}"> |]
caminoLegTypeIcon BusLink = [ihamlet| <span .leg-type .ca-bus-link title="_{BusTitle}"> |]
caminoLegTypeIcon TrainLink = [ihamlet| <span .leg-type .ca-train-link title="_{TrainTitle}"> |]
caminoLegTypeIcon _ = [ihamlet| <span .leg-type title="_{RoadTitle}"> |]

caminoLegTypeLabel :: LegType -> CaminoMsg
caminoLegTypeLabel Trail = TrailTitle
caminoLegTypeLabel CyclePath = CyclePathTitle
caminoLegTypeLabel FerryLink = FerryTitle
caminoLegTypeLabel BoatLink = BoatTitle
caminoLegTypeLabel BusLink = BusTitle
caminoLegTypeLabel TrainLink = TrainTitle
caminoLegTypeLabel _ = RoadTitle

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
caminoAccommodationTypeIcon CasaRural = [ihamlet| <span .accommodation .casa-rural .ca-casa-rural title="_{CasaRuralTitle}"> |]
caminoAccommodationTypeIcon GuestHouse = [ihamlet| <span .accommodation .guest-house .ca-guesthouse title="_{GuestHouseTitle}"> |]
caminoAccommodationTypeIcon HomeStay = [ihamlet| <span .accommodation .home-stay .ca-homestay title="_{HomeStayTitle}"> |]
caminoAccommodationTypeIcon House = [ihamlet| <span .accommodation .house .ca-house title="_{HouseTitle}"> |]
caminoAccommodationTypeIcon Hotel = [ihamlet| <span .accommodation .hotel .ca-hotel title="_{HotelTitle}"> |]
caminoAccommodationTypeIcon Gite = [ihamlet| <span .accommodation .gite .ca-gite title="_{GiteTitle}"> |]
caminoAccommodationTypeIcon CampGround = [ihamlet| <span .accommodation .camp-ground .ca-campground title="_{CampGroundTitle}"> |]
caminoAccommodationTypeIcon Refuge = [ihamlet| <span .accommodation .refuge .ca-refuge title="_{RefugeTitle}"> |]
caminoAccommodationTypeIcon Camping = [ihamlet| <span .accommodation .camping .ca-tent title="_{CampingTitle}"> |]
caminoAccommodationTypeIcon PlaceholderAccommodation = [ihamlet| <span .accommodation title="_{PlaceholderLabel}">? |]

caminoAccommodationTypeMsg :: AccommodationType -> CaminoMsg
caminoAccommodationTypeMsg PilgrimAlbergue = PilgrimAlbergueTitle
caminoAccommodationTypeMsg PrivateAlbergue = PrivateAlbergueTitle
caminoAccommodationTypeMsg Hostel = HostelTitle
caminoAccommodationTypeMsg CasaRural = CasaRuralTitle
caminoAccommodationTypeMsg GuestHouse = GuestHouseTitle
caminoAccommodationTypeMsg HomeStay = HomeStayTitle
caminoAccommodationTypeMsg House = HouseTitle
caminoAccommodationTypeMsg Hotel = HotelTitle
caminoAccommodationTypeMsg Gite = GiteTitle
caminoAccommodationTypeMsg CampGround = CampGroundTitle
caminoAccommodationTypeMsg Refuge = RefugeTitle
caminoAccommodationTypeMsg Camping = CampingTitle
caminoAccommodationTypeMsg PlaceholderAccommodation = PlaceholderLabel

caminoAccommodationLabel :: Accommodation -> CaminoMsg
caminoAccommodationLabel (GenericAccommodation PilgrimAlbergue) = PilgrimAlbergueTitle
caminoAccommodationLabel (GenericAccommodation PrivateAlbergue) = PrivateAlbergueTitle
caminoAccommodationLabel (GenericAccommodation Hostel) = HostelTitle
caminoAccommodationLabel (GenericAccommodation CasaRural) = CasaRuralTitle
caminoAccommodationLabel (GenericAccommodation GuestHouse) = GuestHouseTitle
caminoAccommodationLabel (GenericAccommodation HomeStay) = HomeStayTitle
caminoAccommodationLabel (GenericAccommodation House) = HouseTitle
caminoAccommodationLabel (GenericAccommodation Hotel) = HotelTitle
caminoAccommodationLabel (GenericAccommodation Gite) = GiteTitle
caminoAccommodationLabel (GenericAccommodation CampGround) = CampGroundTitle
caminoAccommodationLabel (GenericAccommodation Refuge) = RefugeTitle
caminoAccommodationLabel (GenericAccommodation Camping) = CampingTitle
caminoAccommodationLabel (GenericAccommodation PlaceholderAccommodation) = PlaceholderLabel
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
caminoServiceIcon Bedlinen = [ihamlet| <span .service .ca-bedlinen title="_{BedlinenTitle}"> |]
caminoServiceIcon Towels = [ihamlet| <span .service .ca-towels title="_{TowelsTitle}"> |]
caminoServiceIcon Pool = [ihamlet| <span .service .ca-pool title="_{PoolTitle}"> |]
caminoServiceIcon Heating = [ihamlet| <span .service .ca-heating title="_{HeatingTitle}"> |]
caminoServiceIcon Prayer = [ihamlet| <span .service .ca-prayer title="_{PrayerTitle}"> |]
caminoServiceIcon Train = [ihamlet| <span .service .ca-train title="_{TrainTitle}"> |]
caminoServiceIcon Bus = [ihamlet| <span .service .ca-bus title="_{BusTitle}"> |]
caminoServiceIcon Ferry = [ihamlet| <span .service .ca-wharf title="_{FerryTitle}"> |]

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
caminoServiceMsg Ferry = FerryTitle

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
caminoFitnessMsg Casual = CasualTitle
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
caminoAccommodationSummaryHtml a@(Accommodation _id _name _description type' services' sleeping' _multi) = [ihamlet|
    <span .accommodation>
      <span .pr-4>
        ^{caminoAccommodationTypeIcon type'} _{Txt (accommodationName a)}
      <span .pr-4>
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
      services' = maybe S.empty (tripChoiceFeatures . fst) (L.uncons $ metricsAccommodationChoice metrics)
      hasServices = not (S.null services')

caminoAccommodationNameHtml :: Accommodation -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoAccommodationNameHtml (GenericAccommodation type') = [ihamlet|_{caminoAccommodationTypeMsg type'}|]
caminoAccommodationNameHtml (Accommodation _id name' _description _type  _services _sleeping _multi) = [ihamlet|_{Txt name'}|]

caminoAccommodationHtml :: Accommodation -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoAccommodationHtml accommodation = [ihamlet|
  <div .row .accommodation>
    <div .col-1 .text-end>
      ^{caminoAccommodationTypeIcon type'}
    <div .col-5>
      ^{caminoAccommodationNameHtml accommodation}
    <div .col-4>
      $forall service <- accommodationServices accommodation
        ^{caminoServiceIcon service}
    <div .col-2>
      $forall sleeping <- accommodationSleeping accommodation
        ^{caminoSleepingIcon sleeping}
  $maybe desc <- accommodationDescription accommodation
    <div .row .accommodation>
      <div .offset-1 .col .text-secondary>
        ^{descriptionBlock True True desc}
      <div .col-1>

 |]
   where
     type' = accommodationType accommodation

-- | Get elements of a possible solution
solutionElements :: Camino -> Maybe Solution -> (Maybe Pilgrimage, Maybe (Failure Location Leg Metrics Metrics), Maybe (Failure Location Day Metrics Metrics), S.Set Location, S.Set Location, S.Set Location, S.Set Location, S.Set Leg)
solutionElements camino Nothing = (
    Nothing,
    Nothing,
    Nothing,
    S.empty,
    S.empty,
    S.empty,
    S.fromList $ caminoLocations camino,
    S.fromList $ caminoLegs camino
  )
solutionElements _camino (Just solution) = (
    pilgrimage',
    solutionJourneyFailure solution,
    solutionPilgrimageFailure solution,
    maybe S.empty (S.fromList . pilgrimageRests) pilgrimage',
    maybe S.empty (S.fromList . pilgrimageStockpoints) pilgrimage',
    maybe S.empty (S.fromList . pilgrimageStops) pilgrimage',
    maybe S.empty (S.fromList . pilgrimageWaypoints) pilgrimage',
    maybe S.empty (S.fromList . pilgrimageLegs) pilgrimage'
  ) where
    pilgrimage' = solutionPilgrimage solution

calendarBlock :: EventCalendar -> HtmlUrlI18n CaminoMsg CaminoRoute
calendarBlock Daily = [ihamlet|_{DailyLabel}|]
calendarBlock (Weekly dow) = [ihamlet|
  <ol .comma-list .days-of-week-list>
    $forall d <- dow
      <li>_{DayOfWeekName d}#
  |]
calendarBlock (Monthly dom) = [ihamlet|
  <ol .comma-list .days-of-month-list>
    $forall d <- dom
      <li>_{DayOfMonthName d}#
  |]
calendarBlock (Yearly moy) = [ihamlet|
  <ol .comma-list .months-of-year-list>
    $forall m <- moy
      <li>_{MonthOfYearName m}#
  |]
calendarBlock (DayOfYear days) = [ihamlet|
  <ol .comma-list .days-of-year-list>
    $forall (month, day) <- days
      <li>_{MonthOfYearName month} #{day}#
  |]
calendarBlock (RangeCalendar rfrom rto) = [ihamlet|
  ^{calendarBlock rfrom} - ^{calendarBlock rto}
  |]
calendarBlock (UnionCalendar calendars) = [ihamlet|
  <ol .or-list .days-of-year-list>
    $forall cal <- calendars
      <li>^{calendarBlock cal}#
  |]
calendarBlock (IntersectionCalendar calendars) = [ihamlet|
  <ol .and-list .days-of-year-list>
    $forall cal <- calendars
      <li>^{calendarBlock cal}#
  |]
calendarBlock (InvertedCalendar calendar) = [ihamlet|
  _{ExceptText} ^{calendarBlock calendar}
  |]
calendarBlock (NthDayAfter nth calendar) = [ihamlet|
  _{OrdinalBeforeAfter nth DayText} ^{calendarBlock calendar}
  |]
calendarBlock (NthWeekday wom dow) = [ihamlet|
  _{NthWeekdayText wom dow}
  |]
calendarBlock (NthWeekdayAfter nth dow calendar) = [ihamlet|
  _{OrdinalAfterWeekday nth dow} ^{calendarBlock calendar}
  |]
calendarBlock (ListCalendar days) = [ihamlet|
  <ol .comma-list .days-list>
    $forall day <- days
      _{DateMsg day}
  |]
calendarBlock (NamedCalendar key) = [ihamlet|
  _{NamedCalendarLabel key}
  |]
calendarBlock (PublicHoliday region) = [ihamlet|
  _{PublicHolidayLabel region}
  |]
calendarBlock (ClosedDay region) = [ihamlet|
  _{ClosedDayLabel region}
  |]
calendarBlock (Conditional cal cond) = [ihamlet|
  ^{calendarBlock cal}
  <div .calendar-condition>_{Txt cond}
  |]

timeBlock :: EventTime -> HtmlUrlI18n CaminoMsg CaminoRoute
timeBlock EventClosed = [ihamlet|_{ClosedText}|]
timeBlock EventOpen = [ihamlet|_{OpenText}|]
timeBlock (EventTime hours) = [ihamlet|
  <ol .comma-list .open-hours-list>
    $forall (from, to) <- hours
      <li>_{Time from}-_{Time to}#
  |]


hoursBlock :: OpenHours -> HtmlUrlI18n CaminoMsg CaminoRoute
hoursBlock (OpenHours []) = [ihamlet||]
hoursBlock (OpenHours [EventHours Daily time]) = [ihamlet|
  <div .row .event-hours>
    <div .col>
    <div .col>
      <span .description-icon .ca-clock title="_{OpenHoursTitle}">
      ^{timeBlock time}
  |]
hoursBlock (OpenHours [EventHours calendar EventOpen]) = [ihamlet|
  <div .row .event-hours>
    <div .col>
      <span .description-icon .ca-calendar title="_{CalendarTitle}">
      ^{calendarBlock calendar}
    <div .col>
  |]
hoursBlock (OpenHours hours) = [ihamlet|
  $with (EventHours calendar time) <- headWithDefault (EventHours Daily EventOpen)  hours
    <div .row .event-hours>
      <div .col>
        <span .description-icon .ca-calendar title="_{CalendarTitle}">
        ^{calendarBlock calendar}
      <div .col>
        <span .description-icon .ca-clock title="_{OpenHoursTitle}">
        ^{timeBlock time}
  $forall (EventHours calendar time) <- tailOrEmpty hours
    <div .row .event-hours>
      <div .col>
        <span .invisible .description-icon .ca-calendar title="_{CalendarTitle}">
        ^{calendarBlock calendar}
      <div .col>
        <span .invisible .description-icon .ca-clock title="_{OpenHoursTitle}">
        ^{timeBlock time}
  |]


descriptionNoteTypeMsg :: NoteType -> CaminoMsg
descriptionNoteTypeMsg Information = InformationTitle
descriptionNoteTypeMsg Warning = WarningTitle
descriptionNoteTypeMsg Address = AddressTitle
descriptionNoteTypeMsg Directions = DirectionsTitle

descriptionNoteTypeIcon :: NoteType -> HtmlUrlI18n CaminoMsg CaminoRoute
descriptionNoteTypeIcon Information = [ihamlet| <span .note-type .ca-information title="_{InformationTitle}">|]
descriptionNoteTypeIcon Warning = [ihamlet| <span .note-type .ca-warning title="_{WarningTitle}">|]
descriptionNoteTypeIcon Address = [ihamlet| <span .note-type .ca-map title="_{AddressTitle}">|]
descriptionNoteTypeIcon Directions = [ihamlet| <span .note-type .ca-map title="_{DirectionsTitle}">|]

descriptionNote :: Note -> HtmlUrlI18n CaminoMsg CaminoRoute
descriptionNote note = [ihamlet|
  <div .note .#{nc}>
    ^{descriptionNoteTypeIcon nt}
    _{Txt (noteText note)}
  |]
  where
    nt = noteType note
    nc = "note-" <> (T.toLower $ T.pack $ show nt)

descriptionLine :: Description -> HtmlUrlI18n CaminoMsg CaminoRoute
descriptionLine description = [ihamlet|
  _{Txt (descriptionSummary description)}
  |]

-- Only partial. This should be enclosed in a .row since it allows extra stuff to be added
descriptionBlock :: Bool -> Bool -> Description -> HtmlUrlI18n CaminoMsg CaminoRoute
descriptionBlock showAbout showImages description = [ihamlet|
    $if showImages
      $maybe img <- mimg
        <div .description-thumbnail .card .float-end>
          <div .card-body>
            <img .rounded .img-fluid src="@{ImgRoute img True}" alt="_{TxtPlain True False (imageTitle img)}" title="_{TxtPlain True False (imageTitle img)}" :isJust attribution:dc:rights="#{javascriptString attribution}" onclick="showImagePopup('@{ImgRoute img False}', '_{TxtPlain True True (imageTitle img)}', #{javascriptString attribution}, #{javascriptString origin})"">
    $if showAbout
      $maybe about <- descAbout description
        <div .float-start .description-icon>
          <a .about href="@{LinkRoute about}" title="_{LinkTitle about ""}">
            <span .ca-link>
    $maybe txt <- descText description
      _{Txt txt}
    $forall note <- descNotes description
      ^{descriptionNote note}
  |]
  where
    mimg = descImage description
    attribution = maybe Nothing imageAttribution mimg
    origin= maybe Nothing imageOrigin mimg

locationLineSimple :: Location -> HtmlUrlI18n CaminoMsg CaminoRoute
locationLineSimple location = [ihamlet|
    _{Txt (locationName location)}
    $maybe r <- locationRegion location
      <span .region>
        _{Txt (regionName r)}
    <span .accommodation-types>
      $forall accommodation <- locationAccommodationTypes location
        ^{caminoAccommodationTypeIcon accommodation}
    <span .services>
      $forall service <- locationServices location
        ^{caminoServiceIcon service}
    <span .poi-types>
      $forall poi <- locationPoiTypes location
        ^{caminoLocationTypeIcon poi}
      $forall event <- locationEventTypes location
        ^{caminoEventTypeIcon event}
  |]

locationLine :: TravelPreferences -> CaminoPreferences -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
locationLine _preferences _camino location = locationLineSimple location


poiLineSimple :: PointOfInterest -> HtmlUrlI18n CaminoMsg CaminoRoute
poiLineSimple poi = [ihamlet|
    _{Txt (poiName poi)}
  |]

poiLine :: TravelPreferences -> CaminoPreferences -> PointOfInterest -> HtmlUrlI18n CaminoMsg CaminoRoute
poiLine _preferences _camino poi = poiLineSimple poi

legLineSimple :: Leg -> HtmlUrlI18n CaminoMsg CaminoRoute
legLineSimple leg = [ihamlet|
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

legLine :: TravelPreferences -> CaminoPreferences -> Leg -> HtmlUrlI18n CaminoMsg CaminoRoute
legLine _preferences _camino leg = legLineSimple leg

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

locationLegSummary :: TravelPreferences -> CaminoPreferences -> S.Set Leg -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
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

caminoEventHtml :: Event -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoEventHtml event = [ihamlet|
  <div .row .clearfix .event .#{eventCss}>
    <div .col>
      <span .poi-types>
      ^{caminoEventTypeIcon (eventType event)}
        _{Txt (eventName event)}
        $maybe d <- eventDescription event
          $maybe about <- descAbout d
            <a .description-icon .about .float-end href="@{LinkRoute about}" title="_{LinkTitle about (eventName event)}">
              <span .ca-link>
        $maybe t <- eventHours event
          ^{hoursBlock t}
  $maybe d <- eventDescription event
    <div .row>
      <div .col>
        ^{descriptionBlock False True d}
  |]
  where
    eventCss = "event-" <> (T.toLower $ T.pack $ show $ eventType event)

-- | List suitable for attribute text
caminoPoiCategoriesAttr :: S.Set PoiCategory -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoPoiCategoriesAttr categories = [ihamlet|$forall (category, i) <- zcategories
  #{connector i}_{caminoPoiCategoryLabel category} #
|]
  where
    zcategories = zip (S.toList categories) [1::Int ..]
    connector i = (if i > 1 then ", " else "") :: T.Text

caminoPointOfInterestHtml :: PointOfInterest -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoPointOfInterestHtml poi = [ihamlet|
  <div id="#{poiID poi}" .row>
    <div .card .g-0>
      <div .card-header>
        <span .poi-types>
          ^{caminoLocationTypeIcon (poiType poi)}
        <span  title="^{caminoPoiCategoriesAttr (poiCategories poi)}">
          _{Txt (poiName poi)}
        $with pos <- poiPosition poi
          <a .description-icon .float-end onclick="showLocationOnMap(#{latitude pos}, #{longitude pos})">
            <span .ca-globe title="_{ShowOnMapTitle}">
        $maybe d <- poiDescription poi
          $maybe about <- descAbout d
            <a .description-icon .about .float-end href="@{LinkRoute about}" title="_{LinkTitle about (poiName poi)}">
              <span .ca-link>
      $if hasBody
        <div .card-body>
            $maybe c <- poiHours poi
              ^{hoursBlock c}
            $maybe d <- poiDescription poi
              ^{descriptionBlock False True d}
            $forall event <- poiEvents poi
              ^{caminoEventHtml event}
  |]
  where
    hasDescBody desc = (isJust $ descSummary desc) || (isJust $ descText desc) || (isJust $ descImage desc) || (not $ null $ descNotes desc)
    hasBody = (maybe False hasDescBody (poiDescription poi)) || (isJust $ poiHours poi) || (not $ null $ poiEvents poi)

caminoTransportLinkHtml :: TravelPreferences -> CaminoPreferences -> Leg -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoTransportLinkHtml preferences camino tlink = [ihamlet|
  <div .row .transport-link>
    <div .offset-1 .col-2>
      <a href="##{tid}" onclick="showLocationDescription('#{tid}');">_{Txt (locationName tloc)}
    <div .col-3 .services>
      $forall service <- locationServices tloc
        ^{caminoServiceIcon service}
    <div .col-2 .accommodation-types>
      $forall accommodation <- locationAccommodationTypes tloc
        ^{caminoAccommodationTypeIcon accommodation}
    <div .col-2 .poi-types>
      $forall poi <- locationPoiTypes tloc
        ^{caminoLocationTypeIcon poi}
      $forall event <- locationEventTypes tloc
        ^{caminoEventTypeIcon event}
    <div .col-2>
      ^{legLine preferences camino tlink}
  |]
  where
    tloc = legTo tlink
    tid = locationID tloc

tripChoiceSummary' :: Config -> Maybe (r -> T.Text) -> TripChoiceMap r Service -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
tripChoiceSummary' _config showName choiceMap location = [ihamlet|
  $maybe tc <- M.lookup location choiceMap
    $maybe sn <- showName
      #{sn (tripChoice tc)} #
    <span .services>
      $forall service <- tripChoiceFeatures tc
        ^{caminoServiceIcon service}
    _{PenanceFormatted True (tripChoicePenance tc)}
  $nothing
    #{endash}
|]

tripChoiceSummary :: Config -> Maybe Solution -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
tripChoiceSummary config msolution location = [ihamlet|
  $maybe choices <- solutionChoices <$> msolution
    <table .table  .table-sm .trip-choice-summary>
      <tr>
        <td>
          ^{tripChoiceSummary' config Nothing (tcStopLocation choices) location}
        <td>
          ^{tripChoiceSummary' config (Just $ localiseDefault . accommodationName) (tcStopAccommodation choices) location}
      <tr>
        <td>
          ^{tripChoiceSummary' config Nothing (tcStockLocation choices) location}
        <td>
          ^{tripChoiceSummary' config (Just $ localiseDefault . accommodationName) (tcStockAccommodation choices) location}
      <tr>
        <td>
          ^{tripChoiceSummary' config Nothing (tcRestLocation choices) location}
        <td>
          ^{tripChoiceSummary' config (Just $ localiseDefault . accommodationName) (tcRestAccommodation choices) location}
      <tr>
        <td>
          ^{tripChoiceSummary' config Nothing (tcRestPressure choices) location}
        <td>
|]

caminoLocationHtml :: Config -> TravelPreferences -> CaminoPreferences -> Maybe Solution -> String -> S.Set Location -> S.Set Location -> S.Set Location -> S.Set Location -> S.Set Leg -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationHtml config preferences camino msolution containerId rests stocks stops waypoints used location = [ihamlet|
  <div id="#{lid}" .accordion-item .p-0 .location-#{routeID route} :isRest:.location-rest :isStockpoint:.location-stockpoint :isStop:.location-stop :isWaypoint:.location-waypoint .location>
    <div .accordion-header>
      <button .accordion-button .collapsed data-bs-toggle="collapse" data-bs-target="#location-body-#{lid}" aria-expanded="false" aria-controls="location-body-#{lid}">
        <div .container-fluid>
          <div .row>
            <h5 .col-4>
              ^{caminoLocationTypeIcon (locationType location)}
              _{Txt (locationName location)}
            <div .col-3 .services>
              <span .services>
                $forall service <- locationServices location
                  ^{caminoServiceIcon service}
              <span .transport-link>
                <span .services>
                  $forall service <- locationAdditionalServices camino' location
                    ^{caminoServiceIcon service}                  
            <div .col-2 .accommodation-types>
              <span .accommodation-types>
                $forall accommodation <- locationAccommodationTypes location
                  ^{caminoAccommodationTypeIcon accommodation}
              <span .transport-link>
                <span .accommodation-types>
                  $forall accommodation <- locationAdditionalAccommodationTypes camino' location
                    ^{caminoAccommodationTypeIcon accommodation}
            <div .col-2 .poi-types>
              <span .poi-types>
                $forall poi <- locationPoiTypes location
                  ^{caminoLocationTypeIcon poi}
              <span .poi-types>
                $forall event <- locationEventTypes  location
                  ^{caminoEventTypeIcon event}
              <span .transport-link>
                <span .poi-types>
                  $forall poi <- locationAdditionalPoiTypes camino' location
                    ^{caminoLocationTypeIcon poi}
            <div .col-1>
              <div .text-end>
                $maybe r <- locationRegion location
                  <span .region>
                    _{Txt (regionName r)}
                $maybe d <- locationDescription location
                  $maybe about <- descAbout d
                    <a .description-icon .about href="@{LinkRoute about}" title="_{LinkTitle about (locationName location)}">
                      <span .ca-link>
                $with pos <- locationPosition location
                  <a .show-on-map onclick="showLocationOnMap(#{latitude pos}, #{longitude pos})">
                    <span .ca-globe title="_{ShowOnMapTitle} #{formatLatLng (latitude pos)},#{formatLatLng (longitude pos)}#{formatElev (elevation pos)}">
     <div id="location-body-#{lid}" .accordion-collapse .collapse aria-labelledby="location-heading-#{lid}" data-parent="##{containerId}">
      <div .accordion-body .container-fluid>
        $if getDebug config
          <div .detail .border .border-info-subtle .rounded .float-end .ms-1 .p-1>
            ^{tripChoiceSummary config msolution location}
        ^{locationLegs preferences camino used location}
        $maybe d <- locationDescription location
          <div .row>
            <div .col>
              ^{descriptionBlock False True d}
        ^{conditionalLabel AccommodationLabel (locationAccommodation location)}
        $forall accommodation <- locationAccommodation location
          ^{caminoAccommodationHtml accommodation}
        ^{conditionalLabel PoisLabel (locationPois location)}
        $forall poi <- locationPois location
          ^{caminoPointOfInterestHtml poi}
        ^{conditionalLabel EventsLabel (locationEvents location)}
        $forall event <- locationEvents location
          ^{caminoEventHtml event}
        ^{conditionalLabel TransportLinksLabel transportLinks}
        $forall link <- transportLinks
          ^{caminoTransportLinkHtml preferences camino link}
  |]
  where
    camino' = preferenceCamino camino
    lid = locationID location
    route = caminoRoute camino' (preferenceRoutes camino) location
    isRest = S.member location rests
    isStockpoint = not isRest && S.member location stocks
    isStop = not isRest && not isStockpoint && S.member location stops
    isWaypoint = not isStop && not isStockpoint && not isRest && (S.member location waypoints)
    transportLinks = locationTransportLinks camino' location
    formatLatLng v = sformat (fixed 5) v
    formatElev mv = maybe "" (\v -> " " <> sformat (fixed 0) v <> "m") mv

caminoLocationsHtml :: Config -> TravelPreferences -> CaminoPreferences -> Maybe Solution -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationsHtml config preferences camino solution = [ihamlet|
  <div .container-fluid>
    <div .row>
      <nav .navbar .navbar-expand-md>
        <button .navbar-toggler type="button" data-bs-toggle="collapse" data-bs-target="#location-nav" aria-controls="location-nav" aria-expanded="false" aria-label="Toggle navigation">
          <span class="navbar-toggler-icon">
        <div .collapse .navbar-collapse #location-nav">
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
              ^{caminoLocationHtml config preferences camino solution "locations" rests stocks stops waypoints usedLegs loc}
  |]
  where
    camino' = preferenceCamino camino
    locationOrder a b = compare (canonicalise $ locationNameLabel a) (canonicalise $ locationNameLabel b)
    locationsSorted = L.sortBy locationOrder (caminoLocations camino')
    locationPartition = partition (\l -> T.toUpper $ canonicalise $ T.take 1 $ locationNameLabel l) locationsSorted
    (_trip, _jerrors, _perrors, rests, stocks, stops, waypoints, usedLegs) = solutionElements camino' solution

preferenceRangeHtml :: (Real a) => PreferenceRange a -> HtmlUrlI18n CaminoMsg CaminoRoute
preferenceRangeHtml range = [ihamlet|
    <span .text-danger>#{maybe "." (format (fixed 1)) (rangeMinimum range)}
    <span>#{endash}
    <span>#{format (fixed 1) (rangeLower range)}
    <span>#{endash}
    <span .text-success .fw-bolder>#{format (fixed 1) (rangeTarget range)}
    <span>#{endash}
    <span>#{format (fixed 1) (rangeUpper range)}
    <span>#{endash}
    <span .text-danger>#{maybe "." (format (fixed 1)) (rangeMaximum range)}
    $maybe d <- rangeDerived range
      <p .text-body-tertiary .smaller>#{d}
  |]

preferenceRangeIntHtml :: (Integral a) => PreferenceRange a -> HtmlUrlI18n CaminoMsg CaminoRoute
preferenceRangeIntHtml range = [ihamlet|
    <span .text-danger>#{maybe "." (format int) (rangeMinimum range)}
    <span>#{endash}
    <span>#{format int (rangeLower range)}
    <span>#{endash}
    <span .text-success .fw-bolder>#{format int (rangeTarget range)}
    <span>#{endash}
    <span>#{format int (rangeUpper range)}
    <span>#{endash}
    <span .text-danger>#{maybe "." (format int) (rangeMaximum range)}
    $maybe d <- rangeDerived range
      <p .text-body-tertiary .smaller>#{d}
  |]

failureTable :: (Edge e Location) => TravelPreferences -> CaminoPreferences -> CaminoMsg -> ChainGraph Location e Metrics -> HtmlUrlI18n CaminoMsg CaminoRoute
failureTable _tprefs _cprefs caption graph = [ihamlet|
  <table .table .table-striped>
    <caption>_{caption}
    <thead>
      <tr>
        <th>_{FromLabel}
        <th>_{ToLabel}
        <th>_{DistanceLabel}
        <th>_{PenanceSummaryLabel}
        <th>_{PathLabel}
    <tbody>
      $forall l <- M.keysSet (forwards graph)
        <tr>
          <td colspan="6">#{summary l} _{Txt (locationName l)}
        $forall t <- outgoing graph l
          <tr>
            <td>
            <td>#{summary (finish t)} _{Txt (locationName (finish t))}
            <td>_{DistanceFormatted (metricsDistance (score t))} _{TimeMsg (metricsTime (score t))}
            <td>#{summary (score t)}
            <td>
              #{summary l}
              $forall p <- path t
                \ #{arrow} #{summary (target p)}

|]
  where
    arrow = '\x2192'

failureReport :: (Edge e Location) => T.Text -> T.Text -> TravelPreferences -> CaminoPreferences -> Maybe (Failure Location e Metrics Metrics) -> HtmlUrlI18n CaminoMsg CaminoRoute
failureReport _tlabel _plabel _tprefs _cprefs Nothing = [ihamlet|
|]
failureReport tlabel plabel tprefs cprefs (Just (Failure msg loc pathGraph programGraph)) = [ihamlet|
  <div .row>
    <div .col>
      #{msg}
      $maybe l <- loc
        #{summary l} _{Txt (locationName l)}
  <div ##{tlabel} .row>
    <div .col>
       ^{failureTable tprefs cprefs TableLabel pathGraph}
  <div ##{plabel} .row>
      <div .col>
        ^{failureTable tprefs cprefs ProgramLabel programGraph}
|]

failureHtml :: TravelPreferences -> CaminoPreferences -> Maybe (Failure Location Leg Metrics Metrics) -> Maybe (Failure Location Day Metrics Metrics) -> HtmlUrlI18n CaminoMsg CaminoRoute
failureHtml tprefs cprefs journey pilgrimage = [ihamlet|
  <div .container-fluid>
    <nav .navbar .navbar-expand-md .bg-body>
      <ul .navbar-nav>
        $if isJust journey
          <li .nav-item>
            <a .nav-link href=#failure-journey-table">_{JourneyLabel}-_{TableLabel}
          <li .nav-item>
            <a .nav-link href="#failure-journey-program">_{JourneyLabel}-_{ProgramLabel}
        $if isJust pilgrimage
          <li .nav-item>
            <a .nav-link href=#failure-pilgrimage-table">_{PilgrimageLabel}-_{TableLabel}
          <li .nav-item>
            <a .nav-link href="#failure-pilgrimage-program">_{PilgrimageLabel}-_{ProgramLabel}
    ^{failureReport "failure-journey-table" "failure-journey-program" tprefs cprefs journey}
    ^{failureReport "failure-pilgrimage-table" "failure-pilgrimage-program" tprefs cprefs pilgrimage}
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
      <div .col-4>_{RestPreferencesLabel}
      <div .col>^{preferenceRangeIntHtml $ preferenceRest preferences}
    <div .row>
      <div .col-4>_{RestPressureLabel}
      <div .col>#{maybe "-" (format (fixed 1)) (preferenceRestPressure preferences)}
    <div .row>
      <div .col>
        <table .table .table-striped>
          <thead>
            <tr>
               <th>
               <th>_{StopLabel}
               <th>_{StockpointLabel}
               <th>_{RestpointLabel}
          <tbody>
            <tr>
              <td>_{TransportLinksLabel}
              <td>#{checkMark (stopTransportLinks (preferenceStop preferences))}
              <td>#{checkMark (stopTransportLinks (preferenceStockStop preferences))}
              <td>#{checkMark (stopTransportLinks (preferenceRestStop preferences))}
            <tr>
              <td colspan=4>_{LocationPreferencesLabel}
            $forall lk <- locationTypes
              <tr>
                <td .ps-3>
                  <span .location-type-sample>^{caminoLocationTypeIcon lk}
                  \ _{caminoLocationTypeLabel lk}
                <td>_{PenanceFormatted False (findLoc (preferenceStop preferences) lk)}
                <td>_{PenanceFormatted False (findLoc (preferenceStockStop preferences) lk)}
                <td>_{PenanceFormatted False (findLoc (preferenceRestStop preferences) lk)}
            <tr colspan=4>
              <td>_{AccommodationPreferencesLabel}
            $forall ak <- accommodationTypes
              <tr>
                <td .ps-3>
                  ^{caminoAccommodationTypeIcon ak}
                  \ _{caminoAccommodationTypeMsg ak}
                <td>_{PenanceFormatted False (findAcc (preferenceStop preferences) ak)}
                <td>_{PenanceFormatted False (findAcc (preferenceStockStop preferences) ak)}
                <td>_{PenanceFormatted False (findAcc (preferenceRestStop preferences) ak)}
            <tr>
              <td colspan=4>_{ServicesPreferencesLabel}
            $forall sk <- serviceTypes
              <tr>
                <td .ps-3>
                  ^{caminoServiceIcon sk}
                  \ _{caminoServiceMsg sk}
                <td>_{PenanceFormatted False (findSv (preferenceStop preferences) sk)}
                <td>_{PenanceFormatted False (findSv (preferenceStockStop preferences) sk)}
                <td>_{PenanceFormatted False (findSv (preferenceRestStop preferences) sk)}
            <tr>
              <td colspan=4>_{RouteServicesPreferencesLabel}
            $forall sk <- routeServiceTypes
              <tr>
                <td .ps-3>
                  ^{caminoServiceIcon sk}
                  \ _{caminoServiceMsg sk}
                <td>_{PenanceFormatted False (findRSv (preferenceStop preferences) sk)}
                <td>_{PenanceFormatted False (findRSv (preferenceStockStop preferences) sk)}
                <td>_{PenanceFormatted False (findRSv (preferenceRestStop preferences) sk)}
    <div .row>
      <div .col-4>_{PoisLabel}
      <div .col>
        <ul .bar-separated-list>
          $forall c <- preferencePoiCategories preferences
            <li>_{caminoPoiCategoryLabel c}
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
    <div .row>
      <div .col-4>_{RestpointsLabel}
      <div .col>
        <ul .bar-separated-list>
          $forall l <- preferenceRestPoints camino
            <li>
              $if showLink
                <a href="##{locationID l}" data-toggle="tab" onclick="showLocationDescription('#{locationID l}')">_{Txt (locationName l)}
              $else
                _{Txt (locationName l)}
    <div .row>
      <div .col-4>_{PoisLabel}
      <div .col>
         <ul .bar-separated-list>
           $forall p <- preferencePois camino
             <li>
               $if showLink
                 $maybe (_, loc) <- M.lookup (poiID p) pois
                   <a href="##{poiID p}" data-toggle="tab" onclick="showLocationDescription('#{locationID loc}')">_{Txt (poiName p)}
                 $nothing
                   _{Txt (poiName p)}
               $else
                 _{Txt (poiName p)}
  |]
  where
    locationTypes = locationStopTypeEnumeration
    accommodationTypes = accommodationTypeEnumeration
    usedServiceTypes s = S.toList $ S.unions $ map (\p -> M.keysSet $ s $ p preferences) [preferenceStop, preferenceStockStop, preferenceRestStop]
    serviceTypes = usedServiceTypes stopServices
    routeServiceTypes = usedServiceTypes stopRouteServices
    findAcc prefs ak = M.findWithDefault mempty ak (stopAccommodation prefs)
    findLoc prefs lk = M.findWithDefault mempty lk (stopLocation prefs)
    findSv prefs sk = M.findWithDefault mempty sk (stopServices prefs)
    findRSv prefs sk = M.findWithDefault mempty sk (stopRouteServices prefs)
    pois = caminoPoiMap $ preferenceCamino camino

caminoTripHtml :: Config -> TravelPreferences -> CaminoPreferences -> Pilgrimage -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoTripHtml config preferences camino pilgrimage = [ihamlet|
  <div .container-fluid>
    <div .row .trip-summary>
      <div .col>
        ^{penanceTable preferences camino False False (score pilgrimage)}
        <ul .trip-stages>
          $forall stage <- path pilgrimage
            <li>
              <a href="#leg-#{locationID $ start stage}">
                _{Txt (locationName $ start stage)}
              $forall l <- map finish $ path stage
                \ -
                <a href="#leg-#{locationID l}">
                  _{Txt (locationName l)}
        <p>
          ^{metricsSummary preferences camino (score pilgrimage) (Just $ sum $ map (length . path) (path pilgrimage)) (Just $ length $ path pilgrimage)}
          <div .container-fluid>
            <div .row>
              <div .col>
                ^{svgElevationProfile config maxelev (pilgrimageLabel pilgrimage) (pilgrimageImportant pilgrimage) (pilgrimageLegs pilgrimage)}
    $forall stage <- path pilgrimage
      <div .card .stage .border-primary .mt-2>
        <div .card-body>
          ^{penanceTable preferences camino False False $ score stage}
          <h3 id="stage-#{locationID (start stage)}" .card-title>
            <a href="@{LocationRoute (start stage)}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">_{Txt (locationName $ start stage)}
            \ -
            <a href="@{LocationRoute (finish stage)}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">_{Txt (locationName $ finish stage)}
            $maybe dates <- metricsDate (score stage)
              _{DateRangeMsg (fst dates) (snd dates)}
          <p .card-text>
            ^{metricsSummary preferences camino (score stage) (Just $ length $ path stage) Nothing}
          <div .container-fluid>
            <div .row>
              <div .col-xs-12 .col-lg-8>
                ^{svgElevationProfile config maxelev (stageLabel stage) (stageImportant stage) (journeyLegs stage)}
      $forall day <- path stage
        <div .card .day .ms-1>
          <div .card-header>
            <h4 id="leg-#{locationID (finish day)}">
              <a href="@{LocationRoute (start day)}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">_{Txt (locationName $ start day)}
              \ -
              <a href="@{LocationRoute (finish day)}" data-toggle="tab" onclick="$('#locations-toggle').tab('show')">_{Txt (locationName $ finish day)}
              $if metricsRestpoint (score day)
                <span title="_{RestpointLabel}" .ca-restpoint>
              $if metricsStockpoint (score day)
                <span title="_{StockpointLabel}" .ca-stockpoint>
              $maybe (day1, day2) <- metricsDate (score day)
                _{DateRangeMsg day1 day2}
                $maybe region <- locationRegion $ finish day
                  $forall (d, cal) <- runReader (namedDates region day1 day2) config
                    <span .holiday title="_{HolidayEventTitle}">
                      _{Txt (ceName cal)}
                      $if day1 /= day2
                        \ #{endash} _{DateMsg d}
              _{DistanceFormatted (metricsDistance $ score day)}
          <div .card-body>
            ^{penanceTable preferences camino False True $ score day}
            <p .card-text>
              ^{metricsSummary preferences camino (score day) Nothing Nothing}
            <div .container-fluid>
              <div .row>
                <div .col-xs-12 .col-md-6 .col-lg-4>
                  ^{svgElevationProfile config maxelev (const True) (const False) (dayLegs day)}
            <ul .card-text .list-unstyled>
              <li .ms-3>
                <div .location-summary .plan-leg>
                  ^{caminoLocationTypeIcon (locationType (start day))}
                  ^{locationLine preferences camino (start day)}
              $forall leg <- path day
                $with loc <- legTo leg
                  <li .ms-3>
                    <div .location-summary .plan-leg>
                      ^{caminoLocationTypeIcon (locationType loc)}
                      ^{locationLine preferences camino loc}
                    <div .leg-summary .leg-line>
                      ^{legLine preferences camino leg}
                    $if loc /= finish stage
                      <div .poi-summary .bar-separated-list>
                        <ul>
                          $forall poi <- selectedPois camino loc
                            <li>
                              ^{caminoLocationTypeIcon (poiType poi)}
                              _{Txt (poiName poi)}
                              $maybe time <- poiTime poi
                                \ _{TimeMsgPlain time}
            $forall accom <- metricsAccommodationChoice $ score day
              <p .card-text>
                ^{caminoAccommodationChoiceSummaryHtml (tripChoice accom) (score day)}
 |]
 where
  stageLabel stage loc = loc == start stage || elem loc (map finish (path stage))
  stageImportant stage loc = loc == start stage || loc == finish stage
  pilgrimageLabel pilg loc = loc == start pilg || elem loc (map finish (path pilg)) || (S.member loc $ preferenceStops camino) || (S.member loc $ preferenceRestPoints camino)
  pilgrimageImportant pilg loc = loc == start pilg || loc == finish pilg
  (_minll, maxll) = caminoBbox (preferenceCamino camino)
  maxelev = realToFrac $ ceilingBy 500.0 (maybe 1000.0 id (elevation maxll))

caminoMapHtml :: TravelPreferences -> CaminoPreferences -> Maybe Solution -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapHtml _preferences _camino _solution = [ihamlet|
  <div .container-fluid>
    <div .row>
      <div .col .d-flex .justify-content-center>
        <div #map>
  |]

caminoCaminoKeyHtml :: Camino -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoCaminoKeyHtml camino = [ihamlet|
$forall r <- caminoRoutes camino
  <tr>
    <td>
    <td .border .border-light style="min-width: 1ex; background-color: #{toCssColour $ paletteColour $ routePalette r}">
    <td>
    <td>
    $if r == caminoDefaultRoute camino
      <td .fw-normal>
        _{Txt (caminoName camino)}
    $else
      <td .fw-lighter .ps-2>
        _{Txt (routeName r)}
|]

caminoMapKeyHtml :: Bool -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapKeyHtml full = [ihamlet|
$forall lt <- (L.delete PlaceholderLocation locationTypeEnumeration)
  <tr>
    $if full
      <td .map-key-icon>
        <img src="@{caminoLocationTypeMapIcon lt True False}" title="_{StopLabel}">
    <td .map-key-icon >
      <img src="@{caminoLocationTypeMapIcon lt False True}" title="_{WaypointLabel}">
    $if full
      <td .map-key-icon>
        <img src="@{caminoLocationTypeMapIcon lt False False}" title="_{UnusedLabel}">
    <td .map-key-icon>
      <img src="@{caminoPoiTypeMapIcon lt}" title="_{PoiLabel}">
    <td>
      _{caminoLocationTypeLabel lt}
|]


caminoLocationIconSimple :: Location -> String
caminoLocationIconSimple location =
  "icon" ++ (show $ locationType location) ++ "Used"

caminoLocationIcon :: TravelPreferences -> CaminoPreferences -> S.Set Location -> S.Set Location -> Location -> String
caminoLocationIcon _preferences _camino stops waypoints location =
  "icon" ++ (show $ locationType location) ++ (status location)
   where
    status loc
     | S.member loc stops = "Stop"
     | S.member loc waypoints = "Used"
     | otherwise = "Unused"

caminoPoiIconSimple :: PointOfInterest -> String
caminoPoiIconSimple poi =
  "icon" ++ (show $ poiType poi) ++ "Poi"

caminoPoiIcon :: TravelPreferences -> CaminoPreferences -> PointOfInterest -> String
caminoPoiIcon _preferences _camino poi = caminoPoiIconSimple poi

caminoLocationTooltip :: TravelPreferences -> CaminoPreferences -> Maybe Solution -> S.Set Leg -> Location -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationTooltip preferences camino _solution usedLegs location = [ihamlet|
  <div .location-tooltip .container-fluid>
    <div .row>
      <div .col>
        ^{locationLine preferences camino location}
    ^{locationLegSummary preferences camino usedLegs location}
  |]

caminoLocationTooltipSimple :: Location -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoLocationTooltipSimple location = [ihamlet|
  <div .location-tooltip .container-fluid>
    <div .row>
      <div .col>
        ^{locationLineSimple location}
   |]


caminoPoiTooltip :: TravelPreferences -> CaminoPreferences -> PointOfInterest -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoPoiTooltip preferences camino poi = [ihamlet|
  <div .location-tooltip .container-fluid>
    <div .row>
      <div .col>
        ^{poiLine preferences camino poi}
  |]

caminoPoiTooltipSimple :: PointOfInterest -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoPoiTooltipSimple poi = [ihamlet|
  <div .location-tooltip .container-fluid>
    <div .row>
      <div .col>
        ^{poiLineSimple poi}
  |]

caminoMapScriptBase :: HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapScriptBase = [ihamlet|
var map = L.map('map');
var locations = L.layerGroup();
var legs = L.layerGroup();
var pois = L.layerGroup();
var caminoLabels = L.layerGroup();
var majorRouteLabels = L.layerGroup();
var minorRouteLabels = L.layerGroup();
var marker;
var line;
var label;

var labelState = 0;

L.tileLayer('@{MapTileRoute}', {
    maxZoom: 19,
    attribution: '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
}).addTo(map);
caminoLabels.setZIndex(60);
majorRouteLabels.setZIndex(50);
minorRouteLabels.setZIndex(40);
locations.setZIndex(30);
legs.setZIndex(20);
pois.setZIndex(10);

function showLocationOnMap(lat, lng) {
  \$('#map-toggle').tab('show');
  map.fitBounds([ [lat + 0.01, lng - 0.01], [lat - 0.01, lng + 0.01] ]);
}

function selectZoom() {
  var zoom = map.getZoom();
  var showLocations = zoom > 8;
  var showLegs = true;
  var showPois = zoom > 12;
  var showCaminoLabels = labelState > 0;
  var showMajorRouteLabels = labelState > 1;
  var showMinorRouteLabels = labelState > 2;
  if (showCaminoLabels && !map.hasLayer(caminoLabels))
      map.addLayer(caminoLabels);
  if (!showCaminoLabels && map.hasLayer(caminoLabels))
      map.removeLayer(caminoLabels);
  if (showMajorRouteLabels && !map.hasLayer(majorRouteLabels))
      map.addLayer(majorRouteLabels);
  if (!showMajorRouteLabels && map.hasLayer(majorRouteLabels))
      map.removeLayer(majorRouteLabels);
  if (showMinorRouteLabels && !map.hasLayer(minorRouteLabels))
      map.addLayer(minorRouteLabels);
  if (!showMinorRouteLabels && map.hasLayer(minorRouteLabels))
      map.removeLayer(minorRouteLabels);
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

map.on("zoomend", function() {
  selectZoom();
});
L.easyButton('<span class="ca-label fs-5" title="_{ShowLabelsTitle}"></span>', function(btn, map) {
    labelState = (labelState + 1) % 4;
    selectZoom();
}).addTo(map);

$forall (lt, location, poi) <- icons
  $with name <- T.pack $ show $ lt
    var icon#{name}Used = L.icon({
      iconUrl: '@{caminoLocationTypeMapIcon lt False True}',
      iconSize: [#{fst location}, #{snd location}]
    });
    var icon#{name}Unused = L.icon({
      iconUrl: '@{caminoLocationTypeMapIcon lt False False}',
      iconSize: [#{fst location}, #{snd location}]
    });
    var icon#{name}Stop = L.icon({
      iconUrl: '@{caminoLocationTypeMapIcon lt True True}',
      iconSize: [#{fst location}, #{snd location}]
    });
    var icon#{name}Poi = L.icon({
      iconUrl: '@{caminoPoiTypeMapIcon lt}',
      iconSize: [#{fst poi}, #{snd poi}]
    });
|]
  where
    icons = [
        (Village, (24, 24), (16, 16))
      , (Town, (32, 32), (32, 20))
      , (City, (40, 40), (32, 25))
      , (Monastery, (24, 24), (24, 22))
      , (Bridge, (24, 24), (24, 9))
      , (Intersection, (24, 24), (24, 22))
      , (Peak, (24, 24), (20, 18))
      , (Lookout, (24, 24), (24, 13))
      , (Promontory, (24, 24), (24, 17))
      , (Church, (24, 24), (24, 21))
      , (Cathedral, (24, 24), (19, 24))
      , (Cross, (24, 24), (15, 24))
      , (Fountain, (24, 24), (19, 24))
      , (Statue, (24, 24), (12, 24))
      , (Artwork, (24, 24), (24, 24))
      , (Municipal, (24, 24), (24, 21))
      , (InformationPoint, (24, 24), (24, 24))
      , (PilgrimResource, (24, 24), (16, 24))
      , (Junction, (24, 24), (23, 24))
      , (Shop, (24, 24), (24, 16))
      , (Winery, (24, 24), (22, 24))
      , (Museum, (24, 24), (24, 22))
      , (Theatre, (24, 24), (24, 17))
      , (Historical, (24, 24), (24, 17))
      , (Park, (24, 24), (24, 25))
      , (Beach, (24, 24), (24, 18))
      , (Natural, (24, 24), (24, 25))
      , (Hazard, (24, 24), (20, 18))
      , (Poi, (16, 24), (16, 24))
      ] :: [(LocationType, (Int, Int), (Int, Int))]


caminoMapScriptTabs :: HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapScriptTabs = [ihamlet|
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

|]

caminoMapScriptCamino :: Bool -> (Location -> String) -> (PointOfInterest -> String) -> (Location -> HtmlUrlI18n CaminoMsg CaminoRoute) -> (PointOfInterest -> HtmlUrlI18n CaminoMsg CaminoRoute) ->  (Leg -> Int) -> (Leg -> Float) -> (Leg -> String) -> S.Set Location -> S.Set Leg -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapScriptCamino tlink chooseLocationIcon choosePoiIcon chooseLocationTooltip choosePoiTooltip chooseWidth chooseOpacity chooseColour locations legs = [ihamlet|
$forall location <- locations
  $with position <- locationPosition location
    marker = L.marker([#{latitude position}, #{longitude position}], { icon: #{chooseLocationIcon location} } );
    marker.bindTooltip(`^{chooseLocationTooltip location}`);
    marker.addTo(locations);
    $if tlink
      marker.on('click', function(e) { showLocationDescription("#{locationID location}"); } );
  $forall poi <- locationPois location
    $with position <- poiPosition poi
      marker = L.marker([#{latitude position}, #{longitude position}], { icon: #{choosePoiIcon poi} } );
      marker.bindTooltip(`^{choosePoiTooltip poi}`);
      marker.addTo(pois);
      $if tlink
        marker.on('click', function(e) { showLocationDescription("#{locationID location}"); } );
$forall leg <- legs
  line = L.polyline([
      [#{latitude (locationPosition $ legFrom leg)}, #{longitude (locationPosition $ legFrom leg)}]
  $forall seg <- legSegments leg
    , [#{latitude (lsTo seg)}, #{longitude (lsTo seg)}]
  ], {
     color: '#{chooseColour leg}',
     weight: #{chooseWidth leg},
     opacity: #{chooseOpacity leg}
  });
  line.addTo(legs);
|]

caminoMapScriptLabels :: Camino -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapScriptLabels camino = [ihamlet|
$forall route <- caminoRoutes camino
  $with position <- locationPosition (routeCentralLocation route)
    $if not (route == defr)
      $if routeMajor route
          label = L.tooltip([#{latitude position}, #{longitude position}], { content: `_{Txt (routeName route)}`, direction: "top", permanent: true, className: "map-label-minor" });
          label.addTo(majorRouteLabels);
      $else
          label = L.tooltip([#{latitude position}, #{longitude position}], { content: `_{Txt (routeName route)}`, direction: "top", permanent: true, className: "map-label-subminor" });
          label.addTo(minorRouteLabels);
$with position <- locationPosition caminoStart
  label = L.tooltip([#{latitude position}, #{longitude position}], { content: `_{Txt (caminoName camino)}`, direction: "top", permanent: true, className: "map-label" });
  label.addTo(caminoLabels);
|]
  where
    defr = caminoDefaultRoute camino
    caminoStart = maybe (error "No camino start") fst $ L.uncons $ routeStarts defr


caminoMapScript :: TravelPreferences -> CaminoPreferences -> Maybe Solution -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoMapScript tprefs cprefs solution = [ihamlet|
<script>
  ^{caminoMapScriptBase}
  ^{caminoMapScriptTabs}
  map.fitBounds([ [#{latitude tl}, #{longitude tl}], [#{latitude br}, #{longitude br}] ]);
  ^{caminoMapScriptCamino True chooseLocationIcon choosePoiIcon chooseLocationTooltip choosePoiTooltip chooseWidth chooseOpacity chooseColour locations legs}
  ^{caminoMapScriptLabels camino}
  selectZoom();
|]
  where
    camino = preferenceCamino cprefs
    locations = S.fromList $ caminoLocations camino
    legs = S.fromList $ caminoLegs camino
    (_trip, _jerrors, _perrors, _rests, _stockpoints, stops, waypoints, usedLegs) = solutionElements camino solution
    (tl, br) = if S.null waypoints then caminoBbox camino else locationBbox waypoints
    chooseLocationIcon loc = caminoLocationIcon tprefs cprefs stops waypoints loc
    choosePoiIcon poi = caminoPoiIcon tprefs cprefs poi
    chooseLocationTooltip loc = caminoLocationTooltip tprefs cprefs solution usedLegs loc
    choosePoiTooltip poi = caminoPoiTooltip tprefs cprefs poi
    chooseWidth leg | S.member leg usedLegs = 7 :: Int
      | otherwise = 5 :: Int
    chooseOpacity leg | S.member leg usedLegs = 1.0 :: Float
      | otherwise = 0.5 :: Float
    chooseColour leg | S.member leg usedLegs = toCssColour $ paletteColour $ routePalette $ caminoLegRoute camino leg
      | otherwise = toCssColour $ blend 0.5 lightgrey $ paletteColour $ routePalette $ caminoLegRoute camino leg


caminoAllMapScript :: LatLong -> LatLong -> [Camino] -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoAllMapScript tl br caminos = [ihamlet|
<script>
  ^{caminoMapScriptBase}
  ^{caminoMapScriptCamino False chooseLocationIcon choosePoiIcon chooseLocationTooltip choosePoiTooltip chooseWidth chooseOpacity chooseColour locations legs}
  $forall camino <- caminos
    ^{caminoMapScriptLabels camino}
  map.fitBounds([ [#{latitude tl}, #{longitude tl}], [#{latitude br}, #{longitude br}] ]);
  selectZoom();
 |]
  where
    rmap = M.unions $ map (\c -> M.fromList $ map (\l -> (l, caminoLegRoute c l)) (caminoLegs c)) caminos
    locations = S.unions $ map (S.fromList . caminoLocations) caminos
    legs = S.unions $ map (S.fromList . caminoLegs) caminos
    chooseLocationIcon loc = caminoLocationIconSimple loc
    choosePoiIcon poi = caminoPoiIconSimple poi
    chooseLocationTooltip loc = caminoLocationTooltipSimple loc
    choosePoiTooltip poi = caminoPoiTooltipSimple poi
    chooseWidth _leg = 7 :: Int
    chooseOpacity _leg = 1.0 :: Float
    chooseColour leg = maybe "#000000" (toCssColour . paletteColour . routePalette) (M.lookup leg rmap)


aboutHtml :: Config -> Bool -> TravelPreferences -> CaminoPreferences -> Maybe Solution -> HtmlUrlI18n CaminoMsg CaminoRoute
aboutHtml config showImages _tprefs cprefs msolution = [ihamlet|
  <div .container-fluid>
    <h2>_{Txt (caminoName camino)}
    <div .row>
      <div .col>
        ^{descriptionBlock True showImages (caminoDescription camino)}
    <h3>_{RoutesLabel}
    $forall route <- caminoRoutes camino
      <div ##{routeID route} .row>
        <div .col>
          <span style="color: #{toCssColour $ paletteTextColour $ routePalette route}">_{Txt (routeName route)}
      <div .row>
        <div .offset-1 .col>
          ^{descriptionBlock True True (routeDescription route)}
    <h3>_{RegionsLabel}
    <table .table .table-striped>
      <thead>
        <tr>
          <th>_{RegionLabel}
          <th>
          <th>
          <th>_{ClosedDayText}
      <tbody>
        $forall region <- regions
          <tr ##{regionID region}>
            <td>
              _{Txt (regionName region)}
              $maybe description1 <- regionDescription region
                $maybe about <- descAbout description1
                  \ <a .about href="@{LinkRoute about}" title="_{LinkTitle about (regionName region)}">
                    <span .ca-link>
            <td>
              $maybe parent <- regionParent region
                $if S.member parent regions
                  <a href="##{regionID parent}">_{Txt (regionName parent)}
                $else
                  _{Txt (regionName parent)}
            <td>
              $maybe description2 <- regionDescription region
                ^{descriptionBlock False True description2}
            <td>
              <ul .comma-list>
                $forall cal <- regionClosedDays region
                  ^{calendarBlock cal}
                $forall cal <- getInheritedClosedDays region
                  <span .text-black-50>
                    ^{calendarBlock cal}
    <h4>_{HolidaysLabel}
    <table .table .table-striped .>
      <thead>
        <tr>
          <td>
          <td>
          $forall region <- holidayRegions
            <td .text-center>
              <a href="##{regionID region}">_{Txt (regionName region)}
      <tbody>
        $forall holiday <- holidays
          <tr>
            <td>
              ^{calendarBlock (fst holiday)}
            <td>
              $maybe d <- snd holiday
                _{DateMsg d}
            $forall region <- holidayRegions
              <td .text-center>
                <span :isIndirectHoliday (fst holiday) region:.text-black-50>
                  #{checkMark (isHoliday (fst holiday) region)}
    <h3>_{InformationLabel}
    <p>_{InformationDescription}
    <h4>_{CaminoLabel}
    $with metadata <- caminoMetadata camino
      $forall statement <- metadataStatements metadata
        <div .row>
          <div .col-1>
            <a href="#{show $ statementTerm statement}">#{statementLabel statement}
            $maybe l <- statementLang statement
              <span>#{l}
          <div .col>
            #{statementValue statement}
    $maybe solution <- msolution
      $maybe metadata <- solutionMetadata solution
        <h4>_{PlanLabel}
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
    camino = preferenceCamino cprefs
    regions = S.filter isHolidayRegion $ regionClosure $ caminoRegions camino
    holidayRegions = S.filter (\r -> not $ null $ getRegionalHolidays r) regions
    holidayKeys = S.fold S.union S.empty $ S.map (\r -> S.fromList $ catMaybes $ map calendarKey $ regionHolidays r) holidayRegions
    startDate = preferenceStartDate cprefs
    mapDate k = (c, (\d -> runReader (calendarDateOnOrAfter c d) config) <$> startDate) where c = NamedCalendar k
    holidays = L.sortOn snd $ map mapDate $ S.toList holidayKeys
    isHoliday holiday region = elem holiday (getRegionalHolidays region)
    isIndirectHoliday holiday region = elem holiday (getInheritedRegionalHolidays region)


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
keyHtml _config _preferences camino = $(ihamletFile "templates/help/key-en.hamlet")

helpHtml :: Config -> HtmlUrlI18n CaminoMsg CaminoRoute
helpHtml _config = $(ihamletFile "templates/help/help-en.hamlet")

caminoHtmlBase :: Config -> TravelPreferences -> CaminoPreferences -> Maybe Solution -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoHtmlBase config preferences camino msolution =
  [ihamlet|
    <style>
      $forall css <- caminoCss config (preferenceCamino camino)
        #{renderCss css }
    <div>
      <ul .nav .nav-tabs role="tablist">
        <li .nav-item role="presentation">
          <a #map-toggle .nav-link .active role="tab" data-bs-toggle="tab" href="#map-tab">_{MapLabel}
        $maybe _p <- pilgrimage
          <li .nav-item role="presentation">
            <a .nav-link role="tab" data-bs-toggle="tab" href="#plan-tab">_{PlanLabel}
        <li .nav-item role="presentation">
          <a #locations-toggle .nav-link role="tab" data-bs-toggle="tab" href="#locations-tab">_{LocationsLabel}
        <li .nav-item role="presentation">
          <a #preferences-toggle .nav-link role="tab" data-bs-toggle="tab" href="#preferences-tab">_{PreferencesLabel}
        $if showFailure
          <li .nav-item role="presentation">
            <a #about-toggle .nav-link role="tab" data-bs-toggle="tab" href="#failure-tab">_{FailureLabel}
        <li .nav-item role="presentation">
          <a #about-toggle .nav-link role="tab" data-bs-toggle="tab" href="#about-tab">_{AboutLabel}
        <li .nav-item role="presentation">
          <a #key-toggle .nav-link role="tab" data-bs-toggle="tab" href="#key-tab">_{KeyLabel}
      $maybe solution <- msolution
        $maybe sid <- solutionID solution
          <div .btn-group-vertical .float-end .me-1 .mt-2 .ms-3 .mb-2>
            <a .btn .btn-primary .btn-sm href="#" onclick="toggleDetail()" title="_{ToggleDetailTitle}">
              <span .ca-detail>
            <a .btn .btn-secondary .btn-sm href="@{PlanRoute sid}" title="_{PersistentLinkTitle}">
              <span .ca-link>
            <a .btn .btn-secondary .btn-sm href="#" onclick="navigator.clipboard.writeText('@{PlanRoute sid}')" title="_{CopyLinkTitle}">
              <span .ca-copy>
            <a .btn .btn-secondary .btn-sm href="@{PlanSpreadsheetRoute sid}" title="_{DownloadSpreadsheetTitle}">
              <span .ca-document-spreadsheet>
            <a .btn .btn-secondary .btn-sm href="@{PlanKmlRoute sid}" title="_{DownloadKmlTitle}">
              <span .ca-document-kml>
      <div .tab-content>
        <div .tab-pane .active role="tabpanel" id="map-tab">
          ^{caminoMapHtml preferences camino msolution}
        $maybe p <- pilgrimage
          <div .tab-pane role="tabpanel" id="plan-tab">
            ^{caminoTripHtml config preferences camino p}
        <div .tab-pane role="tabpanel" id="locations-tab">
          ^{caminoLocationsHtml config preferences camino msolution}
        <div .tab-pane role="tabpanel" id="preferences-tab">
          ^{preferencesHtml True preferences camino}
        $if showFailure
          <div .tab-pane role="tabpanel" id="failure-tab">
            ^{failureHtml preferences camino journeyFailure pilgrimageFailure}
        <div .tab-pane role="tabpanel" id="about-tab">
          ^{aboutHtml config True preferences camino msolution}
        <div .tab-pane role="tabpanel" id="key-tab">
          ^{keyHtml config preferences camino}
  ^{caminoMapScript preferences camino msolution}
  |]
  where
    pilgrimage = maybe Nothing solutionPilgrimage msolution
    journeyFailure = maybe Nothing solutionJourneyFailure msolution
    pilgrimageFailure = maybe Nothing solutionPilgrimageFailure msolution
    showFailure = getDebug config && (isJust journeyFailure || isJust pilgrimageFailure)

-- | Display a camino wihout a chosen route
caminoHtmlSimple :: Config -> CaminoPreferences -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoHtmlSimple config camino =
  [ihamlet|
      <style>
        $forall css <- caminoCss config (preferenceCamino camino)
          #{renderCss css }
      <div .row>
        <div .col .m-1>
          ^{descriptionLine (caminoDescription camino')}
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
                ^{caminoLocationsHtml config preferences camino Nothing}
              <div .tab-pane role="tabpanel" id="about-tab">
                ^{aboutHtml config True preferences camino Nothing}
              <div .tab-pane role="tabpanel" id="key-tab">
                ^{keyHtml config preferences camino}
        ^{caminoMapScript preferences camino Nothing}
  |]
  where
    camino' = preferenceCamino camino
    preferences = defaultTravelPreferences Walking Normal Pilgrim Nothing


caminoHtml :: Config -> TravelPreferences -> CaminoPreferences -> Solution -> HtmlUrlI18n CaminoMsg CaminoRoute
caminoHtml config preferences camino solution = let
    title = caminoName $ preferenceCamino camino
  in
    layoutHtml config title Nothing (caminoHtmlBase config preferences camino (Just solution)) Nothing
