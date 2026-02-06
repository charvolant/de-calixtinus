{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : I18n
Description : Common Internationalisation for Camino display
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Camino.Display.I18n (
    CaminoMsg(..)

  , formatDistance
  , formatMaybeDistance
  , formatMaybeHours
  , formatPenance
  , formatHeight
  , formatHours
  , rejectSymbol
  , renderCaminoMsg
  , renderCaminoMsgText
) where

import Camino.Camino
import Camino.Config
import Camino.Planner
import qualified Camino.Units as U
import Data.Localised
import Data.Region
import Data.Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Data.Util (commaJoin)
import Formatting (fixed, int, sformat)
import Text.Blaze.Html (text, text, toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Internal as TB
import Text.Hamlet

-- | Message placeholders for the camino
data CaminoMsg =
    AboutLabel
  | AccessTitle
  | AccessibleTitle
  | AccommodationLabel
  | AccommodationPenanceLabel
  | AccommodationPreferencesLabel
  | AddressTitle
  | AfterText
  | AirportDescription
  | AirportTitle
  | AlwaysOpenLabel
  | ArtworkDescription
  | ArtworkTitle
  | AscentLabel
  | AscentMsg Float
  | AustereTitle
  | BankTitle
  | BeachDescription
  | BeachTitle
  | BedlinenTitle
  | BeforeText
  | BicycleRepairTitle
  | BicycleStorageTitle
  | BoatTitle
  | BreakfastTitle
  | BridgeDescription
  | BridgeTitle
  | BusTitle
  | CalendarTitle
  | CaminoLabel
  | CampGroundTitle
  | CampingTitle
  | CampSiteTitle
  | CasaRuralTitle
  | CasualTitle
  | CathedralDescription
  | CathedralTitle
  | ChurchDescription
  | ChurchTitle
  | CityDescription
  | CityTitle
  | ClosedDayLabel Text
  | ClosedDayText
  | ClosedText
  | ComfortableTitle
  | CopyLinkTitle
  | CrossDescription
  | CrossTitle
  | ComfortLabel
  | CulturalPoiTitle
  | CyclingTitle
  | CyclePathTitle
  | DailyLabel
  | DateLabel
  | DateMsg Data.Time.Calendar.Day
  | DateRangeMsg Data.Time.Calendar.Day Data.Time.Calendar.Day
  | DayLabel
  | DayOfMonthName DayOfMonth
  | DayOfWeekName DayOfWeek
  | DaysElapsedMsg Int
  | DayServicesPenanceLabel
  | DayServicesPreferencesLabel
  | DaySummaryMsg Camino.Planner.Day
  | DaysWalkingMsg Int
  | DayText
  | DescentLabel
  | DescentMsg Float
  | DinnerTitle
  | DirectionsTitle
  | DistanceAdjustLabel
  | DistanceFormatted Float
  | DistanceLabel
  | DistanceMsg Float (Maybe Float) -- ^ actual/perceived distance
  | DistancePenanceLabel
  | DistancePreferencesLabel
  | DistancePreferencesPerceivedLabel
  | DoubleTitle
  | DoubleWcTitle
  | DownloadKmlTitle
  | DownloadSpreadsheetTitle
  | DryerTitle
  | ElevationFormatted Float
  | ElevationLabel
  | EventsLabel
  | ExceptText
  | ExcludedStopsLabel
  | FailureLabel
  | FarmlandDescription
  | FarmlandTitle
  | FatiguePenanceLabel
  | FerryTitle
  | FestivalEventTitle
  | FinishDateLabel
  | FinishLocationLabel
  | FitnessLabel
  | FitTitle
  | FoodEventTitle
  | FromLabel
  | FrugalTitle
  | FountainDescription
  | FountainTitle
  | GiteTitle
  | GroceriesTitle
  | GuestHouseTitle
  | HalfBoardTitle
  | HandwashTitle
  | HazardDescription
  | HazardTitle
  | HeatingTitle
  | HelpLabel
  | HistoricalDescription
  | HistoricalTitle
  | HistoricalPoiTitle
  | HolidayEventTitle
  | HolidaysLabel
  | HomeStayTitle
  | HostelTitle
  | HotelTitle
  | HoursTitle
  | HouseTitle
  | IdentifierLabel
  | IndustryDescription
  | IndustryTitle
  | InformationLabel
  | InformationTitle
  | InformationDescription
  | InformationPointDescription
  | InformationPointTitle
  | IntersectionDescription
  | IntersectionTitle
  | JourneyLabel
  | JourneySummaryMsg Journey
  | JunctionDescription
  | JunctionTitle
  | KeyLabel
  | KitchenTitle
  | LatitudeLabel
  | LegPenanceMsg Penance
  | LinkTitle (Localised TaggedURL) (Localised TaggedText)
  | LinkOut
  | ListMsg [CaminoMsg]
  | LocationPenanceLabel
  | LocationPreferencesLabel
  | LocationLabel
  | LocationsLabel
  | LockersTitle
  | LongitudeLabel
  | LookoutDescription
  | LookoutTitle
  | LuxuriousTitle
  | MapLabel
  | MassEventTitle
  | MattressTitle
  | MedicalTitle
  | MiscPenanceLabel
  | MonasteryDescription
  | MonasteryTitle
  | MonthOfYearName MonthOfYear
  | MunicipalDescription
  | MunicipalTitle
  | MuseumDescription
  | MuseumTitle
  | MusicEventTitle
  | NamedCalendarLabel Text
  | NaturalPoiTitle
  | NaturalDescription
  | NaturalTitle
  | NormalTitle
  | NotesLabel
  | NthWeekdayText WeekOfMonth DayOfWeek
  | OpenHoursTitle
  | OpenText
  | OrdinalAfterWeekday Int DayOfWeek
  | OrdinalBeforeAfter Int CaminoMsg
  | OtherLabel
  | ParkDescription
  | ParkTitle
  | PathLabel
  | PeakDescription
  | PeakTitle
  | PenanceFormatted Bool Penance
  | PenanceFormattedPlain Bool Penance
  | PenanceMsg Penance
  | PenanceReject
  | PenanceSummaryLabel
  | PerceivedDistanceLabel
  | PerformanceEventTitle
  | PersistentLinkTitle
  | PetsTitle
  | PharmacyTitle
  | PilgrimageLabel
  | PilgrimAlbergueTitle
  | PilgrimMassEventTitle
  | PilgrimPoiTitle
  | PilgrimResourceDescription
  | PilgrimResourceTitle
  | PilgrimTitle
  | PlaceholderLabel
  | PlanLabel
  | PoiDescription
  | PoiLabel
  | PoisLabel
  | PoiTime (Maybe Float)
  | PoiTitle
  | PoolTitle
  | PrayerTitle
  | PreferencesLabel
  | PrivateAlbergueTitle
  | ProgramLabel
  | PromontoryDescription
  | PromontoryTitle
  | PublicHolidayLabel Text
  | PublicHolidayText
  | QuadrupleTitle
  | QuadrupleWcTitle
  | RefugeTitle
  | RegionLabel
  | RegionsLabel
  | RecreationPoiTitle
  | ReligiousEventTitle
  | ReligiousPoiTitle
  | RequiredStopsLabel
  | RestaurantTitle
  | RestDaysLabel
  | RestLocationPreferencesLabel
  | RestPenanceLabel
  | RestpointLabel
  | RestpointsLabel
  | RestPointsPenanceLabel
  | RestPreferencesLabel
  | RestPressureLabel
  | RestPressurePenanceLabel
  | RoadTitle
  | RouteLabel
  | RouteServicesPreferencesLabel
  | RoutesLabel
  | ServicesLabel
  | ServicesPreferencesLabel
  | SharedTitle
  | ShopDescription
  | ShopTitle
  | ShowLabelsTitle
  | ShowOnMapTitle
  | SingleTitle
  | SIUnitsTitle
  | SleepingBagTitle
  | StablesTitle
  | StageLabel
  | StagesMsg Int
  | StartDateLabel
  | StartLocationLabel
  | StationDescription
  | StationTitle
  | StatueDescription
  | StatueTitle
  | StockpointLabel
  | StopLabel
  | StopPenanceLabel
  | StopPreferencesLabel
  | StopServicesPenanceLabel
  | SuperFitTitle
  | TableLabel
  | TheatreDescription
  | TheatreTitle
  | Time TimeOfDay
  | TimeAdjustLabel
  | TimeFormatted Float
  | TimeLabel
  | TimeMsg (Maybe Float)
  | TimePenaltyLabel
  | TimePreferencesLabel
  | ToggleDetailTitle
  | ToLabel
  | TowelsTitle
  | TownDescription
  | TownTitle
  | TrailTitle
  | TrainTitle
  | TransportLinksLabel
  | TravelLabel
  | TripFinishLabel
  | TripStartLabel
  | TripleTitle
  | TripleWcTitle
  | Txt (Localised TaggedText)
  | TxtPlain Bool Bool (Localised TaggedText)
  | TypeLabel
  | UnfitTitle
  | UnitsLabel
  | UnusedLabel
  | USUnitsTitle
  | VeryFitTitle
  | VeryUnfitTitle
  | VillageDescription
  | VillageTitle
  | WalkingNaismithTitle
  | WalkingTitle
  | WarningTitle
  | WashingMachineTitle
  | WaypointLabel
  | WharfDescription
  | WharfTitle
  | WiFiTitle
  | WineryDescription
  | WineryTitle
  deriving (Show)

rejectSymbol :: Text
rejectSymbol = "\x25c6"

thinSpace :: Text
thinSpace = "\x2009"

halfSymbol :: Text
halfSymbol = "\x00bd"

quarterSymbol :: Text
quarterSymbol = "\x00bc"

threeQuarterSymbol :: Text
threeQuarterSymbol = "\x00be"

formatForSystemOfUnits U.SIUnits v = sformat (fixed 1) v
formatForSystemOfUnits U.USUnits v = whole <> part where
  (n, f) = properFraction v
  (n', f') = if f > 0.875 then (n + 1, 0.0) else (n, f)
  whole = if n' == 0 then if f' < 0.25 then "0" else "" else sformat int n'
  part = if f' < 0.125 then ""
    else if f' < 0.365 then quarterSymbol
    else if f' < 0.625 then halfSymbol
    else threeQuarterSymbol

-- Note the use of direct blaze scans in the following code, rather than shamlet.
-- Hamlet seems to generate tags as chunks of content, rather than proper blaze HTML,
-- so the direct formatting allows renderCaminoMsgText to strip tags properly.
formatPenance :: U.SystemOfUnits -> Bool -> Penance -> Html
formatPenance _ _ Reject = H.span H.! HA.class_ "penance rejected" H.! HA.title "Rejected" $ text rejectSymbol
formatPenance sou showZero (Penance p) = if showZero || p /= 0.0 then H.span H.! HA.class_ "penance" $ text $ p'' <> thinSpace <> symbol else text ""
  where
    unit = U.preferredUnit sou U.Distance
    p' = U.convertAmount U.Kilometre unit p
    p'' = formatForSystemOfUnits sou p'
    symbol = pack $ U.unitSymbol unit

formatPenancePlain :: U.SystemOfUnits -> Bool -> Penance -> Html
formatPenancePlain _ _ Reject = "Rejected"
formatPenancePlain sou showZero (Penance p) = if showZero || p /= 0.0 then text $ p'' <> symbol else text ""
  where
    unit = U.preferredUnit sou U.Distance
    p' = U.convertAmount U.Kilometre unit p
    p'' = formatForSystemOfUnits sou p'
    symbol = pack $ U.unitSymbol unit

formatDistance :: (RealFrac a) => U.SystemOfUnits -> a -> Html
formatDistance sou d = H.span H.! HA.class_ "distance" $ text $ d'' <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Distance
    d' = U.convertAmount U.Kilometre unit d
    d'' = formatForSystemOfUnits sou d'
    symbol = pack $ U.unitSymbol unit


formatMaybeDistance :: (RealFrac a) => U.SystemOfUnits -> Maybe a -> Html
formatMaybeDistance sou Nothing = formatPenance sou True Reject
formatMaybeDistance sou (Just d) = formatDistance sou d

formatHours :: (RealFrac a) => U.SystemOfUnits -> a -> Html
formatHours sou t = H.span H.! HA.class_ "time" $ text $ sformat (fixed 1) t <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Time
    t' = U.convertAmount U.Hour unit t
    symbol = pack $ U.unitSymbol unit

formatDays :: U.SystemOfUnits -> Int -> Html
formatDays sou d = H.span H.! HA.class_ "days" $ text $  sformat int d' <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Calendar
    d' = round $ U.convertAmount U.Day unit (fromIntegral d :: Float)
    symbol = pack $ U.unitSymbol unit

formatStages :: Int -> Html
formatStages s = H.span H.! HA.class_ "stages" $ text $  sformat  int s <> thinSpace <> "stages"

formatMaybeHours :: (RealFrac a) => U.SystemOfUnits -> Maybe a -> Html
formatMaybeHours sou Nothing = formatPenance sou True Reject
formatMaybeHours sou (Just t) = formatHours sou t

formatHeight :: (RealFrac a) => U.SystemOfUnits -> a -> Html
formatHeight sou h = H.span H.! HA.class_ "height" $ text $  sformat  (fixed 0) h' <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Elevation
    h' = U.convertAmount U.Metre unit h
    symbol = pack $ U.unitSymbol unit

-- | Default English translation
renderCaminoMsgDefault :: U.SystemOfUnits -> Config -> CaminoMsg -> Html
renderCaminoMsgDefault _ _ AboutLabel = "About"
renderCaminoMsgDefault _ _ AccessTitle = "Access"
renderCaminoMsgDefault _ _ AccessibleTitle = "Accessible"
renderCaminoMsgDefault _ _ AccommodationLabel = "Accommodation"
renderCaminoMsgDefault _ _ AccommodationPenanceLabel = "Accommodation"
renderCaminoMsgDefault _ _ AccommodationPreferencesLabel = "Accommodation Preferences"
renderCaminoMsgDefault _ _ AddressTitle = "Address"
renderCaminoMsgDefault _ _ AirportDescription = "An airport, airfield or aerodrome"
renderCaminoMsgDefault _ _ AirportTitle = "Airport"
renderCaminoMsgDefault _ _ AlwaysOpenLabel = "Always Open"
renderCaminoMsgDefault _ _ AfterText = "after"
renderCaminoMsgDefault _ _ ArtworkDescription = "A piece of art"
renderCaminoMsgDefault _ _ ArtworkTitle = "Art"
renderCaminoMsgDefault _ _ AscentLabel = "Ascent"
renderCaminoMsgDefault sou _ (AscentMsg ascent) = [shamlet|Ascent ^{formatHeight sou ascent}|]
renderCaminoMsgDefault _ _ AustereTitle = "Austere"
renderCaminoMsgDefault _ _ BankTitle = "Bank"
renderCaminoMsgDefault _ _ BeachDescription = "A beach (sea or river)"
renderCaminoMsgDefault _ _ BeachTitle = "Beach"
renderCaminoMsgDefault _ _ BedlinenTitle = "Bedlinen"
renderCaminoMsgDefault _ _ BeforeText = "before"
renderCaminoMsgDefault _ _ BicycleRepairTitle = "Bicycle Repair"
renderCaminoMsgDefault _ _ BicycleStorageTitle = "Bicycle Storage"
renderCaminoMsgDefault _ _ BoatTitle = "Boat/Canoe (paddled)"
renderCaminoMsgDefault _ _ BreakfastTitle = "Breakfast"
renderCaminoMsgDefault _ _ BridgeDescription = "A bridge acting as a waypoint or point of interest"
renderCaminoMsgDefault _ _ BridgeTitle = "Bridge"
renderCaminoMsgDefault _ _ BusTitle = "Bus"
renderCaminoMsgDefault _ _ CalendarTitle = "Calendar"
renderCaminoMsgDefault _ _ CampGroundTitle = "Camping Ground"
renderCaminoMsgDefault _ _ CaminoLabel = "Camino"
renderCaminoMsgDefault _ _ CampingTitle = "Camping"
renderCaminoMsgDefault _ _ CampSiteTitle = "Camp-site"
renderCaminoMsgDefault _ _ CasaRuralTitle = "Casa Rural/Quinta"
renderCaminoMsgDefault _ _ CasualTitle = "Casual"
renderCaminoMsgDefault _ _ CathedralDescription = "A cathedral, basillica, shrine or other large religious building"
renderCaminoMsgDefault _ _ CathedralTitle = "Cathedral"
renderCaminoMsgDefault _ _ ChurchDescription = "A church or chapel"
renderCaminoMsgDefault _ _ ChurchTitle = "Church"
renderCaminoMsgDefault _ _ CityDescription = "A large urban area with multiple options for services and accommodation"
renderCaminoMsgDefault _ _ CityTitle = "City"
renderCaminoMsgDefault _ _ ClosedDayText = "Closed Day"
renderCaminoMsgDefault _ _ ClosedText = "closed"
renderCaminoMsgDefault _ _ ComfortableTitle = "Comfortable"
renderCaminoMsgDefault _ _ ComfortLabel = "Comfort"
renderCaminoMsgDefault _ _ CopyLinkTitle = "Copy Link"
renderCaminoMsgDefault _ _ CrossDescription = "A roadside crucifix"
renderCaminoMsgDefault _ _ CrossTitle = "Cross"
renderCaminoMsgDefault _ _ CulturalPoiTitle = "Cultural"
renderCaminoMsgDefault _ _ CyclingTitle = "Cycling"
renderCaminoMsgDefault _ _ CyclePathTitle = "Cycle Path (bicycles only)"
renderCaminoMsgDefault _ _ DailyLabel = "Daily"
renderCaminoMsgDefault _ _ DateLabel = "Date"
renderCaminoMsgDefault _ _ DayLabel = "Day"
renderCaminoMsgDefault sou _ (DaysElapsedMsg d) = toHtml $ (sformat int d) <> " days total"
renderCaminoMsgDefault _ _ DayServicesPenanceLabel = "Missing Services (Day)"
renderCaminoMsgDefault _ _ DayServicesPreferencesLabel = "Missing Day Services"
renderCaminoMsgDefault sou _ (DaysWalkingMsg d) = toHtml $ (sformat int d) <> " days walking"
renderCaminoMsgDefault _ _ DayText = "day"
renderCaminoMsgDefault _ _ DescentLabel = "Descent"
renderCaminoMsgDefault sou _ (DescentMsg ascent) = [shamlet|Descent ^{formatHeight sou ascent}|]
renderCaminoMsgDefault _ _ DirectionsTitle = "Directions"
renderCaminoMsgDefault _ _ DinnerTitle = "Dinner"
renderCaminoMsgDefault _ _ DistanceAdjustLabel = "Distance Adjustment"
renderCaminoMsgDefault sou _ (DistanceMsg actual perceived) = [shamlet|Distance ^{formatDistance sou actual} (feels like ^{formatMaybeDistance sou perceived})|]
renderCaminoMsgDefault sou _ (DistanceFormatted distance) = formatDistance sou distance
renderCaminoMsgDefault _ _ DistanceLabel = "Distance"
renderCaminoMsgDefault _ _ DistancePenanceLabel = "Distance"
renderCaminoMsgDefault sou _ DistancePreferencesLabel = [shamlet|Distance Preferences (#{U.unitSymbol (U.preferredUnit sou U.Distance)})|]
renderCaminoMsgDefault sou _ DistancePreferencesPerceivedLabel = [shamlet|Perceived Distance Preferences (#{U.unitSymbol (U.preferredUnit sou U.Distance)})|]
renderCaminoMsgDefault _ _ DoubleTitle = "Double"
renderCaminoMsgDefault _ _ DoubleWcTitle = "Double with WC"
renderCaminoMsgDefault _ _ DownloadKmlTitle = "Download KML"
renderCaminoMsgDefault _ _ DownloadSpreadsheetTitle = "Download Spreadsheet"
renderCaminoMsgDefault _ _ DryerTitle = "Dryer"
renderCaminoMsgDefault sou _ (ElevationFormatted elev) = formatHeight sou elev
renderCaminoMsgDefault sou _ ElevationLabel = "Elevation (" <> (toHtml $ U.unitSymbol $ U.preferredUnit sou U.Elevation) <> ")"
renderCaminoMsgDefault _ _ EventsLabel = "Events"
renderCaminoMsgDefault _ _ ExceptText = "except"
renderCaminoMsgDefault _ _ ExcludedStopsLabel = "Excluded Stops"
renderCaminoMsgDefault _ _ FailureLabel = "Failure"
renderCaminoMsgDefault _ _ FarmlandDescription = "Farmland, greenhouses, pasture etc."
renderCaminoMsgDefault _ _ FarmlandTitle = "Farmland"
renderCaminoMsgDefault _ _ FatiguePenanceLabel = "Fatigue"
renderCaminoMsgDefault _ _ FerryTitle = "Ferry"
renderCaminoMsgDefault _ _ FestivalEventTitle = "Festival"
renderCaminoMsgDefault _ _ FinishDateLabel = "Finish Date"
renderCaminoMsgDefault _ _ FinishLocationLabel = "Finish Location"
renderCaminoMsgDefault _ _ FitnessLabel = "Fitness"
renderCaminoMsgDefault _ _ FitTitle = "Fit"
renderCaminoMsgDefault _ _ FoodEventTitle = "Food"
renderCaminoMsgDefault _ _ FountainDescription = "A fountain or spring, either a large decorative fountain or a source of water"
renderCaminoMsgDefault _ _ FountainTitle = "Fountain"
renderCaminoMsgDefault _ _ FromLabel = "From"
renderCaminoMsgDefault _ _ FrugalTitle = "Frugal"
renderCaminoMsgDefault _ _ GiteTitle = "Gîtes d'Étape"
renderCaminoMsgDefault _ _ GroceriesTitle = "Groceries"
renderCaminoMsgDefault _ _ GuestHouseTitle = "Guesthouse"
renderCaminoMsgDefault _ _ HalfBoardTitle = "Half-Board"
renderCaminoMsgDefault _ _ HandwashTitle = "Handwash"
renderCaminoMsgDefault _ _ HazardDescription = "A road crossing, bridge, slope etc. that needs to be treated with caution"
renderCaminoMsgDefault _ _ HazardTitle = "Hazard"
renderCaminoMsgDefault _ _ HeatingTitle = "Heating"
renderCaminoMsgDefault _ _ HelpLabel = "Help"
renderCaminoMsgDefault _ _ HistoricalPoiTitle = "Historical"
renderCaminoMsgDefault _ _ HistoricalDescription = "A historical site, archaeological site, ruin or other historical point of interest"
renderCaminoMsgDefault _ _ HistoricalTitle = "Historical site"
renderCaminoMsgDefault _ _ HolidayEventTitle = "Holiday"
renderCaminoMsgDefault _ _ HolidaysLabel = "Holidays"
renderCaminoMsgDefault _ _ HomeStayTitle = "Home Stay"
renderCaminoMsgDefault _ _ HostelTitle = "Hostel"
renderCaminoMsgDefault _ _ HotelTitle = "Hotel"
renderCaminoMsgDefault _ _ HoursTitle = "Hours"
renderCaminoMsgDefault _ _ HouseTitle = "House"
renderCaminoMsgDefault _ _ IdentifierLabel = "ID"
renderCaminoMsgDefault _ _ IndustryTitle = "Industrial"
renderCaminoMsgDefault _ _ InformationLabel = "Information"
renderCaminoMsgDefault _ _ InformationTitle = "Information"
renderCaminoMsgDefault _ _ InformationDescription = "Information on the source data used when generating this plan."
renderCaminoMsgDefault _ _ InformationPointDescription = "A local tourist office, guide, etc. not specifically geared towards pilgrims"
renderCaminoMsgDefault _ _ InformationPointTitle = "Information Point"
renderCaminoMsgDefault _ _ IntersectionDescription = "A road or path intersection"
renderCaminoMsgDefault _ _ IntersectionTitle = "Intersection"
renderCaminoMsgDefault _ _ JourneyLabel = "Journey"
renderCaminoMsgDefault _ _ JunctionTitle = "Junction"
renderCaminoMsgDefault _ _ KeyLabel = "Key"
renderCaminoMsgDefault _ _ KitchenTitle = "Kitchen"
renderCaminoMsgDefault _ _ LatitudeLabel = "Latitude"
renderCaminoMsgDefault sou _ (LegPenanceMsg penance') = [shamlet|+^{formatPenance sou True penance'}|]
renderCaminoMsgDefault _ _ LinkOut = [shamlet|More information|]
renderCaminoMsgDefault _ _ LocationLabel = "Location"
renderCaminoMsgDefault _ _ LocationPenanceLabel = "Location"
renderCaminoMsgDefault _ _ LocationPreferencesLabel = "Location Preferences"
renderCaminoMsgDefault _ _ LocationsLabel = "Locations"
renderCaminoMsgDefault _ _ LockersTitle = "Lockers"
renderCaminoMsgDefault _ _ LongitudeLabel = "Longitude"
renderCaminoMsgDefault _ _ LookoutDescription = "A lookout or scenic view"
renderCaminoMsgDefault _ _ LookoutTitle = "Lookout"
renderCaminoMsgDefault _ _ LuxuriousTitle = "Luxurious"
renderCaminoMsgDefault _ _ MapLabel = "Map"
renderCaminoMsgDefault _ _ MassEventTitle = "Mass"
renderCaminoMsgDefault _ _ MattressTitle = "Mattress"
renderCaminoMsgDefault _ _ MedicalTitle = "Medical"
renderCaminoMsgDefault _ _ MiscPenanceLabel = "Other"
renderCaminoMsgDefault _ _ MonasteryDescription = "A monastery or cloister, often with a hospice attached"
renderCaminoMsgDefault _ _ MonasteryTitle = "Monastery"
renderCaminoMsgDefault _ _ MunicipalDescription = "A town square, market place, town hall or other municipal building"
renderCaminoMsgDefault _ _ MunicipalTitle = "Municipal"
renderCaminoMsgDefault _ _ MuseumDescription = "A museum or gallery"
renderCaminoMsgDefault _ _ MuseumTitle = "Museum/gallery"
renderCaminoMsgDefault _ _ MusicEventTitle = "Music"
renderCaminoMsgDefault _ _ NaturalPoiTitle = "Natural"
renderCaminoMsgDefault _ _ NaturalDescription = "A nature park or area of natural beauty, as well as large landscape features such as reservoirs or dams"
renderCaminoMsgDefault _ _ NaturalTitle = "Nature park"
renderCaminoMsgDefault _ _ NormalTitle = "Normal"
renderCaminoMsgDefault _ _ NotesLabel = "Notes"
renderCaminoMsgDefault _ _ OpenHoursTitle = "Open Hours"
renderCaminoMsgDefault _ _ OpenText = "open"
renderCaminoMsgDefault _ _ ParkDescription = "A park or garden"
renderCaminoMsgDefault _ _ ParkTitle = "Park"
renderCaminoMsgDefault _ _ PathLabel = "Path"
renderCaminoMsgDefault _ _ PeakDescription = "A peak or mountain pass"
renderCaminoMsgDefault _ _ PeakTitle = "Peak/pass"
renderCaminoMsgDefault sou _ (PenanceFormatted showZero penance') = formatPenance sou showZero penance'
renderCaminoMsgDefault sou _ (PenanceFormattedPlain showZero penance') = formatPenancePlain sou showZero penance'
renderCaminoMsgDefault sou _ (PenanceMsg penance') = [shamlet|Penance ^{formatPenance sou True penance'}|]
renderCaminoMsgDefault _ _ PenanceReject = "Rejected"
renderCaminoMsgDefault _ _ PenanceSummaryLabel = "Penance"
renderCaminoMsgDefault _ _ PerceivedDistanceLabel = "Perceived Distance"
renderCaminoMsgDefault _ _ PerformanceEventTitle = "Performance"
renderCaminoMsgDefault _ _ PersistentLinkTitle = "Persistent Link"
renderCaminoMsgDefault _ _ PetsTitle = "Pets"
renderCaminoMsgDefault _ _ PharmacyTitle = "Pharmacy"
renderCaminoMsgDefault _ _ PilgrimageLabel = "Pilgrimage"
renderCaminoMsgDefault _ _ PilgrimAlbergueTitle = "Pilgrim Albergue"
renderCaminoMsgDefault _ _ PilgrimMassEventTitle = "Pilgrim's Mass"
renderCaminoMsgDefault _ _ PilgrimPoiTitle = "Pilgrim"
renderCaminoMsgDefault _ _ PilgrimResourceDescription = "A pilgrim's office, rest point, etc. specifically geared towards pilgrims"
renderCaminoMsgDefault _ _ PilgrimResourceTitle = "Pilgrim Resource"
renderCaminoMsgDefault _ _ PilgrimTitle = "Pilgrim"
renderCaminoMsgDefault _ _ PlaceholderLabel = "Placeholder"
renderCaminoMsgDefault _ _ PlanLabel = "Plan"
renderCaminoMsgDefault _ _ PoiDescription = "A generic waypoint or point of interest"
renderCaminoMsgDefault _ _ PoiLabel = "Point of Interest"
renderCaminoMsgDefault _ _ PoisLabel = "Points of Interest"
renderCaminoMsgDefault _ _ (PoiTime Nothing) = ""
renderCaminoMsgDefault sou _ (PoiTime (Just time)) = [shamlet|(^{formatHours sou time} stops)|]
renderCaminoMsgDefault _ _ PoiTitle = "Locality"
renderCaminoMsgDefault _ _ PoolTitle = "Pool"
renderCaminoMsgDefault _ _ PrayerTitle = "Prayer"
renderCaminoMsgDefault _ _ PreferencesLabel = "Preferences"
renderCaminoMsgDefault _ _ PrivateAlbergueTitle = "Private Albergue"
renderCaminoMsgDefault _ _ ProgramLabel = "Program"
renderCaminoMsgDefault _ _ PromontoryDescription = "A promontory, cliff or headland"
renderCaminoMsgDefault _ _ PromontoryTitle = "Promontory/Headland"
renderCaminoMsgDefault _ _ PublicHolidayText = "Public Holiday"
renderCaminoMsgDefault _ _ QuadrupleTitle = "Quadruple"
renderCaminoMsgDefault _ _ QuadrupleWcTitle = "Quadruple with WC"
renderCaminoMsgDefault _ _ RefugeTitle = "Refuge"
renderCaminoMsgDefault _ _ RegionLabel = "Region"
renderCaminoMsgDefault _ _ RegionsLabel = "Regions"
renderCaminoMsgDefault _ _ RecreationPoiTitle = "Recreation"
renderCaminoMsgDefault _ _ ReligiousEventTitle = "Religious Ceremony"
renderCaminoMsgDefault _ _ ReligiousPoiTitle = "Religious"
renderCaminoMsgDefault _ _ RequiredStopsLabel = "Required Stops"
renderCaminoMsgDefault _ _ RestaurantTitle = "Restaurant"
renderCaminoMsgDefault _ _ RestDaysLabel = "Rest Days"
renderCaminoMsgDefault _ _ RestLocationPreferencesLabel = "Rest Location Preferences"
renderCaminoMsgDefault _ _ RestPressureLabel = "Rest Pressure"
renderCaminoMsgDefault _ _ RestPenanceLabel = "Rest"
renderCaminoMsgDefault _ _ RestpointLabel = "Rest Point"
renderCaminoMsgDefault _ _ RestpointsLabel = "Rest Points"
renderCaminoMsgDefault _ _ RestPointsPenanceLabel = "Rest Points"
renderCaminoMsgDefault _ _ RestPreferencesLabel = "Rest Preferences (days travelling)"
renderCaminoMsgDefault _ _ RestPressurePenanceLabel = "Rest Pressure"
renderCaminoMsgDefault _ _ RoadTitle = "Road/path"
renderCaminoMsgDefault _ _ RouteLabel = "Route"
renderCaminoMsgDefault _ _ RouteServicesPreferencesLabel = "Missing Services on Route"
renderCaminoMsgDefault _ _ RoutesLabel = "Routes"
renderCaminoMsgDefault _ _ ServicesLabel = "Services"
renderCaminoMsgDefault _ _ ServicesPreferencesLabel = "Missing Services"
renderCaminoMsgDefault _ _ SharedTitle = "Shared"
renderCaminoMsgDefault _ _ ShopDescription = "A shop of note or usefulness"
renderCaminoMsgDefault _ _ ShopTitle = "Shop"
renderCaminoMsgDefault _ _ ShowLabelsTitle = "Show camino and route labels on map"
renderCaminoMsgDefault _ _ ShowOnMapTitle = "Show on map"
renderCaminoMsgDefault _ _ SingleTitle = "Single"
renderCaminoMsgDefault _ _ SIUnitsTitle = "Kilometres, metres, hours"
renderCaminoMsgDefault _ _ SleepingBagTitle = "Sleeping Bag"
renderCaminoMsgDefault _ _ StablesTitle = "Stables"
renderCaminoMsgDefault _ _ StageLabel = "Stage"
renderCaminoMsgDefault _ _ (StagesMsg d) = formatStages d
renderCaminoMsgDefault _ _ StartDateLabel = "Start Date"
renderCaminoMsgDefault _ _ StartLocationLabel = "Start Location"
renderCaminoMsgDefault _ _ StationDescription = "A train or bus station"
renderCaminoMsgDefault _ _ StationTitle = "Station"
renderCaminoMsgDefault _ _ StatueDescription = "A statue, bas-relief, etc."
renderCaminoMsgDefault _ _ StatueTitle = "Statue"
renderCaminoMsgDefault _ _ StockpointLabel = "Stocking Point"
renderCaminoMsgDefault _ _ StopLabel = "Stop"
renderCaminoMsgDefault _ _ StopPenanceLabel = "Stop"
renderCaminoMsgDefault _ _ StopServicesPenanceLabel = "Missing Services (Stop)"
renderCaminoMsgDefault _ _ StopPreferencesLabel = "Stop Cost"
renderCaminoMsgDefault _ _ SuperFitTitle = "Super-fit"
renderCaminoMsgDefault _ _ TableLabel = "Table"
renderCaminoMsgDefault _ _ TheatreDescription = "A theatre, opera house or concert hall"
renderCaminoMsgDefault _ _ TheatreTitle = "Theatre"
renderCaminoMsgDefault _ _ TimeAdjustLabel = "Time Adjustment"
renderCaminoMsgDefault sou _ (TimeFormatted time) = formatHours sou time
renderCaminoMsgDefault sou _ (TimeMsg time) = [shamlet|over ^{formatMaybeHours sou time}|]
renderCaminoMsgDefault _ _ TimeLabel = "Time"
renderCaminoMsgDefault _ _ TimePenaltyLabel = "Time Penalty"
renderCaminoMsgDefault sou _ TimePreferencesLabel = [shamlet|Time Preferences (#{U.unitSymbol (U.preferredUnit sou U.Time)})|]
renderCaminoMsgDefault _ _ ToggleDetailTitle = "Show/hide additional detail"
renderCaminoMsgDefault _ _ ToLabel = "To"
renderCaminoMsgDefault _ _ TowelsTitle = "Towels"
renderCaminoMsgDefault _ _ TownDescription = "A larger locality with most services and a variety of accommodation usually available"
renderCaminoMsgDefault _ _ TownTitle = "Town"
renderCaminoMsgDefault _ _ TrailTitle = "Trail (walkers only)"
renderCaminoMsgDefault _ _ TrainTitle = "Train"
renderCaminoMsgDefault _ _ TransportLinksLabel = "Transport Links"
renderCaminoMsgDefault _ _ TravelLabel = "Travel Estimation"
renderCaminoMsgDefault _ _ TripFinishLabel = "Trip Finish"
renderCaminoMsgDefault _ _ TripStartLabel = "Trip Start"
renderCaminoMsgDefault _ _ TripleTitle = "Triple"
renderCaminoMsgDefault _ _ TripleWcTitle = "Triple with WC"
renderCaminoMsgDefault _ _ TypeLabel = "Type"
renderCaminoMsgDefault _ _ UnfitTitle = "Unfit"
renderCaminoMsgDefault _ _ UnitsLabel = "Units"
renderCaminoMsgDefault _ _ UnusedLabel = "Unused"
renderCaminoMsgDefault _ _ USUnitsTitle = "Miles, feet, hours"
renderCaminoMsgDefault _ _ VeryFitTitle = "Very fit"
renderCaminoMsgDefault _ _ VeryUnfitTitle = "Very unfit"
renderCaminoMsgDefault _ _ VillageDescription = "A smaller locality with limited, or no, services and accommodation"
renderCaminoMsgDefault _ _ VillageTitle = "Village"
renderCaminoMsgDefault _ _ WalkingTitle = "Walking"
renderCaminoMsgDefault _ _ WalkingNaismithTitle = "Walking (strong walkers)"
renderCaminoMsgDefault _ _ WarningTitle= "Warning"
renderCaminoMsgDefault _ _ WashingMachineTitle = "Washing Machine"
renderCaminoMsgDefault _ _ WaypointLabel = "Waypoint"
renderCaminoMsgDefault _ _ WharfDescription = "A wharf, quay or pier connecting to a boat"
renderCaminoMsgDefault _ _ WharfTitle = "Wharf"
renderCaminoMsgDefault _ _ WiFiTitle = "WiFi"
renderCaminoMsgDefault _ _ WineryDescription = "A winery or port warehouse"
renderCaminoMsgDefault _ _ WineryTitle = "Winery"
renderCaminoMsgDefault _ _ msg = [shamlet|Unknown message #{show msg}|]

hasLocalisedText :: (Tagged a) => [Locale] -> Localised a -> Bool
hasLocalisedText locales locd = maybe False (\t -> plainText t /= "") $ localise locales locd

renderLocalisedText :: (Tagged a) => [Locale] -> Bool -> Bool -> Bool -> Localised a -> Html
renderLocalisedText locales plain attr js locd = let
    elt = localise locales locd
    txt = maybe "" plainText elt
    txt' = if attr then replace "\"" "'" txt else txt
    txt'' = if js then replace "'" "\\'" txt' else txt'
    loc = maybe rootLocale locale elt
    lng = localeLanguageTag loc
  in
    if plain || attr || Data.Text.null lng then
      toHtml txt''
    else
      H.span H.! HA.lang (toValue lng) $ Text.Blaze.Html.text txt''

renderLocalisedDate :: (FormatTime t) => U.SystemOfUnits -> Bool -> [Locale] -> t -> Html
renderLocalisedDate sou weekDay [] day = renderLocalisedDate sou weekDay [rootLocale] day
renderLocalisedDate _sou weekDay (loc:_) day = toHtml $ if weekDay then dwf ++ df else df
  where
    tl = localeTimeLocale loc
    df = formatTime tl (dateFmt tl) day
    dwf = formatTime tl "%a " day

renderLocalisedTime :: (FormatTime t) => U.SystemOfUnits -> [Locale] -> String -> t -> Html
renderLocalisedTime sou [] fmt t = renderLocalisedTime sou [rootLocale] fmt t
renderLocalisedTime _sou (loc:_) fmt t = toHtml $ formatTime (localeTimeLocale loc) fmt t

renderLocalisedMonth :: U.SystemOfUnits -> [Locale] -> MonthOfYear -> Html
renderLocalisedMonth sou [] t = renderLocalisedMonth sou [rootLocale] t
renderLocalisedMonth _sou (loc:_) t = toHtml $ snd $ (months $ localeTimeLocale loc) !! t

renderLocalisedOrdinal :: U.SystemOfUnits -> [Locale] -> Int -> Html
renderLocalisedOrdinal sou [] o = renderLocalisedOrdinal sou [rootLocale] o
renderLocalisedOrdinal _sou (loc:_) o = toHtml $ (localeOrdinalRender loc) o

renderLocalisedBeforeAfter :: U.SystemOfUnits -> Config -> [Locale] -> Int -> Html
renderLocalisedBeforeAfter sou config [] n = renderLocalisedBeforeAfter sou config [rootLocale] n
renderLocalisedBeforeAfter sou config locales n = renderCaminoMsg sou config locales $ if n < 0 then BeforeText else AfterText

renderLocalisedWeekOfMonth :: U.SystemOfUnits -> [Locale] -> WeekOfMonth -> Html
renderLocalisedWeekOfMonth sou [] wom = renderLocalisedWeekOfMonth sou [rootLocale] wom
renderLocalisedWeekOfMonth _sou (loc:_) wom = toHtml $ (localeWeekOfMonthRender loc) wom

renderLocalisedDayOfWeek :: U.SystemOfUnits -> [Locale] -> DayOfWeek -> Html
renderLocalisedDayOfWeek sou [] dow = renderLocalisedDayOfWeek sou [rootLocale] dow
renderLocalisedDayOfWeek _sou (loc:_) dow = let
    idx = (fromEnum dow) - 1
    names = wDays $ localeTimeLocale loc
    name = names !! idx
  in
    toHtml $ snd name

renderLocalisedPublicHoliday :: U.SystemOfUnits -> Config -> [Locale] -> Text -> Html
renderLocalisedPublicHoliday sou config locs rid = [shamlet|^{renderCaminoMsg sou config locs PublicHolidayText} (^{name})|]
  where
    region = (regionConfigLookup $ getRegionConfig config) rid
    name = maybe (toHtml rid) (\r -> renderLocalisedText locs False False False (regionName r)) region

renderLocalisedClosedDay :: U.SystemOfUnits -> Config -> [Locale] -> Text -> Html
renderLocalisedClosedDay sou config locs rid = [shamlet|^{renderCaminoMsg sou config locs ClosedDayText} (^{name})|]
  where
    region = (regionConfigLookup $ getRegionConfig config) rid
    name = maybe (toHtml rid) (\r -> renderLocalisedText locs False False False (regionName r)) region

-- | Convert a message placeholder into actual HTML
renderCaminoMsg :: U.SystemOfUnits -- ^ The system of units to use for distances, times etc
  -> Config -- ^ The configuration
  -> [Locale] -- ^ The locale list
  -> CaminoMsg -- ^ The message
  -> Html -- ^ The resulting Html to interpolate
renderCaminoMsg sou config locales (ClosedDayLabel region) = renderLocalisedClosedDay sou config locales region
renderCaminoMsg sou _config locales (DateMsg day) = H.span H.! HA.class_ "date" $ renderLocalisedDate sou True locales day
renderCaminoMsg sou _config locales (DateRangeMsg day1 day2) = H.span H.! HA.class_ "date-range" $
  if day1 == day2 then
    renderLocalisedDate sou True locales day1
  else do
    renderLocalisedDate sou True locales day1
    " - "
    renderLocalisedDate sou True locales day2
renderCaminoMsg sou _config locales (DayOfWeekName dow) = renderLocalisedTime sou locales "%a" dow
renderCaminoMsg _sou _config _locales (DayOfMonthName dom) = toHtml dom
renderCaminoMsg sou _config locales (DaySummaryMsg day) = [shamlet|
  #{start'} to #{finish'}
  ^{formatDistance sou $ metricsDistance metrics} (feels like ^{formatMaybeDistance sou $ metricsPerceivedDistance metrics})
  over ^{formatMaybeHours sou $ metricsTime metrics}
  Ascent ^{formatHeight sou $ metricsAscent metrics} Descent ^{formatHeight sou $ metricsDescent metrics}
  Penance ^{formatPenance sou True $ metricsPenance metrics}
  |]
  where
   metrics = score day
   start' = renderLocalisedText locales False False False (locationName $ start day)
   finish' = renderLocalisedText locales False False False (locationName $ finish day)
renderCaminoMsg sou _config locales (JourneySummaryMsg journey) = [shamlet|
  #{start'} to #{finish'}
  Total distance ^{formatDistance sou $ metricsDistance metrics} (feels like ^{formatMaybeDistance sou $ metricsPerceivedDistance metrics})
  over ^{formatDays sou $ Prelude.length $ path journey}
  Total ascent ^{formatHeight sou $ metricsAscent metrics} Total descent ^{formatHeight sou $ metricsDescent metrics}
  Penance ^{formatPenance sou True $ metricsPenance metrics}
  |]
  where
   metrics = score journey
   start' = renderLocalisedText locales False False False (locationName $ start journey)
   finish' = renderLocalisedText locales False False False (locationName $ finish journey)
renderCaminoMsg _sou _config locales (LinkTitle locd defd) = if hasLocalisedText locales locd then
    renderLocalisedText locales True True False locd
  else
    renderLocalisedText locales True True False defd
renderCaminoMsg sou config locales (ListMsg msgs) = [shamlet|
  <ul .comma-separated-list>
    $forall m <- msgs
      <li>^{renderCaminoMsg sou config locales m}
  |]
renderCaminoMsg sou _config locales (MonthOfYearName moy) = renderLocalisedMonth sou locales moy
renderCaminoMsg sou config locales (OrdinalAfterWeekday nth dow) = [shamlet|^{renderLocalisedOrdinal sou locales (abs nth)} ^{renderLocalisedDayOfWeek sou locales dow} ^{renderLocalisedBeforeAfter sou config locales nth}|]
renderCaminoMsg sou config locales (OrdinalBeforeAfter nth unit) = [shamlet|^{renderLocalisedOrdinal sou locales (abs nth)} ^{renderCaminoMsg sou config locales unit} ^{renderLocalisedBeforeAfter sou config locales nth}|]
renderCaminoMsg _sou config locales (NamedCalendarLabel key) = renderLocalisedText locales False False False $ getCalendarName config key
renderCaminoMsg sou _config locales (NthWeekdayText nth dow) = [shamlet|^{renderLocalisedWeekOfMonth sou locales nth} ^{renderLocalisedDayOfWeek sou locales dow}|]
renderCaminoMsg sou config locales (PublicHolidayLabel region) = renderLocalisedPublicHoliday sou config locales region
renderCaminoMsg sou _config locales (Time time) = renderLocalisedTime sou locales "%H%M" time
renderCaminoMsg _sou _config locales (Txt locd) = renderLocalisedText locales False False False locd
renderCaminoMsg _sou _config locales (TxtPlain attr js locd) = renderLocalisedText locales True attr js locd
renderCaminoMsg sou config _ msg = renderCaminoMsgDefault sou config msg

-- | Render a camino message as un-markuped text
renderCaminoMsgText :: U.SystemOfUnits -> Config -> [Locale] -> CaminoMsg -> Text
renderCaminoMsgText sou config locales (ListMsg msgs) = commaJoin $ Prelude.map (renderCaminoMsgText sou config locales) msgs
renderCaminoMsgText sou config locales msg = renderMarkupToText $ renderCaminoMsg sou config locales msg

renderMarkupToText :: TB.MarkupM a -> Text
renderMarkupToText (TB.Parent _ _ _ c)           = renderMarkupToText c
renderMarkupToText (TB.CustomParent _ c)         = renderMarkupToText c
renderMarkupToText (TB.Content s _)              = renderChoiceStringToText s
renderMarkupToText (TB.Append c1 c2)             = renderMarkupToText c1 <> renderMarkupToText c2
renderMarkupToText (TB.AddAttribute _ _ _ c)     = renderMarkupToText c
renderMarkupToText (TB.AddCustomAttribute _ _ c) = renderMarkupToText c
renderMarkupToText _                          = ""

renderChoiceStringToText :: TB.ChoiceString -> Text
renderChoiceStringToText (TB.Static ss) = TB.getText ss
renderChoiceStringToText (TB.String s) = pack s
renderChoiceStringToText (TB.Text t) = t
renderChoiceStringToText (TB.ByteString bs) = decodeUtf8Lenient bs
renderChoiceStringToText (TB.PreEscaped c) = renderChoiceStringToText c
renderChoiceStringToText (TB.External c) = renderChoiceStringToText c
renderChoiceStringToText (TB.AppendChoiceString c1 c2) = renderChoiceStringToText c1 <> renderChoiceStringToText c2
renderChoiceStringToText (TB.EmptyChoiceString) =""
