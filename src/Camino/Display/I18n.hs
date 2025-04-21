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
  , formatHours
  , rejectSymbol
  , renderCaminoMsg
  , renderCaminoMsgText
) where

import Camino.Camino
import Camino.Config
import Camino.Planner
import Data.Localised
import Data.Region
import Data.Text
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Text.Lazy (toStrict)
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Data.Util (commaJoin)
import Formatting (fixed, int, sformat)
import Text.Blaze.Html (Html, contents, text, text, toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Internal as TB
import Text.Hamlet
import Debug.Trace

-- | Message placeholders for the camino
data CaminoMsg =
    AboutLabel
  | AccessibleTitle
  | AccommodationLabel
  | AccommodationPenanceMsg Penance
  | AccommodationPreferencesLabel
  | AddressTitle
  | AfterText
  | AlwaysOpenLabel
  | ArtworkTitle
  | AscentLabel
  | AscentMsg Float
  | AustereTitle
  | BankTitle
  | BeachTitle
  | BedlinenTitle
  | BeforeText
  | BicycleRepairTitle
  | BicycleStorageTitle
  | BoatTitle
  | BreakfastTitle
  | BridgeTitle
  | BusTitle
  | CalendarTitle
  | CaminoLabel
  | CampGroundTitle
  | CampingTitle
  | CampSiteTitle
  | CasualTitle
  | CathedralTitle
  | ChurchTitle
  | CityTitle
  | ClosedDayLabel Text
  | ClosedDayText
  | ClosedText
  | ComfortableTitle
  | CrossTitle
  | ComfortLabel
  | CulturalPoiTitle
  | CyclingTitle
  | CyclePathTitle
  | DailyLabel
  | DateLabel
  | DateMsg Data.Time.Calendar.Day
  | DateRangeMsg Data.Time.Calendar.Day Data.Time.Calendar.Day
  | DayOfMonthName DayOfMonth
  | DayOfWeekName DayOfWeek
  | DayServicesPenanceMsg Penance
  | DayServicesPreferencesLabel
  | DaysMsg Int
  | DaySummaryMsg Camino.Planner.Day
  | DayText
  | DescentLabel
  | DescentMsg Float
  | DinnerTitle
  | DirectionsTitle
  | DistanceAdjustMsg Penance
  | DistanceFormatted Float
  | DistanceLabel
  | DistanceMsg Float (Maybe Float) -- ^ actual/perceived distance
  | DistancePenanceMsg (Maybe Float)
  | DistancePreferencesLabel
  | DistancePreferencesPerceivedLabel
  | DoubleTitle
  | DoubleWcTitle
  | DryerTitle
  | EventsLabel
  | ExceptText
  | ExcludedStopsLabel
  | FailureLabel
  | FatiguePenanceMsg Penance
  | FerryTitle
  | FestivalEventTitle
  | FinishDateLabel
  | FinishLocationLabel
  | FitnessLabel
  | FitTitle
  | FoodEventTitle
  | FromLabel
  | FrugalTitle
  | FountainTitle
  | GiteTitle
  | GroceriesTitle
  | GuestHouseTitle
  | HandwashTitle
  | HazardTitle
  | HeatingTitle
  | HelpLabel
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
  | InformationLabel
  | InformationTitle
  | InformationDescription
  | IntersectionTitle
  | JunctionTitle
  | KeyLabel
  | KitchenTitle
  | LatitudeLabel
  | LegPenanceMsg Penance
  | LinkTitle (Localised TaggedURL) (Localised TaggedText)
  | LinkOut
  | ListMsg [CaminoMsg]
  | LocationPenanceMsg Penance
  | LocationPreferencesLabel
  | LocationLabel
  | LocationsLabel
  | LockersTitle
  | LongitudeLabel
  | LookoutTitle
  | LuxuriousTitle
  | MapLabel
  | MassEventTitle
  | MattressTitle
  | MedicalTitle
  | MiscPenanceMsg Penance
  | MonasteryTitle
  | MonthOfYearName MonthOfYear
  | MunicipalTitle
  | MuseumTitle
  | MusicEventTitle
  | NamedCalendarLabel Text
  | NaturalPoiTitle
  | NaturalTitle
  | NormalTitle
  | NotesLabel
  | NthWeekdayText WeekOfMonth DayOfWeek
  | OpenHoursTitle
  | OpenText
  | OrdinalAfterWeekday Int DayOfWeek
  | OrdinalBeforeAfter Int CaminoMsg
  | OtherLabel
  | ParkTitle
  | PathLabel
  | PeakTitle
  | PenanceFormatted Penance
  | PenanceFormattedPlain Penance
  | PenanceMsg Penance
  | PenanceReject
  | PenanceSummaryLabel
  | PerceivedDistanceLabel
  | PerformanceEventTitle
  | PetsTitle
  | PharmacyTitle
  | PilgrimAlbergueTitle
  | PilgrimMassEventTitle
  | PilgrimPoiTitle
  | PilgrimResourceTitle
  | PilgrimTitle
  | PlaceholderLabel
  | PlanLabel
  | PoiLabel
  | PoisLabel
  | PoiTime (Maybe Float)
  | PoiTitle
  | PoolTitle
  | PrayerTitle
  | PreferencesLabel
  | PrivateAlbergueTitle
  | ProgramLabel
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
  | RestPenanceMsg Penance
  | RestpointLabel
  | RestPreferencesLabel
  | RoadTitle
  | RouteLabel
  | RouteServicesPreferencesLabel
  | RoutesLabel
  | ServicesLabel
  | ServicesPreferencesLabel
  | SharedTitle
  | ShopTitle
  | ShowOnMapTitle
  | SingleTitle
  | SleepingBagTitle
  | StablesTitle
  | StagesMsg Int
  | StartDateLabel
  | StartLocationLabel
  | StatueTitle
  | StockpointLabel
  | StopLabel
  | StopPenanceMsg Penance
  | StopPreferencesLabel
  | StopServicesPenanceMsg Penance
  | SuperFitTitle
  | TableLabel
  | TheatreTitle
  | Time TimeOfDay
  | TimeAdjustMsg Penance
  | TimeLabel
  | TimeMsg (Maybe Float)
  | TimeMsgPlain Float
  | TimePenaltyLabel
  | TimePreferencesLabel
  | ToLabel
  | TowelsTitle
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
  | UnfitTitle
  | UnusedLabel
  | VeryFitTitle
  | VeryUnfitTitle
  | VillageTitle
  | WalkingNaismithTitle
  | WalkingTitle
  | WarningTitle
  | WashingMachineTitle
  | WaypointLabel
  | WiFiTitle
  | WineryTitle
  deriving (Show)

rejectSymbol :: Text
rejectSymbol = "\x25c6"

thinSpace :: Text
thinSpace = "\x2009"

-- Note the use of direct blaze scans in the following code, rather than shamlet.
-- Hamlet seems to generate tags as chunks of content, rather than proper blaze HTML,
-- so the direct formatting allows renderCaminoMsgText to strip tags properly.
formatPenance :: Penance -> Html
formatPenance Reject = H.span H.! HA.class_ "penance rejected" H.! HA.title "Rejected" $ text rejectSymbol
formatPenance (Penance p) = H.span H.! HA.class_ "penance" $ text $  sformat  (fixed 1) p <> thinSpace <> "km"

formatPenancePlain :: Penance -> Html
formatPenancePlain Reject = "Rejected"
formatPenancePlain (Penance p) = text $  sformat  (fixed 1) p <> "km"

formatDistance :: (Real a) => a -> Html
formatDistance d = H.span H.! HA.class_ "distance" $ text $ sformat (fixed 1) d <> thinSpace <> "km"

formatMaybeDistance :: (Real a) => Maybe a -> Html
formatMaybeDistance Nothing = formatPenance Reject
formatMaybeDistance (Just d) = formatDistance d

formatHours :: (Real a) => a -> Html
formatHours t = H.span H.! HA.class_ "time" $ text $ sformat (fixed 1) t <> thinSpace <> "hrs"

formatDays :: Int -> Html
formatDays d = H.span H.! HA.class_ "days" $ text $  sformat  int d <> thinSpace <> "days"

formatStages :: Int -> Html
formatStages s = H.span H.! HA.class_ "stages" $ text $  sformat  int s <> thinSpace <> "stages"

formatMaybeHours :: (Real a) => Maybe a -> Html
formatMaybeHours Nothing = formatPenance Reject
formatMaybeHours (Just t) = formatHours t

formatHeight :: (Real a) => a -> Html
formatHeight h = H.span H.! HA.class_ "height" $ text $  sformat  (fixed 0) h <> thinSpace <> "m"

-- | Default English translation
renderCaminoMsgDefault :: Config -> CaminoMsg -> Html
renderCaminoMsgDefault _ AboutLabel = "About"
renderCaminoMsgDefault _ AccessibleTitle = "Accessible"
renderCaminoMsgDefault _ AccommodationLabel = "Accommodation"
renderCaminoMsgDefault _ (AccommodationPenanceMsg penance') = [shamlet|Accommodation ^{formatPenance penance'}|]
renderCaminoMsgDefault _ AccommodationPreferencesLabel = "Accommodation Preferences"
renderCaminoMsgDefault _ AddressTitle = "Address"
renderCaminoMsgDefault _ AlwaysOpenLabel = "Always Open"
renderCaminoMsgDefault _ AfterText = "after"
renderCaminoMsgDefault _ ArtworkTitle = "Art"
renderCaminoMsgDefault _ AscentLabel = "Ascent"
renderCaminoMsgDefault _ (AscentMsg ascent) = [shamlet|Ascent ^{formatHeight ascent}|]
renderCaminoMsgDefault _ AustereTitle = "Austere"
renderCaminoMsgDefault _ BankTitle = "Bank"
renderCaminoMsgDefault _ BeachTitle = "Beach"
renderCaminoMsgDefault _ BedlinenTitle = "Bedlinen"
renderCaminoMsgDefault _ BeforeText = "before"
renderCaminoMsgDefault _ BicycleRepairTitle = "Bicycle Repair"
renderCaminoMsgDefault _ BicycleStorageTitle = "Bicycle Storage"
renderCaminoMsgDefault _ BoatTitle = "Boat/Canoe (paddled)"
renderCaminoMsgDefault _ BreakfastTitle = "Breakfast"
renderCaminoMsgDefault _ BridgeTitle = "Bridge"
renderCaminoMsgDefault _ BusTitle = "Bus"
renderCaminoMsgDefault _ CalendarTitle = "Calendar"
renderCaminoMsgDefault _ CampGroundTitle = "Camping Ground"
renderCaminoMsgDefault _ CaminoLabel = "Camino"
renderCaminoMsgDefault _ CampingTitle = "Camping"
renderCaminoMsgDefault _ CampSiteTitle = "Camp-site"
renderCaminoMsgDefault _ CasualTitle = "Casual"
renderCaminoMsgDefault _ CathedralTitle = "Cathedral"
renderCaminoMsgDefault _ ChurchTitle = "Church"
renderCaminoMsgDefault _ CityTitle = "City"
renderCaminoMsgDefault _ ClosedDayText = "Closed Day"
renderCaminoMsgDefault _ ClosedText = "closed"
renderCaminoMsgDefault _ ComfortableTitle = "Comfortable"
renderCaminoMsgDefault _ ComfortLabel = "Comfort"
renderCaminoMsgDefault _ CrossTitle = "Cross"
renderCaminoMsgDefault _ CulturalPoiTitle = "Cultural"
renderCaminoMsgDefault _ CyclingTitle = "Cycling"
renderCaminoMsgDefault _ CyclePathTitle = "Cycle Path (bicycles only)"
renderCaminoMsgDefault _ DailyLabel = "Daily"
renderCaminoMsgDefault _ DateLabel = "Date"
renderCaminoMsgDefault _ (DayServicesPenanceMsg penance') = [shamlet|Missing Services (Day) ^{formatPenance penance'}|]
renderCaminoMsgDefault _ DayServicesPreferencesLabel = "Missing Day Services"
renderCaminoMsgDefault _ (DaysMsg d) = formatDays d
renderCaminoMsgDefault _ DayText = "day"
renderCaminoMsgDefault _ DescentLabel = "Descent"
renderCaminoMsgDefault _ (DescentMsg ascent) = [shamlet|Descent ^{formatHeight ascent}|]
renderCaminoMsgDefault _ DirectionsTitle = "Directions"
renderCaminoMsgDefault _ DinnerTitle = "Dinner"
renderCaminoMsgDefault _ (DistanceAdjustMsg penance') = [shamlet|Distance Adjustment ^{formatPenance penance'}|]
renderCaminoMsgDefault _ (DistanceMsg actual perceived) = [shamlet|Distance ^{formatDistance actual} (feels like ^{formatMaybeDistance perceived})|]
renderCaminoMsgDefault _ (DistanceFormatted distance) = formatDistance distance
renderCaminoMsgDefault _ DistanceLabel = "Distance"
renderCaminoMsgDefault _ (DistancePenanceMsg distance) = [shamlet|Distance ^{formatMaybeDistance distance}|]
renderCaminoMsgDefault _ DistancePreferencesLabel = "Distance Preferences (km)"
renderCaminoMsgDefault _ DistancePreferencesPerceivedLabel = "Perceived Distance Preferences (km)"
renderCaminoMsgDefault _ DoubleTitle = "Double"
renderCaminoMsgDefault _ DoubleWcTitle = "Double with WC"
renderCaminoMsgDefault _ DryerTitle = "Dryer"
renderCaminoMsgDefault _ EventsLabel = "Events"
renderCaminoMsgDefault _ ExceptText = "except"
renderCaminoMsgDefault _ ExcludedStopsLabel = "Excluded Stops"
renderCaminoMsgDefault _ FailureLabel = "Failure"
renderCaminoMsgDefault _ (FatiguePenanceMsg penance') = [shamlet|Fatigue ^{formatPenance penance'}|]
renderCaminoMsgDefault _ FerryTitle = "Ferry"
renderCaminoMsgDefault _ FestivalEventTitle = "Festival"
renderCaminoMsgDefault _ FinishDateLabel = "Finish Date"
renderCaminoMsgDefault _ FinishLocationLabel = "Finish Location"
renderCaminoMsgDefault _ FitnessLabel = "Fitness"
renderCaminoMsgDefault _ FitTitle = "Fit"
renderCaminoMsgDefault _ FoodEventTitle = "Food"
renderCaminoMsgDefault _ FountainTitle = "Fountain"
renderCaminoMsgDefault _ FromLabel = "From"
renderCaminoMsgDefault _ FrugalTitle = "Frugal"
renderCaminoMsgDefault _ GiteTitle = "Gîtes d'Étape"
renderCaminoMsgDefault _ GroceriesTitle = "Groceries"
renderCaminoMsgDefault _ GuestHouseTitle = "Guesthouse"
renderCaminoMsgDefault _ HandwashTitle = "Handwash"
renderCaminoMsgDefault _ HazardTitle = "Hazard"
renderCaminoMsgDefault _ HeatingTitle = "Heating"
renderCaminoMsgDefault _ HelpLabel = "Help"
renderCaminoMsgDefault _ HistoricalPoiTitle = "Historical"
renderCaminoMsgDefault _ HistoricalTitle = "Historical site, archaeological site or ruin"
renderCaminoMsgDefault _ HolidayEventTitle = "Holiday"
renderCaminoMsgDefault _ HolidaysLabel = "Holidays"
renderCaminoMsgDefault _ HomeStayTitle = "Home Stay"
renderCaminoMsgDefault _ HostelTitle = "Hostel"
renderCaminoMsgDefault _ HotelTitle = "Hotel"
renderCaminoMsgDefault _ HoursTitle = "Hours"
renderCaminoMsgDefault _ HouseTitle = "House"
renderCaminoMsgDefault _ IdentifierLabel = "ID"
renderCaminoMsgDefault _ InformationLabel = "Information"
renderCaminoMsgDefault _ InformationTitle = "Information"
renderCaminoMsgDefault _ InformationDescription = "Information on the source data used when generating this plan."
renderCaminoMsgDefault _ IntersectionTitle = "Intersection"
renderCaminoMsgDefault _ JunctionTitle = "Junction"
renderCaminoMsgDefault _ KeyLabel = "Key"
renderCaminoMsgDefault _ KitchenTitle = "Kitchen"
renderCaminoMsgDefault _ LatitudeLabel = "Latitude"
renderCaminoMsgDefault _ (LegPenanceMsg penance') = [shamlet|+^{formatPenance penance'}|]
renderCaminoMsgDefault _ LinkOut = [shamlet|More information|]
renderCaminoMsgDefault _ (LocationPenanceMsg penance') = [shamlet|Location ^{formatPenance penance'}|]
renderCaminoMsgDefault _ LocationPreferencesLabel = "Location Preferences"
renderCaminoMsgDefault _ LocationLabel = "Location"
renderCaminoMsgDefault _ LocationsLabel = "Locations"
renderCaminoMsgDefault _ LockersTitle = "Lockers"
renderCaminoMsgDefault _ LongitudeLabel = "Longitude"
renderCaminoMsgDefault _ LookoutTitle = "Lookout"
renderCaminoMsgDefault _ LuxuriousTitle = "Luxurious"
renderCaminoMsgDefault _ MapLabel = "Map"
renderCaminoMsgDefault _ MassEventTitle = "Mass"
renderCaminoMsgDefault _ MattressTitle = "Mattress"
renderCaminoMsgDefault _ MedicalTitle = "Medical"
renderCaminoMsgDefault _ (MiscPenanceMsg penance') = [shamlet|Other ^{formatPenance penance'}|]
renderCaminoMsgDefault _ MonasteryTitle = "Monastery"
renderCaminoMsgDefault _ MunicipalTitle = "Town square, market, etc."
renderCaminoMsgDefault _ MuseumTitle = "Museum or gallery"
renderCaminoMsgDefault _ MusicEventTitle = "Music"
renderCaminoMsgDefault _ NaturalPoiTitle = "Natural"
renderCaminoMsgDefault _ NaturalTitle = "Nature park, site of natural beauty, etc."
renderCaminoMsgDefault _ NormalTitle = "Normal"
renderCaminoMsgDefault _ NotesLabel = "Notes"
renderCaminoMsgDefault _ OpenHoursTitle = "Open Hours"
renderCaminoMsgDefault _ OpenText = "open"
renderCaminoMsgDefault _ ParkTitle = "Park or garden"
renderCaminoMsgDefault _ PathLabel = "Path"
renderCaminoMsgDefault _ PeakTitle = "Peak, pass or lookout"
renderCaminoMsgDefault _ (PenanceFormatted penance') = formatPenance penance'
renderCaminoMsgDefault _ (PenanceFormattedPlain penance') = formatPenancePlain penance'
renderCaminoMsgDefault _ (PenanceMsg penance') = [shamlet|Penance ^{formatPenance penance'}|]
renderCaminoMsgDefault _ PenanceReject = "Rejected"
renderCaminoMsgDefault _ PenanceSummaryLabel = "Penance"
renderCaminoMsgDefault _ PerceivedDistanceLabel = "Perceived Distance"
renderCaminoMsgDefault _ PerformanceEventTitle = "Performance"
renderCaminoMsgDefault _ PetsTitle = "Pets"
renderCaminoMsgDefault _ PharmacyTitle = "Pharmacy"
renderCaminoMsgDefault _ PilgrimAlbergueTitle = "Pilgrim Albergue"
renderCaminoMsgDefault _ PilgrimMassEventTitle = "Pilgrim's Mass"
renderCaminoMsgDefault _ PilgrimPoiTitle = "Pilgrim"
renderCaminoMsgDefault _ PilgrimResourceTitle = "Pilgrim Resource"
renderCaminoMsgDefault _ PilgrimTitle = "Pilgrim"
renderCaminoMsgDefault _ PlaceholderLabel = "Placeholder"
renderCaminoMsgDefault _ PlanLabel = "Plan"
renderCaminoMsgDefault _ PoiLabel = "Point of Interest"
renderCaminoMsgDefault _ PoisLabel = "Points of Interest"
renderCaminoMsgDefault _ (PoiTime Nothing) = ""
renderCaminoMsgDefault _ (PoiTime (Just time)) = [shamlet|(^{formatHours time} stops)|]
renderCaminoMsgDefault _ PoiTitle = "Locality"
renderCaminoMsgDefault _ PoolTitle = "Pool"
renderCaminoMsgDefault _ PrayerTitle = "Prayer"
renderCaminoMsgDefault _ PreferencesLabel = "Preferences"
renderCaminoMsgDefault _ PrivateAlbergueTitle = "Private Albergue"
renderCaminoMsgDefault _ ProgramLabel = "Program"
renderCaminoMsgDefault _ PromontoryTitle = "Promontory or Headland"
renderCaminoMsgDefault _ PublicHolidayText = "Public Holiday"
renderCaminoMsgDefault _ QuadrupleTitle = "Quadruple"
renderCaminoMsgDefault _ QuadrupleWcTitle = "Quadruple with WC"
renderCaminoMsgDefault _ RefugeTitle = "Refuge"
renderCaminoMsgDefault _ RegionLabel = "Region"
renderCaminoMsgDefault _ RegionsLabel = "Regions"
renderCaminoMsgDefault _ RecreationPoiTitle = "Recreation"
renderCaminoMsgDefault _ ReligiousEventTitle = "Religious Ceremony"
renderCaminoMsgDefault _ ReligiousPoiTitle = "Religious"
renderCaminoMsgDefault _ RequiredStopsLabel = "Required Stops"
renderCaminoMsgDefault _ RestaurantTitle = "Restaurant"
renderCaminoMsgDefault _ RestDaysLabel = "Rest Days"
renderCaminoMsgDefault _ RestLocationPreferencesLabel = "Rest Location Preferences"
renderCaminoMsgDefault _ (RestPenanceMsg penance') = [shamlet|Rest ^{formatPenance penance'}|]
renderCaminoMsgDefault _ RestpointLabel = "Rest Point"
renderCaminoMsgDefault _ RestPreferencesLabel = "Rest Preferences (days travelling)"
renderCaminoMsgDefault _ RoadTitle = "Road/path"
renderCaminoMsgDefault _ RouteLabel = "Route"
renderCaminoMsgDefault _ RouteServicesPreferencesLabel = "Missing Services on Route"
renderCaminoMsgDefault _ RoutesLabel = "Routes"
renderCaminoMsgDefault _ ServicesLabel = "Services"
renderCaminoMsgDefault _ ServicesPreferencesLabel = "Missing Services"
renderCaminoMsgDefault _ SharedTitle = "Shared"
renderCaminoMsgDefault _ ShopTitle = "Shop"
renderCaminoMsgDefault _ ShowOnMapTitle = "Show on map"
renderCaminoMsgDefault _ SingleTitle = "Single"
renderCaminoMsgDefault _ SleepingBagTitle = "Sleeping Bag"
renderCaminoMsgDefault _ StablesTitle = "Stables"
renderCaminoMsgDefault _ (StagesMsg d) = formatStages d
renderCaminoMsgDefault _ StartDateLabel = "Start Date"
renderCaminoMsgDefault _ StartLocationLabel = "Start Location"
renderCaminoMsgDefault _ StatueTitle = "Statue"
renderCaminoMsgDefault _ StockpointLabel = "Stocking Point"
renderCaminoMsgDefault _ StopLabel = "Stop"
renderCaminoMsgDefault _ (StopPenanceMsg penance') = [shamlet|Stop ^{formatPenance penance'}|]
renderCaminoMsgDefault _ (StopServicesPenanceMsg penance') = [shamlet|Missing Services (Stop) ^{formatPenance penance'}|]
renderCaminoMsgDefault _ StopPreferencesLabel = "Stop Cost"
renderCaminoMsgDefault _ SuperFitTitle = "Super-fit"
renderCaminoMsgDefault _ TableLabel = "Table"
renderCaminoMsgDefault _ TheatreTitle = "Theatre"
renderCaminoMsgDefault _ (TimeAdjustMsg penance') = [shamlet|Time Adjustment ^{formatPenance penance'}|]
renderCaminoMsgDefault _ (TimeMsg time) = [shamlet|over ^{formatMaybeHours time}|]
renderCaminoMsgDefault _ TimeLabel = "Time"
renderCaminoMsgDefault _ (TimeMsgPlain time) = formatHours time
renderCaminoMsgDefault _ TimePenaltyLabel = "Time Penalty"
renderCaminoMsgDefault _ TimePreferencesLabel = "Time Preferences (hours)"
renderCaminoMsgDefault _ ToLabel = "To"
renderCaminoMsgDefault _ TowelsTitle = "Towels"
renderCaminoMsgDefault _ TownTitle = "Town"
renderCaminoMsgDefault _ TrailTitle = "Trail (walkers only)"
renderCaminoMsgDefault _ TrainTitle = "Train"
renderCaminoMsgDefault _ TransportLinksLabel = "Transport Links"
renderCaminoMsgDefault _ TravelLabel = "Travel Estimation"
renderCaminoMsgDefault _ TripFinishLabel = "Trip Finish"
renderCaminoMsgDefault _ TripStartLabel = "Trip Start"
renderCaminoMsgDefault _ TripleTitle = "Triple"
renderCaminoMsgDefault _ TripleWcTitle = "Triple with WC"
renderCaminoMsgDefault _ UnfitTitle = "Unfit"
renderCaminoMsgDefault _ UnusedLabel = "Unused"
renderCaminoMsgDefault _ VeryFitTitle = "Very fit"
renderCaminoMsgDefault _ VeryUnfitTitle = "Very unfit"
renderCaminoMsgDefault _ VillageTitle = "Village"
renderCaminoMsgDefault _ WalkingTitle = "Walking"
renderCaminoMsgDefault _ WalkingNaismithTitle = "Walking (strong walkers)"
renderCaminoMsgDefault _ WarningTitle= "Warning"
renderCaminoMsgDefault _ WashingMachineTitle = "Washing Machine"
renderCaminoMsgDefault _ WaypointLabel = "Waypoint"
renderCaminoMsgDefault _ WiFiTitle = "WiFi"
renderCaminoMsgDefault _ WineryTitle = "Winery"
renderCaminoMsgDefault _ msg = [shamlet|Unknown message #{show msg}|]

hasLocalisedText :: (Tagged a) => [Locale] -> Localised a -> Bool
hasLocalisedText locales locd = maybe False (\t -> plainText t /= "") $ localise locales locd

renderLocalisedText :: (Tagged a) => [Locale] -> Bool -> Bool -> Localised a -> Html
renderLocalisedText locales attr js locd = let
    elt = localise locales locd
    txt = maybe "" plainText elt
    txt' = if attr then replace "\"" "'" txt else txt
    txt'' = if js then replace "'" "\\'" txt' else txt'
    loc = maybe rootLocale locale elt
    lng = localeLanguageTag loc
  in
    if attr || Data.Text.null lng then
      toHtml txt''
    else
      H.span H.! HA.lang (toValue lng) $ Text.Blaze.Html.text txt''

renderLocalisedDate :: (FormatTime t) => Bool -> [Locale] -> t -> Html
renderLocalisedDate weekDay [] day = renderLocalisedDate weekDay [rootLocale] day
renderLocalisedDate weekDay (loc:_) day = toHtml $ if weekDay then dwf ++ df else df
  where
    tl = localeTimeLocale loc
    df = formatTime tl (dateFmt tl) day
    dwf = formatTime tl "%a " day

renderLocalisedTime :: (FormatTime t) => [Locale] -> String -> t -> Html
renderLocalisedTime [] fmt t = renderLocalisedTime [rootLocale] fmt t
renderLocalisedTime (loc:_) fmt t = toHtml $ formatTime (localeTimeLocale loc) fmt t

renderLocalisedMonth :: [Locale] -> MonthOfYear -> Html
renderLocalisedMonth [] t = renderLocalisedMonth [rootLocale] t
renderLocalisedMonth (loc:_) t = toHtml $ snd $ (months $ localeTimeLocale loc) !! t

renderLocalisedOrdinal :: [Locale] -> Int -> Html
renderLocalisedOrdinal [] o = renderLocalisedOrdinal [rootLocale] o
renderLocalisedOrdinal (loc:_) o = toHtml $ (localeOrdinalRender loc) o

renderLocalisedBeforeAfter :: Config -> [Locale] -> Int -> Html
renderLocalisedBeforeAfter config [] n = renderLocalisedBeforeAfter config [rootLocale] n
renderLocalisedBeforeAfter config locales n = renderCaminoMsg config locales $ if n < 0 then BeforeText else AfterText

renderLocalisedWeekOfMonth :: [Locale] -> WeekOfMonth -> Html
renderLocalisedWeekOfMonth [] wom = renderLocalisedWeekOfMonth [rootLocale] wom
renderLocalisedWeekOfMonth (loc:_) wom = toHtml $ (localeWeekOfMonthRender loc) wom

renderLocalisedDayOfWeek :: [Locale] -> DayOfWeek -> Html
renderLocalisedDayOfWeek [] dow = renderLocalisedDayOfWeek [rootLocale] dow
renderLocalisedDayOfWeek (loc:_) dow = let
    idx = (fromEnum dow) - 1
    names = wDays $ localeTimeLocale loc
    name = names !! idx
  in
    toHtml $ snd name

renderLocalisedPublicHoliday :: Config -> [Locale] -> Text -> Html
renderLocalisedPublicHoliday config locs rid = [shamlet|^{renderCaminoMsg config locs PublicHolidayText} (^{name})|]
  where
    region = (regionConfigLookup $ getRegionConfig config) rid
    name = maybe (toHtml rid) (\r -> renderLocalisedText locs False False (regionName r)) region

renderLocalisedClosedDay :: Config -> [Locale] -> Text -> Html
renderLocalisedClosedDay config locs rid = [shamlet|^{renderCaminoMsg config locs ClosedDayText} (^{name})|]
  where
    region = (regionConfigLookup $ getRegionConfig config) rid
    name = maybe (toHtml rid) (\r -> renderLocalisedText locs False False (regionName r)) region

-- | Convert a message placeholder into actual HTML
renderCaminoMsg :: Config -- ^ The configuration
  -> [Locale] -- ^ The locale list
  -> CaminoMsg -- ^ The message
  -> Html -- ^ The resulting Html to interpolate
renderCaminoMsg config locales (ClosedDayLabel region) = renderLocalisedClosedDay config locales region
renderCaminoMsg _config locales (DateMsg day) = H.span H.! HA.class_ "date" $ renderLocalisedDate True locales day
renderCaminoMsg _config locales (DateRangeMsg day1 day2) = H.span H.! HA.class_ "date-range" $
  if day1 == day2 then
    renderLocalisedDate True locales day1
  else do
    renderLocalisedDate True locales day1
    " - "
    renderLocalisedDate True locales day2
renderCaminoMsg _config locales (DayOfWeekName dow) = renderLocalisedTime locales "%a" dow
renderCaminoMsg _config _locales (DayOfMonthName dom) = toHtml dom
renderCaminoMsg _config locales (DaySummaryMsg day) = [shamlet|
  #{start'} to #{finish'}
  ^{formatDistance $ metricsDistance metrics} (feels like ^{formatMaybeDistance $ metricsPerceivedDistance metrics})
  over ^{formatMaybeHours $ metricsTime metrics}
  |]
  where
   metrics = score day
   start' = renderLocalisedText locales False False (locationName $ start day)
   finish' = renderLocalisedText locales False False (locationName $ finish day)
renderCaminoMsg _config locales (LinkTitle locd defd) = if hasLocalisedText locales locd then
    renderLocalisedText locales True False locd
  else
    renderLocalisedText locales True False defd
renderCaminoMsg config locales (ListMsg msgs) = [shamlet|
  <ul .comma-separated-list>
    $forall m <- msgs
      <li>^{renderCaminoMsg config locales m}
  |]
renderCaminoMsg _config locales (MonthOfYearName moy) = renderLocalisedMonth locales moy
renderCaminoMsg config locales (OrdinalAfterWeekday nth dow) = [shamlet|^{renderLocalisedOrdinal locales (abs nth)} ^{renderLocalisedDayOfWeek locales dow} ^{renderLocalisedBeforeAfter config locales nth}|]
renderCaminoMsg config locales (OrdinalBeforeAfter nth unit) = [shamlet|^{renderLocalisedOrdinal locales (abs nth)} ^{renderCaminoMsg config locales unit} ^{renderLocalisedBeforeAfter config locales nth}|]
renderCaminoMsg config locales (NamedCalendarLabel key) = renderLocalisedText locales False False $ getCalendarName config key
renderCaminoMsg _config locales (NthWeekdayText nth dow) = [shamlet|^{renderLocalisedWeekOfMonth locales nth} ^{renderLocalisedDayOfWeek locales dow}|]
renderCaminoMsg config locales (PublicHolidayLabel region) = renderLocalisedPublicHoliday config locales region
renderCaminoMsg _config locales (Time time) = renderLocalisedTime locales "%H%M" time
renderCaminoMsg _config locales (Txt locd) = renderLocalisedText locales False False locd
renderCaminoMsg _config locales (TxtPlain attr js locd) = renderLocalisedText locales attr js locd
renderCaminoMsg config _ msg = renderCaminoMsgDefault config msg

-- | Render a camino message as un-markup text
renderCaminoMsgText :: Config -> [Locale] -> CaminoMsg -> Text
renderCaminoMsgText config locales (ListMsg msgs) = commaJoin $ Prelude.map (renderCaminoMsgText config locales) msgs
renderCaminoMsgText config locales msg = renderMarkupToText $ renderCaminoMsg config locales msg

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
