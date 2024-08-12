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
  CaminoMsg(..),

  formatDistance,
  formatMaybeDistance,
  formatMaybeHours,
  formatPenance,
  formatHours,
  renderCaminoMsg
) where

import Camino.Camino
import Camino.Config
import Camino.Planner
import Data.Localised
import Data.Region
import Data.Text
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Formatting
import Text.Blaze.Html (toHtml)
import Text.Hamlet

-- | Message placeholders for the camino
data CaminoMsg =
    AboutLabel
  | AccessibleTitle
  | AccommodationLabel
  | AccommodationPenanceMsg Penance
  | AccommodationPreferencesLabel
  | AddressTitle
  | AfterText
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
  | CampGroundTitle
  | CampingTitle
  | CampSiteTitle
  | CathedralTitle
  | ChurchTitle
  | CityTitle
  | ClosedText
  | ComfortableTitle
  | CrossTitle
  | ComfortLabel
  | CyclingTitle
  | CyclePathTitle
  | DailyLabel
  | DayText
  | DateMsg Data.Time.Calendar.Day
  | DayOfMonthName DayOfMonth
  | DayOfWeekName DayOfWeek
  | DayServicesPenanceMsg Penance
  | DayServicesPreferencesLabel
  | DaysMsg Int
  | DaySummaryMsg Camino.Planner.Day
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
  | FerryTitle
  | FestivalEventTitle
  | FitnessLabel
  | FitTitle
  | FoodEventTitle
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
  | HolidayEventTitle
  | HomeStayTitle
  | HostelTitle
  | HotelTitle
  | HoursTitle
  | HouseTitle
  | InformationLabel
  | InformationTitle
  | InformationDescription
  | IntersectionTitle
  | JunctionTitle
  | KeyLabel
  | KitchenTitle
  | LegPenanceMsg Penance
  | LinkTitle (Localised TaggedURL)
  | LinkOut
  | LocationPenanceMsg Penance
  | LocationPreferencesLabel
  | LocationsLabel
  | LockersTitle
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
  | NaturalTitle
  | NormalTitle
  | NthWeekdayText WeekOfMonth DayOfWeek
  | OpenHoursTitle
  | OpenText
  | OrdinalAfterWeekday Int DayOfWeek
  | OrdinalBeforeAfter Int CaminoMsg
  | OtherLabel
  | ParkTitle
  | PeakTitle
  | PenanceFormatted Penance
  | PenanceMsg Penance
  | PenanceReject
  | PenanceSummaryLabel
  | PerceivedDistanceLabel
  | PerformanceEventTitle
  | PetsTitle
  | PharmacyTitle
  | PilgrimAlbergueTitle
  | PilgrimMassEventTitle
  | PilgrimResourceTitle
  | PilgrimTitle
  | PlanLabel
  | PoiLabel
  | PoisLabel
  | PoiTitle
  | PoolTitle
  | PrayerTitle
  | PreferencesLabel
  | PrivateAlbergueTitle
  | PublicHolidayLabel Text
  | PublicHolidayText
  | QuadrupleTitle
  | QuadrupleWcTitle
  | ReligiousEventTitle
  | RequiredStopsLabel
  | RestaurantTitle
  | RoadTitle
  | RouteLabel
  | RoutesLabel
  | ServicesLabel
  | SharedTitle
  | ShopTitle
  | ShowOnMapTitle
  | SingleTitle
  | SleepingBagTitle
  | StablesTitle
  | StatueTitle
  | StopLabel
  | StopPenanceMsg Penance
  | StopPreferencesLabel
  | StopServicesPenanceMsg Penance
  | StopServicesPreferencesLabel
  | SuperFitTitle
  | Time TimeOfDay
  | TimeAdjustMsg Penance
  | TimeMsg (Maybe Float)
  | TimePenaltyLabel
  | TimePreferencesLabel
  | TowelsTitle
  | TownTitle
  | TrailTitle
  | TrainTitle
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

formatPenance :: Penance -> Html
formatPenance Reject = [shamlet|<span .penance .rejected title="Rejected">#{rejectSymbol}</span>|]
formatPenance (Penance p) = [shamlet|<span .penance>#{format (fixed 1) p}#{thinSpace}km</span>|]

formatDistance :: (Real a) => a -> Html
formatDistance d = [shamlet|<span .distance>#{format (fixed 1) d}#{thinSpace}km</span>#|]

formatMaybeDistance :: (Real a) => Maybe a -> Html
formatMaybeDistance Nothing = [shamlet|<span .distance .rejected title="Rejected">#{rejectSymbol}</span>#|]
formatMaybeDistance (Just d) = formatDistance d

formatHours :: (Real a) => a -> Html
formatHours t = [shamlet|<span .time>#{format (fixed 1) t}#{thinSpace}hrs</span>|]

formatDays :: Int -> Html
formatDays d = [shamlet|<span .days>#{format int d}#{thinSpace}days</span>#|]

formatMaybeHours :: (Real a) => Maybe a -> Html
formatMaybeHours Nothing = [shamlet|<span .time .rejected title="Rejected">#{rejectSymbol}</span>|]
formatMaybeHours (Just t) = formatHours t

formatHeight :: (Real a) => a -> Html
formatHeight h = [shamlet|<span .height>#{format (fixed 0) h}#{thinSpace}m</span>|]

-- | Default English translation
renderCaminoMsgDefault :: Config -> CaminoMsg -> Html
renderCaminoMsgDefault _ AboutLabel = "About"
renderCaminoMsgDefault _ AccessibleTitle = "Accessible"
renderCaminoMsgDefault _ AccommodationLabel = "Accommodation"
renderCaminoMsgDefault _ (AccommodationPenanceMsg penance') = [shamlet|Accommodation ^{formatPenance penance'}|]
renderCaminoMsgDefault _ AccommodationPreferencesLabel = "Accommodation Preferences"
renderCaminoMsgDefault _ AddressTitle = "Address"
renderCaminoMsgDefault _ AfterText = "after"
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
renderCaminoMsgDefault _ CampingTitle = "Camping"
renderCaminoMsgDefault _ CampSiteTitle = "Camp-site"
renderCaminoMsgDefault _ CathedralTitle = "Cathedral"
renderCaminoMsgDefault _ ChurchTitle = "Church"
renderCaminoMsgDefault _ CityTitle = "City"
renderCaminoMsgDefault _ ClosedText = "closed"
renderCaminoMsgDefault _ ComfortableTitle = "Comfortable"
renderCaminoMsgDefault _ ComfortLabel = "Comfort"
renderCaminoMsgDefault _ CrossTitle = "Cross"
renderCaminoMsgDefault _ CyclingTitle = "Cycling"
renderCaminoMsgDefault _ CyclePathTitle = "Cycle Path (bicycles only)"
renderCaminoMsgDefault _ DailyLabel = "Daily"
renderCaminoMsgDefault _ DayText = "day"
renderCaminoMsgDefault _ (DayServicesPenanceMsg penance') = [shamlet|Missing Services (Day) ^{formatPenance penance'}|]
renderCaminoMsgDefault _ DayServicesPreferencesLabel = "Missing Day Services"
renderCaminoMsgDefault _ (DaysMsg d) = formatDays d
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
renderCaminoMsgDefault _ FerryTitle = "Ferry"
renderCaminoMsgDefault _ FestivalEventTitle = "Festival"
renderCaminoMsgDefault _ FitnessLabel = "Fitness"
renderCaminoMsgDefault _ FitTitle = "Fit"
renderCaminoMsgDefault _ FoodEventTitle = "Food"
renderCaminoMsgDefault _ FountainTitle = "Fountain"
renderCaminoMsgDefault _ FrugalTitle = "Frugal"
renderCaminoMsgDefault _ GiteTitle = "Gîtes d'Étape"
renderCaminoMsgDefault _ GroceriesTitle = "Groceries"
renderCaminoMsgDefault _ GuestHouseTitle = "Guesthouse"
renderCaminoMsgDefault _ HandwashTitle = "Handwash"
renderCaminoMsgDefault _ HazardTitle = "Hazard"
renderCaminoMsgDefault _ HeatingTitle = "Heating"
renderCaminoMsgDefault _ HelpLabel = "Help"
renderCaminoMsgDefault _ HistoricalTitle = "Historical site, archaeological site or ruin"
renderCaminoMsgDefault _ HolidayEventTitle = "Holiday"
renderCaminoMsgDefault _ HomeStayTitle = "Home Stay"
renderCaminoMsgDefault _ HostelTitle = "Hostel"
renderCaminoMsgDefault _ HotelTitle = "Hotel"
renderCaminoMsgDefault _ HouseTitle = "House"
renderCaminoMsgDefault _ InformationLabel = "Information"
renderCaminoMsgDefault _ InformationTitle = "Information"
renderCaminoMsgDefault _ InformationDescription = "Information on the source data used when generating this plan."
renderCaminoMsgDefault _ IntersectionTitle = "Intersection"
renderCaminoMsgDefault _ JunctionTitle = "Junction"
renderCaminoMsgDefault _ KeyLabel = "Key"
renderCaminoMsgDefault _ KitchenTitle = "Kitchen"
renderCaminoMsgDefault _ (LegPenanceMsg penance') = [shamlet|+^{formatPenance penance'}|]
renderCaminoMsgDefault _ LinkOut = [shamlet|More information|]
renderCaminoMsgDefault _ (LocationPenanceMsg penance') = [shamlet|Location ^{formatPenance penance'}|]
renderCaminoMsgDefault _ LocationPreferencesLabel = "Location Preferences"
renderCaminoMsgDefault _ LocationsLabel = "Locations"
renderCaminoMsgDefault _ LockersTitle = "Lockers"
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
renderCaminoMsgDefault _ NaturalTitle = "Nature park, site of natural beauty, etc."
renderCaminoMsgDefault _ NormalTitle = "Normal"
renderCaminoMsgDefault _ OtherLabel = "Other"
renderCaminoMsgDefault _ OpenHoursTitle = "Open Hours"
renderCaminoMsgDefault _ OpenText = "open"
renderCaminoMsgDefault _ ParkTitle = "Park or garden"
renderCaminoMsgDefault _ PeakTitle = "Peak, pass or lookout"
renderCaminoMsgDefault _ (PenanceFormatted penance') = formatPenance penance'
renderCaminoMsgDefault _ (PenanceMsg penance') = [shamlet|Penance ^{formatPenance penance'}|]
renderCaminoMsgDefault _ PenanceReject = "Rejected"
renderCaminoMsgDefault _ PenanceSummaryLabel = "Penance"
renderCaminoMsgDefault _ PerceivedDistanceLabel = "Perceived Distance"
renderCaminoMsgDefault _ PerformanceEventTitle = "Performance"
renderCaminoMsgDefault _ PetsTitle = "Pets"
renderCaminoMsgDefault _ PharmacyTitle = "Pharmacy"
renderCaminoMsgDefault _ PilgrimAlbergueTitle = "Pilgrim Albergue"
renderCaminoMsgDefault _ PilgrimMassEventTitle = "Pilgrim's Mass"
renderCaminoMsgDefault _ PilgrimResourceTitle = "Pilgrim Resource"
renderCaminoMsgDefault _ PilgrimTitle = "Pilgrim"
renderCaminoMsgDefault _ PlanLabel = "Plan"
renderCaminoMsgDefault _ PoiLabel = "Point of Interest"
renderCaminoMsgDefault _ PoisLabel = "Points of Interest"
renderCaminoMsgDefault _ PoiTitle = "Locality"
renderCaminoMsgDefault _ PoolTitle = "Pool"
renderCaminoMsgDefault _ PrayerTitle = "Prayer"
renderCaminoMsgDefault _ PreferencesLabel = "Preferences"
renderCaminoMsgDefault _ PrivateAlbergueTitle = "Private Albergue"
renderCaminoMsgDefault _ PublicHolidayText = "Public Holiday"
renderCaminoMsgDefault _ QuadrupleTitle = "Quadruple"
renderCaminoMsgDefault _ QuadrupleWcTitle = "Quadruple with WC"
renderCaminoMsgDefault _ ReligiousEventTitle = "Religious Ceremony"
renderCaminoMsgDefault _ RequiredStopsLabel = "Required Stops"
renderCaminoMsgDefault _ RestaurantTitle = "Restaurant"
renderCaminoMsgDefault _ RoadTitle = "Road/path"
renderCaminoMsgDefault _ RouteLabel = "Route"
renderCaminoMsgDefault _ RoutesLabel = "Routes"
renderCaminoMsgDefault _ ServicesLabel = "Services"
renderCaminoMsgDefault _ SharedTitle = "Shared"
renderCaminoMsgDefault _ ShopTitle = "Shop"
renderCaminoMsgDefault _ ShowOnMapTitle = "Show on map"
renderCaminoMsgDefault _ SingleTitle = "Single"
renderCaminoMsgDefault _ SleepingBagTitle = "Sleeping Bag"
renderCaminoMsgDefault _ StablesTitle = "Stables"
renderCaminoMsgDefault _ StatueTitle = "Statue"
renderCaminoMsgDefault _ StopLabel = "Stop"
renderCaminoMsgDefault _ (StopPenanceMsg penance') = [shamlet|Stop ^{formatPenance penance'}|]
renderCaminoMsgDefault _ (StopServicesPenanceMsg penance') = [shamlet|Missing Services (Stop) ^{formatPenance penance'}|]
renderCaminoMsgDefault _ StopPreferencesLabel = "Stop Cost"
renderCaminoMsgDefault _ StopServicesPreferencesLabel = "Missing Stop Services"
renderCaminoMsgDefault _ SuperFitTitle = "Super-fit"
renderCaminoMsgDefault _ (TimeAdjustMsg penance') = [shamlet|Time Adjustment ^{formatPenance penance'}|]
renderCaminoMsgDefault _ (TimeMsg time) = [shamlet|over ^{formatMaybeHours time}|]
renderCaminoMsgDefault _ TimePenaltyLabel = "Time Penalty"
renderCaminoMsgDefault _ TimePreferencesLabel = "Time Preferences (hours)"
renderCaminoMsgDefault _ TowelsTitle = "Towels"
renderCaminoMsgDefault _ TownTitle = "Town"
renderCaminoMsgDefault _ TrailTitle = "Trail (walkers only)"
renderCaminoMsgDefault _ TrainTitle = "Train"
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

renderLocalisedText :: (Tagged a) => [Locale] -> Bool -> Bool -> Localised a -> Html
renderLocalisedText locales attr js locd = let
    elt = localise locales locd
    txt = maybe "" plainText elt
    txt' = if attr then replace "\"" "'" txt else txt
    txt'' = if js then replace "'" "\\'" txt' else txt'
    loc = maybe rootLocale locale elt
    lang = localeLanguageTag loc
  in
    if attr || Data.Text.null lang then
      toHtml txt''
    else
      [shamlet|<span lang="#{lang}">#{txt''}#|]

renderLocalisedDate :: (FormatTime t) => [Locale] -> t -> Html
renderLocalisedDate [] day = renderLocalisedDate [rootLocale] day
renderLocalisedDate (loc:_) day = toHtml $ formatTime tl (dateFmt tl) day where tl = localeTimeLocale loc

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

-- | Convert a message placeholder into actual HTML
renderCaminoMsg :: Config -- ^ The configuration
  -> [Locale] -- ^ The locale list
  -> CaminoMsg -- ^ The message
  -> Html -- ^ The resulting Html to interpolate
renderCaminoMsg _config locales (DateMsg day) = renderLocalisedDate locales day
renderCaminoMsg _config locales (DayOfWeekName dow) = renderLocalisedTime locales "%a" dow
renderCaminoMsg _config _locales (DayOfMonthName dom) = [shamlet|#{dom}|]
renderCaminoMsg _config locales (DaySummaryMsg day) = [shamlet|
  #{start'} to #{finish'}
  ^{formatDistance $ metricsDistance metrics} (feels like ^{formatMaybeDistance $ metricsPerceivedDistance metrics})
  over ^{formatMaybeHours $ metricsTime metrics}
  |]
  where
   metrics = score day
   start' = renderLocalisedText locales False False (locationName $ start day)
   finish' = renderLocalisedText locales False False (locationName $ finish day)
renderCaminoMsg _config locales (LinkTitle locd) = renderLocalisedText locales False False locd
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
