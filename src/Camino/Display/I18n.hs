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
  formatMaybeTime,
  formatPenance,
  formatTime,
  renderCaminoMsg
) where

import Camino.Camino
import Camino.Config
import Camino.Planner
import Data.Text
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
  | AscentMsg Float
  | AustereTitle
  | BankTitle
  | BedlinenTitle
  | BicycleRepairTitle
  | BicycleStorageTitle
  | BoatTitle
  | BreakfastTitle
  | BridgeTitle
  | BusTitle
  | CampGroundTitle
  | CampingTitle
  | CampSiteTitle
  | CityTitle
  | ComfortableTitle
  | ComfortLabel
  | CyclingTitle
  | CyclePathTitle
  | DayServicesPenanceMsg Penance
  | DayServicesPreferencesLabel
  | DaysMsg Int
  | DaySummaryMsg Day
  | DescentMsg Float
  | DinnerTitle
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
  | ExcludedStopsLabel
  | FerryTitle
  | FitnessLabel
  | FitTitle
  | FrugalTitle
  | GroceriesTitle
  | GuestHouseTitle
  | HandwashTitle
  | HeatingTitle
  | HelpLabel
  | HomeStayTitle
  | HostelTitle
  | HotelTitle
  | HouseTitle
  | InformationLabel
  | InformationDescription
  | IntersectionTitle
  | KeyLabel
  | KitchenTitle
  | LegPenanceMsg Penance
  | LinkLabel LinkConfig
  | LinkOut Text
  | LocationPenanceMsg Penance
  | LocationPreferencesLabel
  | LocationsLabel
  | LockersTitle
  | LuxuriousTitle
  | MapLabel
  | MattressTitle
  | MedicalTitle
  | MiscPenanceMsg Penance
  | MonasteryTitle
  | MunicipalAlbergueTitle
  | NormalTitle
  | OtherLabel
  | PeakTitle
  | PenanceFormatted Penance
  | PenanceMsg Penance
  | PenanceReject
  | PenanceSummaryLabel
  | PerceivedDistanceLabel
  | PetsTitle
  | PharmacyTitle
  | PilgrimTitle
  | PlanLabel
  | PoiTitle
  | PoolTitle
  | PrayerTitle
  | PreferencesLabel
  | PrivateAlbergueTitle
  | QuadrupleTitle
  | QuadrupleWcTitle
  | RequiredStopsLabel
  | RestaurantTitle
  | RoadTitle
  | RouteLabel
  | RoutesLabel
  | ServicesLabel
  | SharedTitle
  | ShowOnMapTitle
  | SingleTitle
  | SleepingBagTitle
  | StablesTitle
  | StopLabel
  | StopPenanceMsg Penance
  | StopPreferencesLabel
  | StopServicesPenanceMsg Penance
  | StopServicesPreferencesLabel
  | SuperFitTitle
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
  | UnfitTitle
  | UnusedLabel
  | VeryFitTitle
  | VeryUnfitTitle
  | VillageTitle
  | WalkingNaismithTitle
  | WalkingTitle
  | WashingMachineTitle
  | WaypointLabel
  | WiFiTitle

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

formatTime :: (Real a) => a -> Html
formatTime t = [shamlet|<span .time>#{format (fixed 1) t}#{thinSpace}hrs</span>|]

formatDays :: Int -> Html
formatDays d = [shamlet|<span .days>#{format int d}#{thinSpace}days</span>#|]

formatMaybeTime :: (Real a) => Maybe a -> Html
formatMaybeTime Nothing = [shamlet|<span .time .rejected title="Rejected">#{rejectSymbol}</span>|]
formatMaybeTime (Just t) = formatTime t

formatHeight :: (Real a) => a -> Html
formatHeight h = [shamlet|<span .height>#{format (fixed 0) h}#{thinSpace}m</span>|]

-- | Default English translation
renderCaminoMsgDefault :: Config -> CaminoMsg -> Html
renderCaminoMsgDefault _ AboutLabel = "About"
renderCaminoMsgDefault _ AccessibleTitle = "Accessible"
renderCaminoMsgDefault _ AccommodationLabel = "Accommodation"
renderCaminoMsgDefault _ (AccommodationPenanceMsg penance') = [shamlet|Accommodation ^{formatPenance penance'}|]
renderCaminoMsgDefault _ AccommodationPreferencesLabel = "Accommodation Preferences"
renderCaminoMsgDefault _ (AscentMsg ascent) = [shamlet|Ascent ^{formatHeight ascent}|]
renderCaminoMsgDefault _ AustereTitle = "Austere"
renderCaminoMsgDefault _ BankTitle = "Bank"
renderCaminoMsgDefault _ BedlinenTitle = "Bedlinen"
renderCaminoMsgDefault _ BicycleRepairTitle = "Bicycle Repair"
renderCaminoMsgDefault _ BicycleStorageTitle = "Bicycle Storage"
renderCaminoMsgDefault _ BoatTitle = "Boat/Canoe (paddled)"
renderCaminoMsgDefault _ BreakfastTitle = "Breakfast"
renderCaminoMsgDefault _ BridgeTitle = "Bridge"
renderCaminoMsgDefault _ BusTitle = "Bus"
renderCaminoMsgDefault _ CampGroundTitle = "Camping Ground"
renderCaminoMsgDefault _ CampingTitle = "Camping"
renderCaminoMsgDefault _ CampSiteTitle = "Camp-site"
renderCaminoMsgDefault _ CityTitle = "City"
renderCaminoMsgDefault _ ComfortableTitle = "Comfortable"
renderCaminoMsgDefault _ ComfortLabel = "Comfort"
renderCaminoMsgDefault _ CyclingTitle = "Cycling"
renderCaminoMsgDefault _ CyclePathTitle = "Cycle Path (bicycles only)"
renderCaminoMsgDefault _ (DayServicesPenanceMsg penance') = [shamlet|Missing Services (Day) ^{formatPenance penance'}|]
renderCaminoMsgDefault _ DayServicesPreferencesLabel = "Missing Day Services"
renderCaminoMsgDefault _ (DaysMsg d) = formatDays d
renderCaminoMsgDefault _ (DaySummaryMsg day) = [shamlet|
  #{locationName $ start day} to #{locationName $ finish day}
  ^{formatDistance $ metricsDistance metrics} (feels like ^{formatMaybeDistance $ metricsPerceivedDistance metrics})
  over ^{formatMaybeTime $ metricsTime metrics}
  |]
  where
    metrics = score day
renderCaminoMsgDefault _ (DescentMsg ascent) = [shamlet|Descent ^{formatHeight ascent}|]
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
renderCaminoMsgDefault _ ExcludedStopsLabel = "Excluded Stops"
renderCaminoMsgDefault _ FerryTitle = "Ferry"
renderCaminoMsgDefault _ FitnessLabel = "Fitness"
renderCaminoMsgDefault _ FitTitle = "Fit"
renderCaminoMsgDefault _ FrugalTitle = "Frugal"
renderCaminoMsgDefault _ GroceriesTitle = "Groceries"
renderCaminoMsgDefault _ GuestHouseTitle = "Guesthouse"
renderCaminoMsgDefault _ HandwashTitle = "Handwash"
renderCaminoMsgDefault _ HeatingTitle = "Heating"
renderCaminoMsgDefault _ HelpLabel = "Help"
renderCaminoMsgDefault config (LinkLabel link) = toHtml $ maybe ident linkLabel (getLink ident ["en", ""] config) where ident = linkId link
renderCaminoMsgDefault _ HomeStayTitle = "Home Stay"
renderCaminoMsgDefault _ HostelTitle = "Hostel"
renderCaminoMsgDefault _ HotelTitle = "Hotel"
renderCaminoMsgDefault _ HouseTitle = "House"
renderCaminoMsgDefault _ InformationLabel = "Information"
renderCaminoMsgDefault _ InformationDescription = "Information on the source data used when generating this plan."
renderCaminoMsgDefault _ IntersectionTitle = "Intersection"
renderCaminoMsgDefault _ KeyLabel = "Key"
renderCaminoMsgDefault _ KitchenTitle = "Kitchen"
renderCaminoMsgDefault _ (LegPenanceMsg penance') = [shamlet|+^{formatPenance penance'}|]
renderCaminoMsgDefault _ (LinkOut name) = [shamlet|More information on #{name}|]
renderCaminoMsgDefault _ (LocationPenanceMsg penance') = [shamlet|Location ^{formatPenance penance'}|]
renderCaminoMsgDefault _ LocationPreferencesLabel = "Location Preferences"
renderCaminoMsgDefault _ LocationsLabel = "Locations"
renderCaminoMsgDefault _ LockersTitle = "Lockers"
renderCaminoMsgDefault _ LuxuriousTitle = "Luxurious"
renderCaminoMsgDefault _ MapLabel = "Map"
renderCaminoMsgDefault _ MattressTitle = "Mattress"
renderCaminoMsgDefault _ MedicalTitle = "Medical"
renderCaminoMsgDefault _ (MiscPenanceMsg penance') = [shamlet|Other ^{formatPenance penance'}|]
renderCaminoMsgDefault _ MonasteryTitle = "Monastery"
renderCaminoMsgDefault _ MunicipalAlbergueTitle = "Municipal Albergue"
renderCaminoMsgDefault _ NormalTitle = "Normal"
renderCaminoMsgDefault _ OtherLabel = "Other"
renderCaminoMsgDefault _ PeakTitle = "Peak"
renderCaminoMsgDefault _ (PenanceFormatted penance') = formatPenance penance'
renderCaminoMsgDefault _ (PenanceMsg penance') = [shamlet|Penance ^{formatPenance penance'}|]
renderCaminoMsgDefault _ PenanceReject = "Rejected"
renderCaminoMsgDefault _ PenanceSummaryLabel = "Penance"
renderCaminoMsgDefault _ PerceivedDistanceLabel = "Perceived Distance"
renderCaminoMsgDefault _ PetsTitle = "Pets"
renderCaminoMsgDefault _ PharmacyTitle = "Pharmacy"
renderCaminoMsgDefault _ PilgrimTitle = "Pilgrim"
renderCaminoMsgDefault _ PlanLabel = "Plan"
renderCaminoMsgDefault _ PoiTitle = "Locality"
renderCaminoMsgDefault _ PoolTitle = "Pool"
renderCaminoMsgDefault _ PrayerTitle = "Prayer"
renderCaminoMsgDefault _ PreferencesLabel = "Preferences"
renderCaminoMsgDefault _ PrivateAlbergueTitle = "Private Albergue"
renderCaminoMsgDefault _ QuadrupleTitle = "Quadruple"
renderCaminoMsgDefault _ QuadrupleWcTitle = "Quadruple with WC"
renderCaminoMsgDefault _ RequiredStopsLabel = "Required Stops"
renderCaminoMsgDefault _ RestaurantTitle = "Restaurant"
renderCaminoMsgDefault _ RoadTitle = "Road/path"
renderCaminoMsgDefault _ RouteLabel = "Route"
renderCaminoMsgDefault _ RoutesLabel = "Routes"
renderCaminoMsgDefault _ ServicesLabel = "Services"
renderCaminoMsgDefault _ SharedTitle = "Shared"
renderCaminoMsgDefault _ ShowOnMapTitle = "Show on map"
renderCaminoMsgDefault _ SingleTitle = "Single"
renderCaminoMsgDefault _ SleepingBagTitle = "Sleeping Bag"
renderCaminoMsgDefault _ StablesTitle = "Stables"
renderCaminoMsgDefault _ StopLabel = "Stop"
renderCaminoMsgDefault _ (StopPenanceMsg penance') = [shamlet|Stop ^{formatPenance penance'}|]
renderCaminoMsgDefault _ (StopServicesPenanceMsg penance') = [shamlet|Missing Services (Stop) ^{formatPenance penance'}|]
renderCaminoMsgDefault _ StopPreferencesLabel = "Stop Cost"
renderCaminoMsgDefault _ StopServicesPreferencesLabel = "Missing Stop Services"
renderCaminoMsgDefault _ SuperFitTitle = "Super-fit"
renderCaminoMsgDefault _ (TimeAdjustMsg penance') = [shamlet|Time Adjustment ^{formatPenance penance'}|]
renderCaminoMsgDefault _ (TimeMsg time) = [shamlet|over ^{formatMaybeTime time}|]
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
renderCaminoMsgDefault _ WashingMachineTitle = "Washing Machine"
renderCaminoMsgDefault _ WaypointLabel = "Waypoint"
renderCaminoMsgDefault _ WiFiTitle = "WiFi"

-- | Convert a message placeholder into actual HTML
renderCaminoMsg :: Config -- ^ The configuration
  -> CaminoMsg -- ^ The message
  -> Html -- ^ The resulting Html to interpolate
renderCaminoMsg = renderCaminoMsgDefault