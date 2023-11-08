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
  AccessibleTitle
  | AccommodationLabel
  | AccommodationPenanceMsg Penance
  | AccommodationPreferencesLabel
  | AscentMsg Float
  | BankTitle
  | BedlinenTitle
  | BicycleRepairTitle
  | BicycleStorageTitle
  | BreakfastTitle
  | BridgeTitle
  | BusTitle
  | CampingTitle
  | CampSiteTitle
  | CityTitle
  | DayServicesPenanceMsg Penance
  | DayServicesPreferencesLabel
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
  | FitnessLabel
  | GroceriesTitle
  | GuestHouseTitle
  | HandwashTitle
  | HeatingTitle
  | HotelTitle
  | HouseTitle
  | IntersectionTitle
  | KitchenTitle
  | LinkLabel LinkConfig
  | LocationsLabel
  | LockersTitle
  | MapLabel
  | MattressTitle
  | MedicalTitle
  | MiscPenanceMsg Penance
  | MunicipalAlbergueTitle
  | OtherLabel
  | PenanceFormatted Penance
  | PenanceMsg Penance
  | PenanceReject
  | PenanceSummaryTitle
  | PerceivedDistanceLabel
  | PetsTitle
  | PharmacyTitle
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
  | ServicesLabel
  | SharedTitle
  | SingleTitle
  | SleepingBagTitle
  | StablesTitle
  | StopPenanceMsg Penance
  | StopPreferencesLabel
  | StopServicesPenanceMsg Penance
  | StopServicesPreferencesLabel
  | TimeAdjustMsg Penance
  | TimeMsg (Maybe Float)
  | TimePenaltyLabel
  | TimePreferencesLabel
  | TowelsTitle
  | TownTitle
  | TrainTitle
  | TripleTitle
  | TripleWcTitle
  | VillageTitle
  | WalkingFunctionLabel
  | WashingMachineTitle
  | WiFiTitle

rejectSymbol :: Text
rejectSymbol = "\x25c6"

thinSpace :: Text
thinSpace = "\x2009"

formatPenance :: Penance -> Html
formatPenance Reject = [shamlet|<span .penance .rejected title="Rejected">#{rejectSymbol}</span>|]
formatPenance (Penance p) = [shamlet|<span .penance>#{format (fixed 1) p}#{thinSpace}km</span>|]

formatDistance :: (Real a) => a -> Html
formatDistance d = [shamlet|<span .distance>#{format (fixed 1) d}#{thinSpace}km</span>|]

formatMaybeDistance :: (Real a) => Maybe a -> Html
formatMaybeDistance Nothing = [shamlet|<span .distance .rejected title="Rejected">#{rejectSymbol}</span>|]
formatMaybeDistance (Just d) = formatDistance d

formatTime :: (Real a) => a -> Html
formatTime t = [shamlet|<span .time>#{format (fixed 1) t}#{thinSpace}hrs</span>|]

formatMaybeTime :: (Real a) => Maybe a -> Html
formatMaybeTime Nothing = [shamlet|<span .time .rejected title="Rejected">#{rejectSymbol}</span>|]
formatMaybeTime (Just t) = formatTime t

formatHeight :: (Real a) => a -> Html
formatHeight h = [shamlet|<span .height>#{format (fixed 0) h}#{thinSpace}m</span>|]

-- | Default English translation
renderCaminoMsgDefault :: Config -> CaminoMsg -> Html
renderCaminoMsgDefault _ AccessibleTitle = "Accessible"
renderCaminoMsgDefault _ AccommodationLabel = "Accommodation"
renderCaminoMsgDefault _ (AccommodationPenanceMsg penance') = [shamlet|Accomodation ^{formatPenance penance'}|]
renderCaminoMsgDefault _ AccommodationPreferencesLabel = "Accommodation Preferences"
renderCaminoMsgDefault _ (AscentMsg ascent) = [shamlet|Ascent ^{formatHeight ascent}|]
renderCaminoMsgDefault _ BankTitle = "Bank"
renderCaminoMsgDefault _ BedlinenTitle = "Bedlinen"
renderCaminoMsgDefault _ BicycleRepairTitle = "Bicycle Repair"
renderCaminoMsgDefault _ BicycleStorageTitle = "Bicycle Storage"
renderCaminoMsgDefault _ BreakfastTitle = "Breakfast"
renderCaminoMsgDefault _ BridgeTitle = "Bridge"
renderCaminoMsgDefault _ BusTitle = "Bus"
renderCaminoMsgDefault _ CampingTitle = "Camping"
renderCaminoMsgDefault _ CampSiteTitle = "Camp-site"
renderCaminoMsgDefault _ CityTitle = "City"
renderCaminoMsgDefault _ (DayServicesPenanceMsg penance') = [shamlet|Missing Services (Day) ^{formatPenance penance'}|]
renderCaminoMsgDefault _ DayServicesPreferencesLabel = "Missing Day Services"
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
renderCaminoMsgDefault _ FitnessLabel = "Fitness"
renderCaminoMsgDefault _ GroceriesTitle = "Groceries"
renderCaminoMsgDefault _ GuestHouseTitle = "Guesthouse"
renderCaminoMsgDefault _ HandwashTitle = "Handwash"
renderCaminoMsgDefault _ HeatingTitle = "Heating"
renderCaminoMsgDefault config (LinkLabel link) = toHtml $ maybe ident linkLabel (getLink ident ["en", ""] config) where ident = linkId link
renderCaminoMsgDefault _ HotelTitle = "Hotel"
renderCaminoMsgDefault _ HouseTitle = "House"
renderCaminoMsgDefault _ IntersectionTitle = "Intersection"
renderCaminoMsgDefault _ KitchenTitle = "Kitchen"
renderCaminoMsgDefault _ LocationsLabel = "Locations"
renderCaminoMsgDefault _ LockersTitle = "Lockers"
renderCaminoMsgDefault _ MapLabel = "Map"
renderCaminoMsgDefault _ MattressTitle = "Mattress"
renderCaminoMsgDefault _ MedicalTitle = "Medical"
renderCaminoMsgDefault _ (MiscPenanceMsg penance') = [shamlet|Other ^{formatPenance penance'}|]
renderCaminoMsgDefault _ MunicipalAlbergueTitle = "Municipal Albergue"
renderCaminoMsgDefault _ OtherLabel = "Other"
renderCaminoMsgDefault _ (PenanceFormatted penance') = formatPenance penance'
renderCaminoMsgDefault _ (PenanceMsg penance') = [shamlet|Penance ^{formatPenance penance'}|]
renderCaminoMsgDefault _ PenanceReject = "Rejected"
renderCaminoMsgDefault _ PenanceSummaryTitle = "Penance Summary"
renderCaminoMsgDefault _ PerceivedDistanceLabel = "Perceived Distance"
renderCaminoMsgDefault _ PetsTitle = "Pets"
renderCaminoMsgDefault _ PharmacyTitle = "Pharmacy"
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
renderCaminoMsgDefault _ ServicesLabel = "Services"
renderCaminoMsgDefault _ SharedTitle = "Shared"
renderCaminoMsgDefault _ SingleTitle = "Single"
renderCaminoMsgDefault _ SleepingBagTitle = "Sleeping Bag"
renderCaminoMsgDefault _ StablesTitle = "Stables"
renderCaminoMsgDefault _ (StopPenanceMsg penance') = [shamlet|Stop ^{formatPenance penance'}|]
renderCaminoMsgDefault _ (StopServicesPenanceMsg penance') = [shamlet|Missing Services (Stop) ^{formatPenance penance'}|]
renderCaminoMsgDefault _ StopPreferencesLabel = "Stop Cost"
renderCaminoMsgDefault _ StopServicesPreferencesLabel = "Missing Stop Services"
renderCaminoMsgDefault _ (TimeAdjustMsg penance') = [shamlet|Time Adjustment ^{formatPenance penance'}|]
renderCaminoMsgDefault _ (TimeMsg time) = [shamlet|over ^{formatMaybeTime time}|]
renderCaminoMsgDefault _ TimePenaltyLabel = "Time Penalty"
renderCaminoMsgDefault _ TimePreferencesLabel = "Time Preferences (hours)"
renderCaminoMsgDefault _ TowelsTitle = "Towels"
renderCaminoMsgDefault _ TownTitle = "Town"
renderCaminoMsgDefault _ TrainTitle = "Train"
renderCaminoMsgDefault _ TripleTitle = "Triple"
renderCaminoMsgDefault _ TripleWcTitle = "Triple with WC"
renderCaminoMsgDefault _ VillageTitle = "Village"
renderCaminoMsgDefault _ WalkingFunctionLabel = "Walking Time Estimation"
renderCaminoMsgDefault _ WashingMachineTitle = "Washing Machine"
renderCaminoMsgDefault _ WiFiTitle = "WiFi"

-- | Convert a message placeholder into actual HTML
renderCaminoMsg :: Config -- ^ The configuration
  -> CaminoMsg -- ^ The message
  -> Html -- ^ The resulting Html to interpolate
renderCaminoMsg = renderCaminoMsgDefault