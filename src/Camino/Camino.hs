{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module      : Camino
Description : Data models for travelling the Camino
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

A camino consists of a graph of legs that can be assembled in various ways.
The legs run between two locations, each with possible accommodation and service options.

Generally, it is expected that these models will be read from JSON files.
-}
module Camino.Camino (
    Accommodation(..)
  , AccommodationType(..)
  , Camino(..)
  , CaminoConfig(..)
  , Comfort(..)
  , Event(..)
  , EventType(..)
  , Feature(..)
  , Fitness(..)
  , HasCaminoConfig(..)
  , LatLong(..)
  , Leg(..)
  , LegSegment(..)
  , LegType(..)
  , Location(..)
  , LocationType(..)
  , Palette(..)
  , Penance(..)
  , PoiCategory(..)
  , PointOfInterest(..)
  , Prioritised(..)
  , Route(..)
  , RouteLogic(..)
  , Service(..)
  , Sleeping(..)
  , SRS(..)
  , Travel(..)

  , module Graph.Programming

  , accommodationDescription
  , accommodationID
  , accommodationMulti
  , accommodationName
  , accommodationNameLabel
  , accommodationType
  , accommodationTypeEnumeration
  , accommodationServices
  , accommodationSleeping
  , allFeatures
  , buildLegSegments -- For testing only
  , caminoAllLegs
  , caminoBbox
  , caminoDump
  , caminoFeatureMap
  , caminoLegRoute
  , caminoNameLabel
  , caminoRegions
  , caminoRoute
  , caminoRouteLocations
  , caminoUsedFeatures
  , caminoWithRoutes
  , centroid
  , comfortEnumeration
  , completeRoutes
  , createAllowsClauses
  , createCaminoConfig
  , createRequiresClauses
  , createProhibitsClauses
  , fitnessEnumeration
  , getCamino
  , haversineDistance
  , isGenericAccommodation
  , legTypeEnumeration
  , locationAccommodationTypes
  , locationAdditionalAccommodationTypes
  , locationAdditionalPoiTypes
  , locationAdditionalServices
  , locationAllAccommodation
  , locationAllPois
  , locationAllServices
  , locationBbox
  , locationEventTypes
  , locationNameLabel
  , locationPoiTypes
  , locationStopTypeEnumeration
  , locationTransportLinks
  , locationTypeEnumeration
  , normaliseLeg
  , normaliseLeg'
  , poiCategoryEnumeration
  , poiNameLabel
  , readCamino
  , routeCentralLocation
  , routeLegTypes
  , serviceEnumeration
  , subtractFloor
  , townServiceEnumeration
  , travelEnumeration
) where

import GHC.Generics (Generic)
import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (toJSONKeyText, typeMismatch)
import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24read, sRGB24show)
import Data.Default.Class
import Data.Description (Description(..), wildcardDescription)
import Data.Event
import Data.Foldable (foldl', minimumBy, toList)
import qualified Data.List as L
import Data.Localised (Localised(..), TaggedText(..), appendText, localiseDefault, rootLocale, wildcardText)
import Data.Maybe (fromJust)
import Data.Metadata
import qualified Data.Map as M
import Data.Placeholder
import Data.Propositional
import Data.Region
import qualified Data.Set as S
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Summary
import Data.Text (Text, isPrefixOf, pack, unpack)
import qualified Data.Text as T
import Data.Util (headWithError, lastWithError)
import Graph.Graph
import Graph.Programming
import Data.Partial (topologicalSort)
import Data.ByteString.Lazy (ByteString)
import Text.Read (readMaybe)

-- For debugging
-- _traceSummary l v = trace (summaryString l ++ " " ++ summaryString v) v

-- | Is this a placeholder name?
--   Used to help detect placeholders
isPlaceholderName :: Localised TaggedText -> Bool
isPlaceholderName (Localised [TaggedText locale text]) = locale == rootLocale && isPrefixOf "Placeholder" text
isPlaceholderName _ = False

-- | The measure of penance.
-- |
-- | Currently, based a simple float that can be thought of as equivalent to kilometres spent walking.
data Penance = Reject -- ^ Unsustainable penance
 | Penance Float -- ^ Simple penance
 deriving (Show, Generic)

instance Semigroup Penance where
  Reject <> _ = Reject
  _ <> Reject = Reject
  p1@(Penance score1) <> p2@(Penance score2) = if p1 == mempty then p2 else if p2 == mempty then p1 else Penance (score1 + score2)

instance Monoid Penance where
  mempty = Penance 0.0

instance Score Penance where
  invalid = Reject

instance Eq Penance where
  Reject == Reject = True
  Reject == _ = False
  _ == Reject = False
  (Penance score1) == (Penance score2) = score1 == score2
    
instance Ord Penance where
  compare Reject Reject = EQ
  compare Reject _ = GT
  compare _ Reject = LT
  compare (Penance score1) (Penance score2) = compare score1 score2

instance FromJSON Penance where
   parseJSON (Number v) = do
     return (Penance $ toRealFloat v)
   parseJSON (String v) = do
     return (if v /= "reject" then error ("Expecting \"reject\" got " ++ show v) else Reject)
   parseJSON v = error ("Unable to parse penance object " ++ show v)

instance ToJSON Penance where
    toJSON Reject = String "reject"
    toJSON (Penance score') = Number $ fromFloatDigits score'

instance NFData Penance

instance Summary Penance where
  summary Reject = "x"
  summary (Penance v) = summary v
  summaryIfNotNull v = if v == mempty then Nothing else Just $ summary v

-- | Reduce a penance by another penance, with 0 penance the minimum
--   @Reject - b = Reject@, @a - Reject = zero@, @a - b = max(zero, a - b)@
subtractFloor :: Penance -> Penance -> Penance
subtractFloor Reject _ = Reject
subtractFloor _ Reject = mempty
subtractFloor (Penance p1) (Penance p2) = if p2 > p1 then mempty else Penance (p1 - p2)

-- | Spatial reference system
data SRS = SRS Text
  deriving (Eq, Ord, Show, Generic)

srsID :: SRS -> Text
srsID (SRS sid) = sid

instance Default SRS where
  def = SRS "WGS84"

instance FromJSON SRS where
  parseJSON Null = return $ def
  parseJSON (String v) = return $ SRS v
  parseJSON v = error ("Can't parse SRS object " ++ show v)

instance ToJSON SRS where
  toJSON srs' = if srs' == def then Null else String (srsID srs')

instance NFData SRS

data LatLong = LatLong {
    latitude :: Double
  , longitude :: Double
  , elevation :: Maybe Double
  , srs :: SRS
} deriving (Eq, Ord, Show, Generic)

instance FromJSON LatLong where
  parseJSON (Object v) = do
    latitude' <- v .: "latitude"
    longitude' <- v .: "longitude"
    elevation' <- v .:? "elevation"
    srs' <- v .:? "srs" .!= def
    return LatLong {
        latitude = latitude'
      , longitude = longitude'
      , elevation = elevation'
      , srs = srs'
      }
  parseJSON v = error ("Unable to parse lat/long object " ++ show v)

instance ToJSON LatLong where
  toJSON (LatLong latitude' longitude' elevation' srs') =
    object [
        "latitude" .= latitude'
      , "longitude" .= longitude'
      , "elevation" .= elevation'
      , "srs" .= (if srs' == def then Nothing else Just srs')
    ]
  toEncoding (LatLong latitude' longitude' elevation' srs') =
    pairs $
         "latitude" .= latitude'
      <> "longitude" .= longitude'
      <> "elevation" .?= elevation'
      <> "srs" .?= (if srs' == def then Nothing else Just srs')

instance NFData LatLong

instance Default LatLong where
  def = LatLong 0.0 0.0 Nothing def

-- Compute the centroid of a list of lat/longs
centroid :: (Foldable t) => t LatLong -> LatLong
centroid lls = let
    (slats, slongs, selevs, len, len', srs'') = foldl' (\(lats, longs, elevs, slen, slen', _srs) -> \(LatLong lat lon elev srs') -> (lats + lat, longs + lon, elevs + maybe 0.0 id elev, slen + 1, slen' + maybe 0 (const 1) elev, srs')) (0.0, 0.0, 0.0, 0, 0, def) lls
  in
    LatLong
      (if len == 0 then 0.0 else (slats / len))
      (if len == 0 then 0.0 else (slongs / len))
      (if len' == 0 then Nothing else Just (selevs / len'))
      srs''

-- Squared Euclidian distance between two lat longs
-- This is not accurate, but good enough for quick estimation
euclidianDistance2 :: LatLong -> LatLong -> Double
euclidianDistance2 (LatLong lat1 long1 _elev1 _srs1) (LatLong lat2 long2 _elev2 _srs2) = (lat2 - lat1) * (lat2 - lat1) + (long2 - long1) * (long2 - long1)

-- | Distance for small angle differences using the Haverisne formula
--   https://en.wikipedia.org/wiki/Haversine_formula
haversineDistance :: LatLong -- ^ From lat/long
  -> LatLong -- ^ To lat/long
  -> Double -- ^ Distance in metres
haversineDistance (LatLong lat1 long1 _elev1 _srs1) (LatLong lat2 long2 _elev2 _srs2) = let
  lat1r = lat1 * pi / 180.0
  long1r = long1 * pi / 180.0
  lat2r = lat2 * pi / 180.0
  long2r = long2 * pi / 180.0
  deltalat = lat2r - lat1r
  deltalong = long2r - long1r
  r = 6378137.0
  hav = sqrt (1.0 - cos deltalat + cos lat1r * cos lat2r * (1 - cos deltalong))
  hav' = max (-1.0) (min 1.0 hav)
  in
    2.0 * r * hav'

-- | Broad accommodation types
data AccommodationType = PilgrimAlbergue -- ^ A pilgrims hostel run by local volunteers
  | PrivateAlbergue -- ^ A hostel run as a local business, oriented towards pilgrims
  | Hostel -- ^ A generic hostel
  | CasaRural -- ^ Rural accommodation, Quinta
  | GuestHouse -- ^ A generic guesthouse
  | HomeStay -- ^ Air B&B room etc.
  | House -- ^ An entire house or apartment for rent
  | Hotel -- ^ A dedicated hotel
  | Gite -- ^ A gîtes d'etape, a stage lodging for walkers or cyclists
  | CampGround -- ^ A dedicated camping ground with services
  | Refuge -- ^ A remote-area refuge without facilities
  | Camping -- ^ Roadside camping (with a tent or without, depending on what carried)
  | PlaceholderAccommodation -- ^ Placeholder for accommodation
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON AccommodationType
instance ToJSON AccommodationType
instance FromJSONKey AccommodationType where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
instance ToJSONKey AccommodationType where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions
instance NFData AccommodationType

-- | Provide an enumeration of all accommodation types, not including the placeholder
accommodationTypeEnumeration :: [AccommodationType]
accommodationTypeEnumeration = [PilgrimAlbergue .. Camping]

-- | Default multi-day stay for the type of accomodation
--   False for pilgrim albergues, true for anything else
accommodationDefaultMulti :: AccommodationType -> Bool
accommodationDefaultMulti PilgrimAlbergue = False
accommodationDefaultMulti _ = True

-- | Services available in a location or accommodation
data Service = WiFi -- ^ Wireless internet available
  | Restaurant -- ^ Bar and/or Restaurant
  | Groceries -- ^ Convenience store or supermarket
  | Pharmacy -- ^ Pharmacy
  | Medical -- ^ Doctor, hospital or other healthcare
  | Bank -- ^ Banking facilities, including an automated teller machine
  | BicycleRepair -- ^ Bicycle repair shop
  | Train -- ^ Train station (or tram or subway)
  | Bus -- ^ Bus stop
  | Ferry -- ^ Ferry terminal
  | Heating -- ^ Heated building
  | WashingMachine -- ^ Washing machine available
  | Dryer -- ^ Dryer or spin-dryer available
  | Handwash -- ^ Handwash laundry facilities
  | Kitchen -- ^ Kitchen facilities
  | Breakfast -- ^ Breakfast available
  | Dinner -- ^ Dinner available
  | HalfBoard -- ^ Half-board (breakfast/lunch or breakfast/dinner) available
  | Lockers -- ^ Lockers or cabinets
  | Accessible -- ^ Fitted for people with disabilities
  | Stables -- ^ Stabling for horses available
  | Pets -- ^ Pets allowed
  | BicycleStorage -- ^ Bicycle storage available
  | CampSite -- ^ Camping sites available
  | Bedlinen -- ^ Bedlinen provided
  | Towels -- ^ Towels provided
  | Pool -- ^ Swimming pool available
  | Prayer -- ^ Community prayer/liturgy
  deriving (Show, Read, Generic, Eq, Ord, Enum, Bounded)

instance FromJSON Service
instance ToJSON Service
instance FromJSONKey Service where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
instance ToJSONKey Service where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance NFData Service

instance Summary Service where
  summary v = pack $ show v

-- | Provide an enumeration of all services
serviceEnumeration :: [Service]
serviceEnumeration = [minBound .. maxBound]

-- | Provide an enumeration of the services that might be available in a village/town/city
townServiceEnumeration :: [Service]
townServiceEnumeration = [Restaurant, Groceries, Pharmacy, Medical, Bank, BicycleRepair, Train, Bus]

-- | Sleeping/room arrangements available  
data Sleeping = Shared -- ^ Shared accommodation in a dormitory
  | Single -- ^ Single room
  | Double -- ^ Double room with shared bathroom
  | DoubleWC -- ^ Double room with a private bathroom
  | Triple -- ^ Triple room with shared bathroom
  | TripleWC -- ^ Triple room with private bathroom
  | Quadruple -- ^ Room for 4 with shared bathroom
  | QuadrupleWC -- ^ Room for 4 room with private bathroom
  | Mattress -- ^ Mattresses on the floor
  | SleepingBag -- ^ Own sleeping bag and mattress
  deriving (Show, Generic, Eq, Ord, Enum, Bounded)

instance FromJSON Sleeping
instance ToJSON Sleeping
instance NFData Sleeping

-- | Somewhere to stay at the end of a leg
data Accommodation =
    Accommodation Text (Localised TaggedText) (Maybe Description) AccommodationType (S.Set Service) (S.Set Sleeping) (Maybe Bool) -- ^ Fully described accommodation
  | GenericAccommodation AccommodationType -- ^ Generic accommodation with default services, sleeping arrangements based on type
  deriving (Show, Generic)

accommodationID :: Accommodation -> Text
accommodationID (Accommodation id' _name _description _type _services _sleeping _multi) = id'
accommodationID (GenericAccommodation type') = pack $ show type'

accommodationName :: Accommodation -> (Localised TaggedText)
accommodationName (Accommodation _id' name' _description _type _services _sleeping _multi) = name'
accommodationName (GenericAccommodation type') = wildcardText $ pack ("Generic " ++ show type')

-- | Get a simple text version of the accommodation name
accommodationNameLabel :: Accommodation -> Text
accommodationNameLabel accommodation = localiseDefault $ accommodationName accommodation

accommodationDescription :: Accommodation -> Maybe Description
accommodationDescription (Accommodation _id' _name' description' _type _services _sleeping _multi) = description'
accommodationDescription (GenericAccommodation _type') = Nothing

accommodationType :: Accommodation -> AccommodationType
accommodationType (Accommodation _id _name _description type' _services _sleeping _multi) = type'
accommodationType (GenericAccommodation type') = type'

accommodationServices :: Accommodation -> S.Set Service
accommodationServices (Accommodation _id _name _description _type services' _sleeping _multi) = services'
accommodationServices (GenericAccommodation PilgrimAlbergue) = S.fromList [ Handwash ]
accommodationServices (GenericAccommodation PrivateAlbergue) = S.fromList [ Handwash, WiFi, Bedlinen, Towels ]
accommodationServices (GenericAccommodation Hostel) = S.fromList [ WiFi, Kitchen, Bedlinen, Towels ]
accommodationServices (GenericAccommodation CasaRural) = S.fromList [ WiFi, Heating, Bedlinen, Towels ]
accommodationServices (GenericAccommodation GuestHouse) = S.fromList [ WiFi, Heating, Bedlinen, Towels ]
accommodationServices (GenericAccommodation HomeStay) = S.fromList [ WiFi, Heating, Bedlinen, Towels ]
accommodationServices (GenericAccommodation House) = S.fromList [ WiFi, Bedlinen, Towels ]
accommodationServices (GenericAccommodation Hotel) = S.fromList [ WiFi, Breakfast, Dinner, Restaurant, Bedlinen, Towels ]
accommodationServices (GenericAccommodation Gite) = S.fromList [ Kitchen, BicycleStorage ]
accommodationServices (GenericAccommodation CampGround) = S.fromList [ Handwash ]
accommodationServices (GenericAccommodation Refuge) = S.empty
accommodationServices (GenericAccommodation Camping) = S.empty
accommodationServices (GenericAccommodation PlaceholderAccommodation) = S.empty


accommodationSleeping :: Accommodation -> S.Set Sleeping
accommodationSleeping (Accommodation _id _name _description _type _services sleeping' _multi) = sleeping'
accommodationSleeping (GenericAccommodation PilgrimAlbergue) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation PrivateAlbergue) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation Hostel) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation CasaRural) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation GuestHouse) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation HomeStay) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation House) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation Hotel) = S.fromList [ DoubleWC ]
accommodationSleeping (GenericAccommodation Gite) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation CampGround) = S.fromList [ SleepingBag ]
accommodationSleeping (GenericAccommodation Refuge) = S.fromList [ SleepingBag ]
accommodationSleeping (GenericAccommodation Camping) = S.fromList [ SleepingBag ]
accommodationSleeping (GenericAccommodation PlaceholderAccommodation) = S.empty

-- | Allow a multi-day stay?
--   If the accommodation does not allow a specific override, then `accommodationDefaultMulti` is used.
accommodationMulti :: Accommodation -> Bool
accommodationMulti (Accommodation _id _name _description _type _services _sleeping (Just multi')) = multi'
accommodationMulti (Accommodation _id _name _description type' _services _sleeping Nothing) = accommodationDefaultMulti type'
accommodationMulti (GenericAccommodation type') = accommodationDefaultMulti type'

-- | Is this a generic accommodation type?
isGenericAccommodation :: Accommodation -> Bool
isGenericAccommodation (GenericAccommodation _type) = True
isGenericAccommodation _ = False

instance Eq Accommodation where
  (Accommodation id1 _name1 _description1 _type1 _services1 _sleeping1 _multi1) == (Accommodation id2 _name2 _description2 _type2 _services2 _sleeping2 _multi2) = id1 == id2
  (GenericAccommodation type1) == (GenericAccommodation type2) = type1 == type2
  _ == _ = False


instance Ord Accommodation where
  compare (Accommodation id1 _name1 _description1 _type1 _services1 _sleeping1 _multi1) (Accommodation id2 _name2 _description2 _type2 _services2 _sleeping2 _multi2) = compare id1 id2
  compare (GenericAccommodation type1) (GenericAccommodation type2) = compare type1 type2
  compare (Accommodation _id1 _name1 _description1 _type1 _services1 _sleeping1 _multi1) (GenericAccommodation _type2) = LT
  compare (GenericAccommodation _type1) (Accommodation _id2 _name2 _description2 _type2 _services2 _sleeping2 _multi2) = GT

instance Placeholder Text Accommodation where
  placeholderID = accommodationID
  placeholder aid = let
      mt = readMaybe (unpack aid)
    in
      maybe
        (Accommodation aid (wildcardText $ "Placeholder for " <> aid) Nothing PlaceholderAccommodation S.empty S.empty Nothing)
        GenericAccommodation
        mt
  isPlaceholder accommodation = accommodationType accommodation == PlaceholderAccommodation

instance Dereferencer Text Accommodation Camino where
  dereference camino ac@(Accommodation id' _name _description PlaceholderAccommodation _services _sleeping _multi) =
    maybe ac fst (M.lookup id' (caminoAccommodationMap camino))
  dereference _camino ac = ac

instance Dereferencer Text Accommodation CaminoConfig where
  dereference config ac@(Accommodation id' _name _description PlaceholderAccommodation _services _sleeping _multi) =
    maybe ac fst $ (caminoConfigAccommodationLookup config) id'
  dereference _config ac = ac

instance FromJSON Accommodation where
   parseJSON t@(String _v) = do
     id' <- parseJSON t
     return $ placeholder id'
   parseJSON (Object v) = do
     id' <- v .:? "id" .!= ""
     name' <- v .: "name"
     description' <- v .:? "description"
     type' <- v .: "type"
     services' <- v .: "services"
     sleeping' <- v .: "sleeping"
     multi' <- v .:? "multi-day"
     return $ Accommodation id' name' description' type' services' sleeping' multi'
   parseJSON v = error ("Unable to parse accommodation object " ++ show v)
instance ToJSON Accommodation where
  toJSON (Accommodation id' name' description' type' services' sleeping' multi') =
    object [
        "id" .= (if T.null id' then Nothing else Just id')
      , "name" .= name'
      , "description" .= description'
      , "type" .= type'
      , "services" .= services'
      , "sleeping" .= sleeping'
      , "multi-day" .= multi'
    ]
  toJSON (GenericAccommodation type') =
    toJSON type'
    
  toEncoding (Accommodation id' name' description' type' services' sleeping' multi') =
      pairs $
          "id" .= id'
        <> "name" .= name'
        <> "description" .?= description'
        <> "type" .= type'
        <> "services" .= services'
        <> "sleeping" .= sleeping'
        <> "multi-day" .= multi'
  toEncoding (GenericAccommodation type') =
    toEncoding type'

instance NFData Accommodation

-- | An event that takes place somewhere
data EventType =
    Religious -- ^ A religious festival, ceremony, saints day etc.
  | Food -- ^ An event directed towards food and eating
  | Music -- ^ A concert or musical festival
  | Performance -- ^ A theatre or dance performance
  | Festival -- ^ A festival, often municipal
  | Holiday -- ^ A national or regional holiday
  | PilgrimMass -- ^ A religious service directed towards pilgrims
  | Mass -- ^ A religious service
  deriving (Show, Read, Generic, Eq, Ord, Enum, Bounded)

instance FromJSON EventType
instance ToJSON EventType
instance NFData EventType

-- | An event that occurs at a location or point of interest
--   The location of the event is associated with the unclosing point
data Event = Event {
    eventName :: Localised TaggedText -- ^ The name of the event
  , eventDescription :: Maybe Description -- ^ Detailed description
  , eventType :: EventType -- ^ The event type
  , eventHours :: Maybe OpenHours -- ^ Dates/times when the event occurs
  } deriving (Show, Generic)

instance FromJSON Event where
  parseJSON (Object v) = do
    name' <- v .: "name"
    description' <- v .:? "description" .!= Nothing
    type' <- v .: "type"
    hours' <- v .:? "hours"
    return Event {
        eventName = name'
      , eventDescription = description'
      , eventType = type'
      , eventHours = hours'
    }
  parseJSON v = typeMismatch "expecting object" v

instance ToJSON Event where
    toJSON (Event name' description' type' hours') =
      object [
          "name" .= name'
        , "description" .= description'
        , "type" .= type'
        , "hours" .= hours'
        ]
    toEncoding (Event name' description' type' hours') =
      pairs $ 
          "name" .= name'
        <> "description" .?= description'
        <> "type" .= type'
        <> "hours" .?= hours'

instance NFData Event

-- | The type of location or point of interest
data LocationType =
     Village -- ^ A village
   | Town -- ^ A town
   | City -- ^ A city
   | Monastery -- ^ A monastery/convent
   | Bridge -- ^ A bridge or underpass
   | Intersection -- ^ An intersection
   | Peak -- ^ A peak or mountain pass
   | Lookout -- ^ A lookout or scenic view
   | Promontory -- ^ A headland or promontory
   | Church -- ^ A church or chapel
   | Cathedral -- ^ A cathedral, basillica, shrine or similar large religious building
   | Cross -- ^ A crucifix or other religious monument
   | Fountain -- ^ A fountain or spring
   | Statue -- ^ A statue, artwork, etc
   | Artwork -- ^ An artwork
   | Municipal -- ^ An office, square, market etc
   | InformationPoint -- ^ Tourist information
   | PilgrimResource -- ^ An office, rest-spot etc. for pilgrims
   | Junction -- ^ A junction in pilgrim routes
   | Shop -- ^ A shop of some interest
   | Winery -- ^ A winery or port warehouse
   | Museum -- ^ A museum or gallery
   | Theatre -- ^ A theatre, opera house etc.
   | Historical -- ^ A historical or archaeological site
   | Park -- ^ A park or gardens
   | Beach -- ^ A beach
   | Natural -- ^ A site of natural beauty
   | Hazard -- ^ A dangerous location (busy road crossing, etc)
   | Station -- ^ A train station, bus terminal etc.
   | Wharf -- ^ A boat/ferry transfer point
   | Airport -- ^ An airport, airfield, aerodrome etc.
   | Farmland -- ^ Field, greenhouses, pasture etc.
   | Industry -- ^ An industiral area
   | Poi -- ^ A generic point of interest
   | PlaceholderLocation -- ^ A placeholder location
   deriving (Show, Read, Generic, Eq, Ord, Enum, Bounded)
 
instance FromJSON LocationType
instance ToJSON LocationType
instance FromJSONKey LocationType where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
instance ToJSONKey LocationType where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions
instance NFData LocationType

-- | Provide an enumeration of all location types
locationTypeEnumeration :: [LocationType]
locationTypeEnumeration = [minBound .. maxBound]

-- | Provide an enumeration of all places to stop
locationStopTypeEnumeration :: [LocationType]
locationStopTypeEnumeration = [Village, Town, City, Monastery, Bridge, Intersection, Peak]

-- | The default state for allowing camping.
--   Towns and cities don't usually allow camping
locationCampingDefault :: LocationType -> Bool
locationCampingDefault Town = False
locationCampingDefault City = False
locationCampingDefault _ = True

-- | The default state for whether a location is always open on weekends/holidays
locationAlwaysOpenDefault :: LocationType -> Bool
locationAlwaysOpenDefault _ = False

-- | Broad categories of points of interest, used to decide whether a pilgrim is likely to visit them.
--   A single point of interest is quite likely to fit multiple classes.
data PoiCategory =
    ReligiousPoi -- ^ Religious or spiritual interest
  | HistoricalPoi -- ^ Historical or archaeological interest
  | CulturalPoi -- ^ Art galleries, cultural museums
  | NaturalPoi -- ^ Natural attractions, parks and beauty spots
  | PilgrimPoi -- ^ Something specifically geared to pilgrims
  | RecreationPoi -- ^ A recreational resource
  deriving (Show, Read, Generic, Eq, Ord, Enum, Bounded)

instance ToJSON PoiCategory
instance FromJSON PoiCategory
instance NFData PoiCategory

-- | Provide an enumeration of all poi categories
poiCategoryEnumeration :: [PoiCategory]
poiCategoryEnumeration = [minBound .. maxBound]

-- | The default categories when not directly specified
defaultPoiCategories :: LocationType -> S.Set PoiCategory
defaultPoiCategories Monastery = S.fromList [ReligiousPoi, HistoricalPoi]
defaultPoiCategories Bridge = S.fromList [HistoricalPoi]
defaultPoiCategories Church = S.fromList [ReligiousPoi]
defaultPoiCategories Cathedral = S.fromList [ReligiousPoi, HistoricalPoi]
defaultPoiCategories Cross = S.fromList [ReligiousPoi]
defaultPoiCategories Fountain = S.fromList [RecreationPoi]
defaultPoiCategories Statue = S.fromList [HistoricalPoi]
defaultPoiCategories Artwork = S.fromList [CulturalPoi]
defaultPoiCategories Municipal = S.fromList [HistoricalPoi, RecreationPoi]
defaultPoiCategories InformationPoint = S.fromList [RecreationPoi]
defaultPoiCategories PilgrimResource = S.fromList [PilgrimPoi]
defaultPoiCategories Shop = S.fromList [RecreationPoi]
defaultPoiCategories Winery = S.fromList [RecreationPoi]
defaultPoiCategories Museum = S.fromList [CulturalPoi]
defaultPoiCategories Theatre = S.fromList [CulturalPoi]
defaultPoiCategories Historical = S.fromList [HistoricalPoi]
defaultPoiCategories Park = S.fromList [NaturalPoi, RecreationPoi]
defaultPoiCategories Beach = S.fromList [NaturalPoi, RecreationPoi]
defaultPoiCategories Natural = S.fromList [NaturalPoi]
defaultPoiCategories Lookout = S.fromList [NaturalPoi]
defaultPoiCategories _ = S.empty

  
-- | A point of interest, attached to a parent location or leg
data PointOfInterest = PointOfInterest {
    poiID :: Text -- ^ The identifier of the point of interest
  , poiName :: Localised TaggedText -- ^ The name of the point of interest
  , poiDescription :: Maybe Description -- ^ Detailed description
  , poiType :: LocationType -- ^ The point of interest type, same as a location type
  , poiCategories :: S.Set PoiCategory -- ^ The broad categories that are of interest
  , poiPosition :: LatLong -- ^ Lat/long/elevation
  , poiHours :: Maybe OpenHours -- ^ Dates and times when the point of interest is open
  , poiTime :: Maybe Float -- ^ The amount of time, in hours that someone might spend investigating the Poi
  , poiEvents :: [Event] -- ^ Associated events
} deriving (Show, Generic)

instance Placeholder Text PointOfInterest where
  placeholderID = poiID
  placeholder lid = PointOfInterest {
      poiID = lid
    , poiName = wildcardText $ ("Placeholder for " <> lid)
    , poiDescription = Nothing
    , poiType = PlaceholderLocation
    , poiCategories = S.empty
    , poiPosition = def
    , poiHours = Nothing
    , poiTime = Nothing
    , poiEvents = []
  }
  isPlaceholder poi = poiType poi == PlaceholderLocation

instance Dereferencer Text PointOfInterest Camino where
  dereference camino poi = maybe poi fst (M.lookup (placeholderID poi) (caminoPoiMap camino))

instance FromJSON PointOfInterest where
  parseJSON (String v) = do
    return $ placeholder v
  parseJSON (Object v) = do
    id' <- v .: "id"
    name' <- v .: "name"
    description' <- v .:? "description" .!= Nothing
    type' <- v .:? "type" .!= Poi
    categories <- v .:? "categories" .!= defaultPoiCategories type'
    position' <- v .: "position"
    hours' <- v .:? "hours"
    time' <- v .:? "time"
    events' <- v .:? "events" .!= []
    return PointOfInterest {
        poiID = id'
      , poiName = name'
      , poiDescription = description'
      , poiType = type'
      , poiCategories = categories
      , poiPosition = position'
      , poiHours = hours'
      , poiTime = time'
      , poiEvents = events'
    }
  parseJSON v = typeMismatch "expecting object" v

instance ToJSON PointOfInterest where
    toJSON (PointOfInterest id' name' description' type' categories' position' hours' time' events') =
      object [
          "id" .= id'
        , "name" .= name'
        , "description" .= description'
        , "type" .= type'
        , "categories" .= (if categories' == defaultPoiCategories type' then Nothing else Just categories')
        , "position" .= position'
        , "hours" .= hours'
        , "time" .= time'
        , "events" .= (if null events' then Nothing else Just events')
        ]
    toEncoding (PointOfInterest id' name' description' type' categories' position' hours' time' events') =
      pairs $
          "id" .= id'
        <> "name" .= name'
        <> "description" .?= description'
        <> "type" .= type'
        <> "categories" .?= (if categories' == defaultPoiCategories type' then Nothing else Just categories')
        <> "position" .?= position'
        <> "hours" .?= hours'
        <> "time" .?= time'
        <> "events" .?= (if null events' then Nothing else Just events')

instance NFData PointOfInterest

instance Eq PointOfInterest where
  a == b = poiID a == poiID b

instance Ord PointOfInterest where
  a `compare` b = poiID a `compare` poiID b

instance Summary PointOfInterest where
  summary = placeholderLabel

-- | Get a simple text version of the poi name
poiNameLabel :: PointOfInterest -> Text
poiNameLabel poi = localiseDefault $ poiName poi

-- | A location, usually a city/town/village that marks the start and end points of a leg
--   and which may have accommodation and other services available.
--   Locations form the vertexes on the travel graph
data Location = Location {
    locationID :: Text
  , locationName :: Localised TaggedText
  , locationDescription :: Maybe Description
  , locationType :: LocationType
  , locationPosition :: LatLong
  , locationRegion :: Maybe Region
  , locationServices :: S.Set Service
  , locationAccommodation :: [Accommodation]
  , locationPois :: [PointOfInterest]
  , locationEvents :: [Event]
  , locationCamping :: Bool
  , locationAlwaysOpen :: Bool
} deriving (Show, Generic)

instance Placeholder Text Location where
  placeholderID = locationID
  placeholder lid = Location {
      locationID = lid
    , locationName = wildcardText $ ("Placeholder for " <> lid)
    , locationDescription = Nothing
    , locationType = PlaceholderLocation
    , locationPosition = def
    , locationRegion = Nothing
    , locationServices = S.empty
    , locationAccommodation = []
    , locationPois = []
    , locationEvents = []
    , locationCamping = False
    , locationAlwaysOpen = False
  }
  isPlaceholder location = locationType location == PlaceholderLocation

setAccommodationID :: Accommodation -> Text -> Int -> Accommodation
setAccommodationID (Accommodation "" name' description' type' services' sleeping' multi') lid pos =
  Accommodation id' name' description' type' services' sleeping' multi'
  where
    id' = lid <> "#" <> pack (show pos)
setAccommodationID accommodation _lid _pos = accommodation

instance Normaliser Text Location CaminoConfig where
  normalise config location = location {
      locationRegion = dereference (caminoConfigRegions config) <$> (locationRegion location)
    , locationAccommodation = map (\(ac, pos) -> setAccommodationID ac (locationID location) pos) (zip (locationAccommodation location) [1..])
  }

instance Dereferencer Text Location CaminoConfig where
  dereference config location = dereference (caminoConfigLocationLookup config) location

instance Dereferencer Text Location Camino where
  dereference camino location = dereference (caminoLocations camino) location

instance FromJSON Location where
  parseJSON (String v) = do
    return $ placeholder v
  parseJSON (Object v) = do
    id' <- v .: "id"
    name' <- v .: "name"
    description' <- v .:? "description" .!= Nothing
    type' <- v .:? "type" .!= Poi
    position' <- v .: "position"
    region' <- v .:? "region"
    services' <- v .: "services"
    accommodation' <- v .: "accommodation"
    pois' <- v .:? "pois" .!= []
    events' <- v .:? "events" .!= []
    camping' <- v .:? "camping" .!= locationCampingDefault type'
    alwaysOpen' <- v .:? "always-open" .!= locationAlwaysOpenDefault type'
    return Location {
        locationID = id'
      , locationName = name'
      , locationDescription = description'
      , locationType = type'
      , locationPosition = position'
      , locationRegion = placeholder <$> region'
      , locationServices = services'
      , locationAccommodation = accommodation'
      , locationPois = pois'
      , locationEvents = events'
      , locationCamping = camping'
      , locationAlwaysOpen = alwaysOpen'
    }
  parseJSON v = error ("Unable to parse location object " ++ show v)

instance ToJSON Location where
    toJSON (Location id' name' description' type' position' region' services' accommodation' pois' events' camping' alwaysOpen') =
      object [
          "id" .= id'
        , "name" .= name'
        , "description" .= description'
        , "type" .= type'
        , "position" .= position'
        , "region" .= (placeholderID <$> region')
        , "services" .= services'
        , "accommodation" .= accommodation'
        , "pois" .= (if null pois' then Nothing else Just pois')
        , "events" .= (if null events' then Nothing else Just events')
        , "camping" .= (if camping' == locationCampingDefault type' then Nothing else Just camping')
        , "always-open" .= (if alwaysOpen' == locationAlwaysOpenDefault type' then Nothing else Just alwaysOpen')
        ]
        
    toEncoding (Location id' name' description' type' position' region' services' accommodation' pois' events' camping' alwaysOpen') =
      pairs $
          "id" .= id'
        <> "name" .= name'
        <> "description" .?= description'
        <> "type" .= type'
        <> "position" .?= position'
        <> "region" .?= (placeholderID <$> region')
        <> "services" .= services'
        <> "accommodation" .= accommodation'
        <> "pois" .?= (if null pois' then Nothing else Just pois')
        <> "events" .?= (if null events' then Nothing else Just events')
        <> "camping" .?= (if camping' == locationCampingDefault type' then Nothing else Just camping')
        <> "always-open" .?= (if alwaysOpen' == locationAlwaysOpenDefault type' then Nothing else Just alwaysOpen')

instance FromJSONKey Location where
  fromJSONKey = FromJSONKeyTextParser $ \lid -> pure $ placeholder lid
instance ToJSONKey Location where
  toJSONKey = toJSONKeyText placeholderID

instance NFData Location

instance Vertex Location where
  identifier v = unpack $ locationID v

instance Eq Location where
  a == b = locationID a == locationID b
  
instance Ord Location where
  a `compare` b = locationID a `compare` locationID b

instance Summary Location where
  summary = placeholderLabel

-- | Get the accommodation types available at a location
--   These are ordered into enumeration order
locationAccommodationTypes :: Location -> S.Set AccommodationType
locationAccommodationTypes location = S.fromList $ map accommodationType (locationAccommodation location)

-- | Get the point of interest types available at a location
--   These are ordered into enumeration order
locationPoiTypes :: Location -> S.Set LocationType
locationPoiTypes location = S.fromList $ map poiType (locationPois location)

-- | Get the event types available at a location from both the location itself and any points of interest
--   These are ordered into enumeration order
locationEventTypes :: Location -> S.Set EventType
locationEventTypes location = let
   locet = map eventType (locationEvents location)
   poiets = map (\poi -> map eventType (poiEvents poi)) (locationPois location)
 in
  S.fromList $ concat $ locet:poiets

-- | Get the additional resources available at a location via transport links
locationAdditional :: (Ord a) => (Location -> S.Set a) -> Camino -> Location -> S.Set a
locationAdditional resource camino location = let
  locs = map legTo (locationTransportLinks camino location)
  addn = S.unions (map resource locs)
  in
    addn `S.difference` resource location

-- | Get additional accommodation types available from transport links
locationAdditionalAccommodationTypes :: Camino -> Location -> S.Set AccommodationType
locationAdditionalAccommodationTypes camino location = locationAdditional locationAccommodationTypes camino location

-- | Get additional services available from transport links
locationAdditionalServices :: Camino -> Location -> S.Set Service
locationAdditionalServices camino location = locationAdditional locationServices camino location

-- | Get additional points of interes available from transport links
locationAdditionalPoiTypes :: Camino -> Location -> S.Set LocationType
locationAdditionalPoiTypes camino location = locationAdditional locationPoiTypes camino location

-- | Get the resources available at a location locally or via transport links
locationAllSet :: (Ord a) => (Location -> S.Set a) -> Camino -> Location -> S.Set a
locationAllSet resource camino location = let
  locs = map legTo (locationTransportLinks camino location)
  addn = S.unions (map resource locs)
  in
    addn `S.union` resource location

-- | Get all services accessible from a location
locationAllServices :: Camino -> Location -> S.Set Service
locationAllServices camino location = locationAllSet locationServices camino location

-- | Get all possible resources available at a location locally or via transport links
locationAll :: (Location -> [a]) -> Camino -> Location -> [a]
locationAll resource camino location = let
  locs = map legTo (locationTransportLinks camino location)
  addn = concat (map resource locs)
  in
    resource location ++ addn

-- | Get all accommodation accessible from a location
locationAllAccommodation :: Camino -> Location -> [Accommodation]
locationAllAccommodation camino location = locationAll locationAccommodation camino location

-- | Get all pois accessible from a location
locationAllPois :: Camino -> Location -> [PointOfInterest]
locationAllPois camino location = locationAll locationPois camino location

-- | Get a simple text version of the location name
locationNameLabel :: Location -> Text
locationNameLabel location = localiseDefault $ locationName location


-- | Get a bounding box for set of locations
locationBbox :: (Foldable f) => f Location -- ^ The collection of locations
 -> (LatLong, LatLong) -- ^ The bouding box (top-left, bottom-right)
locationBbox locations = (LatLong (maximum lats) (minimum longs) (if maxelev == 0.0 && minelev == 0.0 then Nothing else Just minelev)  def, LatLong (minimum lats) (maximum longs) (if maxelev == 0.0 && minelev == 0.0 then Nothing else Just maxelev) def)
  where
    positions = map locationPosition $ toList locations
    lats = map latitude positions
    longs = map longitude positions
    elevs = map (\p -> maybe 0.0 id (elevation p)) positions
    minelev = minimum elevs
    maxelev = maximum elevs

-- | The type of transport available on a leg
data LegType = Road -- ^ Walk or cycle on a road or suitable path
   | Trail -- ^ Walking only path
   | CyclePath -- ^ Cycling only route
   | FerryLink -- ^ Boat transfer
   | BoatLink -- ^ Self-powered boat or canoe
   | BusLink -- ^ Bus transfer (not normally considered suitable for a Camino)
   | TrainLink -- ^ Train transfer (not normally considered suitable for a Camino)
   deriving (Show, Generic, Eq, Ord, Enum, Bounded)

-- | Provide an enumeration of all services
legTypeEnumeration :: [LegType]
legTypeEnumeration = [minBound .. maxBound]

instance FromJSON LegType
instance ToJSON LegType
instance NFData LegType

instance Default LegType where
  def = Road

-- | A leg segment is a convienient part of a leg
--   It is used to partition up complex legs into segments that can be processed easily
data LegSegment = LegSegment {
    lsFrom :: LatLong
  , lsTo :: LatLong
  , lsDistance :: Float
  , lsAscent :: Float
  , lsDescent :: Float
} deriving (Eq, Show, Generic)

instance NFData LegSegment

-- | A leg from one location to another.
--   Legs form the edges of a camino graph.
-- 
--   Legs have a total ascent and descent. 
--   These are usually assumed to be relatively lumpy and are grouped together for
--   time calculations
data Leg = Leg {
    legType :: LegType -- ^ The type of leg
  , legFrom :: Location -- ^ The start location
  , legTo :: Location -- ^ The end location
  , legDescription :: Maybe Description -- ^ Additional descriptive information
  , legDistance :: Float -- ^ The distance between the start and end in kilometres
  , legTime :: Maybe Float -- ^ An explicit time associated with the leg
  , legAscent :: Float -- ^ The total ascent on the leg in metres
  , legDescent :: Float -- ^ The total descent on the leg in metres
  , legPenance :: Maybe Penance -- ^ Any additional penance associated with the leg
  , legWaypoints :: [LatLong] -- ^ Intermediate waypoints on the leg, usually empty
  , legSegments :: [LegSegment] -- ^ Pre-calculated leg segments
} deriving (Show, Generic)

instance FromJSON Leg where
    parseJSON (Object v) = do
      type' <- v .:? "type" .!= def
      from' <- v .: "from"
      to' <- v .: "to"
      description' <- v .:? "description" .!= Nothing
      distance' <- v .: "distance"
      time' <- v .:? "time" .!= Nothing
      ascent' <- v .: "ascent"
      descent' <- v .: "descent"
      penance' <- v .:? "penance" .!= Nothing
      waypoints' <- v .:? "waypoints" .!= []

      return Leg {
          legType = type'
        , legFrom = from'
        , legTo = to'
        , legDescription = description'
        , legDistance = distance'
        , legTime = time'
        , legAscent = ascent'
        , legDescent = descent'
        , legPenance = penance'
        , legWaypoints = waypoints'
        , legSegments = []
        }
    parseJSON v = error ("Unable to parse leg object " ++ show v)

instance ToJSON Leg where
  toJSON (Leg type' from' to' description' distance' time' ascent' descent' penance' waypoints' _segments') =
      object [
          "type" .= (if type' == def then Nothing else Just type')
        , "from" .= locationID from'
        , "to" .= locationID to'
        , "description" .= description'
        , "distance" .= distance'
        , "time" .= time'
        , "ascent" .= ascent'
        , "descent" .= descent'
        , "penance" .= penance'
        , "waypoints" .= if null waypoints' then Nothing else Just waypoints'
        ]
  toEncoding (Leg type' from' to' description' distance' time' ascent' descent' penance' waypoints' _segments') =
      pairs $
          "type" .?= (if type' == def then Nothing else Just type')
        <> "from" .= locationID from'
        <> "to" .= locationID to'
        <> "description" .?= description'
        <> "distance" .= distance'
        <> "time" .?= time'
        <> "ascent" .= ascent'
        <> "descent" .= descent'
        <> "penance" .?= penance'
        <> "waypoints" .?= if null waypoints' then Nothing else Just waypoints'

instance NFData Leg

instance Edge Leg Location where
  source = legFrom
  target = legTo

instance Eq Leg where
  a == b = legType a == legType b && legFrom a == legFrom b && legTo a == legTo b && legDistance a == legDistance b

instance Ord Leg where
  a `compare` b
    | legType a /= legType b = legType a `compare` legType b
    | legFrom a /= legFrom b = legFrom a `compare` legFrom b
    | legTo a /= legTo b = legTo a `compare` legTo b
    | otherwise = legDistance a `compare` legDistance b

instance Summary Leg where
  summary leg = "{" <> (pack $ show $ legType leg) <> ", " <> locationID (legFrom leg) <> " -> " <> locationID (legTo leg) <> "}"

-- | Ensure a leg has locations mapped correctly within a camino
normaliseLeg :: Camino -> Leg -> Leg
normaliseLeg camino leg =
  leg {
      legFrom = lf
    , legTo = lt
    , legSegments = segs
  } where
    lf = dereference camino (legFrom leg)
    lt = dereference camino (legTo leg)
    segs = buildLegSegments lf lt (legWaypoints leg) (legDistance leg) (legAscent leg) (legDescent leg)

-- | Ensure a leg has locations mapped correctly within a camino, using the full camino config
normaliseLeg' :: CaminoConfig -> Leg -> Leg
normaliseLeg' config leg =
  leg {
      legFrom = lf
    , legTo = lt
    , legSegments = segs
  } where
    lf = dereference config (legFrom leg)
    lt = dereference config (legTo leg)
    segs = buildLegSegments lf lt (legWaypoints leg) (legDistance leg) (legAscent leg) (legDescent leg)

-- Portion out distance and elevation changes to segments, based on the distance between points
buildLegSegments :: Location -> Location -> [LatLong] -> Float -> Float -> Float -> [LegSegment]
buildLegSegments fl tl [] distance ascent descent = let
    fll = locationPosition fl
    tll = locationPosition tl
    distance' = if distance == 0.0 then (realToFrac $ haversineDistance fll tll / 1000.0) else distance
  in
    [LegSegment fll tll distance' ascent descent]
buildLegSegments fl tl waypoints distance ascent descent = let
    fll = locationPosition fl
    tll = locationPosition tl
    llpairs = (snd $ foldl (\(sp, ps) -> \p -> (p, ps ++ [(sp, p)])) (fll, []) waypoints) ++ [(lastWithError waypoints, tll)]
    dists = map (\(sp, ep) -> realToFrac $ haversineDistance sp ep / 1000.0) llpairs -- Distance weights
    tdist = max 0.01 (sum dists)
    tdist' = if distance == 0.0 then (realToFrac $ haversineDistance fll tll / 1000.0) else distance
    dists' = map (\d -> d / tdist * tdist') dists -- Matching actual distance
    mdelevs = map (\(sp, ep) -> realToFrac <$> ((-) <$> elevation ep <*> elevation sp)) llpairs
    ascs = map (\me -> maybe 0.0 (\e -> if e >= 0 then e else 0.0) me) mdelevs
    dscs = map (\me -> maybe 0.0 (\e -> if e < 0 then negate e else 0.0) me) mdelevs
    usedasc = sum ascs
    useddsc = sum dscs
    slop = 20.0 -- A bit left over for everyone
    unusedasc = if ascent < usedasc then error ("Total ascent is less than provided ascent from " ++ (T.unpack $ locationID fl) ++ " - " ++ show fll ++ " to " ++ (T.unpack $ locationID tl) ++ " - " ++ show tll ++ " expected=" ++ show ascent ++ " actual=" ++ show ascs ++ "=" ++ show usedasc) else ascent - usedasc
    unuseddsc = if descent < useddsc then error ("Total descent is less than provided descent from " ++ (T.unpack $ locationID fl) ++ " - " ++ show fll ++ " to " ++ (T.unpack $ locationID tl) ++ " - " ++ show tll ++ " expected=" ++ show descent ++ " actual=" ++ show dscs ++ "=" ++ show useddsc) else descent - useddsc
    ascprop = unusedasc / (usedasc + tdist' * slop)
    dscprop = unuseddsc / (useddsc + tdist' * slop)
    ascs' = map (\(d, u) -> u + (slop * d + u) * ascprop) (zip dists' ascs)
    dscs' = map (\(d, u) -> u + (slop * d + u) * dscprop) (zip dists' dscs)
    segs = map (\((sp, ep), d, asc, dsc) -> LegSegment sp ep d asc dsc) (L.zip4 llpairs dists' ascs' dscs')
  in
    segs

-- Make Colour NFData
-- Colour is already strict, so leave it be
instance NFData (Colour a) where
  rnf _ = ()

-- | A palette, graphical styles to use for displaying information
data Palette = Palette {
    paletteColour :: Colour Double -- ^ The basic colour of the element
  , paletteTextColour :: Colour Double -- ^ The text colour of the element
} deriving (Show, Generic)
      
instance FromJSON Palette where
  parseJSON (Object v) = do
    colour' <- v .: "colour"
    textColour' <- v .:? "text-colour" .!= colour'
    return Palette { 
        paletteColour = sRGB24read colour'
      , paletteTextColour = sRGB24read textColour' 
    }
  parseJSON v = error ("Unable to parse palette object " ++ show v)

instance ToJSON Palette where
  toJSON (Palette colour' textColour') =
    object [
        "colour" .= sRGB24show colour'
      , "text-colour" .= (if colour' == textColour' then Nothing else Just $ sRGB24show textColour')
      ]
  toEncoding (Palette colour' textColour') =
    pairs $ 
        "colour" .= sRGB24show colour'
      <> "text-colour" .?= (if colour' == textColour' then Nothing else Just $ sRGB24show textColour')

instance NFData Palette

instance Default Palette where
  def = Palette {
      paletteColour = sRGB24read "f9b34a" -- Camino yellow
    , paletteTextColour = sRGB24read "f9b34a" -- Camino yellow
  }

-- | Read formulas from JSON
instance (Eq a, Placeholder Text a) => FromJSON (Formula a) where
  parseJSON (Bool v) = do
    return $ if v then T else F
  parseJSON (String v) = do
    return $ Variable $ placeholder v
  parseJSON (Object v) = do
    and' <- v .:? "and"
    or' <- v .:? "or"
    not' <- v .:? "not"
    imp' <- v .:? "implies"
    return $ case (and', or', not', imp') of
      (Just a, Nothing, Nothing, Nothing) -> And a
      (Nothing, Just a, Nothing, Nothing) -> Or a
      (Nothing, Nothing, Just a, Nothing) -> Not a
      (Nothing, Nothing, Nothing, Just [a, b]) -> Implies a b
      _ -> error ("No logical object, must have one of and, or, not or implies: " ++ show v)
  parseJSON v = typeMismatch "Unable to parse condition" v

-- | Produce formulas a JSON
instance (Eq a, Placeholder Text a) => ToJSON (Formula a) where
  toJSON T = Bool True
  toJSON F = Bool False
  toJSON (Variable v) = String $ placeholderID v
  toJSON (And fs) = object [ "and" .= fs ]
  toJSON (Or fs) = object [ "or" .= fs ]
  toJSON (Not f) = object [ "not" .= f ]
  toJSON (Implies p c) = object [ "implies" .= [p, c] ]
  
dereferenceFormula :: (Eq a, Dereferencer k a ctx) => ctx -> Formula a -> Formula a
dereferenceFormula context (Variable v) = Variable $ dereference context v
dereferenceFormula context (And fs) = And (map (dereferenceFormula context) fs)
dereferenceFormula context (Or fs) = Or (map (dereferenceFormula context) fs)
dereferenceFormula context (Not f) = Not $ dereferenceFormula context f
dereferenceFormula context (Implies p c) = Implies (dereferenceFormula context p) (dereferenceFormula context c)
dereferenceFormula _context f = f

-- | A geographical feature, used to display things like route trails.
--   Features have conditions under which they
data Feature = Feature {
    featureID :: Text -- ^ The feature identifier
  , featureType :: LegType -- ^ The sort of route that this represents
  , featureName :: Localised TaggedText -- ^ The feature name
  , featureDescription :: Maybe Description -- ^ Any descriptive information
  , featureCondition :: Formula Location -- ^ The conditions under which a feature is used
  , featureDummy :: Bool -- ^ True if this is a temporary placeholder feature
  , featureGeometry :: Maybe Text -- ^ The geometry identifier (currently just a reference to GeoJSON)
  , featureFeatures :: [Feature] -- ^ Any identified sub-features
} deriving (Show, Generic)

instance FromJSON Feature where
  parseJSON (String v) = do
    return $ placeholder v
  parseJSON (Object v) = do
    id' <- v .: "id"
    type' <- v .:? "type" .!= Road
    name' <- v .: "name"
    description' <- v .:? "description"
    condition' <- v .:? "condition" .!= T
    dummy' <- v .:? "dummy" .!= False
    geometry' <- v .:? "geometry"
    features' <- v .:? "features" .!= []
    return Feature {
        featureID = id'
      , featureType = type'
      , featureName = name'
      , featureDescription = description'
      , featureCondition = condition'
      , featureDummy = dummy'
      , featureGeometry = geometry'
      , featureFeatures = features'
      }
  parseJSON v = typeMismatch "Feature must be string or object" v

instance ToJSON Feature where
  toJSON (Feature id' type' name' description' condition' dummy' geometry' features') =
    object [
        "id" .= id'
      , "type" .= (if type' == Road then Nothing else Just type')
      , "name" .= name'
      , "description" .= description'
      , "condition" .= (if condition' == T then Nothing else Just condition')
      , "dummy" .= (if dummy' == False then Nothing else Just dummy')
      , "geometry" .= geometry'
      , "features" .= (if null features' then Nothing else Just features')
      ]
  toEncoding (Feature id' type' name' description' condition' dummy' geometry' features') =
    pairs $
         "id" .= id'
      <> "type" .?= (if type' == Road then Nothing else Just type')
      <> "name" .= name'
      <> "description" .?= description'
      <> "condition" .?= (if condition' == T then Nothing else Just condition')
      <> "dummy" .= (if dummy' == False then Nothing else Just dummy')
      <> "geometry" .?= geometry'
      <> "features" .?= (if null features' then Nothing else Just features')

instance NFData Feature

instance Eq Feature where
  a == b = featureID a == featureID b

instance Ord Feature where
  a `compare` b = featureID a `compare` featureID b

instance Placeholder Text Feature where
  placeholderID = featureID
  placeholder fid = Feature {
      featureID = fid
    , featureType = Road
    , featureName = wildcardText $ ("Placeholder for " <> fid)
    , featureDescription = Nothing
    , featureCondition = F
    , featureDummy = True
    , featureGeometry = Nothing
    , featureFeatures = []
  }
  isPlaceholder feature = isPlaceholderName (featureName feature)

instance Normaliser Text Feature Camino where
  normalise camino feature = feature {
       featureCondition = dereferenceFormula camino $ featureCondition feature
  }

instance Dereferencer Text Feature Camino where
  dereference camino route = dereference (caminoFeatureMap camino) route

instance Summary Feature where
  summary feature = "F{"
    <> featureID feature
    <> ", geometry=" <> summary (featureGeometry feature)
    <> "}"

-- | Get all the features, including sub-features as a list
allFeatures :: Feature -> [Feature]
allFeatures feature = feature:(concat $ map allFeatures $ featureFeatures feature)

-- | A route, a sub-section of the camino with graphical information
data Route = Route {
    routeID :: Text -- ^ An identifier for the route
  , routeName :: Localised TaggedText -- ^ The route name
  , routeDescription :: Description -- ^ The route description
  , routeMajor :: Bool -- ^ Is this a major route (a multi-day variant to the main route)
  , routeLocations :: [Location] -- ^ The locations along the route specifci to that route
  , routeLocationSet :: S.Set Location -- ^ Locations for easy membership checks
  , routeLegs :: [Leg] -- ^ The legs that are specific to that route
  , routeStops :: [Location] -- ^ The suggested stops for the route, with priorities
  , routeRestPoints :: [Location] -- ^ The suggested preferred rest points for the route
  , routeStarts :: [Prioritised Text Location] -- ^ A list of suggested start points for the route, with priorities
  , routeFinishes :: [Prioritised Text Location] -- ^ A list of suggested finish points for the route, with priorities
  , routeSuggestedPois :: [PointOfInterest] -- ^ A list of suggested points of interest for the route
  , routePalette :: Palette
  , routeFeatures :: [Feature] -- ^ Features that map the route
} deriving (Show, Generic)

instance FromJSON Route where
    parseJSON (String v) = do
      return $ placeholder v
    parseJSON (Object v) = do
      id' <- v .: "id"
      name' <- v .: "name"
      description' <- v .: "description"
      major' <- v .:? "major" .!= False
      locations' <- v .:? "locations" .!= []
      legs' <- v .:? "legs" .!= []
      stops' <- v .:? "stops" .!= []
      rests' <- v .:? "rest-points" .!= []
      starts' <- v .:? "starts" .!= []
      finishes' <- v .:? "finishes" .!= []
      pois' <- v .:? "suggested-pois" .!= []
      palette' <- v .: "palette"
      features' <- v .:? "features" .!= []
      return Route { 
          routeID = id'
        , routeName = name'
        , routeDescription = description'
        , routeMajor = major'
        , routeLocations = locations'
        , routeLocationSet = S.fromList locations'
        , routeLegs = legs'
        , routeStops = stops'
        , routeRestPoints = rests'
        , routeStarts = starts'
        , routeFinishes = finishes'
        , routeSuggestedPois = pois'
        , routePalette = palette'
        , routeFeatures = features'
      }
    parseJSON v = error ("Unable to parse route object " ++ show v)

instance ToJSON Route where
    toJSON (Route id' name' description' major' locations' _locationSet' legs' stops' rests' starts' finishes' pois' palette' features') =
      object [ 
          "id" .= id'
        , "name" .= name'
        , "description" .= description'
        , "major" .= (if major' then Just major' else Nothing)
        , "locations" .= (if null locations' then Nothing else Just $ map locationID locations')
        , "legs" .= (if null legs' then Nothing else Just legs')
        , "stops" .= (if null stops' then Nothing else Just $ map locationID stops')
        , "rest-points" .= (if null rests' then Nothing else Just $ map locationID rests')
        , "starts" .= (if null starts' then Nothing else Just starts')
        , "finishes" .= (if null finishes' then Nothing else Just finishes')
        , "suggested-pois" .= (if null pois' then Nothing else Just $ map poiID pois')
        , "palette" .= palette'
        , "features" .= (if null features' then Nothing else Just features')
        ]
    toEncoding (Route id' name' description' major' locations' _locationSet' legs' stops' rests' starts' finishes' pois' palette' features') =
      pairs $ 
          "id" .= id'
        <> "name" .= name'
        <> "description" .= description'
        <> "major" .?= (if major' then Just major' else Nothing)
        <> "locations" .?= (if null locations' then Nothing else Just $ map locationID locations')
        <> "legs" .?= (if null legs' then Nothing else Just legs')
        <> "stops" .?= (if null stops' then Nothing else Just $ map locationID stops')
        <> "rest-points" .?= (if null rests' then Nothing else Just $ map locationID rests')
        <> "starts" .?= (if null starts' then Nothing else Just starts')
        <> "finishes" .?= (if null finishes' then Nothing else Just finishes')
        <> "suggested-pois" .?= (if null pois' then Nothing else Just $ map poiID pois')
        <> "palette" .= palette'
        <> "features" .?= (if null features' then Nothing else Just features')


instance NFData Route

instance Eq Route where
  a == b = routeID a == routeID b

instance Ord Route where
  a `compare` b = routeID a `compare` routeID b

instance Placeholder Text Route where
  placeholderID = routeID
  placeholder rid = Route {
      routeID = rid
    , routeName = wildcardText $ ("Placeholder for " <> rid)
    , routeDescription = wildcardDescription ""
    , routeMajor = False
    , routeLocations = []
    , routeLocationSet = S.empty
    , routeLegs = []
    , routeStops = []
    , routeRestPoints = []
    , routeStarts = []
    , routeFinishes = []
    , routeSuggestedPois = []
    , routePalette = def
    , routeFeatures = []
  }
  isPlaceholder route = isPlaceholderName (routeName route)

instance Normaliser Text Route Camino where
  normalise camino route = route {
       routeLocations = locations'
     , routeLocationSet = S.fromList locations'
     , routeLegs = map (normaliseLeg camino) (routeLegs route)
     , routeStops = dereferenceF camino (routeStops route)
     , routeRestPoints = dereferenceF camino (routeRestPoints route)
     , routeStarts = map (normalisePrioritised camino) (routeStarts route)
     , routeFinishes = map (normalisePrioritised camino) (routeFinishes route)
     , routeSuggestedPois = dereferenceF camino (routeSuggestedPois route)
   }
   where
    locations' = dereferenceF camino (routeLocations route)

instance Dereferencer Text Route Camino where
  dereference camino route = dereference (caminoRoutes camino) route

instance Summary Route where
  summary route = "R{"
    <> routeID route
    <> ", locs=" <> summary (routeLocations route)
    <> ", starts=" <> summary (map prItem $ routeStarts route)
    <> ", finishes=" <> summary (map prItem $ routeFinishes route)
    <> "}"

routeAllLocations :: Route -> S.Set Location
routeAllLocations route = (routeLocationSet route) `S.union` llocs
  where
    llocs = foldr (\l -> \ls -> S.insert (legFrom l) $ S.insert (legTo l) ls) S.empty (routeLegs route)

-- Find the location closest to the centre of the route
routeCentralLocation :: Route -> Location
routeCentralLocation route = let
    locations = routeAllLocations route
  in
    if S.null locations then
      error ("No locations for route " ++ (T.unpack $ routeID route))
    else let
        centre = centroid $ S.map locationPosition locations
        closest = minimumBy (\l1 -> \l2 -> compare (euclidianDistance2 centre (locationPosition l1)) (euclidianDistance2 centre (locationPosition l2))) locations
      in
        closest

routeLegTypes' :: Feature -> S.Set LegType
routeLegTypes' feature = S.unions $ (S.singleton $ featureType feature):(map routeLegTypes' (featureFeatures feature))

routeLegTypes :: Route -> S.Set LegType
routeLegTypes route = if null fs then (S.singleton Road) else S.unions (map routeLegTypes' fs) where fs = routeFeatures route

-- | Statements about how routes weave together
--   Route logic allows you to say, if you choose this combination of routes then you must also have these routes and
--   can't have these. You'll also need to include these locations and remove those.
--   The formula construction allows to to make arbitrary 
data RouteLogic = RouteLogic {
    routeLogicDescription :: Maybe Text -- ^ Explain what is happening
  , routeLogicCondition :: Formula Route -- ^ What triggers this bit of logic
  , routeLogicRequires :: S.Set Route -- ^ Routes that must be included for things to work
  , routeLogicAllows :: S.Set Route -- ^ Routes that this implies can be included
  , routeLogicProhibits :: S.Set Route -- ^ Routes that this implies should be excluded
  , routeLogicInclude :: [Location] -- ^ Stuff to add to the allowed locations
  , routeLogicExclude :: [Location] -- ^ Stuff to remove from the allowed locations
} deriving (Show, Generic)

instance FromJSON RouteLogic where
  parseJSON (Object v) = do
    description' <- v .:? "description"
    condition' <- v .: "condition"
    requires' <- v .:? "requires" .!= S.empty
    allows' <- v .:? "allows" .!= S.empty
    prohibits' <- v .:? "prohibits" .!= S.empty
    include' <- v .:? "include" .!= []
    exclude' <- v .:? "exclude" .!= []
    return RouteLogic {
        routeLogicDescription = description'
      , routeLogicCondition = condition'
      , routeLogicRequires = requires'
      , routeLogicAllows = allows'
      , routeLogicProhibits = prohibits'
      , routeLogicInclude = include'
      , routeLogicExclude = exclude'
    }
  parseJSON v = error ("Unable to parse route object " ++ show v)

instance ToJSON RouteLogic where
  toJSON (RouteLogic description' condition' requires' allows' prohibits' include' exclude') =
    object [
        "description" .= description'
      , "condition" .= condition'
      , "requires" .= nonEmptyS (S.map placeholderID requires')
      , "allows" .= nonEmptyS (S.map placeholderID allows')
      , "prohibits" .= nonEmptyS (S.map placeholderID prohibits')
      , "include" .= nonEmptyL (map placeholderID include')
      , "exclude" .= nonEmptyL (map placeholderID exclude')
      ]
    where
      nonEmptyL v = if null v then Nothing else Just v
      nonEmptyS v = if S.null v then Nothing else Just v
      
  toEncoding (RouteLogic description' condition' requires' allows' prohibits' include' exclude') =
    pairs $
        "description" .?= description'
      <> "condition" .= condition'
      <> "requires" .?= nonEmptyS requires'
      <> "allows" .?= nonEmptyS allows'
      <> "prohibits" .?= nonEmptyS prohibits'
      <> "include" .?= nonEmptyL (map placeholderID include')
      <> "exclude" .?= nonEmptyL (map placeholderID exclude')
    where
      nonEmptyL v = if null v then Nothing else Just v
      nonEmptyS v = if S.null v then Nothing else Just v

instance NFData RouteLogic

normaliseRouteLogic :: Camino -> RouteLogic -> RouteLogic
normaliseRouteLogic camino logic = logic {
      routeLogicCondition = dereferenceFormula camino (routeLogicCondition logic)
    , routeLogicRequires = S.map (dereference camino) (routeLogicRequires logic)
    , routeLogicAllows = S.map (dereference camino) (routeLogicAllows logic)
    , routeLogicProhibits = S.map (dereference camino) (routeLogicProhibits logic)
    , routeLogicInclude = map (dereference camino) (routeLogicInclude logic)
    , routeLogicExclude = map (dereference camino) (routeLogicExclude logic)
  }

createLogicClauses' :: RouteLogic -> S.Set Route -> [Formula Route]
createLogicClauses' logic consequents =
    map (\r -> Implies condition (Variable r)) (toList $ consequents)
  where
    condition = routeLogicCondition logic

-- | Convert the route logic into clauses for requirement deduction
createRequiresClauses :: RouteLogic -- ^ The piece of route logic
  -> [Formula Route] -- ^ A list of clauses that imply anything required is true and anything prohibited is false
createRequiresClauses logic = createLogicClauses' logic (routeLogicRequires logic)

-- | Convert the route logic into clauses for allowed route deduction
createAllowsClauses :: RouteLogic -- ^ The piece of route logic
  -> [Formula Route] -- ^ A list of clauses that imply anything allowed is true and anything prohibited is false
createAllowsClauses logic = createLogicClauses' logic (routeLogicAllows logic)

-- | Convert the route logic into clauses for prohibited route deduction
createProhibitsClauses :: RouteLogic -- ^ The piece of route logic
  -> [Formula Route] -- ^ A list of clauses that imply anything allowed is true and anything prohibited is false
createProhibitsClauses logic = createLogicClauses' logic (routeLogicProhibits logic)

-- | A way, consisting of a number of legs with a start and end
--   The purpose of the Camino Planner is to divide a camino into 
data Camino = Camino {
    caminoId :: Text
  , caminoName :: Localised TaggedText
  , caminoDescription :: Description
  , caminoMetadata :: Metadata
  , caminoFragment :: Bool -- ^ Indicates that this a fragmentary camino intended to be imported by other caminos
  , caminoImports :: [Camino] -- ^ Imported segments of other camino information
  , caminoLocations :: [Location] -- ^ The camino locations
  , caminoLegs :: [Leg] -- ^ The legs between locations
  , caminoTransportLinks :: [Leg] -- ^ Transport links between locations
  , caminoRoutes :: [Route] -- ^ Named sub-routes
  , caminoRouteLogic :: [RouteLogic] -- ^ Additional logic for named sub-routes
  , caminoDefaultRoute :: Route -- ^ The default route to use
  , caminoLocationMap :: M.Map Text Location -- ^ The camino locations as a map
  , caminoAccommodationMap :: M.Map Text (Accommodation, Location) -- ^ The camino accommodation
  , caminoPoiMap :: M.Map Text (PointOfInterest, Location) -- ^ The camino points of interest
} deriving (Show, Generic)


-- Internal, construct the location map from a list of locations
buildLocationMap :: [Location] -> M.Map Text Location
buildLocationMap locs = M.fromList $ map (\l -> (placeholderID l, l)) locs

-- Internal, construct the PoI map from a list of locations
buildPoiMap :: [Location] -> M.Map Text (PointOfInterest, Location)
buildPoiMap locs = M.fromList $ foldl (\ps -> \l -> map (\p -> (poiID p, (p, l))) (locationPois l) ++ ps)  [] locs

-- Internal, construct the accommodation map from a list of locations
buildAccommodationMap :: [Location] -> M.Map Text (Accommodation, Location)
buildAccommodationMap locs = M.fromList $ foldl (\as -> \l -> map (\a -> (accommodationID a, (a, l))) (filter (not . isGenericAccommodation) (locationAccommodation l)) ++ as)  [] locs

instance Eq Camino where
  a == b = caminoId a == caminoId b

instance Ord Camino where
  a `compare` b = caminoId a `compare` caminoId b

instance FromJSON Camino where
  parseJSON (String v) = do
    return $ placeholder v
  parseJSON (Object v) = do
    id' <- v .: "id"
    name' <- v .: "name"
    description' <- v .: "description"
    metadata' <- v .:? "metadata" .!= def
    fragment' <- v .:? "fragment" .!= False
    imports' <- v .:? "imports" .!= []
    locs' <- v .: "locations"
    legs' <- v .:? "legs" .!= []
    links' <- v .:? "links" .!= []
    routes' <- v .: "routes"
    routeLogic' <- v .: "route-logic"
    defaultRoute' <- v .:? "default-route" .!= (routeID $ headWithError routes')
    let defaultRoute'' = fromJust $ L.find (\r -> routeID r == defaultRoute') routes'
    let otherRoutes = filter (\r -> routeID r /= defaultRoute') routes'
    let defaultLocations' = (defaultRouteLocations locs' otherRoutes) ++ (routeLocations defaultRoute'')
    let defaultRoute''' = defaultRoute'' { -- Add unassigned locations to the default
        routeLocations = defaultLocations'
      , routeLocationSet = S.fromList defaultLocations'
      }
    let routes'' = defaultRoute''' : otherRoutes
    let locMap' = buildLocationMap locs'
    let accommodation' = buildAccommodationMap locs'
    let pois' = buildPoiMap locs'
    return $ Camino {
        caminoId = id'
      , caminoName = name'
      , caminoDescription = description'
      , caminoMetadata = metadata'
      , caminoFragment = fragment'
      , caminoImports = imports'
      , caminoLocations = locs'
      , caminoLegs = legs'
      , caminoTransportLinks = links'
      , caminoRoutes = routes''
      , caminoRouteLogic = routeLogic'
      , caminoDefaultRoute = defaultRoute'''
      , caminoLocationMap = locMap'
      , caminoAccommodationMap = accommodation'
      , caminoPoiMap = pois'
    }
  parseJSON v = error ("Unable to parse camino object " ++ show v)

instance ToJSON Camino where
  toJSON (Camino id' name' description' metadata' fragment' imports' locations' legs' links' routes' routeLogic' defaultRoute' _locMap _accommodation _pois) =
    object [
        "id" .= id'
      , "name" .= name'
      , "description" .= description'
      , "metadata" .= (if null (metadataStatements metadata') then Nothing else Just metadata')
      , "fragment" .= (if fragment' then Just fragment' else Nothing)
      , "imports" .= (if null imports' then Nothing else Just (map placeholderID imports'))
      , "locations" .= locations'
      , "legs" .= (if null legs' then Nothing else Just legs')
      , "links" .= (if null links' then Nothing else Just links')
      , "routes" .= map (\r -> if routeID r == routeID defaultRoute' then r { routeLocations = [], routeLocationSet = S.empty } else r) routes'
      , "route-logic" .= routeLogic'
      , "default-route" .= routeID defaultRoute'
      ]
  toEncoding (Camino id' name' description' metadata' fragment' imports' locations' legs' links' routes' routeLogic' defaultRoute' _locMap _accommodation _pois) =
    pairs $
        "id" .= id'
      <> "name" .= name'
      <> "description" .= description'
      <> "metadata" .?= (if null (metadataStatements metadata') then Nothing else Just metadata')
      <> "fragment" .?= (if fragment' then Just fragment' else Nothing)
      <> "imports" .?= (if null imports' then Nothing else Just (map placeholderID imports'))
      <> "locations" .= locations'
      <> "legs" .?= (if null legs' then Nothing else Just legs')
      <> "links" .?= (if null links' then Nothing else Just links')
      <> "routes" .= routes'
      <> "route-logic" .= routeLogic'
      <> "default-route" .= routeID defaultRoute'

instance NFData Camino

instance Graph Camino Leg Location where
  vertex camino vid = (caminoLocationMap camino) M.! (pack vid)
  edge camino loc1 loc2 = L.find (\l -> loc1 == legFrom l && loc2 == legTo l) (caminoLegs camino)
  incoming camino location = filter (\l -> location == legTo l) (caminoLegs camino)
  outgoing camino location = filter (\l -> location == legFrom l) (caminoLegs camino)
  subgraph (Camino id' name' description' metadata' fragment' imports' locations' legs' links' routes' routeLogic' defaultRoute' _locMap _accommodation' _pois') allowed  =
    let
      isAllowed l = S.member l allowed
      isAllowedLeg l = isAllowed (legFrom l) && isAllowed (legTo l)
      id'' = id' <> "'"
      name'' = name' `appendText` " subgraph"
      locations'' = filter isAllowed locations'
      legs'' = filter isAllowedLeg legs'
      links'' = filter isAllowedLeg links'
      routes'' = map (\r -> r {
          routeLocations = filter isAllowed (routeLocations r)
        , routeLocationSet = S.filter isAllowed (routeLocationSet r)
        , routeLegs = filter isAllowedLeg (routeLegs r)
        , routeStops = filter isAllowed (routeStops r)
        }) routes'
      routeLogic'' = map (\l -> l {
          routeLogicInclude = filter isAllowed (routeLogicInclude l)
        , routeLogicExclude = filter isAllowed (routeLogicExclude l)
        }) routeLogic'
      defaultRoute'' = fromJust $ L.find (\r -> routeID r == routeID defaultRoute') routes'
      locMap'' = buildLocationMap locations''
      accommodation'' = buildAccommodationMap locations''
      pois'' = buildPoiMap locations''
    in
      Camino {
          caminoId = id''
        , caminoName = name''
        , caminoDescription = description'
        , caminoMetadata = metadata'
        , caminoFragment = fragment'
        , caminoImports = imports'
        , caminoLocations = locations''
        , caminoLegs = legs''
        , caminoTransportLinks = links''
        , caminoRoutes = routes''
        , caminoRouteLogic = routeLogic''
        , caminoDefaultRoute = defaultRoute''
        , caminoLocationMap = locMap''
        , caminoAccommodationMap = accommodation''
        , caminoPoiMap = pois''
      }
  mirror camino =
    let
      id'' = caminoId camino <> "'"
      name'' = caminoName camino `appendText` " mirrored"
      legs'' = map (\l -> l { legFrom = legTo l, legTo = legFrom l, legAscent = legDescent l, legDescent = legAscent l}) (caminoLegs camino)
    in
      camino { caminoId = id'', caminoName = name'', caminoLegs = legs'' }
      
instance Placeholder Text Camino where
  placeholderID = caminoId
  placeholder cid = Camino {
        caminoId = cid
      , caminoName = wildcardText $ "Placeholder for " <> cid
      , caminoDescription = wildcardDescription ""
      , caminoMetadata = Metadata [] []
      , caminoFragment = False
      , caminoImports = []
      , caminoLocations = []
      , caminoLegs = []
      , caminoTransportLinks = []
      , caminoRoutes = [dr]
      , caminoRouteLogic = []
      , caminoDefaultRoute = dr
      , caminoLocationMap = M.empty
      , caminoAccommodationMap = M.empty
      , caminoPoiMap = M.empty
    }
    where
      dr = placeholder ("DR-" <> cid)
  isPlaceholder camino = isPlaceholderName (caminoName camino)

-- Note that this completely denormalises all imports, assuming that the configuration already has
-- the denormalised imports.
-- This means that imports have to be loaded in dependency order
instance Normaliser Text Camino CaminoConfig where
  normalise config camino = let
      imports' = dereferenceF config (caminoImports camino) -- ^ Imports have already been normalised
      locations' = map (normalise config) (caminoLocations camino)
      locations'' = map caminoLocations imports'
      locations''' = concat (locations':locations'')
      camino1 = camino {
          caminoImports = [] -- ^ We've done the importing by the end of this
        , caminoLocations = locations'''
        , caminoLocationMap = buildLocationMap locations'''
      }
      legs' = map (normaliseLeg camino1) (caminoLegs camino1)
      legs'' = map caminoLegs imports'
      links' = map (normaliseLeg camino1) (caminoTransportLinks camino1)
      links'' = map caminoTransportLinks imports'
      locs' = caminoLocations camino1
      camino2 = camino1 {
          caminoLegs = concat (legs':legs'')
        , caminoTransportLinks = concat (links':links'')
        , caminoAccommodationMap = buildAccommodationMap locs'
        , caminoPoiMap = buildPoiMap locs'
      }
      splitroutes c rs = let
          drid = routeID $ caminoDefaultRoute c
          (dfs, ndfs) = L.partition (\r -> drid == routeID r) rs
        in
          case dfs of
            [df] -> (df, ndfs)
            _ -> error "Expecting single default route"
      routes' = map (normalise camino2) (caminoRoutes camino2)
      (d, nds) = splitroutes camino2 routes'
      routes'' = map (\c -> splitroutes c $ caminoRoutes c) imports'
      ids = map fst routes''
      inds = map snd routes''
      sunion field r rs = S.unions (field r:map field rs)
      lunion field r rs = S.toList $ sunion (S.fromList . field) r rs
      punion field r rs = uniquePrioritised (concat (field r:map field rs))
      d' = d {
          routeLocations = lunion routeLocations d ids
        , routeLocationSet = sunion routeLocationSet d ids
        , routeStops = lunion routeStops d ids
        , routeStarts = punion routeStarts d ids
        , routeFinishes = punion routeFinishes d ids
        , routeSuggestedPois = lunion routeSuggestedPois d ids
      }
      nds' = concat (nds:inds)
      camino3 = camino2 {
          caminoRoutes = d':nds'
        , caminoDefaultRoute = d'
      }
      rl' =  map (normaliseRouteLogic camino3) (caminoRouteLogic camino3)
      rl'' = map caminoRouteLogic imports'
      camino4 = camino3 { caminoRouteLogic = concat (rl':rl'') }
    in
      camino4

instance Dereferencer Text Camino CaminoConfig where
  dereference config camino = dereference (caminoConfigLookup config) camino

instance Summary Camino where
  summary camino = "C{"
    <> placeholderLabel camino
    <> ", routes=" <> summary (caminoRoutes camino)
    <> "}"

-- | Get a simple text version of the camino name
caminoNameLabel :: Camino -> Text
caminoNameLabel camino = localiseDefault $ caminoName camino

-- | Get a bounding box for the camino
caminoBbox :: Camino -- ^ The entire camino
 -> (LatLong, LatLong) -- ^ The bouding box (top-left, bottom-right)
caminoBbox camino = locationBbox $ caminoLocations camino

-- | All the locations on the camino assumed to be part of the default route
defaultRouteLocations :: [Location] -- ^ The possible locations
  -> [Route]
  -> [Location] -- ^ The locations on the camino that have not been explicitly assigned to a route
defaultRouteLocations locations routes =
  let
    routeLocs = S.unions $ map routeLocationSet routes
  in
    filter (\l -> not $ S.member l routeLocs) locations

-- | Choose a route for a location
--   If the location isn't on a specific route, then the default route is returned
--   The route list is scanned backwards, effectively running from most-specific to least specific
caminoRoute :: Camino -- ^ The camino being interrogated
  -> S.Set Route -- ^ The routes in use
  -> Location -- ^ The location to test
  -> Route -- ^ The route that the location is on
caminoRoute camino routes location = let
    route = L.find (\r -> S.member r routes && S.member location (routeLocationSet r)) (reverse $ caminoRoutes camino)
    route' = maybe (caminoDefaultRoute camino) id (L.find (\r -> S.member location (routeLocationSet r)) (reverse $ caminoRoutes camino))
  in
    maybe route' id route

-- | Choose a route for a leg
--   If the leg isn't on a specific route, then the default route is returned
caminoLegRoute :: Camino -- ^ The camino being interrogated
  -> Leg -- ^ The location to test
  -> Route -- ^ The route that the location is on
caminoLegRoute camino leg = let
    dflt = caminoDefaultRoute camino
    route' = L.find (\r -> r /= dflt && elem leg (routeLegs r)) (caminoRoutes camino)
    from' = legFrom leg
    to' = legTo leg
    route'' = L.find (\r -> r /= dflt && (S.member from' (routeLocationSet r) || S.member to' (routeLocationSet r))) (reverse $ caminoRoutes camino)
  in
    maybe dflt id $ route' <|> route''

-- | Build a set of complete routes, including any required by the actual supplied set of routes
completeRoutes :: Camino -> S.Set Route -> (S.Set Route, Substitution Route)
completeRoutes camino routes = let
    logics = caminoRouteLogic camino
    sub = substitutionFromDomain (S.fromList $ caminoRoutes camino) (routes `S.union` (S.singleton $ caminoDefaultRoute camino))
    requires = concat $ map createRequiresClauses logics
    required = implications requires sub
    prohibits = concat $ map createProhibitsClauses logics
    prohibited = implicationsSingle prohibits (overlay sub required) -- Single to stop mutual exclusions from implicating themselves
    membership = (sub `overlay` invert prohibited) `overlay` required
    complete = S.fromList $ filter (\v -> membership v == Just T) (caminoRoutes camino)
  in
   (complete, membership)

-- | Work out what locations are acceptable in a camino, based on the chosen routes.
--   The default route is always included, followed by the routes specified in the preferences.
--   The routes are worked through in order (with the default route always first).
--   The any route logics are worked through in order
--   That way, locations can be included by one route and then excluded by a ltere route
caminoRouteLocations :: Camino -- ^ The base camino definition
  -> S.Set Route -- ^ The base routes that are being used
  -> S.Set Location -- ^ The allowed locations
caminoRouteLocations camino used =
  let
    (routes, membership) = completeRoutes camino used
    baseLocations = S.unions $ S.map routeLocationSet routes
    logics =  filter (\l -> evaluate membership (routeLogicCondition l) == T) (caminoRouteLogic camino)
  in
    foldl (\allowed -> \logic -> (allowed `S.union` (S.fromList $ routeLogicInclude logic) `S.difference` (S.fromList $ routeLogicExclude logic))) baseLocations logics

-- | Work out which route featurees are being used by this set of routes and locations
caminoUsedFeatures :: Camino -- ^ The base camino definition
  -> S.Set Route -- ^ The base routes that are being used
  -> S.Set Location -- ^ The locations that are being used
  -> S.Set Feature -- ^ The features that are being used
caminoUsedFeatures camino routes locations =
  let
    membership = substitutionFromDomain (S.fromList $ caminoLocations camino) locations
  in
    foldl (\fs -> \r -> fs `S.union` (S.fromList $ filter (\f -> evaluate membership (featureCondition f) == T) $ routeFeatures r)) S.empty routes

-- | Get all the regions in the Camino.
--  This does not include parent regions.
caminoRegions :: Camino -> S.Set Region
caminoRegions camino = foldl (\rs -> \l -> maybe rs (\r -> S.insert r rs) (locationRegion l)) S.empty (caminoLocations camino)

-- Get all the geographical features as a map
caminoFeatureMap :: Camino -> M.Map Text Feature
caminoFeatureMap camino = foldr (\r -> \fs -> M.union fs (M.fromList $ map (\f -> (placeholderID f, f)) (routeFeatures r))) M.empty (caminoRoutes camino)

-- | Get any transport links from this location
locationTransportLinks :: Camino -> Location -> [Leg]
locationTransportLinks camino location = filter (\l -> legFrom l == location) (caminoTransportLinks camino)

-- | Partial order for a list of caminos, sorting things into import order, with
--   a <= b if b imports (recursively)
caminoPartialOrder :: [Camino] -> Camino -> Camino -> Bool
caminoPartialOrder caminos camino1 camino2 = let
    imports = caminoPartialOrder' caminos S.empty [camino2]
  in
    S.member (caminoId camino1) imports

caminoPartialOrder' _caminos result [] = result
caminoPartialOrder' caminos result imports = let
    imports' = (S.fromList $ map caminoId imports) `S.difference` result
    imports'' = map (\cid -> fromJust $ L.find (\c -> caminoId c == cid) caminos) (S.toList imports')
  in
    caminoPartialOrder' caminos (result `S.union` imports') imports''

-- | A camino with all legs based on selected routes
caminoAllLegs :: Camino -> (Route -> Bool) -> [Leg]
caminoAllLegs camino select = L.concat $ (map routeLegs $ filter select $ caminoRoutes camino) ++ [caminoLegs camino]

-- | A camino including legs from selected routes
caminoWithRoutes :: Camino -> S.Set Route -> Camino
caminoWithRoutes camino routes = camino {
    caminoId = (caminoId camino) <> "'"
  , caminoName = appendText (caminoName camino) " all routes"
  , caminoLegs = caminoAllLegs camino (\r -> S.member r routes)
  }


-- | A configuration environment for caminos
data CaminoConfig = CaminoConfig {
    caminoConfigCaminos :: [Camino] -- ^ The list of known caminos
  , caminoConfigLookup :: Text -> Maybe Camino -- ^ Look up a camino by identifier
  , caminoConfigLocationLookup :: Text -> Maybe Location -- ^ Look up a location by identifier
  , caminoConfigAccommodationLookup :: Text -> Maybe (Accommodation, Location) -- ^ Look up a accommodation by identifier
  , caminoConfigCalendars :: CalendarConfig -- ^ Known calendar dates
  , caminoConfigRegions :: RegionConfig -- ^ Known regions
} deriving (Generic)

instance NFData CaminoConfig

instance Default CaminoConfig where
  def = CaminoConfig {
      caminoConfigCaminos = []
    , caminoConfigLookup = const Nothing
    , caminoConfigLocationLookup = const Nothing
    , caminoConfigAccommodationLookup = const Nothing
    , caminoConfigCalendars = createCalendarConfig []
    , caminoConfigRegions = createRegionConfig []
    }

-- | Has-class for ReaderT usage
class HasCaminoConfig a where
  getCaminoConfig :: a -> CaminoConfig

instance HasCaminoConfig CaminoConfig where
  getCaminoConfig = id

instance HasRegionConfig CaminoConfig where
  getRegionConfig = caminoConfigRegions

instance HasCalendarConfig CaminoConfig where
  getCalendarConfig = caminoConfigCalendars

-- | Get a camino from an environment.
--   This returns a maybe instance
getCamino :: (MonadReader env m, HasCaminoConfig env) => Text -> m (Maybe Camino)
getCamino key = do
  env <- ask
  return $ (caminoConfigLookup $ getCaminoConfig env) key

-- | Create a camino configuration
--   This takes raw, un-normalised caminos and returns a configuraton with completely normalised and
--   dereferenced results.
createCaminoConfig :: CalendarConfig -> RegionConfig -> [Camino] -> CaminoConfig
createCaminoConfig calendars regions caminos = let
    caminos' = topologicalSort (caminoPartialOrder caminos) caminos
    config' = CaminoConfig [] (const Nothing) (const Nothing) (const Nothing) calendars regions
  in
    createCaminoConfig' config' caminos'

createCaminoConfig' config [] = config
createCaminoConfig' config (camino:rest) = let
    camino' = normalise config camino
    caminos' = (caminoConfigCaminos config) ++ [camino']
    caminoMap' = M.fromList $ map (\c -> (placeholderID c, c)) caminos'
    locationMap' =  M.unions (map caminoLocationMap caminos')
    accommodationMap' =  M.unions (map caminoAccommodationMap caminos')
    config' = config {
        caminoConfigCaminos = caminos'
      , caminoConfigLookup = \k -> M.lookup k caminoMap'
      , caminoConfigLocationLookup = \k -> M.lookup k locationMap'
      , caminoConfigAccommodationLookup = \k -> M.lookup k accommodationMap'
    }
  in
    createCaminoConfig' config' rest


-- | The travel function to use
data Travel = Walking -- ^ Walking
   | Cycling -- ^ Cycling
  deriving (Generic, Eq, Read, Show, Ord, Enum, Bounded) 

instance FromJSON Travel
instance ToJSON Travel
instance NFData Travel

-- | Provide an enumeration of all travel types
travelEnumeration :: [Travel]
travelEnumeration = [minBound .. maxBound]

-- | An approximate level of fitness.
-- 
--   In the Tranter corrections, the levels of fitness are defined by
--   The number of minutes it takes to climb 1000ft (305m) over half a mile (805m)
--   Or a slope of approximately 1 in 2.5

data Fitness = SuperFit -- ^ 15 minutes for 1000ft over 1/2 a mile
  | VeryFit -- ^ 20 minutes for 1000ft over 1/2 a mile
  | Fit -- ^ 25 minutes for 1000ft over 1/2 a mile
  | Normal -- ^ 30 minutes for 1000ft over 1/2 a mile
  | Unfit -- ^ 40 minutes for 1000ft over 1/2 a mile 
  | Casual -- ^ Recerational walking
  | VeryUnfit -- ^ 50 minutes for 1000ft over 1/2 a mile
  deriving (Generic, Read, Show, Eq, Ord, Enum, Bounded)

instance FromJSON Fitness
instance ToJSON Fitness
instance NFData Fitness

-- | Provide an enumeration of all fitness levesls
fitnessEnumeration :: [Fitness]
fitnessEnumeration = [minBound .. maxBound]

-- | The general comfort level
data Comfort = Austere -- ^ Minimal services and accommodation
   | Frugal -- ^ Try to get by with minimal services
   | Pilgrim -- ^ General pilgrim services, use albergues where possible but be open to other options
   | Comfortable -- ^ Albergues are fine but
   | Luxurious -- ^ Major services and comfortable accommodation
  deriving (Generic, Eq, Read, Show, Ord, Enum, Bounded)

instance FromJSON Comfort
instance ToJSON Comfort
instance NFData Comfort

-- | Provide an enumeration of all comfort levesls
comfortEnumeration :: [Comfort]
comfortEnumeration = [minBound .. maxBound]

-- Read a camino description from a file, using an existing context
-- This is an unnormalised camino with placeholders. It will  be normalised when placed in a camino configuration
readCamino :: ByteString -> Camino
readCamino bytes = let
    decoded = eitherDecode bytes :: Either String Camino
  in
    case decoded of
      Left msg -> error msg
      Right camino' -> camino'

-- Dump a camino as a semi-readable string
caminoDump :: Camino -> String
caminoDump camino = "Camino { " ++ unpack (caminoId camino) ++ ": " ++ unpack (localiseDefault $ caminoName camino) ++
  ", defaultRoute = " ++ unpack (routeID $ caminoDefaultRoute camino) ++
  ", routes = " ++ show (map routeID $ caminoRoutes camino) ++
  ", locations = "++ show (map locationID $ caminoLocations camino)