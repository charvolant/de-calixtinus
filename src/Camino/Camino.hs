{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , Comfort(..)
  , Event(..)
  , EventType(..)
  , Fitness(..)
  , LatLong(..)
  , Leg(..)
  , LegType(..)
  , Location(..)
  , LocationType(..)
  , Palette(..)
  , Penance(..)
  , PointOfInterest(..)
  , Route(..)
  , RouteLogic(..)
  , Service(..)
  , Sleeping(..)
  , Travel(..)

  , module Graph.Programming

  , accommodationName
  , accommodationNameLabel
  , accommodationType
  , accommodationTypeEnumeration
  , accommodationServices
  , accommodationSleeping
  , caminoBbox
  , caminoDump
  , caminoLegRoute
  , caminoLocationList
  , caminoNameLabel
  , caminoRoute
  , caminoRouteLocations
  , comfortEnumeration
  , completeRoutes
  , createAllowsClauses
  , createRequiresClauses
  , createProhibitsClauses
  , fitnessEnumeration
  , locationAccommodationTypes
  , locationBbox
  , locationEventTypes
  , locationNameLabel
  , locationPoiTypes
  , locationStopTypeEnumeration
  , locationTypeEnumeration
  , readCamino
  , serviceEnumeration
  , subtractFloor
  , townServiceEnumeration
  , travelEnumeration
) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as LB (readFile)
import Data.Colour (Colour)
import Data.Colour.SRGB (sRGB24read, sRGB24show)
import Data.Default.Class
import Data.Description (Description(..), wildcardDescription)
import Data.Event
import Data.Foldable (toList)
import Data.List (find)
import Data.Localised (Localised(..), TaggedText(..), appendText, localiseDefault, wildcardText)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Metadata
import qualified Data.Map as M (Map, (!), empty, filter, fromList, elems, keys, lookup)
import Data.Placeholder
import Data.Propositional
import qualified Data.Set as S (Set, difference, empty, intersection, map, null, fromList, member, union, unions, singleton)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Text (Text, unpack, pack)
import Graph.Graph
import Graph.Programming

-- | The measure of penance.
-- |
-- | Currently, based a simple float that can be thought of as equivalent to kilometres spent walking.
data Penance = Reject -- ^ Unsustainable penance
 | Penance Float -- ^ Simple penance
 deriving (Show)

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

-- | Reduce a penance by another penance, with 0 penance the minimum
--   @Reject - b = Reject@, @a - Reject = zero@, @a - b = max(zero, a - b)@
subtractFloor :: Penance -> Penance -> Penance
subtractFloor Reject _ = Reject
subtractFloor _ Reject = mempty
subtractFloor (Penance p1) (Penance p2) = if p2 > p1 then mempty else Penance (p1 - p2)

-- | Spatial reference system
data SRS = SRS Text
  deriving (Eq, Show)

srsID :: SRS -> Text
srsID (SRS sid) = sid

instance Default SRS where
  def = SRS "WGS84"

data LatLong = LatLong {
  latitude :: Double,
  longitude :: Double,
  srs :: SRS
} deriving (Show)

instance FromJSON LatLong where
  parseJSON (Object v) = do
    latitude' <- v .: "latitude"
    longitude' <- v .: "longitude"
    srs' <- v .:? "srs"
    return LatLong { latitude = latitude', longitude = longitude', srs = maybe def SRS srs' }
  parseJSON v = error ("Unable to parse lat/long object " ++ show v)

instance ToJSON LatLong where
  toJSON (LatLong latitude' longitude' srs') = object [ "latitude" .= latitude', "longitude" .= longitude', "srs" .= (if srs' == def then Nothing else Just (srsID srs')) ]

-- | Broad accommodation types
data AccommodationType = PilgrimAlbergue -- ^ A pilgrims hostel run by local volunteers
  | PrivateAlbergue -- ^ A hostel run as a local business, oriented towards pilgrims
  | Hostel -- ^ A generic hostel
  | GuestHouse -- ^ A generic guesthouse
  | HomeStay -- ^ Rural accommodation, Quinta, Air B&B room etc.
  | House -- ^ An entire house or apartment for rent
  | Hotel -- ^ A dedicated hotel
  | Gite -- ^ A gîtes d'etape, a stage lodging for walkers or cyclists
  | CampGround -- ^ A dedicated camping ground with services
  | Camping -- ^ Roadside camping (with a tent or without, depending on what carried)
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON AccommodationType
instance ToJSON AccommodationType
instance FromJSONKey AccommodationType where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
instance ToJSONKey AccommodationType where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

-- | Provide an enumeration of all accommodation types
accommodationTypeEnumeration :: [AccommodationType]
accommodationTypeEnumeration = [minBound .. maxBound]

-- | Services available in a location or accommodation
data Service = WiFi -- ^ Wireless internet available
  | Restaurant -- ^ Bar and/or Restaurant
  | Pharmacy -- ^ Pharmacy
  | Bank -- ^ Banking facilities, including an automated teller machine
  | BicycleRepair -- ^ Bicycle repair shop
  | Groceries -- ^ Convenience store or supermarket
  | Medical -- ^ Doctor, hospital or other healthcare
  | WashingMachine -- ^ Washing machine available
  | Dryer -- ^ Dryer or spin-dryer available
  | Handwash -- ^ Handwash laundry facilities
  | Kitchen -- ^ Kitchen facilities
  | Breakfast -- ^ Breakfast available
  | Dinner -- ^ Dinner available
  | Lockers -- ^ Lockers or cabinets
  | Accessible -- ^ Fitted for people with disabilities
  | Stables -- ^ Stabling for horses available
  | Pets -- ^ Pets allowed
  | BicycleStorage -- ^ Bicycle storage available
  | CampSite -- ^ Camping sites available
  | Bedlinen -- ^ Bedlinen provided
  | Towels -- ^ Towels provided
  | Pool -- ^ Swimming pool available
  | Heating -- ^ Heated building
  | Prayer -- ^ Community prayer/liturgy 
  | Train -- ^ Train station (or tram or subway)
  | Bus -- ^ Bus stop
  deriving (Show, Read, Generic, Eq, Ord, Enum, Bounded)

instance FromJSON Service
instance ToJSON Service
instance FromJSONKey Service where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
instance ToJSONKey Service where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions


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

-- | Somewhere to stay at the end of a leg
data Accommodation =
  Accommodation (Localised TaggedText) AccommodationType (S.Set Service) (S.Set Sleeping) -- ^ Fully described accommodation
  | GenericAccommodation AccommodationType -- ^ Generic accommodation with default services, sleeping arrangements based on type
  deriving (Show)
  
accommodationName :: Accommodation -> (Localised TaggedText)
accommodationName (Accommodation name' _type _services _sleeping) = name'
accommodationName (GenericAccommodation type') = wildcardText $ pack ("Generic " ++ show type')

-- | Get a simple text version of the accommodation name
accommodationNameLabel :: Accommodation -> Text
accommodationNameLabel accommodation = localiseDefault $ accommodationName accommodation

accommodationType :: Accommodation -> AccommodationType
accommodationType (Accommodation _name type' _services _sleeping) = type'
accommodationType (GenericAccommodation type') = type'

accommodationServices :: Accommodation -> S.Set Service
accommodationServices (Accommodation _name _type services' _sleeping) = services'
accommodationServices (GenericAccommodation PilgrimAlbergue) = S.fromList [ Handwash ]
accommodationServices (GenericAccommodation PrivateAlbergue) = S.fromList [ Handwash, WiFi, Bedlinen, Towels ]
accommodationServices (GenericAccommodation Hostel) = S.fromList [ WiFi, Kitchen, Bedlinen, Towels ]
accommodationServices (GenericAccommodation GuestHouse) = S.fromList [ WiFi, Heating, Bedlinen, Towels ]
accommodationServices (GenericAccommodation HomeStay) = S.fromList [ WiFi, Heating, Bedlinen, Towels ]
accommodationServices (GenericAccommodation House) = S.fromList [ WiFi, Bedlinen, Towels ]
accommodationServices (GenericAccommodation Hotel) = S.fromList [ WiFi, Breakfast, Dinner, Restaurant, Bedlinen, Towels ]
accommodationServices (GenericAccommodation Gite) = S.fromList [ Kitchen, BicycleStorage ]
accommodationServices (GenericAccommodation CampGround) = S.fromList [ Handwash ]
accommodationServices (GenericAccommodation Camping) = S.empty

accommodationSleeping :: Accommodation -> S.Set Sleeping
accommodationSleeping (Accommodation _name _type _services sleeping') = sleeping'
accommodationSleeping (GenericAccommodation PilgrimAlbergue) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation PrivateAlbergue) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation Hostel) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation GuestHouse) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation HomeStay) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation House) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation Hotel) = S.fromList [ DoubleWC ]
accommodationSleeping (GenericAccommodation Gite) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation CampGround) = S.fromList [ SleepingBag ]
accommodationSleeping (GenericAccommodation Camping) = S.fromList [ SleepingBag ]

instance FromJSON Accommodation where
   parseJSON t@(String _v) = do
     type' <- parseJSON t
     return $ GenericAccommodation type'
   parseJSON (Object v) = do
     name' <- v .: "name"
     type' <- v .: "type"
     services' <- v .: "services"
     sleeping' <- v .: "sleeping"
     return $ Accommodation name' type' services' sleeping'
   parseJSON v = error ("Unable to parse accommodation object " ++ show v)
instance ToJSON Accommodation where
    toJSON (Accommodation name' type' services' sleeping') =
      object [ "name" .= name', "type" .= type', "services" .= services', "sleeping" .= sleeping' ]
    toJSON (GenericAccommodation type' ) =
      toJSON type'

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

-- | An event that occurs at a location or point of interest
--   The location of the event is associated with the unclosing point
data Event = Event {
    eventName :: Localised TaggedText -- ^ The name of the event
  , eventDescription :: Maybe Description -- ^ Detailed description
  , eventType :: EventType -- ^ The event type
  , eventHours :: Maybe OpenHours -- ^ Dates/times when the event occurs
  } deriving (Show)

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

-- | The type of location or point of interest
data LocationType =
     Village -- ^ A village
   | Town -- ^ A town
   | City -- ^ A city
   | Monastery -- ^ A monastery/convent
   | Bridge -- ^ A bridge or underpass
   | Intersection -- ^ An intersection
   | Peak -- ^ A peak, mountain pass or lookout
   | Church -- ^ A church or chapel
   | Cathedral -- ^ A cathedral, basillica, shrine or similar large religious building
   | Cross -- ^ A crucifix or other religious monument
   | Fountain -- ^ A fountain or spring
   | Statue -- ^ A statue, artwork, etc
   | Municipal -- ^ An office,m aquare, market etc
   | PilgrimResource -- ^ An office, rest-spot etc. for pilgrims
   | Junction -- ^ A junction in pilgrim routes
   | Shop -- ^ A shop of some interest
   | Winery -- ^ A winery or port warehouse
   | Museum -- ^ A museum or gallery
   | Historical -- ^ A historical or archaeological site
   | Park -- ^ A park or gardens
   | Beach -- ^ A beach
   | Natural -- ^ A site of natural beauty
   | Hazard -- ^ A dangerous location (busy road crossing, etc)
   | Poi -- ^ A generic point of interest
   deriving (Show, Read, Generic, Eq, Ord, Enum, Bounded)
 
instance FromJSON LocationType
instance ToJSON LocationType
instance FromJSONKey LocationType where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
instance ToJSONKey LocationType where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

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

-- | A point of interest, attached to a parent location or leg
data PointOfInterest = PointOfInterest {
    poiName :: Localised TaggedText -- ^ The name of the point of interest
  , poiDescription :: Maybe Description -- ^ Detailed description
  , poiType :: LocationType -- ^ The point of interest type, same as a location type
  , poiPosition :: Maybe LatLong -- ^ Location, if it's that sort of thing
  , poiHours :: Maybe OpenHours -- ^ Dates and times when the point of interest is open
  , poiEvents :: [Event] -- ^ Associated events
} deriving (Show)

instance FromJSON PointOfInterest where
  parseJSON (Object v) = do
    name' <- v .: "name"
    description' <- v .:? "description" .!= Nothing
    type' <- v .:? "type" .!= Poi
    position' <- v .:? "position"
    hours' <- v .:? "hours"
    events' <- v .:? "events" .!= []
    return PointOfInterest {
        poiName = name'
      , poiDescription = description'
      , poiType = type'
      , poiPosition = position'
      , poiHours = hours'
      , poiEvents = events'
    }
  parseJSON v = typeMismatch "expecting object" v

instance ToJSON PointOfInterest where
    toJSON (PointOfInterest name' description' type' position' hours' events') =
      object [
          "name" .= name'
        , "description" .= description'
        , "type" .= type'
        , "position" .= position'
        , "hours" .= hours'
        , "events" .= if null events' then Nothing else Just events'
      ]

-- | A location, usually a city/town/village that marks the start and end points of a leg
--   and which may have accommodation and other services available.
--   Locations form the vertexes on the travel graph
data Location = Location {
    locationID :: Text
  , locationName :: Localised TaggedText
  , locationDescription :: Maybe Description
  , locationType :: LocationType
  , locationPosition :: Maybe LatLong
  , locationServices :: S.Set Service
  , locationAccommodation :: [Accommodation]
  , locationPois :: [PointOfInterest]
  , locationEvents :: [Event]
  , locationCamping :: Bool
} deriving (Show)

instance Placeholder Text Location where
  placeholderID = locationID
  placeholder lid = Location {
      locationID = lid
    , locationName = wildcardText $ ("Placeholder for " <> lid)
    , locationDescription = Nothing
    , locationType = Poi
    , locationPosition = Nothing
    , locationServices = S.empty
    , locationAccommodation = []
    , locationPois = []
    , locationEvents = []
    , locationCamping = False
  }

instance FromJSON Location where
  parseJSON (String v) = do
    return $ placeholder v
  parseJSON (Object v) = do
    id' <- v .: "id"
    name' <- v .: "name"
    description' <- v .:? "description" .!= Nothing
    type' <- v .:? "type" .!= Poi
    position' <- v .:? "position"
    services' <- v .: "services"
    accommodation' <- v .: "accommodation"
    pois' <- v .:? "pois" .!= []
    events' <- v .:? "events" .!= []
    camping' <- v .:? "camping" .!= locationCampingDefault type'
    return Location { 
        locationID = id'
      , locationName = name'
      , locationDescription = description'
      , locationType = type'
      , locationPosition = position'
      , locationServices = services'
      , locationAccommodation = accommodation'
      , locationPois = pois'
      , locationEvents = events'
      , locationCamping = camping'
    }
  parseJSON v = error ("Unable to parse location object " ++ show v)

instance ToJSON Location where
    toJSON (Location id' name' description' type' position' services' accommodation' pois' events' camping') =
      object [ 
          "id" .= id'
        , "name" .= name'
        , "description" .= description'
        , "type" .= type'
        , "position" .= position'
        , "services" .= services'
        , "accommodation" .= accommodation'
        , "pois" .= pois'
        , "events" .= events'
        , "camping" .= if camping' == locationCampingDefault type' then Nothing else Just camping'
      ]

instance Vertex Location where
  identifier v = unpack $ locationID v

instance Eq Location where
  a == b = locationID a == locationID b
  
instance Ord Location where
  a `compare` b = locationID a `compare` locationID b

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

-- | Get a simple text version of the location name
locationNameLabel :: Location -> Text
locationNameLabel location = localiseDefault $ locationName location


-- | Get a bounding box for set of locations
locationBbox :: (Foldable f) => f Location -- ^ The collection of locations
 -> (LatLong, LatLong) -- ^ The bouding box (top-left, bottom-right)
locationBbox locations = (LatLong (maximum lats) (minimum longs) def, LatLong (minimum lats) (maximum longs) def)
  where
    positions = catMaybes $ map locationPosition $ toList locations
    lats = map latitude positions
    longs = map longitude positions

-- | The type of transport available on a leg
data LegType = Road -- ^ Walk or cycle on a road or suitable path
   | Trail -- ^ Walking only path
   | CyclePath -- ^ Cycling only route
   | Ferry -- ^ Boat transfer
   | Boat -- ^ Self-powered boat or canoe
   deriving (Show, Generic, Eq, Ord, Enum, Bounded)
 
instance FromJSON LegType
instance ToJSON LegType

instance Default LegType where
  def = Road

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
} deriving (Show)

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
       
      return Leg { legType = type', legFrom = from', legTo = to', legDescription = description',  legDistance = distance', legTime = time',  legAscent = ascent', legDescent = descent', legPenance = penance' }
    parseJSON v = error ("Unable to parse leg object " ++ show v)

instance ToJSON Leg where
    toJSON (Leg type' from' to' description' distance' time' ascent' descent' penance') =
      object [ "type" .= (if type' == def then Nothing else Just type'), "from" .= locationID from', "to" .= locationID to', "description" .= description', "distance" .= distance', "time" .= time', "ascent" .= ascent', "descent" .= descent', "penance" .= penance' ]

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

-- | Ensure a leg has locations mapped correctly
normaliseLeg :: M.Map Text Location -> Leg -> Leg
normaliseLeg locs (Leg type' from to description distance time ascent descent penance) =
  Leg { legType = type', legFrom = locs M.! locationID from, legTo = locs M.! locationID to, legDescription = description, legDistance = distance, legTime = time, legAscent = ascent, legDescent = descent, legPenance = penance }

-- | A palette, graphical styles to use for displaying information
data Palette = Palette {
  paletteColour :: Colour Double -- ^ The basic colour of the element
} deriving (Show)
      
instance FromJSON Palette where
  parseJSON (Object v) = do
    colour' <- v .: "colour"
    return Palette { paletteColour = sRGB24read colour' }
  parseJSON v = error ("Unable to parse palette object " ++ show v)

instance ToJSON Palette where
  toJSON (Palette colour') =
    object [ "colour" .= sRGB24show colour' ]

instance Default Palette where
  def = Palette {
    paletteColour = sRGB24read "f9b34a" -- Camino yellow
  }

-- | A route, a sub-section of the camino with graphical information
data Route = Route {
    routeID :: Text -- ^ An identifier for the route
  , routeName :: Localised TaggedText -- ^ The route name
  , routeDescription :: Description -- ^ The route description
  , routeLocations :: S.Set Location -- ^ The locations along the route
  , routeStops :: S.Set Location -- ^ The suggested stops for the route
  , routeStarts :: [Location] -- ^ A list of suggested start points for the route, ordered by likelyhood
  , routeFinishes :: [Location] -- ^ A list of suggested finish points for the route
  , routePalette :: Palette
} deriving (Show)

instance FromJSON Route where
    parseJSON (String v) = do
      return $ placeholder v
    parseJSON (Object v) = do
      id' <- v .: "id"
      name' <- v .: "name"
      description' <- v .: "description"
      locations' <- v .:? "locations" .!= S.empty
      stops' <- v .:? "stops" .!= S.empty
      starts' <- v .:? "starts" .!= []
      finishes' <- v .:? "finishes" .!= []
      palette' <- v .: "palette"
      return Route { 
          routeID = id'
        , routeName = name'
        , routeDescription = description'
        , routeLocations = locations'
        , routeStops = stops'
        , routeStarts = starts'
        , routeFinishes = finishes'
        , routePalette = palette' 
      }
    parseJSON v = error ("Unable to parse route object " ++ show v)

instance ToJSON Route where
    toJSON (Route id' name' description' locations' stops' starts' finishes' palette') =
      object [ 
          "id" .= id'
        , "name" .= name'
        , "description" .= description'
        , "locations" .= S.map locationID locations'
        , "stops" .= S.map locationID stops'
        , "starts" .= map locationID starts'
        , "finishes" .= map locationID finishes'
        , "palette" .= palette' ]


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
    , routeLocations = S.empty
    , routeStops = S.empty
    , routeStarts = []
    , routeFinishes = []
    , routePalette = def
  }

instance Normaliser Text Route Camino where
  normalise camino route = route {
       routeLocations = dereferenceS camino (routeLocations route)
     , routeStops = dereferenceS camino  (routeStops route)
     , routeStarts = dereferenceF camino (routeStarts route)
     , routeFinishes = dereferenceF camino (routeFinishes route)
   }

instance Dereferencer Text Route Camino where
  dereference camino route = maybe route id $ find (\r -> placeholderID r == rid) (caminoRoutes camino) where rid = placeholderID route

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
  , routeLogicInclude :: S.Set Location -- ^ Stuff to add to the allowed locations
  , routeLogicExclude :: S.Set Location -- ^ Stuff to remove from the allowed locations
} deriving (Show)

-- | Read formulas a JSON
instance FromJSON (Formula Route) where
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
  parseJSON v = error ("Unable to parse route object " ++ show v)

-- | Produce formulas a JSON
instance ToJSON (Formula Route) where
  toJSON T = Bool True
  toJSON F = Bool False
  toJSON (Variable route) = String $ placeholderID route
  toJSON (And fs) = object [ "and" .= fs ]
  toJSON (Or fs) = object [ "or" .= fs ]
  toJSON (Not f) = object [ "not" .= f ]
  toJSON (Implies p c) = object [ "implies" .= [p, c] ]

instance FromJSON RouteLogic where
  parseJSON (Object v) = do
    description' <- v .:? "description"
    condition' <- v .: "condition"
    requires' <- v .:? "requires" .!= S.empty
    allows' <- v .:? "allows" .!= S.empty
    prohibits' <- v .:? "prohibits" .!= S.empty
    include' <- v .:? "include" .!= S.empty
    exclude' <- v .:? "exclude" .!= S.empty
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
      , "requires" .= nonEmpty requires'
      , "allows" .= nonEmpty allows'
      , "prohibits" .= nonEmpty prohibits'
      , "include" .= nonEmpty include'
      , "exclude" .= nonEmpty exclude'
    ]
    where
      nonEmpty v = if S.null v then Nothing else Just v

dereferenceFormula :: Camino -> Formula Route -> Formula Route
dereferenceFormula camino (Variable route) = Variable $ dereference camino route
dereferenceFormula camino (And fs) = And (map (dereferenceFormula camino) fs)
dereferenceFormula camino (Or fs) = Or (map (dereferenceFormula camino) fs)
dereferenceFormula camino (Not f) = Not $ dereferenceFormula camino f
dereferenceFormula camino (Implies p c) = Implies (dereferenceFormula camino p) (dereferenceFormula camino c)
dereferenceFormula _camino f = f

normaliseRouteLogic :: Camino -> RouteLogic -> RouteLogic
normaliseRouteLogic camino logic = logic {
      routeLogicCondition = dereferenceFormula camino (routeLogicCondition logic)
    , routeLogicRequires = S.map (dereference camino) (routeLogicRequires logic)
    , routeLogicAllows = S.map (dereference camino) (routeLogicAllows logic)
    , routeLogicProhibits = S.map (dereference camino) (routeLogicProhibits logic)
    , routeLogicInclude = S.map (dereference camino) (routeLogicInclude logic)
    , routeLogicExclude = S.map (dereference camino) (routeLogicExclude logic)
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
  , caminoLocations :: M.Map Text Location -- ^ The camino locations
  , caminoLegs :: [Leg] -- ^ The legs between locations
  , caminoRoutes :: [Route] -- ^ Named sub-routes
  , caminoRouteLogic :: [RouteLogic] -- ^ Additional logic for named sub-routes
  , caminoDefaultRoute :: Route -- ^ The default route to use
} deriving (Show)

instance FromJSON Camino where
  parseJSON (Object v) = do
    id' <- v .: "id"
    name' <- v .: "name"
    description' <- v .: "description"
    metadata' <- v .:? "metadata" .!= def
    locs <- v .: "locations"
    let locMap = M.fromList $ map (\w -> (locationID w, w)) locs
    legs' <- v .: "legs"
    let legs'' = map (normaliseLeg locMap) legs'
    routes' <- v .: "routes"
    routeLogic' <- v .: "route-logic"
    defaultRoute' <- v .:? "default-route" .!= (routeID $ head routes')
    let defaultRoute'' = fromJust $ find (\r -> routeID r == defaultRoute') routes'
    let otherRoutes = filter (\r -> routeID r /= defaultRoute') routes'
    let defaultRoute''' = defaultRoute'' { routeLocations = (defaultRouteLocations locs otherRoutes) `S.union` (routeLocations defaultRoute'') } -- Add unassigned locations to the default
    let routes'' = defaultRoute''' : otherRoutes
    let camino' = normalise [] $ Camino {
        caminoId = id'
      , caminoName = name'
      , caminoDescription = description'
      , caminoMetadata = metadata'
      , caminoLocations = locMap
      , caminoLegs = legs''
      , caminoRoutes = routes''
      , caminoRouteLogic = routeLogic'
      , caminoDefaultRoute = defaultRoute'''
    }
    return camino'
  parseJSON v = error ("Unable to parse camino object " ++ show v)


instance Eq Camino where
  a == b = caminoId a == caminoId b

instance Ord Camino where
  a `compare` b = caminoId a `compare` caminoId b

instance ToJSON Camino where
  toJSON (Camino id' name' description' metadata' locations' legs' routes' routeLogic' defaultRoute') =
    object [ 
        "id" .= id'
      , "name" .= name'
      , "description" .= description'
      , "metadata" .= metadata'
      , "locations" .= (M.elems locations')
      , "legs" .= legs'
      , "routes" .= routes'
      , "route-logic" .= routeLogic'
      , "default-route" .= routeID defaultRoute' 
    ]

instance Graph Camino Leg Location where
  vertex camino vid = (caminoLocations camino) M.! (pack vid)
  edge camino loc1 loc2 = find (\l -> loc1 == legFrom l && loc2 == legTo l) (caminoLegs camino)
  incoming camino location = filter (\l -> location == legTo l) (caminoLegs camino)
  outgoing camino location = filter (\l -> location == legFrom l) (caminoLegs camino)
  subgraph (Camino id' name' description' metadata' locations' legs' routes' routeLogic' defaultRoute') allowed  = 
    let
      id'' = id' <> "'"
      name'' = name' `appendText` " subgraph"
      locations'' = M.filter (\l -> S.member l allowed) locations'
      legs'' = filter (\l -> S.member (legFrom l) allowed && S.member (legTo l) allowed) legs'
      routes'' = map (\r -> r { routeLocations = routeLocations r `S.intersection` allowed, routeStops = routeStops r `S.intersection` allowed }) routes'
      routeLogic'' = map (\l -> l { routeLogicInclude = routeLogicInclude l `S.intersection` allowed, routeLogicExclude = routeLogicExclude l `S.intersection` allowed}) routeLogic'
      defaultRoute'' = fromJust $ find (\r -> routeID r == routeID defaultRoute') routes'
    in
      Camino { caminoId = id'', caminoName = name'', caminoDescription = description', caminoMetadata = metadata', caminoLocations = locations'', caminoLegs = legs'', caminoRoutes = routes'', caminoRouteLogic = routeLogic'', caminoDefaultRoute = defaultRoute'' }
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
      , caminoLocations = M.empty
      , caminoLegs = []
      , caminoRoutes = [dr]
      , caminoRouteLogic = []
      , caminoDefaultRoute = dr
    }
    where
      dr = placeholder ("DR-" <> cid)

instance Normaliser Text Camino [Camino] where
  normalise caminos camino = let
      found = find (\c -> caminoId c == caminoId camino) caminos
    in
      if isJust found then
        fromJust found
    else let
      camino1 = camino { caminoLegs = map (normaliseLeg (caminoLocations camino)) (caminoLegs camino) }
      camino2 = camino1 { caminoRoutes = map (normalise camino1) (caminoRoutes camino1) }
      camino3 = camino2 { caminoDefaultRoute = normalise camino2 (caminoDefaultRoute camino2) }
      camino4 = camino3 { caminoRouteLogic = map (normaliseRouteLogic camino3) (caminoRouteLogic camino3)  }
    in
      camino4

instance Dereferencer Text Location Camino where
  dereference camino location = maybe location id (M.lookup (placeholderID location) (caminoLocations camino))

-- | Get a simple text version of the camino name
caminoNameLabel :: Camino -> Text
caminoNameLabel camino = localiseDefault $ caminoName camino

-- | Get a list of locations for the camino
caminoLocationList :: Camino -- ^ The camino
  -> [Location] -- ^ The list of locations
caminoLocationList camino = M.elems $ caminoLocations camino

-- | Get a bounding box for the camino
caminoBbox :: Camino -- ^ The entire camino
 -> (LatLong, LatLong) -- ^ The bouding box (top-left, bottom-right)
caminoBbox camino = locationBbox $ M.elems $ caminoLocations camino  

-- | All the locations on the camino assumed to be part of the default route
defaultRouteLocations :: [Location] -- ^ The possible locations
  -> [Route]
  -> S.Set Location -- ^ The locations on the camino that have not been explicitly assigned to a route
defaultRouteLocations locations routes =
  let
    allLocs = S.fromList locations
    routeLocs = S.unions $ map routeLocations routes
  in
    allLocs `S.difference` routeLocs

-- | Choose a route for a location
--   If the location isn't on a specific route, then the default route is returned
--   The route list is scanned backwards, effectively running from most-specific to least specific
caminoRoute :: Camino -- ^ The camino being interrogated
  -> S.Set Route -- ^ The routes in use
  -> Location -- ^ The location to test
  -> Route -- ^ The route that the location is on
caminoRoute camino routes location = let
    route = find (\r -> S.member r routes && S.member location (routeLocations r)) (reverse $ caminoRoutes camino)
    route' = maybe (caminoDefaultRoute camino) id (find (\r -> S.member location (routeLocations r)) (reverse $ caminoRoutes camino))
  in
    maybe route' id route

-- | Choose a route for a leg
--   If the leg isn't on a specific route, then the default route is returned
caminoLegRoute :: Camino -- ^ The camino being interrogated
  -> Leg -- ^ The location to test
  -> Route -- ^ The route that the location is on
caminoLegRoute camino leg = let
    dflt = caminoDefaultRoute camino
    from' = legFrom leg
    to' = legTo leg
    route = find (\r -> r /= dflt && (S.member from' (routeLocations r) || S.member to' (routeLocations r))) (caminoRoutes camino)
  in
    maybe dflt id route

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
    baseLocations = S.unions $ S.map routeLocations routes
    logics =  filter (\l -> evaluate membership (routeLogicCondition l) == T) (caminoRouteLogic camino)
  in
    foldl (\allowed -> \logic -> (allowed `S.union` routeLogicInclude logic) `S.difference` routeLogicExclude logic) baseLocations logics

-- | The travel function to use
data Travel = Walking -- ^ Walking
   | Cycling -- ^ Cycling
  deriving (Generic, Eq, Read, Show, Ord, Enum, Bounded) 

instance FromJSON Travel
instance ToJSON Travel

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
  | VeryUnfit -- ^ 50 minutes for 1000ft over 1/2 a mile
  deriving (Generic, Read, Show, Eq, Ord, Enum, Bounded)

instance FromJSON Fitness
instance ToJSON Fitness

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

-- | Provide an enumeration of all comfort levesls
comfortEnumeration :: [Comfort]
comfortEnumeration = [minBound .. maxBound]

-- Read a camino description from a file
readCamino :: FilePath -> IO Camino
readCamino file = do
  cf <- LB.readFile file
  let decoded = eitherDecode cf :: Either String Camino
  return $ case decoded of
    Left msg -> error msg
    Right camino' -> camino'

-- Dump a camino as a semi-readable string
caminoDump :: Camino -> String
caminoDump camino = "Camino { " ++ unpack (caminoId camino) ++ ": " ++ unpack (localiseDefault $ caminoName camino) ++
  ", defaultRoute = " ++ unpack (routeID $ caminoDefaultRoute camino) ++
  ", routes = " ++ show (map routeID $ caminoRoutes camino) ++
  ", locations = "++ show (M.keys $ caminoLocations camino)