{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , Fitness(..)
  , LatLong(..)
  , Leg(..)
  , LegType(..)
  , Location(..)
  , LocationType(..)
  , Palette(..)
  , Penance(..)
  , Route(..)
  , Service(..)
  , Sleeping(..)
  , Travel(..)

  , module Graph.Programming

  , accommodationName
  , accommodationType
  , accommodationTypeEnumeration
  , accommodationServices
  , accommodationSleeping
  , caminoBbox
  , caminoLegRoute
  , caminoLocationList
  , caminoRoute
  , caminoRouteLocations
  , defaultLegType
  , defaultPalette
  , defaultSRS
  , fitnessEnumeration
  , locationAccommodationTypes
  , locationTypeEnumeration
  , normaliseCamino
  , normaliseRoutes
  , placeholderCamino
  , placeholderLocation
  , placeholderRoute
  , readCamino
  , serviceEnumeration
  , townServiceEnumeration
  , travelEnumeration
) where

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.ByteString.Lazy as LB (readFile)
import Data.Text (Text, append, unpack, pack)
import Data.Colour (Colour)
import Data.Colour.Names
import Data.Colour.SRGB (sRGB24read, sRGB24show)
import Data.Maybe (catMaybes, fromJust)
import Data.Metadata
import qualified Data.Map as M (Map, (!), empty, filter, fromList, elems)
import qualified Data.Set as S (Set, difference, empty, intersection, map, fromList, member, union, unions)
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.List (find)
import Graph.Graph
import Graph.Programming

-- | The measure of penance.
-- |
-- | Currently, based a simple float that can be thought of as equivalent to hours spent walking.
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

-- | Spatial reference system
type SRS = String

-- | The default SRS
defaultSRS :: SRS
defaultSRS = "WGS84" :: SRS

data LatLong = LatLong {
  latitude :: Double,
  longitude :: Double,
  srs :: SRS
} deriving (Show)

instance FromJSON LatLong where
  parseJSON (Object v) = do
    latitude' <- v .: "latitude"
    longitude' <- v .: "longitude"
    srs' <- v .:? "srs" .!= defaultSRS
    return LatLong { latitude = latitude', longitude = longitude', srs = srs' }
  parseJSON v = error ("Unable to parse lat/long object " ++ show v)

instance ToJSON LatLong where
  toJSON (LatLong latitude' longitude' srs') = object [ "latitude" .= latitude', "longitude" .= longitude', "srs" .= (if srs' == defaultSRS then Nothing else Just srs') ]

-- | Broad accommodation types
data AccommodationType = MunicipalAlbergue -- ^ A pilgrims hostel run by local volunteers
  | PrivateAlbergue -- ^ A hostel run as a local business, oriented towards pilgrims
  | Hostel -- ^ A generic hostel
  | GuestHouse -- ^ A generic guesthouse
  | HomeStay -- ^ Rural accomodation, Quinta, Air B&B room etc.
  | House -- ^ An entire house or apartment for rent
  | Hotel -- ^ A dedicated hotel
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
  Accommodation Text AccommodationType (S.Set Service) (S.Set Sleeping) -- ^ Fully described accommodation
  | GenericAccommodation AccommodationType -- ^ Generic accommodation with default services, sleeping arrangements based on type
  deriving (Show)
  
accommodationName :: Accommodation -> Text
accommodationName (Accommodation name' _type _services _sleeping) = name'
accommodationName (GenericAccommodation type') = pack ("Generic " ++ show type')

accommodationType :: Accommodation -> AccommodationType
accommodationType (Accommodation _name type' _services _sleeping) = type'
accommodationType (GenericAccommodation type') = type'

accommodationServices :: Accommodation -> S.Set Service
accommodationServices (Accommodation _name _type services' _sleeping) = services'
accommodationServices (GenericAccommodation MunicipalAlbergue) = S.fromList [ Handwash ]
accommodationServices (GenericAccommodation PrivateAlbergue) = S.fromList [ Handwash, WiFi, Bedlinen, Towels ]
accommodationServices (GenericAccommodation Hostel) = S.fromList [ WiFi, Kitchen, Bedlinen, Towels ]
accommodationServices (GenericAccommodation GuestHouse) = S.fromList [ WiFi, Heating, Bedlinen, Towels ]
accommodationServices (GenericAccommodation HomeStay) = S.fromList [ WiFi, Heating, Bedlinen, Towels ]
accommodationServices (GenericAccommodation House) = S.fromList [ WiFi, Bedlinen, Towels ]
accommodationServices (GenericAccommodation Hotel) = S.fromList [ WiFi, Breakfast, Dinner, Restaurant, Bedlinen, Towels ]
accommodationServices (GenericAccommodation CampGround) = S.fromList [ Handwash ]
accommodationServices (GenericAccommodation Camping) = S.empty

accommodationSleeping :: Accommodation -> S.Set Sleeping
accommodationSleeping (Accommodation _name _type _services sleeping') = sleeping'
accommodationSleeping (GenericAccommodation MunicipalAlbergue) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation PrivateAlbergue) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation Hostel) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation GuestHouse) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation HomeStay) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation House) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation Hotel) = S.fromList [ DoubleWC ]
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
   parseJSON v = error ("Unable to parse accomodation object " ++ show v)
instance ToJSON Accommodation where
    toJSON (Accommodation name' type' services' sleeping') =
      object [ "name" .= name', "type" .= type', "services" .= services', "sleeping" .= sleeping' ]
    toJSON (GenericAccommodation type' ) =
      toJSON type'
 
-- | The type of location 
data LocationType = Village -- ^ A village
   | Town -- ^ A town
   | City -- ^ A city
   | Monastery -- ^ A monastery/convent
   | Bridge -- ^ A bridge
   | Intersection -- ^ An intersection
   | Peak -- ^ A peak or lookout
   | Poi -- ^ A generic point of interest
   deriving (Show, Generic, Eq, Ord, Enum, Bounded)
 
instance FromJSON LocationType
instance ToJSON LocationType

-- | Provide an enumeration of all location types
locationTypeEnumeration :: [LocationType]
locationTypeEnumeration = [minBound .. maxBound]

-- | A URL for referencing.
--   Currently, this is plain text.
type URL = Text

-- | A location, usually a city/town/village that marks the start and end points of a leg
--   and which may have accomodation and other services available.
--   Locations form the vertexes on the travel graph
data Location = Location {
    locationID :: String
  , locationName :: Text
  , locationDescription :: Maybe Text
  , locationHref :: Maybe URL
  , locationType :: LocationType
  , locationPosition :: Maybe LatLong
  , locationServices :: S.Set Service
  , locationAccommodation :: [Accommodation]
} deriving (Show)

instance FromJSON Location where
  parseJSON (String v) = do
    return $ placeholderLocation (unpack v)
  parseJSON (Object v) = do
    id' <- v .: "id"
    name' <- v .: "name"
    description' <- v .:? "description" .!= Nothing
    href' <- v .:? "href" .!= Nothing
    type' <- v .:? "type" .!= Poi
    position' <- v .:? "position"
    services' <- v .: "services"
    accommodation' <- v .: "accommodation"
    return Location { locationID = id', locationName = name', locationDescription = description', locationHref = href', locationType = type', locationPosition = position', locationServices = services', locationAccommodation = accommodation' }
  parseJSON v = error ("Unable to parse location object " ++ show v)

instance ToJSON Location where
    toJSON (Location id' name' description' href' type' position' services' accommodation') =
      object [ "id" .= id', "name" .= name', "description" .= description', "href" .= href', "type" .= type', "position" .= position', "services" .= services', "accommodation" .= accommodation' ]

instance Vertex Location where
  identifier = locationID

instance Eq Location where
  a == b = locationID a == locationID b
  
instance Ord Location where
  a `compare` b = locationID a `compare` locationID b

-- | Create a placeholder for a location
placeholderLocation :: String -> Location
placeholderLocation ident = Location {
      locationID = ident
    , locationName = pack ("Placeholder for " ++ ident)
    , locationDescription = Nothing
    , locationHref = Nothing
    , locationType = Poi
    , locationPosition = Nothing
    , locationServices = S.empty
    , locationAccommodation = []
  }

-- | Get the accommodation types available at a location
--   These are ordered into enumeration order
locationAccommodationTypes :: Location -> S.Set AccommodationType
locationAccommodationTypes location = S.fromList $ map accommodationType (locationAccommodation location)

 
-- | The type of transport available on a leg
data LegType = Road -- ^ Walk or cycle on a road or suitable path
   | Trail -- ^ Walking only path
   | CyclePath -- ^ Cycling only route
   | Ferry -- ^ Boat transfer
   | Boat -- ^ Self-powered boat or canoe
   deriving (Show, Generic, Eq, Ord, Enum, Bounded)
 
instance FromJSON LegType
instance ToJSON LegType

-- | The assumed leg type
defaultLegType :: LegType
defaultLegType = Road

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
  , legDistance :: Float -- ^ The distance between the start and end in kilometres
  , legTime :: Maybe Float -- ^ An explicit time associated with the leg
  , legAscent :: Float -- ^ The total ascent on the leg in metres
  , legDescent :: Float -- ^ The total descent on the leg in metres
  , legPenance :: Maybe Penance -- ^ Any additional penance associated with the leg
  , legNotes :: Maybe Text -- ^ Additional notes about the leg
} deriving (Show)

instance FromJSON Leg where
    parseJSON (Object v) = do
      type' <- v .:? "type" .!= defaultLegType
      from' <- v .: "from"
      to' <- v .: "to"
      distance' <- v .: "distance"
      time' <- v .:? "time" .!= Nothing
      ascent' <- v .: "ascent"
      descent' <- v .: "descent"
      penance' <- v .:? "penance" .!= Nothing
      notes' <- v .:? "notes" .!= Nothing
      
      return Leg { legType = type', legFrom = from', legTo = to', legDistance = distance', legTime = time',  legAscent = ascent', legDescent = descent', legPenance = penance', legNotes = notes' }
    parseJSON v = error ("Unable to parse leg object " ++ show v)

instance ToJSON Leg where
    toJSON (Leg type' from' to' distance' time' ascent' descent' penance' notes') =
      object [ "type" .= (if type' == defaultLegType then Nothing else Just type'), "from" .= locationID from', "to" .= locationID to', "distance" .= distance', "time" .= time', "ascent" .= ascent', "descent" .= descent', "penance" .= penance', "notes" .= notes' ]

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
normaliseLeg :: M.Map String Location -> Leg -> Leg
normaliseLeg locs (Leg type' from to distance time ascent descent penance notes) =
  Leg { legType = type', legFrom = locs M.! locationID from, legTo = locs M.! locationID to, legDistance = distance, legTime = time, legAscent = ascent, legDescent = descent, legPenance = penance, legNotes = notes }

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

-- | A default palette for generic display
defaultPalette :: Palette
defaultPalette = Palette {
  paletteColour = yellow
}

-- | A route, a sub-section of the camino with graphical information
data Route = Route {
    routeID :: String -- ^ An identifier for the route
  , routeName :: Text -- ^ The route name
  , routeDescription :: Text -- ^ The route description
  , routeRequires :: S.Set Route -- ^ Other routes required by this route
  , routeExclusive :: S.Set Route -- ^ Other routes mutually exclusive topt his route
  , routeLocations :: S.Set Location -- ^ The locations along the route
  , routeInclusions :: S.Set Location -- ^ The locations on other routes that should eb put back in if they have been excluded by another route
  , routeExclusions :: S.Set Location -- ^ The locations on other routes that are eliminated by this route
  , routeStops :: S.Set Location -- ^ The suggested stops for the route
  , routeStarts :: [Location] -- ^ A list of suggested start points for the route, ordered by likelyhood
  , routeFinishes :: [Location] -- ^ A list of suggested finish points for the route
  , routePalette :: Palette
} deriving (Show)

instance FromJSON Route where
    parseJSON (String v) = do
      return $ placeholderRoute (unpack v)
    parseJSON (Object v) = do
      id' <- v .: "id"
      name' <- v .: "name"
      description' <- v .: "description"
      requires' <- v .:? "requires" .!= S.empty
      exclusive' <- v .:? "exclusive" .!= S.empty
      locations' <- v .:? "locations" .!= S.empty
      inclusions' <- v .:? "inclusions" .!= S.empty
      exclusions' <- v .:? "exclusions" .!= S.empty
      stops' <- v .:? "stops" .!= S.empty
      starts' <- v .:? "starts" .!= []
      finishes' <- v .:? "finishes" .!= []
      palette' <- v .: "palette"
      return Route { 
          routeID = id'
        , routeName = name'
        , routeDescription = description'
        , routeRequires = requires'
        , routeExclusive = exclusive'
        , routeLocations = locations'
        , routeInclusions = inclusions'
        , routeExclusions = exclusions'
        , routeStops = stops'
        , routeStarts = starts'
        , routeFinishes = finishes'
        , routePalette = palette' 
      }
    parseJSON v = error ("Unable to parse route object " ++ show v)

instance ToJSON Route where
    toJSON (Route id' name' description' requires' exclusive' locations' inclusions' exclusions' stops' starts' finishes' palette') =
      object [ 
          "id" .= id'
        , "name" .= name'
        , "description" .= description'
        , "required" .= S.map routeID requires'
        , "exclusive" .= S.map routeID exclusive'
        , "locations" .= S.map locationID locations'
        , "inclusions" .=  S.map locationID inclusions'
        , "exclusions" .= S.map locationID exclusions'
        , "stops" .= S.map locationID stops'
        , "starts" .= map locationID starts'
        , "finishes" .= map locationID finishes'
        , "palette" .= palette' ]


instance Eq Route where
  a == b = routeID a == routeID b

instance Ord Route where
  a `compare` b = routeID a `compare` routeID b

-- | Ensure that the route locations are mapped properly
normaliseRoute :: M.Map String Location -> M.Map String Route -> Route -> Route
normaliseRoute locs routes route = route {
      routeRequires = remapr (routeRequires route)
    , routeExclusive = remapr (routeExclusive route)
    , routeLocations = remap (routeLocations route)
    , routeInclusions = remap (routeInclusions route)
    , routeExclusions = remap (routeExclusions route)
    , routeStops = remap (routeStops route)
    , routeStarts = remapl (routeStarts route)
    , routeFinishes = remapl (routeFinishes route)
  } 
  where
    remap = S.map (\l -> locs M.! locationID l)
    remapl = map (\l -> locs M.! locationID l)
    remapr = S.map (\r -> routes M.! routeID r)

-- | Ensure that a set of routes are what is in a camino.
--   Used to remove placeholders
normaliseRoutes :: Camino -> S.Set Route -> S.Set Route
normaliseRoutes camino routes = S.map (\r -> maybe (error ("Can't find route " ++ show r)) id $ find (\r' -> r' == r) (caminoRoutes camino)) routes

-- | Create a placeholder for a route
placeholderRoute :: String -> Route
placeholderRoute ident = Route {
      routeID = ident
    , routeName = pack ("Placeholder for " ++ ident)
    , routeDescription = ""
    , routeRequires = S.empty
    , routeExclusive = S.empty
    , routeLocations = S.empty
    , routeInclusions = S.empty
    , routeExclusions = S.empty
    , routeStops = S.empty
    , routeStarts = []
    , routeFinishes = []
    , routePalette = defaultPalette
  }

-- | A way, consisting of a number of legs with a start and end
--   The purpose of the Camino Planner is to divide a camino into 
data Camino = Camino {
    caminoId :: String
  , caminoName :: Text
  , caminoDescription :: Text
  , caminoMetadata :: Metadata
  , caminoLocations :: M.Map String Location -- ^ The camino locations
  , caminoLegs :: [Leg] -- ^ The legs between locations
  , caminoRoutes :: [Route] -- ^ Named sub-routes
  , caminoDefaultRoute :: Route -- ^ The default route to use
} deriving (Show)

instance FromJSON Camino where
  parseJSON (Object v) = do
    id' <- v .: "id"
    name' <- v .: "name"
    description' <- v .: "description"
    metadata' <- v .:? "metadata" .!= defaultMetadata
    locs <- v .: "locations"
    let locMap = M.fromList $ map (\w -> (locationID w, w)) locs
    legs' <- v .: "legs"
    let legs'' = map (normaliseLeg locMap) legs'
    routes' <- v .: "routes"
    let routeMap = M.fromList $ map (\r -> (routeID r, r)) routes'
    let routes'' = map (normaliseRoute locMap routeMap) routes'
    defaultRoute' <- v .:? "defaultRoute" .!= (routeID $ head routes'')
    let defaultRoute'' = fromJust $ find (\r -> routeID r == defaultRoute') routes''
    let otherRoutes = filter (\r -> routeID r /= defaultRoute') routes''
    let defaultRoute''' = defaultRoute'' { routeLocations = (defaultRouteLocations locs otherRoutes) `S.union` (routeLocations defaultRoute'') } -- Add unassigned locations to the default
    let routes''' = defaultRoute''' : otherRoutes
    return Camino { caminoId = id', caminoName = name', caminoDescription = description', caminoMetadata = metadata', caminoLocations = locMap, caminoLegs = legs'', caminoRoutes = routes''', caminoDefaultRoute = defaultRoute''' }
  parseJSON v = error ("Unable to parse camino object " ++ show v)


instance Eq Camino where
  a == b = caminoId a == caminoId b

instance Ord Camino where
  a `compare` b = caminoId a `compare` caminoId b

instance ToJSON Camino where
  toJSON (Camino id' name' description' metadata' locations' legs' routes' defaultRoute') =
    object [ "id" .= id', "name" .= name', "description" .= description', "metadata" .= metadata', "locations" .= (M.elems locations'), "legs" .= legs', "routes" .= routes', "defaultRoute" .= routeID defaultRoute' ]

instance Graph Camino Leg Location where
  vertex camino vid = (caminoLocations camino) M.! vid
  edge camino loc1 loc2 = find (\l -> loc1 == legFrom l && loc2 == legTo l) (caminoLegs camino)
  incoming camino location = filter (\l -> location == legTo l) (caminoLegs camino)
  outgoing camino location = filter (\l -> location == legFrom l) (caminoLegs camino)
  subgraph (Camino id' name' description' metadata' locations' legs' routes' defaultRoute') allowed  = 
    let
      id'' = id' ++ "'"
      name'' = name' `append` " subgraph"
      locations'' = M.filter (\l -> S.member l allowed) locations'
      legs'' = filter (\l -> S.member (legFrom l) allowed && S.member (legTo l) allowed) legs'
      routes'' = map (\r -> r { routeLocations = routeLocations r `S.intersection` allowed, routeInclusions = routeInclusions r `S.intersection` allowed, routeExclusions = routeExclusions r `S.intersection` allowed, routeStops = routeStops r `S.intersection` allowed }) routes'
      defaultRoute'' = fromJust $ find (\r -> routeID r == routeID defaultRoute') routes'
    in
      Camino { caminoId = id'', caminoName = name'', caminoDescription = description', caminoMetadata = metadata', caminoLocations = locations'', caminoLegs = legs'', caminoRoutes = routes'', caminoDefaultRoute = defaultRoute'' }

-- | Create a placeholder for a camino
placeholderCamino :: String -> Camino
placeholderCamino ident = Camino {
      caminoId = ident
    , caminoName = pack ("Placeholder for " ++ ident)
    , caminoDescription = ""
    , caminoMetadata = Metadata [] []
    , caminoLocations = M.empty
    , caminoLegs = []
    , caminoRoutes = [dr]
    , caminoDefaultRoute = dr
  }
  where
    dr = placeholderRoute ("DR-" ++ ident)

-- Get a definitive normalised camino from a list of caminos
normaliseCamino :: [Camino] -- ^ The list of possible caminos
  -> Camino -- ^ The camino to normalise, possible a placeholde from @placeholderCamino@
  -> Camino -- ^ The normalised camino
normaliseCamino caminos camino = maybe camino id (find (\c -> caminoId camino == caminoId c) caminos)

-- | Get a list of locations for the camino
caminoLocationList :: Camino -- ^ The camino
  -> [Location] -- ^ The list of locations
caminoLocationList camino = M.elems $ caminoLocations camino

-- | Get a bounding box for the camino
caminoBbox :: Camino -- ^ The entire camino
 -> (LatLong, LatLong) -- ^ The bouding box (top-left, bottom-right)
caminoBbox camino = (LatLong (maximum lats) (minimum longs) defaultSRS, LatLong (minimum lats) (maximum longs) defaultSRS)
  where
    positions = catMaybes $ map locationPosition $ M.elems $ caminoLocations camino
    lats = map latitude positions
    longs = map longitude positions
  

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

-- | Work out what locations are acceptable in a camino, based on the chosen routes.
--   The default route is always included, followed by the routes specified in the preferences.
--   The routes are worked through in order (with the default route always first).
--   That way, locations can be included by one route and then excluded by a ltere route
caminoRouteLocations :: Camino -- ^ The base camino definition
  -> S.Set Route -- ^ The routes that are being used
  -> S.Set Location -- ^ The allowed locations
caminoRouteLocations camino used =
  let
    routes = filter (\r -> S.member r used || r == caminoDefaultRoute camino) (caminoRoutes camino)
  in
    foldl (\allowed -> \route -> (allowed `S.union` routeLocations route `S.union` routeInclusions route) `S.difference` routeExclusions route) S.empty routes

-- | The travel function to use
data Travel = Walking -- ^ Walking using the Tobler estimate of time
   | Walking_Naismith -- ^ Walking using the Naismith estimate of time
   | Cycling -- ^ Cycling using TBD
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

-- Read a camino description from a file
readCamino :: FilePath -> IO Camino
readCamino file = do
  cf <- LB.readFile file
  let decoded = eitherDecode cf :: Either String Camino
  return $ case decoded of
    Left msg -> error msg
    Right camino' -> camino'
