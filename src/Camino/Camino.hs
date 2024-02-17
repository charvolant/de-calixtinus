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
  Accommodation(..),
  AccommodationType(..),
  Camino(..),
  Fitness(..),
  LatLong(..),
  Leg(..),
  LegType(..),
  Location(..),
  LocationType(..),
  Palette(..),
  Penance(..),
  Route(..),
  Service(..),
  Sleeping(..),

  module Graph.Programming,

  accommodationName,
  accommodationType,
  accommodationServices,
  accommodationSleeping,
  caminoBbox,
  caminoLegRoute,
  caminoLocationList,
  caminoRoute,
  defaultLegType,
  defaultPalette,
  defaultRoute,
  defaultSRS,
  locationAccommodationTypes,
  placeholderLocation,
) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Text (Text, unpack, pack)
import Data.Colour (Colour)
import Data.Colour.Names
import Data.Colour.SRGB (sRGB24read, sRGB24show)
import Data.Maybe (catMaybes)
import qualified Data.Map as M (Map, (!), fromList, elems)
import qualified Data.Set as S (Set, empty, map, fromList, member)
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
data AccommodationType = MunicipalAlbergue -- ^ A hostel run by local volunteers
  | PrivateAlbergue -- ^ A hostel run as a local business
  | GuestHouse -- ^ A generic guesthouse
  | House -- ^ An entire house or apartment for rent
  | Hotel -- ^ A dedicated hotel
  | Camping -- ^ Camping (with a tent or without, depending on what carried)
  deriving (Generic, Show, Eq, Ord)

instance FromJSON AccommodationType
instance ToJSON AccommodationType
instance FromJSONKey AccommodationType where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
instance ToJSONKey AccommodationType where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

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
  | Train -- ^ Train station
  | Bus -- ^ Bus station
  deriving (Show, Generic, Eq, Ord)

instance FromJSON Service
instance ToJSON Service
instance FromJSONKey Service where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions
instance ToJSONKey Service where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

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
  deriving (Show, Generic, Eq, Ord)

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
accommodationServices (GenericAccommodation GuestHouse) = S.fromList [ WiFi, Breakfast, Bedlinen, Towels ]
accommodationServices (GenericAccommodation House) = S.fromList [ WiFi, Bedlinen, Towels ]
accommodationServices (GenericAccommodation Hotel) = S.fromList [ WiFi, Breakfast, Dinner, Restaurant, Bedlinen, Towels ]
accommodationServices (GenericAccommodation _) = S.empty

accommodationSleeping :: Accommodation -> S.Set Sleeping
accommodationSleeping (Accommodation _name _type _services sleeping') = sleeping'
accommodationSleeping (GenericAccommodation MunicipalAlbergue) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation PrivateAlbergue) = S.fromList [ Shared ]
accommodationSleeping (GenericAccommodation GuestHouse) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation House) = S.fromList [ Single, Double ]
accommodationSleeping (GenericAccommodation Hotel) = S.fromList [ DoubleWC ]
accommodationSleeping (GenericAccommodation _) = S.fromList [ SleepingBag ]

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
   deriving (Show, Generic, Eq, Ord)
 
instance FromJSON LocationType
instance ToJSON LocationType
    
-- | A URL for referencing.
--   Currently, this is plain text.
type URL = Text

-- | A location, usually a city/town/village that marks the start and end points of a leg
--   and which may have accomodation and other services available.
--   Locations form the vertexes on the travel graph
data Location = Location {
  locationID :: String,
  locationName :: Text,
  locationDescription :: Maybe Text,
  locationHref :: Maybe URL,
  locationType :: LocationType,
  locationPosition :: Maybe LatLong,
  locationServices :: S.Set Service,
  locationAccommodation :: [Accommodation]
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
    toJSON (Location id' name' description' href' type' services' accommodation' position') =
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
    locationID = ident,
    locationName = pack ("Placeholder for " ++ ident),
    locationDescription = Nothing,
    locationHref = Nothing,
    locationType = Poi,
    locationPosition = Nothing,
    locationServices = S.empty,
    locationAccommodation = []
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
   deriving (Show, Generic, Eq, Ord)
 
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
  legType :: LegType, -- ^ The type of leg
  legFrom :: Location, -- ^ The start location
  legTo :: Location, -- ^ The end location
  legDistance :: Float, -- ^ The distance between the start and end in kilometres
  legTime :: Maybe Float, -- ^ An explicit time associated with the leg
  legAscent :: Float, -- ^ The total ascent on the leg in metres
  legDescent :: Float, -- ^ The total descent on the leg in metres
  legPenance :: Maybe Penance, -- ^ Any additional penance associated with the leg
  legNotes :: Maybe Text -- ^ Additional notes about the leg
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
  routeID :: String, -- ^ An identifier for the route
  routeName :: Text, -- ^ The route name
  routeDescription :: Text, -- ^ The route description
  routeLocations :: S.Set Location, -- ^ The locations along the route
  routeExclusions :: S.Set Location, -- ^ The locations on other routes that are eliminated by this route
  routePalette :: Palette,
  routeDefault :: Bool
} deriving (Show)

instance FromJSON Route where
    parseJSON (Object v) = do
      id' <- v .: "id"
      name' <- v .: "name"
      description' <- v .: "description"
      locations' <- v .:? "locations" .!= S.empty
      exclusions' <- v .:? "exclusions" .!= S.empty
      palette' <- v .: "palette"
      default' <- v .:? "default" .!= False
      return Route { routeID = id', routeName = name', routeDescription = description', routeLocations = locations', routeExclusions = exclusions', routePalette = palette', routeDefault = default' }
    parseJSON v = error ("Unable to parse route object " ++ show v)

instance ToJSON Route where
    toJSON (Route id' name' description' locations' exclusions' palette' default') =
      object [ "id" .= id', "name" .= name', "description" .= description', "locations" .= S.map locationID locations', "exclusions" .= S.map locationID exclusions', "palette" .= palette', "default" .= if default' then Just default' else Nothing ]

-- | Ensure that the route locations are mapped properly
normaliseRoute :: M.Map String Location -> Route -> Route
normaliseRoute locs route = route {
    routeLocations = S.map (\l -> locs M.! locationID l) (routeLocations route),
    routeExclusions = S.map (\l -> locs M.! locationID l) (routeExclusions route)
  }

-- | A way, consisting of a number of legs with a start and end
--   The purpose of the Camino Planner is to divide a camino into 
data Camino = Camino {
  caminoId :: String,
  caminoName :: Text,
  caminoDescription :: Text,
  caminoLocations :: M.Map String Location, -- ^ The camino locations
  caminoLegs :: [Leg], -- ^ The legs between locations
  caminoRoutes :: [Route] -- ^ Named sub-routes
} deriving (Show)

instance FromJSON Camino where
  parseJSON (Object v) = do
    id' <- v .: "id"
    name' <- v .: "name"
    description' <- v .: "description"
    locs <- v .: "locations"
    let locMap = M.fromList $ map (\w -> (locationID w, w)) locs
    legs' <- v .: "legs"
    let legs'' = map (normaliseLeg locMap) legs'
    routes' <- v .: "routes"
    let routes'' = map (normaliseRoute locMap) routes'
    return Camino { caminoId = id', caminoName = name', caminoDescription = description', caminoLocations = locMap, caminoLegs = legs'', caminoRoutes = routes'' }
  parseJSON v = error ("Unable to parse camino object " ++ show v)


instance ToJSON Camino where
  toJSON (Camino id' name' description' locations' legs' routes') =
    object [ "id" .= id', "name" .= name', "description" .= description', "locations" .= (M.elems locations'), "legs" .= legs', "routes" .= routes' ]

instance Graph Camino Leg Location where
  vertex camino vid = (caminoLocations camino) M.! vid
  edge camino loc1 loc2 = find (\l -> loc1 == legFrom l && loc2 == legTo l) (caminoLegs camino)
  incoming camino location = filter (\l -> location == legTo l) (caminoLegs camino)
  outgoing camino location = filter (\l -> location == legFrom l) (caminoLegs camino)

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
  

-- | The default route for a camino
defaultRoute :: Camino -> Route
defaultRoute camino = head $ filter routeDefault (caminoRoutes camino)

-- | Choose a route for a location
--   If the location isn't on a specific route, then the default route is returned
caminoRoute :: Camino -- ^ The camino being interrogated
  -> Location -- ^ The location to test
  -> Route -- ^ The route that the location is on
caminoRoute camino location = let
    route = find (\r -> S.member location (routeLocations r)) (caminoRoutes camino)
  in
    maybe (defaultRoute camino) id route
   
-- | Choose a route for a leg
--   If the leg isn't on a specific route, then the default route is returned
caminoLegRoute :: Camino -- ^ The camino being interrogated
  -> Leg -- ^ The location to test
  -> Route -- ^ The route that the location is on
caminoLegRoute camino leg = let
    from' = legFrom leg
    to' = legTo leg
    route = find (\r -> S.member from' (routeLocations r) || S.member to' (routeLocations r)) (caminoRoutes camino)
  in
    maybe (defaultRoute camino) id route
 
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
  deriving (Generic, Show, Eq, Enum)

instance FromJSON Fitness
instance ToJSON Fitness
