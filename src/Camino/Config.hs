{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Config
Description : Configuration data for generating camino display
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

The configuration can be read from a YAML file and overlay a default configuration.
-}

module Camino.Config (
    AssetConfig(..)
  , AssetType(..)
  , Config(..)
  , CrossOriginType(..)
  , LinkConfig(..)
  , LinkType(..)
  , MapConfig(..)
  , WebConfig(..)

  , defaultConfig
  , getAsset
  , getAssets
  , getCalendarName
  , getLink
  , getLinks
  , getMap
  , readAsset
  , readConfigFile
) where

import GHC.Generics (Generic)
import Control.Monad.Reader (runReader)
import Data.Aeson
import Data.Event (CalendarConfig(..), HasCalendarConfig(..), createCalendarConfig, getNamedCalendarName)
import qualified Data.Map as M
import Data.Text (Text, breakOn, drop, unpack)
import Data.List (find)
import Data.Localised
import Data.Region (HasRegionConfig(..), RegionConfig(..), createRegionConfig)
import Data.Yaml (ParseException, decodeEither')
import qualified Data.ByteString as B (readFile)
import Data.Aeson.Types (unexpected)
import qualified Data.ByteString.Lazy as LB (ByteString, readFile)
import Network.HTTP.Simple

-- | Configuration for a map provider
data MapConfig = Map {
  mapId :: Text, -- ^ The map source identifier
  mapTiles :: Text -- ^ The template for map tiles
} deriving (Show)

instance FromJSON MapConfig where
  parseJSON (Object v) = do
    id' <- v .: "id"
    tiles' <- v .: "tiles"
    return $ Map id' tiles'
  parseJSON v = unexpected v
    
instance ToJSON MapConfig where
  toJSON (Map id' tiles') =
    object [ "id" .= id', "tiles" .= tiles' ]
    
-- | The difference asset types
data AssetType = JavaScript
  | JavaScriptEarly
  | Css
  | Font
  | Icon
  | CaminoDefinition
  | Directory
  deriving (Eq, Ord, Show, Generic)

instance FromJSON AssetType
instance ToJSON AssetType

-- | The type of cross-origin support needed for the script
data CrossOriginType = Unused
  | Anonymous
  | UseCredentials
  deriving (Eq, Ord, Show, Generic)

instance FromJSON CrossOriginType
instance ToJSON CrossOriginType

-- | Configuration for an external asset source
data AssetConfig = Asset {
  assetId :: Text, -- ^ The asset identifier
  assetType :: AssetType, -- ^ The type of asset
  assetPath :: Text, -- ^ The path to the asset
  assetIntegrity :: Maybe Text, -- ^ The integrity checksum for the asset
  assetCrossOrigin :: CrossOriginType -- ^ How to handle cross-origin requests
} deriving (Show)

instance FromJSON AssetConfig where
  parseJSON (Object v) = do
    id' <- v .: "id"
    type' <- v .: "type"
    path' <- v .: "path"
    integrity' <- v .:? "integrity"
    cors' <- v .:? "crossorigin" .!= Unused
    return $ Asset id' type' path' integrity' cors'
  parseJSON v = unexpected v
    
instance ToJSON AssetConfig where
  toJSON (Asset id' type' path' integrity' cors') =
    object [ "id" .= id', "type" .= type', "path" .= path', "integrity" .= integrity', "crossorigin" .= cors' ]

-- | Read an asset via it's path, via HTTP or as a file, depending on prefix
readAsset :: AssetConfig -> IO LB.ByteString
readAsset asset = let
    path = assetPath asset
    path' = unpack path
    (scheme, path'') = breakOn ":" path
  in
    if scheme == "file" then
      LB.readFile (unpack $ Data.Text.drop 1 path'')
    else if null path' then
      LB.readFile path'
    else if scheme == "http" || scheme == "https" then do
      request <- parseRequest path'
      response <- httpLBS request
      return $ getResponseBody response
    else
      error ("Unrecognised path: " ++ path')

-- | The functional type of link
data LinkType = Header -- ^ The link is part of the heading menu
  | Footer -- ^ The link is part of the footer
  | Embedded -- ^ The link is embedded
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LinkType
instance ToJSON LinkType

-- | A link to an external, language-specific resource
data LinkConfig = Link {
  linkId :: Text, -- ^ The link identifier
  linkType :: LinkType, -- ^ The type of link
  links :: Localised TaggedURL -- ^ The language-specific links
} deriving (Show)

instance FromJSON LinkConfig where
  parseJSON (Object v) = do
    id' <- v .: "id"
    type' <- v .: "type"
    links' <- v .: "links"
    return $ Link id' type' links'
  parseJSON v = unexpected v
  
instance ToJSON LinkConfig where
  toJSON (Link id' type' links') =
    object [ "id" .= id', "type" .= type', "links" .= links' ]

-- | Configuration for what's needed to set up web pages and other resources
data WebConfig = Web {
  webAssets :: [AssetConfig], -- ^ The assets needed to display the page properly
  webLinks :: [LinkConfig], -- ^ Links to other pages
  webMaps :: [MapConfig] -- ^ The sources of map tiles, with the default first
} deriving (Show)

instance FromJSON WebConfig where
  parseJSON (Object v) = do
    assets' <- v .:? "assets" .!= []
    links' <- v .:? "links" .!= []
    maps' <- v .:? "maps" .!= []
    return $ Web assets' links' maps'
  parseJSON v = unexpected v
    
instance ToJSON WebConfig where
  toJSON (Web assets' links' maps') =
    object [ "assets" .= assets', "links" .= links', "maps" .= maps' ]
   
-- | Configuration for what's needed to set up web pages and other resources
data Config = Config {
    configParent :: Maybe Config -- ^ A parent configuration containing values that can over overridden by this configuration
  , configWeb :: WebConfig -- ^ Configuration for the web interface
  , configCaminos :: [AssetConfig] -- ^ Locations of the various caminos
  , configCalendars :: Maybe CalendarConfig -- ^ Common calendar definitions for named holidays
  , configRegions:: Maybe RegionConfig -- ^ Common region definitions
  , configDebug :: Bool -- ^ Show debugging information
} deriving (Show)

-- | The default configuration
defaultConfig :: Config
defaultConfig = Config {
  configParent = Nothing,
  configWeb = Web {
    webAssets = [
      Asset {
        assetId = "jquery-js",
        assetType = JavaScriptEarly,
        assetPath = "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.7.0/jquery.min.js",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "bootstrap-js",
        assetType = JavaScriptEarly,
        assetPath = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "leaflet-js",
        assetType = JavaScriptEarly,
        assetPath = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js",
        assetIntegrity = Just "sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=",
        assetCrossOrigin = Anonymous
      },
      Asset {
        assetId = "leaflet-js-easybutton",
        assetType = JavaScriptEarly,
        assetPath = "https://cdn.jsdelivr.net/npm/leaflet-easybutton@2/src/easy-button.js",
        assetIntegrity = Nothing,
        assetCrossOrigin = Anonymous
      },
      Asset {
        assetId = "bootstrap-css",
        assetType = Css,
        assetPath = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "leaflet-css",
        assetType = Css,
        assetPath = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "leaflet-css-easybutton",
        assetType = Css,
        assetPath = "https://cdn.jsdelivr.net/npm/leaflet-easybutton@2/src/easy-button.css",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "camino-css",
        assetType = Css,
        assetPath = "https://de-calixtinus.s3.ap-southeast-2.amazonaws.com/static/css/camino.css",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "icons",
        assetType = Directory,
        assetPath = "https://de-calixtinus.s3.ap-southeast-2.amazonaws.com/static/icons",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "images",
        assetType = Directory,
        assetPath = "https://de-calixtinus.s3.ap-southeast-2.amazonaws.com/static/images",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "Camino Icons",
        assetType = Font,
        assetPath = "https://de-calixtinus.s3.ap-southeast-2.amazonaws.com/static/fonts/Camino-Icons.woff",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      }
    ],
    webLinks = [
    ],
    webMaps = [
      Map {
        mapId = "openStreetMap",
        mapTiles = "https://tile.openstreetmap.org/{z}/{x}/{y}.png"
      },
      Map {
        mapId = "googleMaps",
        mapTiles = "http://mt0.google.com/vt/lyrs=s,h&x={x}&y={y}&z={z}"
      }
    ]
  },
  configCaminos = [],
  configCalendars = Just (createCalendarConfig []),
  configRegions = Just (createRegionConfig []),
  configDebug = False
}

instance HasCalendarConfig Config where
  getCalendarConfig config = maybe (maybe (error "No calendar configuration") getCalendarConfig (configParent config)) id (configCalendars config)

instance HasRegionConfig Config where
  getRegionConfig config = maybe (maybe (error "No region configuration") getRegionConfig (configParent config)) id (configRegions config)

instance FromJSON Config where
  parseJSON (Object v) = do
    web' <- v .: "web"
    caminos' <- v .:? "caminos" .!= []
    calendar' <- v .:? "calendar"
    regions' <- v .:? "regions"
    debug' <- v .:? "debug" .!= False
    return $ Config  (Just defaultConfig) web' caminos' calendar' regions' debug'
  parseJSON v = unexpected v
    
instance ToJSON Config where
  toJSON (Config _parent' web' caminos' calendar' regions' debug') =
    object [ "web" .= web', "caminos" .= caminos', "calendar" .= calendar', "regions" .= regions', "debug" .= debug' ]

getAssets' :: AssetType -> Config -> M.Map Text AssetConfig
getAssets' asset config = let
    defaults = maybe M.empty (\p -> getAssets' asset p) (configParent config)
    local = M.fromList $ map (\a -> (assetId a, a)) $ filter (\a -> asset == assetType a) $ webAssets $ configWeb config
  in
    M.union local defaults

-- | Get a list of assets based on asset type
--   If the configuration has a parent, then any parent assets are gathered from the parent
--   and overwritten
getAssets :: AssetType -- ^ The type of asset to retrieve
  -> Config -- ^ The configuration
  -> [AssetConfig] -- ^ The resulting list of assets
getAssets asset config = M.elems $ getAssets' asset config

-- | Get something recursively from the configurations
getRecursive :: Maybe Text -> (Config -> [b]) -> (b -> Text) -> Config -> Maybe b
getRecursive ident lister identifier config = let
    parent = configParent config
    items = lister config
    result = case ident of
      Nothing -> if null items then Nothing else Just (items !! 0)
      (Just ident') -> find (\v -> ident' == identifier v) items
  in
    case result of
      Nothing -> case parent of
        Nothing -> Nothing
        Just p -> getRecursive ident lister identifier p
      r -> r

-- | Get an asset based on identifier
--   If the configuration has a parent and the requisite asset is not present, then the parent is tried
getAsset :: Text -- ^ The asset identifier
  -> Config -- ^ The configuration to query
  -> Maybe AssetConfig -- ^ The asset, if found
getAsset ident config = getRecursive (Just ident) (webAssets . configWeb) assetId config

-- | Get a specific link, based on an identifier and a list of locales
getLink :: Text -- ^ The link identifier
  -> [Locale] -- ^ The locale list
  -> Config -- ^ The configuration to query
  -> Maybe TaggedURL -- ^ The internationalised link
getLink ident locales config = maybe Nothing (localise locales . links) (getRecursive (Just ident) (webLinks . configWeb) linkId config)

-- | Get links, based on a link type
--   The resulting links are in the order specifiedin the configuration, from parent to child
getLinks :: LinkType -- ^ The link type
  -> Config -- ^ The configuration to query
  -> [LinkConfig] -- ^ The links to use
getLinks lt config = let
    makeMap list = M.fromList $ map (\l -> (linkId l, l)) list
    defaults = maybe [] (getLinks lt) (configParent config)
    defaultMap = makeMap defaults
    updates = filter (\l -> linkType l == lt) (webLinks $ configWeb config)
    updateMap = makeMap updates
    newLinks = filter (\l -> not $ M.member (linkId l) defaultMap) updates
    defaultsWithReplace = map (\l -> maybe l id (M.lookup (linkId l) updateMap)) defaults
  in
    defaultsWithReplace ++ newLinks

-- | Get a map, optionally based on an identifier
--   If the configuration has a parent and the requisite map is not present, then the parent is tried
getMap :: Maybe Text -- ^ The map identifier, if Nothing then the first map is chosen
  -> Config -- ^ The configuration
  -> Maybe MapConfig -- ^ The resulting map configuration
getMap ident config = getRecursive ident (webMaps . configWeb) mapId config

-- | Get the name of a calendar from the configuration
getCalendarName :: Config -> Text -> Localised TaggedText
getCalendarName config key = runReader (getNamedCalendarName key) config

-- | Read a configuration from a YAML file
--   The resulting configuration will have @defaultConfig@ as a parent.
readConfigFile :: String -> IO Config
readConfigFile file = do
  cf <- B.readFile file
  let decoded = decodeEither' cf :: Either ParseException Config
  return $ case decoded of
    Left ex -> error $ show ex
    Right config' -> config'
