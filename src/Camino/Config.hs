{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Camino.Config (
  AssetConfig(..),
  AssetType(..),
  Config(..),
  CrossOriginType(..),
  MapConfig(..),
  WebConfig(..),

  defaultConfig,
  getAsset,
  getAssets,
  getMap,
  readConfigFile
) where

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text)
import Data.List (find)
import Data.Yaml (ParseException, decodeEither')
import qualified Data.ByteString as B (readFile)
import Data.Aeson.Types (unexpected)

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
  | Css
  | Font
  | Icon
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

-- | Configuration for what's needed to set up web pages and other resources
data WebConfig = Web {
  webAssets :: [AssetConfig], -- ^ The assets needed to 
  webMaps :: [MapConfig] -- ^ The sources of map tiles, with the default first
} deriving (Show)

instance FromJSON WebConfig where
  parseJSON (Object v) = do
    assets' <- v .:? "assets" .!= []
    maps' <- v .:? "maps" .!= []
    return $ Web assets' maps'
  parseJSON v = unexpected v
    
instance ToJSON WebConfig where
  toJSON (Web assets' maps') =
    object [ "assets" .= assets', "maps" .= maps' ]
   
   
-- | Configuration for what's needed to set up web pages and other resources
data Config = Config {
  configParent :: Maybe Config, -- ^ A parent configuration containing values that can over overridden by this configuration
  configWeb :: WebConfig -- ^ Configuration for the web interface
} deriving (Show)

-- | The default configuration
defaultConfig :: Config
defaultConfig = Config {
  configParent = Nothing,
  configWeb = Web {
    webAssets = [
      Asset {
        assetId = "jQueryJs",
        assetType = JavaScript,
        assetPath = "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.7.0/jquery.min.js",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "bootstrapJs",
        assetType = JavaScript,
        assetPath = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/js/bootstrap.bundle.min.js",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "leafletJs",
        assetType = JavaScript,
        assetPath = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js",
        assetIntegrity = Just "sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo=",
        assetCrossOrigin = Anonymous
      },
      Asset {
        assetId = "bootstrapCss",
        assetType = Css,
        assetPath = "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "leafletCss",
        assetType = Css,
        assetPath = "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "icons",
        assetType = Directory,
        assetPath = "https://camino-planner.s3.ap-southeast-2.amazonaws.com/icons",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      },
      Asset {
        assetId = "Camino Icons",
        assetType = Font,
        assetPath = "https://camino-planner.s3.ap-southeast-2.amazonaws.com/fonts/Camino-Icons.woff",
        assetIntegrity = Nothing,
        assetCrossOrigin = Unused
      }
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
  }
}

instance FromJSON Config where
  parseJSON (Object v) = do
    web' <- v .: "web"
    return $ Config  (Just defaultConfig) web'
  parseJSON v = unexpected v
    
instance ToJSON Config where
  toJSON (Config _parent' web') =
    object [ "web" .= web' ]

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
      r@(Just _) -> r

-- | Get an asset based on identifier
--   If the configuration has a parent and the requisite asset is not present, then the parent is tried
getAsset :: Text -- ^ The asset identifier
  -> Config -- ^ The configuration to query
  -> Maybe AssetConfig -- ^ The asset, if found
getAsset ident config = getRecursive (Just ident) (webAssets . configWeb) assetId config

-- | Get a map, optionally based on an identifier
--   If the configuration has a parent and the requisite map is not present, then the parent is tried
getMap :: Maybe Text -- ^ The map identifier, if Nothing then the first map is chosen
  -> Config -- ^ The configuration
  -> Maybe MapConfig -- ^ The resulting map configuration
getMap ident config = getRecursive ident (webMaps . configWeb) mapId config

-- | Read a configuration from a YAML file
--   The resulting configuration will have @defaultConfig@ as a parent.
readConfigFile :: String -> IO Config
readConfigFile file = do
  cf <- B.readFile file
  let decoded = decodeEither' cf :: Either ParseException Config
  return $ case decoded of
    Left ex -> error $ show ex
    Right config' -> config'
