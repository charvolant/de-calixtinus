{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Camino.Config (
  AssetConfig(..),
  AssetType(..),
  Config(..),
  CrossOriginType(..),
  LinkConfig(..),
  LinkI18n(..),
  LinkType(..),
  MapConfig(..),
  WebConfig(..),

  defaultConfig,
  getAsset,
  getAssets,
  getLink,
  getLinks,
  getMap,
  readConfigFile
) where

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Map as M
import Data.Text (Text)
import Data.List (find)
import Data.Maybe (catMaybes, listToMaybe)
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

-- | The functional type of link
data LinkType = Header -- ^ The link is part of the heading menu
  | Footer -- ^ The link is part of the footer
  deriving (Eq, Ord, Show, Generic)

instance FromJSON LinkType
instance ToJSON LinkType

-- | A locality-specific link
data LinkI18n = LinkI18n {
  linkLocale :: Text, -- ^ The language and optional localisation of the, eg "en" or "pt-BR". The empty string is the default match
  linkLabel :: Text, -- ^ The localised link label
  linkPath :: Text -- ^ The path to the link
}  deriving (Show)

instance FromJSON LinkI18n where
  parseJSON (Object v) = do
    locale' <- v .:? "locale" .!= ""
    label' <- v .: "label"
    path' <- v .: "path"
    return $ LinkI18n locale' label' path'
  parseJSON v = unexpected v
  
instance ToJSON LinkI18n where
  toJSON (LinkI18n locale' label' path') =
    object [ "locale" .= locale', "label" .= label', "path" .= path' ]

-- | A link to an external, language-specific resource
data LinkConfig = Link {
  linkId :: Text, -- ^ The link identifier
  linkType :: LinkType, -- ^ The type of link
  links :: [LinkI18n] -- ^ The language-specific links. (Language matching is done
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
        assetId = "caminoCss",
        assetType = Css,
        assetPath = "https://camino-planner.s3.ap-southeast-2.amazonaws.com/static/camino.css",
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
    webLinks = [
      Link {
        linkId = "helpLink",
        linkType = Header,
        links = [
          LinkI18n {
            linkLocale = "",
            linkLabel = "Help",
            linkPath = "https://camino-planner.s3.ap-southeast-2.amazonaws.com/help/help-en.html"
          }
        ]
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

findVariant :: (LinkI18n -> Bool) -> LinkConfig -> Maybe LinkI18n
findVariant variant link = find variant (links link)

-- Get a map of id onto localised variant
getLinks'' :: (LinkConfig -> Bool) -> (LinkI18n -> Bool) -> Config -> M.Map Text LinkI18n
getLinks'' select variant config = let
  defaults = maybe M.empty (\p -> getLinks'' select variant p) (configParent config)
  local = filter select (webLinks $ configWeb config)
  local' = M.fromList $ catMaybes $ map (\l -> let 
      ml = find variant (links l) 
    in 
      case ml of 
        Nothing -> Nothing
        (Just il) -> Just (linkId l, il)
    ) local
  in
    M.union local' defaults

-- Try in locale order
getLinks' ::  (LinkConfig -> Bool) -> [Text] -> Config -> [LinkI18n]
getLinks' select locales config = M.elems $ foldr (\lo -> \e -> M.union e (getLinks'' select (\l -> linkLocale l == lo) config)) M.empty locales

-- | Get a specific link, based on an identifier and a list of locales
getLink :: Text -- ^ The link identifier
  -> [Text] -- ^ The locale list
  -> Config -- ^ The configuration to query
  -> Maybe LinkI18n -- ^ The internactionalised link
getLink ident locales config = listToMaybe $ getLinks' (\l -> linkId l == ident) locales config

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

-- | Read a configuration from a YAML file
--   The resulting configuration will have @defaultConfig@ as a parent.
readConfigFile :: String -> IO Config
readConfigFile file = do
  cf <- B.readFile file
  let decoded = decodeEither' cf :: Either ParseException Config
  return $ case decoded of
    Left ex -> error $ show ex
    Right config' -> config'
