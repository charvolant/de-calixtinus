{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Routes
Description : Common URL routes for Html, Css and KML
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Common URL routes for Html, Css and KML

Routes, in this case refer to the hamlet-style route placeholders.
See <https://www.yesodweb.com/book/routing-and-handlers>
-}
module Camino.Display.Routes (
    CaminoRoute(..)

  , appendRoot
  , renderCaminoRoute
) where

import Camino.Camino
import Camino.Config
import Data.Description
import Data.Localised
import Data.Text

-- | Common Camino routes
data CaminoRoute = AssetRoute Text -- ^ An identified font or asset
  | IconRoute Text -- ^ A specific icon
  | FeatureRoute Text -- ^ A specific feature
  | ImgRoute Image Bool -- ^ The (locale specific) path for an image
  | LinkRoute (Localised TaggedURL) -- ^ The (locale specific) path for a link
  | LocationRoute Location -- ^ A location
  | MapTileRoute -- ^ The route to the map tile
  | PlanRoute Text -- ^ The route to a persisted plan
  | PlanKmlRoute Text -- ^ The route to a persisted plan in KML form
  | PlanSpreadsheetRoute Text -- ^ The route to a persisted plan in spreadhseet form

findAssetPath ident config = maybe ("invalid/" <> ident) assetPath (getAsset ident config)

-- | Add the web root location from a configuration to a relative URL
--
--   This function is used to allow an application hiding behind a webserver like nginx produce publically visible URLs.
--   Otherwise the route will, most likely, return @http://localhost:3000@
appendRoot :: Config -> Text -> Text
appendRoot config p = if isPrefixOf "http:" p || isPrefixOf "https:" p then p else getWebRoot config <> "/" <> p

-- | The rendering function for the routes
--
--   The resulting routes are (usually) absolte URLs with `appendRoot` added.
renderCaminoRoute :: Config -> [Locale] -> CaminoRoute -> [(Text, Text)] -> Text
renderCaminoRoute config _locales (AssetRoute ident) _ = appendRoot config $ findAssetPath ident config
renderCaminoRoute config _locales (IconRoute ident) _ = appendRoot config $ (findAssetPath "icons" config) <> "/" <> ident
renderCaminoRoute config _locales (FeatureRoute path) _ = appendRoot config $ (findAssetPath "features" config) <> "/" <> path
renderCaminoRoute config _locales (ImgRoute img thumb) _ = appendRoot config $ resolveLink (findAssetPath "images" config) (imageToLink thumb img)
renderCaminoRoute config locales (LinkRoute tl) _ = appendRoot config $ resolveLink (findAssetPath "links" config) (maybe invalidLink id (localise locales tl))
renderCaminoRoute _config  _locales (LocationRoute location) _ = "#" <> locationID location
renderCaminoRoute config  _locales MapTileRoute _ = appendRoot config $ maybe "invalid/map.png" mapTiles (getMap Nothing config)
renderCaminoRoute config  _locales (PlanRoute sid) _ = appendRoot config $ "plan/" <> sid
renderCaminoRoute config  _locales (PlanKmlRoute sid) _ = appendRoot config $ "planKml/" <> sid
renderCaminoRoute config  _locales (PlanSpreadsheetRoute sid) _ = appendRoot config $ "planXlsx/" <> sid
