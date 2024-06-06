{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Routes
Description : Common URL routes for Html, Css and KML
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

-}
module Camino.Display.Routes (
  CaminoRoute(..),
  
  renderCaminoRoute
) where

import Camino.Camino
import Camino.Config
import Data.Localised ()
import Data.Text

-- | Common Camino routes
data CaminoRoute = AssetRoute Text -- ^ An identified font or asset
  | IconRoute Text -- ^ A specific icon
  | KMLRoute -- ^ The KML version of this plan
  | LinkRoute LinkConfig -- ^ The (locale specific) path for a link
  | LinkIDRoute Text -- ^ The (locale specific) path for a link identifier
  | LocationRoute Location -- ^ A location
  | MapTileRoute -- ^ The route to the map tile
  
findLink ident locales config = maybe "/error.html" linkPath (getLink ident locales config)

-- | The rendering function for the routes
renderCaminoRoute :: Config -> [Locale] -> CaminoRoute -> [(Text, Text)] -> Text
renderCaminoRoute config _locales (AssetRoute ident) _ = maybe ("invalid/" <> ident) assetPath (getAsset ident config)
renderCaminoRoute config _locales (IconRoute ident) _ = (maybe "invalid" assetPath (getAsset "icons" config)) <> "/" <> ident
renderCaminoRoute _config _locales KMLRoute _ = "camino.kml"
renderCaminoRoute config locales (LinkRoute link) _ = findLink (linkId link) locales config
renderCaminoRoute config locales (LinkIDRoute ident) _ = findLink ident locales config
renderCaminoRoute _config  _locales (LocationRoute location) _ = "#" <> pack (locationID location)
renderCaminoRoute config  _locales MapTileRoute _ = maybe "invalid/map.png" mapTiles (getMap Nothing config)
