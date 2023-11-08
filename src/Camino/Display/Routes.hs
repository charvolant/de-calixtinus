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
import Data.Text

-- | Common Camino routes
data CaminoRoute = AssetRoute Text -- ^ An identified font or asset
  | IconRoute Text -- ^ A specific icon
  | KMLRoute -- ^ The KML version of this plan
  | LinkRoute LinkConfig -- ^ The (locale specific) path for a link
  | LocationRoute Location -- ^ A location
  | MapTileRoute -- ^ The route to the map tile
  
-- | The rendering function for the routes
renderCaminoRoute :: Config -> [Text] -> CaminoRoute -> [(Text, Text)] -> Text
renderCaminoRoute config _locales (AssetRoute ident) _ = maybe ("invalid/" <> ident) assetPath (getAsset ident config)
renderCaminoRoute config _locales (IconRoute ident) _ = (maybe "invalid" assetPath (getAsset "icons" config)) <> "/" <> ident
renderCaminoRoute _config _locales KMLRoute _ = "camino.kml"
renderCaminoRoute config locales (LinkRoute link) _ = maybe "error.html" linkPath (getLink ident locales config) where ident = linkId link
renderCaminoRoute _config  _locales (LocationRoute location) _ = "#" <> pack (locationID location)
renderCaminoRoute config  _locales MapTileRoute _ = maybe "invalid/map.png" mapTiles (getMap Nothing config)
