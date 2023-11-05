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
data CaminoRoute = AssetRoute Text -- ^ An identified font
  | IconRoute Text -- ^ A specific icon
  | KMLRoute -- ^ The KML version of this route
  | LocationRoute Location -- ^ A location
  | MapTileRoute -- ^ The route to the map tile
  
-- | The rendering function for the routes
renderCaminoRoute :: Config -> CaminoRoute -> [(Text, Text)] -> Text
renderCaminoRoute config (AssetRoute ident) _ = maybe ("invalid/" <> ident) assetPath (getAsset ident config)
renderCaminoRoute config (IconRoute ident) _ = (maybe "invalid" assetPath (getAsset "icons" config)) <> "/" <> ident
renderCaminoRoute _config KMLRoute _ = "camino.kml"
renderCaminoRoute _config (LocationRoute location) _ = "#" <> pack (locationID location)
renderCaminoRoute config MapTileRoute _ = maybe "invalid/map.png" mapTiles (getMap Nothing config)
