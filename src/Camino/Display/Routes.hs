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
import Data.Description
import Data.Localised
import Data.Text

-- | Common Camino routes
data CaminoRoute = AssetRoute Text -- ^ An identified font or asset
  | IconRoute Text -- ^ A specific icon
  | ImgRoute Image Bool -- ^ The (locale specific) path for an image
  | LinkRoute (Localised TaggedURL) -- ^ The (locale specific) path for a link
  | LocationRoute Location -- ^ A location
  | MapTileRoute -- ^ The route to the map tile

findAssetPath ident config = maybe ("invalid/" <> ident) assetPath (getAsset ident config)

-- | The rendering function for the routes
renderCaminoRoute :: Config -> [Locale] -> CaminoRoute -> [(Text, Text)] -> Text
renderCaminoRoute config _locales (AssetRoute ident) _ = findAssetPath ident config
renderCaminoRoute config _locales (IconRoute ident) _ = (findAssetPath "icons" config) <> "/" <> ident
renderCaminoRoute config _locales (ImgRoute img thumb) _ = resolveLink (findAssetPath "images" config) (imageToLink thumb img)
renderCaminoRoute config locales (LinkRoute tl) _ = resolveLink (findAssetPath "links" config) (maybe invalidLink id (localise locales tl))
renderCaminoRoute _config  _locales (LocationRoute location) _ = "#" <> pack (locationID location)
renderCaminoRoute config  _locales MapTileRoute _ = maybe "invalid/map.png" mapTiles (getMap Nothing config)
