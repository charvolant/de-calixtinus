{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Css
Description : Produce Css styles for HTML and KML display
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Camino.Display.Css (
    caminoBlue
  , caminoCss
  , caminoYellow
  , informationBlue
  , mutedBlue
  , recreationGreen
  , staticCss
  , successGreen
  , toCssColour
  , toExcelColour
  , warningRed
) where

import Camino.Camino
import Camino.Config
import Camino.Display.Routes
import Data.Char (ord)
import Data.Colour
import Data.Colour.SRGB
import Data.Default.Class
import Data.Localised (rootLocale)
import Data.Text (Text, pack)
import Numeric
import Text.Cassius
import Text.Hamlet (Render)

-- The traditional blue tile colour. Used as a primary darkish colour
caminoBlue :: Colour Double
caminoBlue = sRGB24read "1964c0"

-- The traditional yellow tile colour. Used as a primary lightish colour
caminoYellow :: Colour Double
caminoYellow = sRGB24read "f9b34a"

-- A blue indicating information. Not the traditional information sign colour, since it's too close to camino blue
informationBlue :: Colour Double
informationBlue = sRGB24read "1c9cf1"

-- A green indicating rest and recreation
recreationGreen :: Colour Double
recreationGreen = sRGB24read "00b820"

-- A muted blue indicating deprectaed information
mutedBlue :: Colour Double
mutedBlue = sRGB24read "a0b3ca"

-- A bootstrap warning color
warningRed :: Colour Double
warningRed = sRGB24read "dc3545"

-- A bootstrap success color
successGreen :: Colour Double
successGreen = sRGB24read "198754"

-- | Create a CSS-able colour
toCssColour :: Colour Double -- ^ The colour to display
 -> String -- ^ A #rrggbb colour triple
toCssColour = sRGB24show

-- | Create an Excel-able colour
toExcelColour :: Colour Double -- ^ The colour to display
  -> Text -- ^ A aarrggbb colour quad
toExcelColour v = "ff" <> (pack $ tail $ sRGB24show v)

paletteCss :: Text -> Palette -> Render CaminoRoute -> Css
paletteCss ident pal = [cassius|
.#{ident}
  h1
    color: #{toCssColour $ paletteTextColour pal}
  h2
    color: #{toCssColour $ paletteTextColour pal}
  h3
    color: #{toCssColour $ paletteTextColour pal}
  h4
    color: #{toCssColour $ paletteTextColour pal}
  h5
    color: #{toCssColour $ paletteTextColour pal}
  |]
  
iconCss :: String -> Char -> Render CaminoRoute -> Css
iconCss ident ch = [cassius|
.#{ident}::before
  font-family: "Camino Icons"
  font-weight: normal
  line-height: 1
  text-rendering: auto
  content: "\#{hex $ ord ch}"
|]
  where
    hex c = showHex c ""

iconList :: [(String, Char)]
iconList = [
    ("ca-accessible", '\xe067'),
    ("ca-albergue", '\xe010'),
    ("ca-altar", '\xe030'),
    ("ca-artwork", '\xe050'),
    ("ca-bank", '\xe042'),
    ("ca-beach", '\xe01f'),
    ("ca-bed-double", '\xe022'),
    ("ca-bed-double-wc", '\xe023'),
    ("ca-bed-quadruple", '\xe026'),
    ("ca-bed-quadruple-wc", '\xe027'),
    ("ca-bed-single", '\xe020'),
    ("ca-bed-triple", '\xe024'),
    ("ca-bed-triple-wc", '\xe025'),
    ("ca-bedlinen", '\xe06c'),
    ("ca-bishop", '\xe031'),
    ("ca-breakfast", '\xe064'),
    ("ca-bicycle-repair", '\xe045'),
    ("ca-bicycle-storage", '\xe06a'),
    ("ca-bridge", '\xe004'),
    ("ca-bus", '\xe047'),
    ("ca-bus-link", '\xe084'),
    ("ca-calendar", '\xe095'),
    ("ca-campground", '\xe019'),
    ("ca-camping", '\xe019'),
    ("ca-cathedral", '\xe008'),
    ("ca-church", '\xe014'),
    ("ca-city", '\xe003'),
    ("ca-clock", '\xe094'),
    ("ca-cooling", '\xe071'),
    ("ca-cross", '\xe009'),
    ("ca-cycling", '\xe081'),
    ("ca-dinner", '\xe065'),
    ("ca-dryer", '\xe062'),
    ("ca-ferry", '\xe082'),
    ("ca-festival", '\xe032'),
    ("ca-food", '\xe036'),
    ("ca-fountain", '\xe00a'),
    ("ca-globe", '\xe090'),
    ("ca-gite", '\xe01a'),
    ("ca-groceries", '\xe041'),
    ("ca-guesthouse", '\xe012'),
    ("ca-handwash", '\xe063'),
    ("ca-hazard", '\xe093'),
    ("ca-heating", '\xe070'),
    ("ca-help", '\xe092'),
    ("ca-historical", '\xe00d'),
    ("ca-holiday", '\xe034'),
    ("ca-homestay", '\xe011'),
    ("ca-hostel", '\xe012'),
    ("ca-hotel", '\xe013'),
    ("ca-house", '\xe011'),
    ("ca-house-small", '\xe098'),
    ("ca-information", '\xe091'),
    ("ca-intersection", '\xe005'),
    ("ca-junction", '\xe01e'),
    ("ca-kitchen", '\xe06b'),
    ("ca-link", '\xe096'),
    ("ca-lockers", '\xe066'),
    ("ca-lookout", '\xe051'),
    ("ca-map", '\xe097'),
    ("ca-mattress", '\xe028'),
    ("ca-medical", '\xe044'),
    ("ca-monastery", '\xe006'),
    ("ca-municipal", '\xe00b'),
    ("ca-museum", '\xe00c'),
    ("ca-music", '\xe033'),
    ("ca-natural", '\xe00f'),
    ("ca-park", '\xe00e'),
    ("ca-peak", '\xe007'),
    ("ca-pets", '\xe069'),
    ("ca-pharmacy", '\xe043'),
    ("ca-pilgrim", '\xe072'),
    ("ca-poi", '\xe000'),
    ("ca-pool", '\xe06e'),
    ("ca-prayer", '\xe06f'),
    ("ca-promontory", '\xe052'),
    ("ca-refuge", '\xe01a'),
    ("ca-restpoint", '\xe073'),
    ("ca-restaurant", '\xe040'),
    ("ca-rowing", '\xe083'),
    ("ca-shared", '\xe021'),
    ("ca-shop", '\xe01d'),
    ("ca-sleeping-bag", '\xe028'),
    ("ca-stables", '\xe068'),
    ("ca-statue", '\xe01b'),
    ("ca-stockpoint", '\xe041'),
    ("ca-stop", '\xe020'),
    ("ca-tent", '\xe018'),
    ("ca-theatre", '\xe035'),
    ("ca-towels", '\xe06d'),
    ("ca-town", '\xe002'),
    ("ca-train", '\xe046'),
    ("ca-train-link", '\xe085'),
    ("ca-village", '\xe001'),
    ("ca-walking", '\xe080'),
    ("ca-warning", '\xe093'),
    ("ca-washing-machine", '\xe061'),
    ("ca-wharf", '\xe048'),
    ("ca-wifi", '\xe060'),
    ("ca-winery", '\xe01c')
  ]
  
caminoIconCss :: [Render CaminoRoute -> Css]
caminoIconCss = map (\(ident, ch) -> iconCss ident ch) iconList

caminoFontCss :: AssetConfig -> Render CaminoRoute -> Css
caminoFontCss asset = [cassius|
@font-face
  font-family: "#{ident}"
  font-weight: normal
  font-style: normal
  src: url(@{AssetRoute ident})
|]
  where
    ident = assetId asset

caminoBaseCss :: Render CaminoRoute -> Css
caminoBaseCss = $(cassiusFile "templates/css/base.cassius")

-- | Generate CSS that can be placed in a static file
staticCss :: Config -- ^ The base configuration
  -> [Render CaminoRoute -> Css] -- ^ A list of CSS fragments for static CSS
staticCss config = [caminoBaseCss, paletteCss "location-default" def] ++ (map caminoFontCss (getAssets Font config)) ++ caminoIconCss

-- | Generate CSS for a camino.
--   This is intended to be embedded in a camino plan and contains things like specific route palettes
caminoCss :: Config -> Camino -> [Css]
caminoCss config camino = map (\c -> c router) (default':routes')
  where
    router = renderCaminoRoute config [rootLocale]
    default' = paletteCss "location-default" (routePalette $ caminoDefaultRoute camino)
    routes' =  map (\r -> paletteCss ("location-" <> routeID r) (routePalette r)) (caminoRoutes camino)
