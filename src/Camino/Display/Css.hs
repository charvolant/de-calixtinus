{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
  caminoCss,
  toCssColour
) where

import Camino.Camino
import Camino.Config
import Camino.Display.Routes
import Data.Char (ord)
import Data.Colour
import Data.Colour.SRGB
import Data.Text ()
import Numeric
import Text.Cassius
import Text.Hamlet (Render)
  
-- | Create a CSS-able colour
toCssColour :: Colour Double -- ^ The colour to display
 -> String -- ^ A #rrggbb colour triple
toCssColour = sRGB24show

paletteCss :: String -> Palette -> Render CaminoRoute -> Css
paletteCss ident pal = [cassius|
.#{ident}
  h1
    color: #{toCssColour $ paletteColour pal}
  h2
    color: #{toCssColour $ paletteColour pal}
  h3
    color: #{toCssColour $ paletteColour pal}
  h4
    color: #{toCssColour $ paletteColour pal}

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
    ("ca-bank", '\xe042'),
    ("ca-bed-double", '\xe022'),
    ("ca-bed-double-wc", '\xe023'),
    ("ca-bed-quadruple", '\xe026'),
    ("ca-bed-quadruple-wc", '\xe027'),
    ("ca-bed-single", '\xe020'),
    ("ca-bed-triple", '\xe024'),
    ("ca-bed-triple-wc", '\xe025'),
    ("ca-bedlinen", '\xe06c'),
    ("ca-breakfast", '\xe064'),
    ("ca-bicycle-repair", '\xe045'),
    ("ca-bicycle-storage", '\xe06a'),
    ("ca-bridge", '\xe004'),
    ("ca-bus", '\xe047'),
    ("ca-camping", '\xe019'),
    ("ca-city", '\xe003'),
    ("ca-dinner", '\xe065'),
    ("ca-dryer", '\xe062'),
    ("ca-groceries", '\xe041'),
    ("ca-guesthouse", '\xe012'),
    ("ca-handwash", '\xe063'),
    ("ca-heating", '\xe070'),
    ("ca-hotel", '\xe013'),
    ("ca-house", '\xe011'),
    ("ca-intersection", '\xe005'),
    ("ca-kitchen", '\xe06b'),
    ("ca-lockers", '\xe066'),
    ("ca-mattress", '\xe028'),
    ("ca-medical", '\xe044'),
    ("ca-pets", '\xe069'),
    ("ca-pharmacy", '\xe043'),
    ("ca-poi", '\xe000'),
    ("ca-pool", '\xe06e'),
    ("ca-prayer", '\xe06f'),
    ("ca-restaurant", '\xe040'),
    ("ca-shared", '\xe021'),
    ("ca-sleeping-bag", '\xe028'),
    ("ca-stables", '\xe068'),
    ("ca-tent", '\xe018'),
    ("ca-towels", '\xe06d'),
    ("ca-town", '\xe002'),
    ("ca-train", '\xe046'),
    ("ca-village", '\xe001'),
    ("ca-washing-machine", '\xe061'),
    ("ca-wifi", '\xe060')
  ]
  
caminoIconCss :: Camino -> [Render CaminoRoute -> Css]
caminoIconCss _camino = map (\(ident, ch) -> iconCss ident ch) iconList

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
    
caminoBaseCss :: Camino -> Render CaminoRoute -> Css
caminoBaseCss _camino = [cassius|
#map
  width: 80%
  height: 800px
  padding: 1em
a
  text-decoration: none
.day
  h4
    .distance
      margin-left: 1em
      font-size: initial
    .penance-summary
      margin-left: 1em
      font-size: initial
      display: inline-block
.rejected
  color: #c5321b
.service
  color: #1964c0
.accomodation
  color: #1964c0
.accomodation.card
  padding: 1ex
.accomodation.municipal-albergue
  color: #f9b34a
.distance-summary
  display: inline-block
  margin-left: 1em
  .leg-distance
    margin-left: 1em
  .leg-ascent
    font-size: smaller
  .leg-ascent::before
    content: "\2191"
  .leg-descent
    font-size: smaller
  .leg-descent::before
    content: "\2193"
.location 
  .card-title 
    h4
      display: inline-block
    .accomodation-types
      display: inline-block
      margin-left: 2em
    .services
      display: inline-block
      margin-left: 2em
.location-summary
  display: inline-block
  .accomodation-types
    font-size: smaller
    display: inline-block
    margin-left: 1ex
  .services
    font-size: smaller
    display: inline-block
    margin-left: 0,5ex
.location-waypoint
  .card-title
    h4
      font-weight: bolder
.location-stop
  .card-title
    h4
      font-weight: bolder
    h4::after
      margin-left: 1ex
      color: #f9b34a
      font-family: "Camino Icons"
      font-weight: normal
      content: "\e020"
  |]

caminoCss :: Config -> Camino -> [Render CaminoRoute -> Css]
caminoCss config camino = (base':default':routes') ++ fonts' ++ icons'
  where
    base' = caminoBaseCss camino
    default' = paletteCss "location-default" (palette camino)
    routes' = map (\r -> paletteCss ("location-" ++ (routeID r)) (routePalette r)) (routes camino)
    fonts' = map caminoFontCss (getAssets Font config)
    icons' = caminoIconCss camino
