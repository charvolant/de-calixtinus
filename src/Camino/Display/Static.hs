{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : Static
Description : Generate static assets
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Create static HTML, CSS etc that can be placed on a server somewhere
-}

module Camino.Display.Static (
    createCssFiles
  , createColourSwatch
  , createHelpFiles
) where

import Camino.Colour
import Camino.Config
import Camino.Display.Css
import Camino.Display.Html
import Camino.Display.I18n
import Camino.Display.Routes
import Data.ByteString.Lazy as LB (writeFile)
import Data.Localised
import Data.Text (Text)
import qualified Data.Text.Lazy as LT (concat)
import qualified Data.Text.Lazy.IO as LTIO (writeFile)
import qualified Data.Units as U
import System.Directory
import System.FilePath
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Cassius
import Text.Hamlet

-- | Create a static @camino.css@ file containing common CSS for the application
--
--   See `staticCss`
createCssFiles :: Config -> FilePath -> IO ()
createCssFiles config output = do
  let router = renderCaminoRoute config [rootLocale]
  let css = staticCss config
  let file = output </> "camino.css"
  createDirectoryIfMissing True output
  LTIO.writeFile file $ LT.concat (map (\c -> renderCss $ c router) css)

{-
  <svg width="#{width + padding * 2}" height="#{height + padding * 2}" xmlns="http://www.w3.org/2000/svg">
    $forall (sx, sy, tx, ty, name, colour) <- colours
      <rect width="#{width}" height="#{rowheight}" x="#{padding}" y="#{i * rowheight + padding}" fill="#{toCssColour colour}" stroke="darkgrey" stroke-width="1">
      <text x="#{padding * 2}" y="#{i * rowheight + padding + 5}" font-size="12" font-weight="normal" text-anchor="start" fill="black">#{name}
-}
-- | Create an SVG colour map
createColourMap :: Config -> HtmlUrlI18n CaminoMsg CaminoRoute
createColourMap _config = [ihamlet|
  <svg width="#{totalwidth}" height="#{totalheight}" xmlns="http://www.w3.org/2000/svg">
    $forall (sx, sy, tx, ty, cx, name, colour) <- colours
      <rect width="#{width}" height="#{rowheight}" x="#{sx}" y="#{sy}" fill="#{toCssColour colour}" stroke="darkgrey" stroke-width="1">
      <text x="#{tx}" y="#{ty}" font-family="sans-serif" font-size="#{fontsize}" font-weight="normal" text-anchor="start" fill="#{textColour colour}">#{name}
      <text x="#{cx}" y="#{ty}" font-family="sans-serif" font-size="#{fontsize}" font-weight="normal" text-anchor="start" fill="#{textColour colour}">#{toCssColour colour}
  |]
  where
    width = 300 :: Int
    padding = 10 :: Int
    totalwidth = width + padding * 2
    fontsize = 14 :: Int
    rowheight = fontsize + 10
    colours = map (\(i, (name, colour)) -> (
        padding
      , i * rowheight + padding
      , padding * 2
      , i * rowheight + padding + fontsize * 12 `div` 10
      , width `div` 2 + padding * 2
      , name
      , colour
      )) $ zip [0..] namedColours
    textColour c = if luminance c < 0.5 then "white" :: Text else "black" :: Text
    height = length colours * rowheight
    totalheight = height + padding * 2

-- | Create an SVG colour swatch for colour display
createColourSwatch :: Config -> FilePath -> IO ()
createColourSwatch config output = do
  let locales = [rootLocale]
  let router = renderCaminoRoute config locales
  let messages = renderCaminoMsg config locales
  let swatch = createColourMap config
  let file = output </> "colours.svg"
  createDirectoryIfMissing True output
  LB.writeFile file $ renderHtml $ swatch messages router

createHelpFile :: Config -> Locale -> FilePath -> HtmlUrlI18n CaminoMsg CaminoRoute -> IO ()
createHelpFile config loc file html = do
  let locales = [loc, rootLocale]
  let router = renderCaminoRoute config locales
  let messages = renderCaminoMsg config locales
  LB.writeFile file $ renderHtml $ html messages router
  
_createStandAloneHelpFile :: Config -> Locale -> FilePath -> HtmlUrlI18n CaminoMsg CaminoRoute -> Text -> IO ()
_createStandAloneHelpFile config loc file html title = createHelpFile config loc file (layoutHtml config (wildcardText title) Nothing html Nothing)

-- | Create static help files.
--
--  Currently not used, with help file being dynamically served
createHelpFiles :: Config -> FilePath -> IO ()
createHelpFiles _config output = do
  let _loc = localeFromIDOrError "en"
  createDirectoryIfMissing True output
  {-
  createStandAloneHelpFile config loc (output </> "help-en.html") $(ihamletFile "templates/help/help-en.hamlet") "Help"
  createHelpFile config loc (output </> "travel-help-en.html") $(ihamletFile "templates/help/travel-help-en.hamlet")
  createHelpFile config loc (output </> "range-help-en.html") $(ihamletFile "templates/help/range-help-en.hamlet")
  createHelpFile config loc (output </> "services-help-en.html") $(ihamletFile "templates/help/services-help-en.hamlet")
  createHelpFile config loc (output </> "routes-help-en.html") $(ihamletFile "templates/help/routes-help-en.hamlet")
  createHelpFile config loc (output </> "start-help-en.html") $(ihamletFile "templates/help/start-help-en.hamlet")
  createHelpFile config loc (output </> "stops-help-en.html") $(ihamletFile "templates/help/stops-help-en.hamlet")
  -}