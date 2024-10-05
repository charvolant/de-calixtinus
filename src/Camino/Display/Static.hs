{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Static
Description : Generate static assests
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Create static HTML, CSS etc that can be placed a server somewhere
-}

module Camino.Display.Static (
    createCssFiles
  , createHelpFiles
) where

import Camino.Camino
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
import System.Directory
import System.FilePath
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Cassius
import Text.Hamlet

createCssFiles :: Config -> FilePath -> IO ()
createCssFiles config output = do
  let router = renderCaminoRoute config [rootLocale]
  let css = staticCss config
  let file = output </> "camino.css"
  createDirectoryIfMissing True output
  LTIO.writeFile file $ LT.concat (map (\c -> renderCss $ c router) css)

createHelpFile :: Config -> Locale -> FilePath -> HtmlUrlI18n CaminoMsg CaminoRoute -> IO ()
createHelpFile config loc file html = do
  let locales = [loc, rootLocale]
  let router = renderCaminoRoute config locales
  let messages = renderCaminoMsg config locales
  LB.writeFile file $ renderHtml $ html messages router
  
createStandAloneHelpFile :: Config -> Locale -> FilePath -> HtmlUrlI18n CaminoMsg CaminoRoute -> Text -> IO ()
createStandAloneHelpFile config loc file html title = createHelpFile config loc file (layoutHtml config (wildcardText title) Nothing html Nothing)

createHelpFiles :: Config -> FilePath -> IO ()
createHelpFiles config output = do
  let loc = localeFromIDOrError "en"
  createDirectoryIfMissing True output
  createStandAloneHelpFile config loc (output </> "help-en.html") $(ihamletFile "templates/help/help-en.hamlet") "Help"
  createHelpFile config loc (output </> "travel-help-en.html") $(ihamletFile "templates/help/travel-help-en.hamlet")
  createHelpFile config loc (output </> "range-help-en.html") $(ihamletFile "templates/help/range-help-en.hamlet")
  createHelpFile config loc (output </> "services-help-en.html") $(ihamletFile "templates/help/services-help-en.hamlet")
  createHelpFile config loc (output </> "routes-help-en.html") $(ihamletFile "templates/help/routes-help-en.hamlet")
  createHelpFile config loc (output </> "start-help-en.html") $(ihamletFile "templates/help/start-help-en.hamlet")
  createHelpFile config loc (output </> "stops-help-en.html") $(ihamletFile "templates/help/stops-help-en.hamlet")