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
  let router = renderCaminoRoute config [""]
  let css = staticCss config
  let file = output </> "camino.css"
  createDirectoryIfMissing True output
  LTIO.writeFile file $ LT.concat (map (\c -> renderCss $ c router) css)

createHelpFile :: Config -> Locale -> FilePath -> HtmlUrlI18n CaminoMsg CaminoRoute -> IO ()
createHelpFile config locale file html = do
  let router = renderCaminoRoute config [locale, ""]
  let messages = renderCaminoMsg config
  LB.writeFile file $ renderHtml $ html messages router
  
createStandAloneHelpFile :: Config -> Locale -> FilePath -> HtmlUrlI18n CaminoMsg CaminoRoute -> Text -> IO ()
createStandAloneHelpFile config locale file html title = createHelpFile config locale file (layoutHtml config title Nothing html Nothing)

createHelpFiles :: Config -> FilePath -> IO ()
createHelpFiles config output = do
  createDirectoryIfMissing True output
  createStandAloneHelpFile config "en" (output </> "help-en.html") $(ihamletFile "templates/help/help-en.hamlet") "Help"
  createHelpFile config "en" (output </> "fitness-help-en.html") $(ihamletFile "templates/help/fitness-help-en.hamlet")
  createHelpFile config "en" (output </> "range-help-en.html") $(ihamletFile "templates/help/range-help-en.hamlet")