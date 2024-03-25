{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs          #-}
{-|
Module      : Foundation
Description : Yesod foundation for the Calixtinus application
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}

module Camino.Server.Foundation where

import Camino.Camino
import Camino.Display.I18n (CaminoMsg, renderCaminoMsg)
import Camino.Display.Routes (CaminoRoute(..))
import qualified Camino.Config as C
import Camino.Server.Settings
import Data.Aeson
import qualified Data.ByteString.Lazy as LB (toStrict)
import Data.Default.Class (def)
import Data.Maybe (isNothing)
import Data.Text (Text, unpack)
import Data.Text.Lazy (toStrict)
import Data.Yaml.Aeson (decodeEither)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Yesod
import Yesod.Default.Util (WidgetFileSettings, addStaticContentExternal, widgetFileNoReload, widgetFileReload)
import Yesod.Static (Static)
import Debug.Trace

data CaminoApp = CaminoApp {
    caminoAppPort :: Int
  , caminoAppDevel :: Bool
  , caminoAppStatic :: Static
  , caminoAppConfig :: C.Config
  , caminoAppCaminos :: [Camino]
}

mkYesodData "CaminoApp" $(parseRoutesFile "config/routes.yesodroutes")

mkMessage "CaminoApp" "messages" "en"

instance RenderMessage CaminoApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage CaminoApp CaminoMsg where
    renderMessage master _langs msg = toStrict $ renderHtml $ renderCaminoMsg (caminoAppConfig master) msg

instance Yesod CaminoApp where
  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    langs <- languages
    message <- getMessage
    render <- getMessageRender
    let config = caminoAppConfig master
    let micons = C.getAsset "icons" config
    let css = C.getAssets C.Css config
    let headLinks = C.getLocalisedLinks C.Header config langs
    let scriptsHeader = C.getAssets C.JavaScriptEarly config
    let scriptsFooter = C.getAssets C.JavaScript config
    let helpLabel = render MsgHelpLabel
    pc <- widgetToPageContent widget
    withUrlRenderer [hamlet|
      $doctype 5
      <html>
        <head>
          <title>#{pageTitle pc}
          <meta charset="utf-8">
          <meta name="viewport" content="width=device-width, initial-scale=1.0, shrink-to-fit=no">
          $maybe icons <- micons
            <link rel="icon" type="image/x-icon" href="#{C.assetPath icons}/favicon.ico">
          $forall c <- css
            <link rel="stylesheet" href="#{C.assetPath c}">
          $forall s <- scriptsHeader
            <script src="#{C.assetPath s}">
          ^{pageHead pc}
        <body>
          <header .p-2>
            <nav .navbar .navbar-expand-md>
              <div .container-fluid>
                <a .navbar-brand href="#">
                $maybe icons <- C.getAsset "icons" config
                  <a .m-2 href="@{HomeR}">
                    <img width="64" height="64" src="#{C.assetPath icons}/tile-64.png" alt="#{render MsgCaminoPlannerLabel}">
                <h1>#{pageTitle pc}
                <div .collapse .navbar-collapse .d-flex .justify-content-end #navcol-links">
                  <ul .navbar-nav>
                    $forall link <- headLinks
                      <li .nav-item>
                        <a .nav-item href="#{C.linkPath link}">#{C.linkLabel link}
                    <li .nav-item>
                      <a .nav-item href=@{HelpR}>#{helpLabel}
            $maybe msg <- message
              <div>#{msg}
          <main .p-2>
            ^{pageBody pc}
          <footer .text-center .py-4 .px-2>
            <div .row .row-cols-1 .row-cols-lg-3>
              <div .col>
                <p .text-muted .my-2>
                  <a href="https://github.com/charvolant/camino-planner">#{render MsgCaminoPlannerLabel}
              <div .col>
                <p .text-muted .my-2>
              <div .col>
                <p .text-muted .my-2>#{render MsgTestMessage}
           $forall s <- scriptsFooter
             <script src="#{C.assetPath s}">
   |]

decodeFromSession :: (FromJSON a) => Text -> Handler (Maybe a)
decodeFromSession key = do
  saved <- lookupSessionBS key
  return $ maybe Nothing (\v -> either (const Nothing) Just (eitherDecodeStrict v)) saved

encodeToSession :: (ToJSON a, Show a) => Text -> a -> Handler ()
encodeToSession key value = do
  let enc = LB.toStrict $ encode value
  setSessionBS key enc
