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
import Camino.Preferences
import Camino.Display.I18n (CaminoMsg, renderCaminoMsg)
import Camino.Display.Routes (CaminoRoute(..))
import qualified Camino.Config as C
import Camino.Server.Settings
import Data.Aeson
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as LB (toStrict)
import Data.Default.Class (def)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import qualified Data.Set as S
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Yaml.Aeson (decodeEither)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Cookie
import Yesod
import Yesod.Default.Util (WidgetFileSettings, addStaticContentExternal, widgetFileNoReload, widgetFileReload)
import Yesod.Static (Static)
import Debug.Trace

data CaminoApp = CaminoApp {
    caminoAppRoot :: Text
  , caminoAppPort :: Int
  , caminoAppDevel :: Bool
  , caminoAppStatic :: Static
  , caminoAppConfig :: C.Config
  , caminoAppCaminos :: [Camino]
}


data PreferenceData = PreferenceData {
    prefTravel :: Travel -- ^ The travel mode
  , prefFitness :: Fitness -- ^ The fitness level
  , prefDistance :: PreferenceRange Float -- ^ The distance travelled preferences
  , prefTime :: PreferenceRange Float -- ^ The time travelled preferences
  , prefStop :: Penance -- ^ The stop cost
  , prefAccommodation :: M.Map AccommodationType Penance -- ^ The accomodation type preferences
  , prefStopServices :: M.Map Service Penance -- ^ The day's end service preferences
  , prefDayServices :: M.Map Service Penance -- ^ The during-day service preferences
  , prefCamino :: Camino -- ^ The camino to travel
  , prefRoutes :: S.Set Camino.Camino.Route -- ^ The chosen routes
  , prefStart :: Location -- ^ The start location
  , prefFinish :: Location -- ^ The finish location
  , prefStops :: S.Set Location -- ^ Any explcit stops
  , prefExcluded :: S.Set Location -- ^ Any explicit exclusions
} deriving (Show)

instance FromJSON PreferenceData where
   parseJSON (Object v) = do
      travel' <- v .: "travel"
      fitness' <- v .: "fitness"
      distance' <- v .: "distance"
      time' <- v .: "time"
      stop' <- v .: "stop"
      accommodation' <- v .: "accommodation"
      stopServices' <- v .: "stop-services"
      dayServices' <- v .: "day-services"
      camino' <- v .: "camino"
      routes' <- v .: "routes"
      start' <- v .: "start"
      finish' <- v .: "finish"
      stops' <- v .: "stops"
      excluded' <- v .: "excluded"
      let camino'' = placeholderCamino camino'
      let routes'' = S.map placeholderRoute routes'
      let start'' = placeholderLocation start'
      let finish'' = placeholderLocation finish'
      let stops'' = S.map placeholderLocation stops'
      let excluded'' = S.map placeholderLocation excluded'
      return PreferenceData {
          prefTravel = travel'
        , prefFitness = fitness'
        , prefDistance = distance'
        , prefTime = time'
        , prefStop = stop'
        , prefAccommodation = accommodation'
        , prefStopServices = stopServices'
        , prefDayServices = dayServices'
        , prefCamino = camino''
        , prefRoutes = routes''
        , prefStart = start''
        , prefFinish = finish''
        , prefStops = stops''
        , prefExcluded = excluded''
      }
   parseJSON v = error ("Unable to parse preferences data object " ++ show v)

instance ToJSON PreferenceData where
    toJSON prefs =
      object [ 
          "travel" .= prefTravel prefs
        , "fitness" .= prefFitness prefs
        , "distance" .= prefDistance prefs
        , "time" .= prefTime prefs
        , "stop" .= prefStop prefs
        , "accommodation" .= prefAccommodation prefs
        , "stop-services" .= prefStopServices prefs
        , "day-services" .= prefDayServices prefs
        , "camino" .= (caminoId $ prefCamino prefs)
        , "routes" .= (S.map routeID (prefRoutes prefs))
        , "start" .= (locationID $ prefStart prefs)
        , "finish" .= (locationID $ prefFinish prefs)
        , "stops" .= (S.map locationID (prefStops prefs))
        , "excluded" .= (S.map locationID (prefExcluded prefs))
      ]

defaultPreferenceData :: CaminoApp -> PreferenceData
defaultPreferenceData master = let
    travel' = Walking
    fitness' = Unfit
    dtp = defaultTravelPreferences travel' fitness'
    camino' = head $ caminoAppCaminos master
    dcp = defaultCaminoPreferences camino'
  in
    PreferenceData {
        prefTravel = travel'
      , prefFitness = fitness'
      , prefDistance = preferenceDistance dtp
      , prefTime = preferenceTime dtp
      , prefStop = preferenceStop dtp
      , prefAccommodation = preferenceAccommodation dtp
      , prefStopServices = preferenceStopServices dtp
      , prefDayServices = preferenceDayServices dtp
      , prefCamino = camino'
      , prefRoutes = preferenceRoutes dcp
      , prefStart = preferenceStart dcp
      , prefFinish = preferenceFinish dcp
      , prefStops = preferenceStops dcp
      , prefExcluded = preferenceExcluded dcp
    }

travelPreferencesFrom :: PreferenceData -> TravelPreferences
travelPreferencesFrom prefs = TravelPreferences {
    preferenceTravelFunction = prefTravel prefs
  , preferenceFitness = prefFitness prefs
  , preferenceDistance = prefDistance prefs
  , preferencePerceivedDistance = perceivedDistanceRange (prefFitness prefs) (prefDistance prefs)
  , preferenceTime = prefTime prefs
  , preferenceStop = prefStop prefs
  , preferenceAccommodation = prefAccommodation prefs
  , preferenceStopServices = prefStopServices prefs
  , preferenceDayServices = prefDayServices prefs
}


caminoPreferencesFrom :: PreferenceData -> CaminoPreferences
caminoPreferencesFrom prefs = CaminoPreferences {
    preferenceCamino = prefCamino prefs
  , preferenceStart = prefStart prefs
  , preferenceFinish = prefFinish prefs
  , preferenceRoutes = prefRoutes prefs
  , preferenceStops = prefStops prefs
  , preferenceExcluded = prefExcluded prefs
}

-- | Find stops that do not have rejected accomodation
permittedStops :: PreferenceData -> S.Set Location -> S.Set Location
permittedStops prefs locs = let
    accommodation = prefAccommodation prefs
    allowed ac = maybe mempty id (M.lookup ac accommodation) /= Reject
    campingAllowed = allowed Camping -- Anything goes
  in
    S.filter (\l -> any (allowed . accommodationType) (locationAccommodation l)) locs


mkYesodData "CaminoApp" $(parseRoutesFile "config/routes.yesodroutes")

mkMessage "CaminoApp" "messages" "en"

instance RenderMessage CaminoApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage CaminoApp CaminoMsg where
    renderMessage master _langs msg = toStrict $ renderHtml $ renderCaminoMsg (caminoAppConfig master) msg

instance Yesod CaminoApp where
  approot = ApprootMaster caminoAppRoot
  
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
                    <img width="64" height="64" src="#{C.assetPath icons}/tile-64.png" alt="#{render MsgAppName}">
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
                  <a href="@{AboutR}">#{render MsgAboutLabel}
              <div .col>
                <p .text-muted .my-2>
              <div .col>
                <p .text-muted .my-2>#{render MsgTestMessage}
           $forall s <- scriptsFooter
             <script src="#{C.assetPath s}">
   |]


noticeCookie :: Text
noticeCookie = "de-calixtinus-notice"

preferencesCookie :: Text
preferencesCookie = "de-calixtinue-preferences"

-- | Check to see if the notice has been accepted
checkNotice :: Handler Bool
checkNotice = do
  notice <- lookupCookie noticeCookie
  return $ maybe False ( == "Accept") notice

-- | Set the notice, based on the 
setNotice :: Bool -> Handler ()
setNotice accept =
  if accept then do
    let cookie  = defaultSetCookie { setCookieName = encodeUtf8 noticeCookie, setCookieValue = "Accept" }
    setCookie cookie
  else
    deleteCookie noticeCookie "/"
  
decodePreferences :: Handler (Maybe PreferenceData)
decodePreferences = do
  saved <- lookupCookie preferencesCookie
  return $ maybe Nothing (\v -> either (const Nothing) Just (eitherDecodeStrict $ encodeUtf8 v)) saved

encodePreferences :: PreferenceData -> Handler ()
encodePreferences preferences = do
  let enc = LB.toStrict $ encode preferences
  let cookie = defaultSetCookie { setCookieName = encodeUtf8 preferencesCookie, setCookieValue = enc }
  setCookie cookie
