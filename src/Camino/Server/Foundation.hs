{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
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
import Camino.Display.I18n (CaminoMsg(..), renderCaminoMsg)
import qualified Camino.Config as C
import Data.Aeson
import qualified Data.ByteString.Lazy as LB (toStrict)
import Data.Localised (Locale, localeFromID, localeLanguageTag, rootLocale)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Placeholder
import qualified Data.Set as S
import Data.Text (Text, concat, intercalate, pack, splitOn, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Cookie
import Yesod
import Yesod.Static (Static)
import Camino.Server.Settings (widgetFile)
import Text.Hamlet (HtmlUrlI18n, ihamletFile)
import Text.Read (readMaybe)
import Camino.Display.Routes (CaminoRoute, renderCaminoRoute)


readValue :: (Read a) => Text -> Maybe a
readValue v = readMaybe $ unpack v

writeValue :: (Show a) => a -> Text
writeValue = pack . show

readMaybeValue :: (Read a) => Text -> Maybe (Maybe a)
readMaybeValue v = if v == "--" then Just Nothing else Just <$> readValue v

writeMaybeValue :: (Show a) => Maybe a -> Text
writeMaybeValue v = maybe "--" writeValue v


instance PathPiece LocationType where
  fromPathPiece = readValue
  toPathPiece = writeValue

instance PathPiece AccommodationType where
  fromPathPiece = readValue
  toPathPiece = writeValue

instance PathPiece Service where
  fromPathPiece = readValue
  toPathPiece = writeValue

instance PathPiece Travel where
  fromPathPiece = readValue
  toPathPiece = writeValue

instance PathPiece Fitness where
  fromPathPiece = readValue
  toPathPiece = writeValue

instance PathPiece Comfort where
  fromPathPiece = readValue
  toPathPiece = writeValue

instance PathPiece Float where
  fromPathPiece = readValue
  toPathPiece = writeValue

instance PathPiece Penance where
  fromPathPiece v = if v == "reject" then Just $ Reject else Penance <$> readValue v
  toPathPiece Reject = "reject"
  toPathPiece (Penance v) = writeValue v

instance (PathPiece a, PathPiece b) => PathPiece (a, b) where
  fromPathPiece v = case splitOn ":" v of
    [a, b] -> (,) <$> fromPathPiece a <*> fromPathPiece b
    _ -> Nothing
  toPathPiece (a, b) = Data.Text.concat [toPathPiece a, ":", toPathPiece b]

instance (PathPiece a) => PathPiece [a] where
  fromPathPiece v = case splitOn "|" v of
    [""] -> Just []
    ["--"] -> Just []
    sp -> let vals = map fromPathPiece sp in
      if any isNothing vals then Nothing else Just $ catMaybes vals
  toPathPiece v = if null v then "--" else intercalate "|" (map toPathPiece v)

-- Has to be specialised to avoid mis-typing
instance PathPiece (M.Map LocationType Penance) where
  fromPathPiece v = fmap M.fromList (fromPathPiece v)
  toPathPiece v = toPathPiece $ M.toList v

-- Has to be specialised to avoid mis-typing
instance PathPiece (M.Map AccommodationType Penance) where
  fromPathPiece v = fmap M.fromList (fromPathPiece v)
  toPathPiece v = toPathPiece $ M.toList v

-- Has to be specialised to avoid mis-typing
instance PathPiece (M.Map Service Penance) where
  fromPathPiece v = fmap M.fromList (fromPathPiece v)
  toPathPiece v = toPathPiece $ M.toList v

instance (Ord a, PathPiece a) => PathPiece (S.Set a) where
  fromPathPiece v = fmap S.fromList (fromPathPiece v)
  toPathPiece v = toPathPiece $ S.toList v

instance PathPiece Location where
  fromPathPiece v = if v == "" then Nothing else Just $ placeholder $ unpack v
  toPathPiece v = pack $ locationID v

instance PathPiece Camino.Camino.Route where
  fromPathPiece v = if v == "" then Nothing else Just $ placeholder $ unpack v
  toPathPiece v = pack $ routeID v

instance PathPiece Camino where
  fromPathPiece v = if v == "" then Nothing else Just $ placeholder $ unpack v
  toPathPiece v = pack $ caminoId v

instance (Show a, Read a) => PathPiece (PreferenceRange a) where
  fromPathPiece v =
    let
      vals = map readMaybeValue (splitOn "|" v)
      vals' = if all isJust vals then map fromJust vals else []
    in
      if length vals' == 5 then
        Just (PreferenceRange Nothing (fromJust $ vals' !! 0) (fromJust $ vals' !! 1) (fromJust $ vals' !! 2) (vals' !! 3) (vals' !! 4))
      else
        Nothing
  toPathPiece (PreferenceRange _derivation'  target'  lower' upper' min' max') = intercalate "|" $ map writeMaybeValue
    [
      Just target',
      Just lower',
      Just upper',
      min',
      max'
    ]

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
  , prefComfort :: Comfort -- ^ The comfort level
  , prefDistance :: PreferenceRange Float -- ^ The distance travelled preferences
  , prefTime :: PreferenceRange Float -- ^ The time travelled preferences
  , prefLocation :: M.Map LocationType Penance -- ^ The location type preferences
  , prefAccommodation :: M.Map AccommodationType Penance -- ^ The accommodation type preferences
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
      comfort' <- v .: "comfort"
      distance' <- v .: "distance"
      time' <- v .: "time"
      location' <- v .: "location"
      accommodation' <- v .: "accommodation"
      stopServices' <- v .: "stop-services"
      dayServices' <- v .: "day-services"
      camino' <- v .: "camino"
      routes' <- v .: "routes"
      start' <- v .: "start"
      finish' <- v .: "finish"
      stops' <- v .: "stops"
      excluded' <- v .: "excluded"
      let camino'' = placeholder camino'
      let routes'' = S.map placeholder routes'
      let start'' = placeholder start'
      let finish'' = placeholder finish'
      let stops'' = S.map placeholder stops'
      let excluded'' = S.map placeholder excluded'
      return PreferenceData {
          prefTravel = travel'
        , prefFitness = fitness'
        , prefComfort = comfort'
        , prefDistance = distance'
        , prefTime = time'
        , prefLocation = location'
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
        , "comfort" .= prefComfort prefs
        , "distance" .= prefDistance prefs
        , "time" .= prefTime prefs
        , "location" .= prefLocation prefs
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
    comfort' = Pilgrim
    dtp = defaultTravelPreferences travel' fitness' comfort'
    camino' = head $ caminoAppCaminos master
    dcp = defaultCaminoPreferences camino'
  in
    PreferenceData {
        prefTravel = travel'
      , prefFitness = fitness'
      , prefComfort = comfort'
      , prefDistance = preferenceDistance dtp
      , prefTime = preferenceTime dtp
      , prefLocation = preferenceLocation dtp
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
    preferenceTravel = prefTravel prefs
  , preferenceFitness = prefFitness prefs
  , preferenceComfort = prefComfort prefs
  , preferenceDistance = prefDistance prefs
  , preferenceTime = prefTime prefs
  , preferenceLocation = prefLocation prefs
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

-- | Find stops that do not have rejected accommodation
permittedStops :: PreferenceData -> S.Set Location -> S.Set Location
permittedStops prefs locs = let
    accommodation = prefAccommodation prefs
    allowed ac = maybe mempty id (M.lookup ac accommodation) /= Reject
  in
    S.filter (\l -> any (allowed . accommodationType) (locationAccommodation l)) locs


mkYesodData "CaminoApp" $(parseRoutesFile "config/routes.yesodroutes")

mkMessage "CaminoApp" "messages" "en"

instance RenderMessage CaminoApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage CaminoApp CaminoMsg where
    renderMessage master langs msg = toStrict $ renderHtml $ renderCaminoMsg (caminoAppConfig master) (catMaybes $ map localeFromID langs) msg

instance Yesod CaminoApp where
  approot = ApprootMaster caminoAppRoot

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    locales <- getLocales
    message <- getMessage
    render <- getMessageRender
    cn <- checkNotice
    let notice = not cn
    let config = caminoAppConfig master
    let micons = C.getAsset "icons" config
    let css = C.getAssets C.Css config
    let headLinks = C.getLocalisedLinks C.Header config locales
    let scriptsHeader = C.getAssets C.JavaScriptEarly config
    let scriptsFooter = C.getAssets C.JavaScript config
    let helpLabel = render MsgHelpLabel
    let caminoTitle c = renderCaminoMsg config locales (Txt (caminoName c))
    pc <- widgetToPageContent widget
    np <- noticePopup
    nc <- widgetToPageContent np
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
          $if notice
            ^{pageHead nc}
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
                        <a target="_blank" .nav-link href="#{C.linkPath link}">#{C.linkLabel link}
                    <li .nav-item .dropdown>
                      <a .nav-link .dropdown-toggle href="#" role="button" data-bs-toggle="dropdown" aria-expanded="false">#{render MsgCaminosLabel}
                      <ul .dropdown-menu>
                        $forall camino <- caminoAppCaminos master
                          <li>
                            <a target="_blank" .nav-link .dropdown-item href="@{CaminoR (pack $ caminoId camino)}">#{caminoTitle camino}
                    <li .nav-item>
                      <a target="_blank" .nav-link href=@{HelpR} title="#{render MsgHelpTitle}">#{helpLabel}
            $maybe msg <- message
              <div>#{msg}
          <main .container-fluid .p-2>
            ^{pageBody pc}
          <footer .text-center .py-4 .px-2>
            <div .row .row-cols-1 .row-cols-lg-3>
              <div .col>
                <p .text-muted .my-2>
                  <a target="_blank" href="@{AboutR}">#{render MsgAboutLabel}
              <div .col>
                <p .text-muted .my-2>#{render MsgTestMessage}
              <div .col>
                <p .text-muted .my-2>
                  <a target="_blank" href="@{MetricR}" title="#{render MsgMetricTitle}">#{render MsgMetricLabel}
          $if notice
            ^{pageBody nc}
          $forall s <- scriptsFooter
            <script src="#{C.assetPath s}">
   |]

noticePopupText :: [Locale] -> HtmlUrlI18n CaminoMsg CaminoRoute
noticePopupText [] = noticePopupText [rootLocale]
noticePopupText (locale:rest)
 | localeLanguageTag locale == "" = $(ihamletFile "templates/notice/notice-en.hamlet")
 | localeLanguageTag locale == "en" = $(ihamletFile "templates/notice/notice-en.hamlet")
 | otherwise = noticePopupText rest

noticePopup :: Handler Widget
noticePopup = do
  master <- getYesod
  locales <- getLocales
  let config = caminoAppConfig master
  let router = renderCaminoRoute config locales
  let messages = renderCaminoMsg config locales
  let notice = (noticePopupText locales) messages router
  return $(widgetFile "notice-popup")

-- | Update this as terms change
noticeVersion :: Text
noticeVersion = "Accept 0.4"

noticeCookie :: Text
noticeCookie = "de-calixtinus-notice"

noticeAge :: DiffTime
noticeAge = secondsToDiffTime $ 182 * 24 * 60 * 60 -- About half a year in seconds

preferencesCookie :: Text
preferencesCookie = "de-calixtinue-preferences"

preferencesAge :: DiffTime
preferencesAge = secondsToDiffTime $ 91 * 24 * 60 * 60 -- About a quarter of a year in seconds

-- | Check to see if the notice has been accepted
checkNotice :: Handler Bool
checkNotice = do
  notice <- lookupCookie noticeCookie
  return $ maybe False ( == noticeVersion) notice

-- | Set the notice, based on the
setNotice :: Bool -> Handler ()
setNotice accept =
  if accept then do
    let cookie  = defaultSetCookie { 
        setCookieName = encodeUtf8 noticeCookie
      , setCookieValue = encodeUtf8 noticeVersion
      , setCookiePath = Just "/"
      , setCookieMaxAge = Just noticeAge 
      , setCookieSecure = True
      , setCookieSameSite = Just sameSiteStrict
    }
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
  let cookie = defaultSetCookie { 
      setCookieName = encodeUtf8 preferencesCookie
    , setCookieValue = enc
    , setCookiePath = Just "/"
    , setCookieMaxAge = Just preferencesAge
    , setCookieSecure = True
    , setCookieSameSite = Just sameSiteStrict
  }
  setCookie cookie

getLocales :: MonadHandler m => m [Locale]
getLocales = do
  langs <- languages
  return $ catMaybes $ map localeFromID langs