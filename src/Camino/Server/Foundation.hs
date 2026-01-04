{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
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
import Camino.Planner (Solution)
import Camino.Preferences
import Camino.Display.I18n (CaminoMsg(..), renderCaminoMsg)
import qualified Camino.Config as C
import Data.Aeson
import qualified Data.ByteString.Lazy as LB (toStrict)
import Data.Cache (Cache(..), newDummyCache)
import Data.Localised (Locale, Tagged(..), TaggedLink(..), localeFromID, localeLanguageTag, localise, rootLocale)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Placeholder
import qualified Data.Set as S
import Data.Text (Text, concat, intercalate, isPrefixOf, pack, splitOn, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy (toStrict)
import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Cookie
import Yesod
import Yesod.Static (Static)
import Camino.Server.Settings (widgetFile)
import Text.Hamlet (HtmlUrlI18n, ihamletFile)
import Text.Read (readMaybe)
import Camino.Display.Routes (CaminoRoute, renderCaminoRoute)
import Data.Util (headWithDefault)
import Camino.Config (AssetConfig(assetPath))

-- | Generic placeholder date
placeholderDate :: Day
placeholderDate = fromGregorian 1970 1 1

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

instance PathPiece PoiCategory where
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

instance PathPiece PointOfInterest where
  fromPathPiece v = if v == "" then Nothing else Just $ placeholder v
  toPathPiece v = poiID v

instance PathPiece Location where
  fromPathPiece v = if v == "" then Nothing else Just $ placeholder v
  toPathPiece v = locationID v

instance PathPiece Camino.Camino.Route where
  fromPathPiece v = if v == "" then Nothing else Just $ placeholder  v
  toPathPiece v = routeID v

instance PathPiece Camino where
  fromPathPiece v = if v == "" then Nothing else Just $ placeholder v
  toPathPiece v = caminoId v

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
    caminoAppPort :: Int
  , caminoAppDevel :: Bool
  , caminoAppStatic :: Static
  , caminoAppFeature :: Static
  , caminoAppImage :: Static
  , caminoAppPlans :: Cache Text Solution
  , caminoAppConfig :: C.Config
  , caminoAppCaminoConfig :: CaminoConfig
}


-- | Get a list if the available full caminos
caminoAppCaminos :: CaminoApp -> [Camino]
caminoAppCaminos app = filter (not . caminoFragment) $ caminoConfigCaminos $ caminoAppCaminoConfig app

data PreferenceData = PreferenceData {
    prefEasyMode :: Bool -- ^ Use easy preferences
  , prefTravel :: Travel -- ^ The travel mode
  , prefFitness :: Fitness -- ^ The fitness level
  , prefComfort :: Comfort -- ^ The comfort level
  , prefDistance :: PreferenceRange Float -- ^ The distance travelled preferences
  , prefTime :: PreferenceRange Float -- ^ The time travelled preferences
  , prefRest :: PreferenceRange Int -- ^ The rest day preferences
  , prefRestPressure :: Maybe Float -- ^ The rest pressure
  , prefStop :: StopPreferences -- ^ The day's stop preferences
  , prefStockStop :: StopPreferences -- ^ The preferences for a 'stock-up' day
  , prefRestStop :: StopPreferences -- ^ The preferences for a rest day
  , prefPoiCategories :: S.Set PoiCategory -- ^ Types of stop-off you're interested in
  , prefCamino :: Camino -- ^ The camino to travel
  , prefRoutes :: S.Set Camino.Camino.Route -- ^ The chosen routes
  , prefStart :: Location -- ^ The start location
  , prefFinish :: Location -- ^ The finish location
  , prefStops :: S.Set Location -- ^ Any explcit stops
  , prefExcluded :: S.Set Location -- ^ Any explicit exclusions
  , prefRestPoints :: S.Set Location -- ^ Any preferred rest points
  , prefPois :: S.Set PointOfInterest -- ^ The points of interest
  , prefStartDate :: Day -- ^ The start date
} deriving (Show)

instance FromJSON PreferenceData where
   parseJSON (Object v) = do
      easy' <- v .:? "easy" .!= True
      travel' <- v .: "travel"
      fitness' <- v .: "fitness"
      comfort' <- v .: "comfort"
      distance' <- v .: "distance"
      time' <- v .: "time"
      rest' <- v .: "rest"
      restpressure' <- v .:? "rest-pressure"
      stop' <- v .: "stop"
      stockStop' <- v .: "stop-stock"
      restStop' <- v .: "stop-rest"
      poiCategories' <- v .: "poi-categories"
      camino' <- v .: "camino"
      routes' <- v .: "routes"
      start' <- v .: "start"
      finish' <- v .: "finish"
      stops' <- v .: "stops"
      excluded' <- v .: "excluded"
      rests' <- v .: "rest-points"
      pois' <- v .: "pois"
      startDate' <- v .:? "start-date" .!= placeholderDate
      let camino'' = placeholder camino'
      let routes'' = S.map placeholder routes'
      let start'' = placeholder start'
      let finish'' = placeholder finish'
      let stops'' = S.map placeholder stops'
      let excluded'' = S.map placeholder excluded'
      let rests'' = S.map placeholder rests'
      let pois'' = S.map placeholder pois'
      return PreferenceData {
          prefEasyMode = easy'
        , prefTravel = travel'
        , prefFitness = fitness'
        , prefComfort = comfort'
        , prefDistance = distance'
        , prefTime = time'
        , prefRest = rest'
        , prefRestPressure = restpressure'
        , prefStop = stop'
        , prefStockStop = stockStop'
        , prefRestStop = restStop'
        , prefPoiCategories = poiCategories'
        , prefCamino = camino''
        , prefRoutes = routes''
        , prefStart = start''
        , prefFinish = finish''
        , prefStops = stops''
        , prefExcluded = excluded''
        , prefRestPoints = rests''
        , prefPois = pois''
        , prefStartDate = startDate'
      }
   parseJSON v = error ("Unable to parse preferences data object " ++ show v)

instance ToJSON PreferenceData where
    toJSON prefs =
      object [
          "easy" .= prefEasyMode prefs
        , "travel" .= prefTravel prefs
        , "fitness" .= prefFitness prefs
        , "comfort" .= prefComfort prefs
        , "distance" .= prefDistance prefs
        , "time" .= prefTime prefs
        , "rest" .= prefRest prefs
        , "rest-pressure" .= prefRestPressure prefs
        , "stop" .= prefStop prefs
        , "stop-stock" .= prefStockStop prefs
        , "stop-rest" .= prefRestStop prefs
        , "poi-categories" .= prefPoiCategories prefs
        , "camino" .= (caminoId $ prefCamino prefs)
        , "routes" .= S.map routeID (prefRoutes prefs)
        , "start" .= (locationID $ prefStart prefs)
        , "finish" .= (locationID $ prefFinish prefs)
        , "stops" .= S.map locationID (prefStops prefs)
        , "excluded" .= S.map locationID (prefExcluded prefs)
        , "rest-points" .= S.map locationID (prefRestPoints prefs)
        , "pois" .= S.map poiID (prefPois prefs)
        , "start-date" .= prefStartDate prefs
      ]

defaultPreferenceData :: CaminoApp -> Day -> PreferenceData
defaultPreferenceData master current = let
    travel' = Walking
    fitness' = Unfit
    comfort' = Pilgrim
    dtp = defaultTravelPreferences travel' fitness' comfort' Nothing
    camino' = headWithDefault (error "At least one Camino required") $ caminoAppCaminos master
    dcp = defaultCaminoPreferences camino'
  in
    PreferenceData {
        prefEasyMode = True
      , prefTravel = travel'
      , prefFitness = fitness'
      , prefComfort = comfort'
      , prefDistance = preferenceDistance dtp
      , prefTime = preferenceTime dtp
      , prefRest = preferenceRest dtp
      , prefRestPressure = preferenceRestPressure dtp
      , prefStop = preferenceStop dtp
      , prefStockStop = preferenceStockStop dtp
      , prefRestStop = preferenceRestStop dtp
      , prefPoiCategories = preferencePoiCategories dtp
      , prefCamino = camino'
      , prefRoutes = preferenceRoutes dcp
      , prefStart = preferenceStart dcp
      , prefFinish = preferenceFinish dcp
      , prefStops = preferenceStops dcp
      , prefExcluded = preferenceExcluded dcp
      , prefRestPoints = preferenceRestPoints dcp
      , prefPois = preferencePois dcp
      , prefStartDate = current
    }

travelPreferencesFrom :: PreferenceData -> TravelPreferences
travelPreferencesFrom prefs = TravelPreferences {
    preferenceTravel = prefTravel prefs
  , preferenceFitness = prefFitness prefs
  , preferenceComfort = prefComfort prefs
  , preferenceDistance = prefDistance prefs
  , preferenceTime = prefTime prefs
  , preferenceRest = prefRest prefs
  , preferenceRestPressure = prefRestPressure prefs
  , preferenceStop = prefStop prefs
  , preferenceStockStop = prefStockStop prefs
  , preferenceRestStop = prefRestStop prefs
  , preferencePoiCategories = prefPoiCategories prefs
}


caminoPreferencesFrom :: PreferenceData -> CaminoPreferences
caminoPreferencesFrom prefs = CaminoPreferences {
    preferenceCamino = prefCamino prefs
  , preferenceStart = prefStart prefs
  , preferenceFinish = prefFinish prefs
  , preferenceRoutes = prefRoutes prefs
  , preferenceStops = prefStops prefs
  , preferenceExcluded = prefExcluded prefs
  , preferenceRestPoints = prefRestPoints prefs
  , preferencePois = prefPois prefs
  , preferenceStartDate = Just $ prefStartDate prefs
}

-- | Find stops that do not have rejected accommodation
permittedStops :: PreferenceData -> S.Set Location -> S.Set Location
permittedStops prefs locs = let
    stopAc = stopAccommodation $ prefStop prefs
    stockAc = stopAccommodation $ prefStockStop prefs
    restAc = stopAccommodation $ prefRestStop prefs
    allowed ac = (M.findWithDefault mempty ac stopAc /= Reject) ||
      (M.findWithDefault mempty ac stockAc /= Reject) ||
      (M.findWithDefault mempty ac restAc /= Reject)
  in
    S.filter (\l -> any (allowed . accommodationType) (locationAccommodation l)) locs

-- | Find acceptable rest points
permittedRestPoints :: PreferenceData -> S.Set Location -> S.Set Location
permittedRestPoints prefs locs = let
    restLoc = stopLocation $ prefRestStop prefs
    restAc = stopAccommodation $ prefRestStop prefs
    allowed lo = M.findWithDefault Reject (locationType lo) restLoc /= Reject &&
      any (\ac -> M.findWithDefault Reject (accommodationType ac) restAc /= Reject) (locationAccommodation lo)
   in
    S.filter allowed locs


mkYesodData "CaminoApp" $(parseRoutesFile "config/routes.yesodroutes")

mkMessage "CaminoApp" "messages" "en"

instance RenderMessage CaminoApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance RenderMessage CaminoApp CaminoMsg where
    renderMessage master langs msg = toStrict $ renderHtml $ renderCaminoMsg (caminoAppConfig master) (catMaybes $ map localeFromID langs) msg

instance Yesod CaminoApp where
  approot = ApprootMaster (C.getWebRoot . caminoAppConfig)

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
    let headLinks = catMaybes $ map (localise locales . C.links) (C.getLinks C.Header config)
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
                $maybe icons <- C.getAsset "icons" config
                 <a .navbar-brand .m-2 href="@{HomeR}">
                   <img width="64" height="64" src="#{C.assetPath icons}/tile-64.png" alt="#{render MsgAppName}">
                <h1>#{pageTitle pc}
                <button .navbar-toggler type="button" data-bs-toggle="collapse" data-bs-target="#navcol-links" aria-controls="navcol-links" aria-expanded="false" aria-label="Toggle navigation">
                  <span class="navbar-toggler-icon">
                <div .collapse .navbar-collapse .justify-content-end #navcol-links">
                  <ul .navbar-nav>
                    <li .nav-item>
                      <a .nav-link href=@{HomeR}>#{render MsgHomeLabel}
                    <li .nav-item>
                      <a .nav-link href=@{MapR} title="#{render MsgMapTitle}">#{render MsgMapLabel}
                    $forall link <- headLinks
                      <li .nav-item>
                        <a target="_blank" .nav-link href="#{linkText link}">#{plainText link}
                    <li .nav-item .dropdown>
                      <a .nav-link .dropdown-toggle href="#" role="button" data-bs-toggle="dropdown" aria-expanded="false">#{render MsgCaminosLabel}
                      <ul .dropdown-menu>
                        $forall camino <- caminoAppCaminos master
                          <li>
                            <a target="_blank" .nav-link .dropdown-item href="@{CaminoR (caminoId camino)}">#{caminoTitle camino}
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

noticePopupText :: Text -> [Locale] -> HtmlUrlI18n CaminoMsg CaminoRoute
noticePopupText licence [] = noticePopupText licence [rootLocale]
noticePopupText licence (locale':rest)
 | localeLanguageTag locale' == "" = $(ihamletFile "templates/notice/notice-en.hamlet")
 | localeLanguageTag locale' == "en" = $(ihamletFile "templates/notice/notice-en.hamlet")
 | otherwise = noticePopupText licence rest

noticePopup :: Handler Widget
noticePopup = do
  master <- getYesod
  locales <- getLocales
  let config = caminoAppConfig master
  let router = renderCaminoRoute config locales
  let messages = renderCaminoMsg config locales
  let licence = maybe "LICENSE" assetPath $ C.getAsset "license" config
  let notice = (noticePopupText licence locales) messages router
  return $(widgetFile "notice-popup")

-- Ensure secure cookies
defaultCookieSecure :: Handler Bool
defaultCookieSecure = do
  master <- getYesod
  return $ isPrefixOf "https:" (C.getWebRoot $ caminoAppConfig master)

-- | Update this as terms change
noticeVersion :: Text
noticeVersion = "Accept 1.0"

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
    secure <- defaultCookieSecure
    let cookie  = defaultSetCookie { 
        setCookieName = encodeUtf8 noticeCookie
      , setCookieValue = encodeUtf8 noticeVersion
      , setCookiePath = Just "/"
      , setCookieMaxAge = Just noticeAge 
      , setCookieSecure = secure
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
  secure <- defaultCookieSecure
  let enc = LB.toStrict $ encode preferences
  let cookie = defaultSetCookie { 
      setCookieName = encodeUtf8 preferencesCookie
    , setCookieValue = enc
    , setCookiePath = Just "/"
    , setCookieMaxAge = Just preferencesAge
    , setCookieSecure = secure
    , setCookieSameSite = Just sameSiteStrict
  }
  setCookie cookie

getLocales :: MonadHandler m => m [Locale]
getLocales = do
  langs <- languages
  return $ catMaybes $ map localeFromID langs