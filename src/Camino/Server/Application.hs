{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-|
Module      : Application
Description : De Calixtinus yesod application
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Yesod application that allows the user to enter preferences and have a route generated.
-}

module Camino.Server.Application where

import Camino.Camino
import Camino.Config (AssetConfig(..), getAsset, getWebRoot)
import Camino.Planner (Solution(..), Pilgrimage, normaliseSolution, planCamino)
import Camino.Preferences
import Camino.Display.Html
import Camino.Display.I18n
import Camino.Display.KML
import Camino.Display.Routes
import Camino.Display.XLSX
import Camino.Server.Forms
import Camino.Server.Foundation
import Camino.Server.Settings
import Codec.Xlsx
import Data.Cache
import Data.DublinCore
import Data.Localised
import Data.Maybe (isJust)
import Data.Metadata
import Data.Text (Text, unpack, pack)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Util
import Data.UUID (toText)
import Data.UUID.V4
import Graph.Graph
import Text.Hamlet
import Text.Read (readMaybe)
import Text.XML
import Yesod

mkYesodDispatch "CaminoApp" resourcesCaminoApp

data PreferenceStep =
    TravelStep
  | RangeStep
  | ServicesStopStep
  | ServicesStockStep
  | ServicesRestStep
  | CaminoStep
  | RoutesStep
  | StartStep
  | StopsStep
  | PoiStep
  | ShowPreferencesStep
  | PlanStep
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

homeP :: Handler Html
homeP = do
  master <- getYesod
  (help, helpPop) <- helpPopup $ Just $(ihamletFile "templates/help/homepage-help-en.hamlet")
  let mapUrl = maybe "" assetPath (getAsset "images" (caminoAppConfig master)) <> "/Map.png"
  defaultLayout $ do
    setTitleI MsgAppName
    toWidget helpPop
    $(widgetFile "homepage")

getNoticeR :: Handler Text
getNoticeR = do
  accepted <- lookupGetParam "accepted"
  let accepted' = maybe False ( == "true") accepted
  setNotice accepted'
  return $ pack $ "Accepted " ++ (show accepted')

getHelpR :: Handler Html
getHelpR = do
  master <- getYesod
  locales <- getLocales
  let config = caminoAppConfig master
  let router = renderCaminoRoute config locales
  let messages = renderCaminoMsg config locales
  defaultLayout $ do
    setTitleI MsgHelpTitle
    toWidget ((helpWidget locales) messages router)

-- | Help for the languages that we have
helpWidget :: [Locale] -> HtmlUrlI18n CaminoMsg CaminoRoute
helpWidget [] = helpWidget [rootLocale]
helpWidget (loc:rest)
 | localeLanguageTag loc == "" = $(ihamletFile "templates/help/help-en.hamlet")
 | localeLanguageTag loc == "en" = $(ihamletFile "templates/help/help-en.hamlet")
 | otherwise = helpWidget rest

getCaminoR :: Text -> Handler Html
getCaminoR cid = do
  master <- getYesod
  let camino = findCaminoById (caminoAppCaminoConfig master) cid
  case camino of
    Nothing -> do
      setMessageI MsgInvalidCamino
      getHomeR
    Just camino' ->
      caminoPage camino'

caminoPage :: Camino -> Handler Html
caminoPage camino = do
    master <- getYesod
    locales <- getLocales
    time <- liftIO getCurrentTime
    let current = utctDay time
    let cprefs = (defaultCaminoPreferences camino) { preferenceStartDate = Just current }
    let config = caminoAppConfig master
    let router = renderCaminoRoute config locales
    let messages = renderCaminoMsg config locales
    let html = (caminoHtmlSimple config cprefs) messages router
    defaultLayout $ do
      setTitle [shamlet|#{localiseText locales $ caminoName (preferenceCamino cprefs)}|]
      (toWidget html)
      imagePopup

getMapR :: Handler Html
getMapR = do
  master <- getYesod
  locales <- getLocales
  let config = caminoAppConfig master
  let router = renderCaminoRoute config locales
  let messages = renderCaminoMsg config locales
  defaultLayout $ do
    setTitleI MsgMapTitle
    toWidget ((mapWidget (caminoAppCaminos master)) messages router)

-- | Map of all caminos
mapWidget :: [Camino] -> HtmlUrlI18n CaminoMsg CaminoRoute
mapWidget caminos = $(ihamletFile "templates/map.hamlet")

getMetricR :: Handler Html
getMetricR = do
  master <- getYesod
  locales <- getLocales
  let config = caminoAppConfig master
  let router = renderCaminoRoute config locales
  let messages = renderCaminoMsg config locales
  defaultLayout $ do
    setTitleI MsgMetricTitle
    toWidget ((metricWidget locales) messages router)

-- | Help for the languages that we have
metricWidget :: [Locale] -> HtmlUrlI18n CaminoMsg CaminoRoute
metricWidget [] = metricWidget [rootLocale]
metricWidget (loc:rest)
 | localeLanguageTag loc == "" = $(ihamletFile "templates/help/metric-en.hamlet")
 | localeLanguageTag loc == "en" = $(ihamletFile "templates/help/metric-en.hamlet")
 | otherwise = metricWidget rest

getAboutR :: Handler Html
getAboutR = do
  master <- getYesod
  locales <- getLocales
  let config = caminoAppConfig master
  let router = renderCaminoRoute config locales
  let messages = renderCaminoMsg config locales
  defaultLayout $ do
    setTitleI MsgAboutTitle
    toWidget ((aboutWidget locales) messages router)

-- | About text for the languages that we have
aboutWidget :: [Locale] -> HtmlUrlI18n CaminoMsg CaminoRoute
aboutWidget [] = aboutWidget [rootLocale]
aboutWidget (loc:rest)
 | localeLanguageTag loc == "" = $(ihamletFile "templates/about/about-en.hamlet")
 | localeLanguageTag loc == "en" = $(ihamletFile "templates/about/about-en.hamlet")
 | otherwise = aboutWidget rest

getHomeR :: Handler Html
getHomeR = do
  homeP

postMakePlanR :: Handler Html
postMakePlanR = do
  stepp <- lookupPostParam "_step"
  case maybe Nothing (readMaybe . unpack) stepp of
    Just step' -> do
      ((result, widget), enctype) <- runFormPost $ (stepForm step' blankHelp) Nothing
      case result of
          FormSuccess prefs -> do
             encodePreferences prefs
             planPage prefs
          FormFailure errs -> do
            setMessage [shamlet|
              $forall err <- errs
                <div .text-danger>#{err}
            |]
            stepPage ShowPreferencesStep PlanStep Nothing blankHelp widget enctype
          _ ->
            getHomeR
    _ ->
      invalidArgs ["Bad preferences step"]


getPlanR :: Text -> Handler Html
getPlanR sid = do
  master <- getYesod
  msolution <- liftIO $ do
    let cache = caminoAppPlans master
    msolution' <- cacheLookup cache sid
    return $ normaliseSolution (caminoAppCaminoConfig master) <$> msolution'
  case msolution of
    Nothing -> do
      setMessage [shamlet|
        <div ..alert .alert-warning role="alert">
          Can't find #{sid}
        |]
      notFound
    Just solution ->
      showPage solution

getPlanKmlR :: Text -> Handler TypedContent
getPlanKmlR sid = do
  master <- getYesod
  msolution <- liftIO $ do
    let cache = caminoAppPlans master
    msolution' <- cacheLookup cache sid
    return $ normaliseSolution (caminoAppCaminoConfig master) <$> msolution'
  case msolution of
    Nothing -> do
      setMessage [shamlet|
        <div ..alert .alert-warning role="alert">
          Can't find #{sid}
        |]
      notFound
    Just solution ->
      showKml solution

getPlanXlsxR :: Text -> Handler TypedContent
getPlanXlsxR sid = do
  master <- getYesod
  msolution <- liftIO $ do
    let cache = caminoAppPlans master
    msolution' <- cacheLookup cache sid
    return $ normaliseSolution (caminoAppCaminoConfig master) <$> msolution'
  case msolution of
    Nothing -> do
      setMessage [shamlet|
        <div ..alert .alert-warning role="alert">
          Can't find #{sid}
        |]
      notFound
    Just solution ->
      showXlsx solution


getPreferencesR :: Handler Html
getPreferencesR = do
    master <- getYesod
    prefs <- decodePreferences
    time <- liftIO getCurrentTime
    let current = utctDay time
    let prefs' = maybe (defaultPreferenceData master current) id prefs
    (embedded, help) <- preferenceHelpPopup TravelStep
    ((_result, widget), enctype) <- runFormPost $ chooseTravelForm embedded (Just prefs')
    stepPage TravelStep RangeStep Nothing help widget enctype

postPreferencesR :: Handler Html
postPreferencesR = do
  stepp <- lookupPostParam "_step"
  nextp <- lookupPostParam "_next"
  let step' = maybe Nothing (readMaybe . unpack) stepp
  case step' of
    Nothing ->
      getPreferencesR
    Just step'' ->
      nextStep step'' nextp

nextStep :: PreferenceStep -> Maybe Text -> Handler Html
nextStep stepp dir = do
  (embedded, help) <- preferenceHelpPopup stepp
  ((result, widget), enctype) <- runFormPost $ (stepForm stepp embedded) Nothing
  case result of
      FormSuccess prefs -> do
        let easy = prefEasyMode prefs
        let nextp = if maybe False (== "next") dir then stepForward easy stepp else stepBackward easy stepp
        let nextp' = stepForward easy nextp
        (embedded', help') <- preferenceHelpPopup nextp
        (widget', enctype') <- generateFormPost $ (stepForm nextp embedded') (Just prefs)
        stepPage nextp nextp' (Just prefs) help' widget' enctype'
      FormFailure errs -> do
        setMessage [shamlet|
          $forall err <- errs
            <div .text-danger>#{err}
        |]
        stepPage stepp (stepForward True stepp) Nothing help widget enctype
      _ ->
        getHomeR

stepForm :: PreferenceStep -> Widget -> Maybe PreferenceData -> (Html -> MForm Handler (FormResult PreferenceData, Widget))
stepForm TravelStep help prefs = chooseTravelForm help prefs
stepForm RangeStep help prefs = chooseRangeForm help prefs
stepForm ServicesStopStep help prefs = chooseStopServicesForm help prefs
stepForm ServicesStockStep help prefs = chooseStockServicesForm help prefs
stepForm ServicesRestStep help prefs = chooseRestServicesForm help prefs
stepForm CaminoStep help prefs = chooseCaminoForm help prefs
stepForm RoutesStep help prefs = chooseRoutesForm help prefs
stepForm StartStep help prefs = chooseStartForm help prefs
stepForm StopsStep help prefs = chooseStopsForm help prefs
stepForm PoiStep help prefs = choosePoiForm help prefs
stepForm ShowPreferencesStep help prefs = confirmPreferencesForm help prefs
stepForm PlanStep help prefs = confirmPreferencesForm help prefs -- Shouldn't end up here, so go and confirm it

stepForward :: Bool -> PreferenceStep -> PreferenceStep
stepForward False TravelStep = RangeStep
stepForward True TravelStep = CaminoStep
stepForward _ RangeStep = ServicesStopStep
stepForward _ ServicesStopStep = ServicesStockStep
stepForward _ ServicesStockStep = ServicesRestStep
stepForward _ ServicesRestStep = CaminoStep
stepForward _ CaminoStep = RoutesStep
stepForward _ RoutesStep = StartStep
stepForward False StartStep = StopsStep
stepForward True StartStep = PlanStep
stepForward _ StopsStep = PoiStep
stepForward _ PoiStep = ShowPreferencesStep
stepForward _ ShowPreferencesStep = PlanStep
stepForward _ PlanStep = ShowPreferencesStep

stepBackward :: Bool -> PreferenceStep -> PreferenceStep
stepBackward _ TravelStep = TravelStep
stepBackward _ RangeStep = TravelStep
stepBackward _ ServicesStopStep = RangeStep
stepBackward _ ServicesStockStep = ServicesStopStep
stepBackward _ ServicesRestStep = ServicesStockStep
stepBackward False CaminoStep = ServicesRestStep
stepBackward True CaminoStep = TravelStep
stepBackward _ RoutesStep = CaminoStep
stepBackward _ StartStep = RoutesStep
stepBackward _ StopsStep = StartStep
stepBackward _ PoiStep = StopsStep
stepBackward _ ShowPreferencesStep = PoiStep
stepBackward _ PlanStep = ShowPreferencesStep

stepPage' :: CaminoAppMessage -> CaminoAppMessage -> Maybe CaminoAppMessage -> Maybe CaminoAppMessage -> PreferenceStep -> PreferenceStep -> Maybe Widget -> Widget -> Widget -> Enctype -> Handler Html
stepPage' title top1 top2 bottom stepp nextp display help widget enctype = do
  defaultLayout $ do
    setTitleI title
    toWidget help
    $(widgetFile "step")
    imagePopup

stepPage :: PreferenceStep -> PreferenceStep -> Maybe PreferenceData -> Widget -> Widget -> Enctype -> Handler Html
stepPage TravelStep nextp _ help widget enctype = stepPage' MsgTravelTitle MsgTravelText1 (Just MsgTravelText2) (Just MsgTravelBottom) TravelStep nextp Nothing help widget enctype
stepPage RangeStep nextp _ help widget enctype = stepPage' MsgRangeTitle MsgRangeText Nothing Nothing RangeStep nextp Nothing help widget enctype
stepPage ServicesStopStep nextp _ help widget enctype = stepPage' MsgServicesStopTitle MsgServicesStopText (Just MsgServicesText2) Nothing ServicesStopStep nextp Nothing help widget enctype
stepPage ServicesStockStep nextp _ help widget enctype = stepPage' MsgServicesStockTitle MsgServicesStockText (Just MsgServicesText2) Nothing ServicesStockStep nextp Nothing help widget enctype
stepPage ServicesRestStep nextp _ help widget enctype = stepPage' MsgServicesRestTitle MsgServicesRestText (Just MsgServicesText2) Nothing ServicesRestStep nextp Nothing help widget enctype
stepPage CaminoStep nextp _ help widget enctype = stepPage' MsgCaminoTitle MsgCaminoText Nothing Nothing CaminoStep nextp Nothing help widget enctype
stepPage RoutesStep nextp _ help widget enctype = stepPage' MsgRoutesTitle MsgRoutesText Nothing Nothing RoutesStep nextp Nothing help widget enctype
stepPage StartStep nextp _ help widget enctype = stepPage' MsgStartTitle MsgStartText Nothing Nothing StartStep nextp Nothing help widget enctype
stepPage StopsStep nextp _ help widget enctype = stepPage' MsgStopsTitle MsgStopsText Nothing Nothing StopsStep nextp Nothing help widget enctype
stepPage PoiStep nextp _ help widget enctype = stepPage' MsgPoiTitle MsgPoiText Nothing Nothing PoiStep nextp Nothing help widget enctype
stepPage ShowPreferencesStep nextp (Just prefs) help widget enctype = do
    master <- getYesod
    locales <- getLocales
    let config = caminoAppConfig master
    let router = renderCaminoRoute config locales
    let messages = renderCaminoMsg config locales
    let preferences = travelPreferencesFrom prefs
    let camino = caminoPreferencesFrom prefs
    let display = toWidget $ (preferencesHtml False preferences camino) messages router
    stepPage' MsgShowPreferencesTitle MsgShowPreferencesText Nothing Nothing ShowPreferencesStep nextp (Just display) help widget enctype
stepPage ShowPreferencesStep nextp _ help widget enctype = stepPage' MsgShowPreferencesTitle MsgShowPreferencesText Nothing Nothing ShowPreferencesStep nextp Nothing help widget enctype
stepPage PlanStep nextp prefs help widget enctype = stepPage ShowPreferencesStep nextp prefs help widget enctype


-- For use where a help widget is required but not present
blankHelp :: Widget
blankHelp = [whamlet||]

helpPopup :: Maybe (HtmlUrlI18n CaminoMsg CaminoRoute) -> Handler (Widget, Widget)
helpPopup help' = do
  master <- getYesod
  locales <- getLocales
  let config = caminoAppConfig master
  let router = renderCaminoRoute config locales
  let messages = renderCaminoMsg config locales
  let help'' = (\h -> h messages router) <$> help'
  return $ case help'' of
    Nothing -> (
         blankHelp
       , blankHelp
      )
    Just help -> (
           [whamlet|
            <a .text-primary href="#" onclick="showHelpPopup()" title="_{MsgMoreInformation}">
              <span .ca-help>
           |]
        , $(widgetFile "help-popup")
      )


preferenceHelpPopup' :: PreferenceStep -> [Locale] -> Maybe (HtmlUrlI18n CaminoMsg CaminoRoute)
preferenceHelpPopup' TravelStep _ = Just $(ihamletFile "templates/help/travel-help-en.hamlet")
preferenceHelpPopup' RangeStep _ = Just $(ihamletFile "templates/help/range-help-en.hamlet")
preferenceHelpPopup' ServicesStopStep _ = Just $(ihamletFile "templates/help/services-help-en.hamlet")
preferenceHelpPopup' ServicesStockStep _ = Just $(ihamletFile "templates/help/services-help-en.hamlet")
preferenceHelpPopup' ServicesRestStep _ = Just $(ihamletFile "templates/help/services-help-en.hamlet")
preferenceHelpPopup' PoiStep _ = Just $(ihamletFile "templates/help/poi-help-en.hamlet")
preferenceHelpPopup' RoutesStep _ = Just $(ihamletFile "templates/help/routes-help-en.hamlet")
preferenceHelpPopup' StartStep _ = Just $(ihamletFile "templates/help/start-help-en.hamlet")
preferenceHelpPopup' StopsStep _ = Just $(ihamletFile "templates/help/stops-help-en.hamlet")
preferenceHelpPopup' _ _ = Nothing

preferenceHelpPopup :: PreferenceStep -> Handler (Widget, Widget)
preferenceHelpPopup stepp = do
  locales <- getLocales
  let help = preferenceHelpPopup' stepp locales
  helpPopup help

imagePopup :: Widget
imagePopup = $(widgetFile "image-popup")

addError' :: (Edge e Location, Score s1, Score s2) => (Failure Location e s1 s2) -> [Locale] -> Html
addError' (Failure msg loc _ _) locales = [shamlet|
  #{msg}
  $maybe l <- loc
    #{locationID l} #{localiseText locales $ locationName l}
|]

addError :: (Edge e Location, Score s1, Score s2) => Maybe (Failure Location e s1 s2) -> Handler ()
addError (Just f) = do
  locales <- getLocales
  setMessage [shamlet|
    <div ..alert .alert-warning role="alert">
      ^{addError' f locales}
    |]
  return ()
addError _ = do
  return ()

createMetadata :: Solution -> Text -> IO Metadata
createMetadata solution origin = do
  timestamp <- getCurrentTime
  let created = Statement dctermsCreated (TaggedText rootLocale (pack $ iso8601Show timestamp))
  let creator = Statement dctermsCreator (TaggedText rootLocale "de-calixtinus")
  let ident = Statement dctermsIdentifier (TaggedText rootLocale (maybe "unknown" id (solutionID solution)))
  let license = Statement dctermsLicense (TaggedText rootLocale "https://creativecommons.org/licenses/by/4.0/")
  let src = Statement dctermsSource (TaggedText rootLocale origin)
  return $ def { metadataStatements = [ created, creator, ident, license, src ] }

planPage :: PreferenceData -> Handler Html
planPage prefs = do
    master <- getYesod
    let tprefs = travelPreferencesFrom prefs
    let cprefs = caminoPreferencesFrom prefs
    let solution = planCamino (caminoAppCaminoConfig master) tprefs cprefs
    solution' <- if isJust (solutionPilgrimage solution) then liftIO $ do
        uuid <- nextRandom
        let sid = toText uuid
        let so = solution { solutionID = Just sid }
        metadata <- liftIO $ createMetadata so (getWebRoot $ caminoAppConfig master)
        let so' = so { solutionMetadata = Just metadata }
        let cache = caminoAppPlans master
        cachePut cache sid so'
        return so'
      else
        return solution
    showPage solution'

showPage :: Solution -> Handler Html
showPage solution = do
    master <- getYesod
    locales <- getLocales
    let config = caminoAppConfig master
    let router = renderCaminoRoute config locales
    let messages = renderCaminoMsg config locales
    let tprefs = solutionTravelPreferences solution
    let cprefs = solutionCaminoPreferences solution
    let html = (caminoHtmlBase config tprefs cprefs (Just solution)) messages router
    addError (solutionJourneyFailure solution)
    addError (solutionPilgrimageFailure solution)
    defaultLayout $ do
      setTitle [shamlet|#{localiseText locales $ locationName (preferenceStart cprefs)} - #{localiseText locales $ locationName (preferenceFinish cprefs)}|]
      (toWidget html)
      imagePopup

-- | The MIME type for KML
kmlType :: ContentType
kmlType = "application/vnd.google-earth.kml+xml"

-- | Generate a file name for this
kmlFileName :: CaminoPreferences -> Maybe Pilgrimage -> Text
kmlFileName camino Nothing = (toFileName $ caminoNameLabel $ preferenceCamino camino) <> ".kml"
kmlFileName camino (Just trip) = (toFileName $ caminoNameLabel $ preferenceCamino camino) <> "-" <> (toFileName $ locationNameLabel $ start trip) <> "-" <> (toFileName $ locationNameLabel $ finish trip) <> ".kml"

showKml :: Solution -> Handler TypedContent
showKml solution = do
    master <- getYesod
    locales <- getLocales
    let tprefs = solutionTravelPreferences solution
    let cprefs = solutionCaminoPreferences solution
    let pilgrimage = solutionPilgrimage solution
    let config = caminoAppConfig master
    let kml = createCaminoDoc config locales tprefs cprefs (Just solution)
    let result = renderLBS (def { rsUseCDATA = useCDATA }) kml
    addHeader "content-disposition" ("attachment; filename=\"" <> kmlFileName cprefs pilgrimage <> "\"")
    return $ TypedContent kmlType (toContent result)


-- | The MIME type for KML
xlsxType :: ContentType
xlsxType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

-- | Generate a file name for this
xlsxFileName :: CaminoPreferences -> Maybe Pilgrimage -> Text
xlsxFileName camino _ = (toFileName $ caminoNameLabel $ preferenceCamino camino) <> ".xlsx"

showXlsx :: Solution -> Handler TypedContent
showXlsx solution = do
    master <- getYesod
    locales <- getLocales
    ct <- liftIO getPOSIXTime
    let config = caminoAppConfig master
    let messages = renderCaminoMsgText config locales
    let tprefs = solutionTravelPreferences solution
    let cprefs = solutionCaminoPreferences solution
    let pilgrimage = solutionPilgrimage solution
    let xlsx = createCaminoXlsx config messages tprefs cprefs solution
    let result = fromXlsx ct xlsx
    addHeader "content-disposition" ("attachment; filename=\"" <> xlsxFileName cprefs pilgrimage <> "\"")
    return $ TypedContent xlsxType (toContent result)


runCaminoApp :: CaminoApp -> IO ()
runCaminoApp app = warp (caminoAppPort app) app