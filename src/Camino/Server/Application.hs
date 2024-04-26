{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ViewPatterns          #-}
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
import Camino.Planner (Solution(..), Trip)
import Camino.Preferences
import Camino.Util
import Camino.Display.Html
import Camino.Display.I18n
import Camino.Display.KML
import Camino.Display.Routes
import Camino.Server.Forms
import Camino.Server.Foundation
import Camino.Server.Settings
import Data.Default.Class
import Data.Text (Text, unpack, pack)
import Text.Hamlet
import Text.Read (readMaybe)
import Text.XML
import Yesod
import Camino.Planner (planCamino)

mkYesodDispatch "CaminoApp" resourcesCaminoApp

data PreferenceStep =
    FitnessStep
  | RangeStep
  | ServicesStep
  | CaminoStep
  | RoutesStep
  | StartStep
  | StopsStep
  | ShowPreferencesStep
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

homeP :: Handler Html
homeP =
  defaultLayout $ do
    setTitleI MsgAppName
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
  langs <- languages
  let config = caminoAppConfig master
  let router = renderCaminoRoute config langs
  let messages = renderCaminoMsg config
  defaultLayout $ do
    setTitleI MsgHelpTitle
    toWidget ((helpWidget langs) messages router)

-- | Help for the languages that we have
helpWidget :: [Text] -> HtmlUrlI18n CaminoMsg CaminoRoute
helpWidget [] = helpWidget ["en"]
helpWidget ("en":_) = $(ihamletFile "templates/help/help-en.hamlet")
helpWidget (_:other) = helpWidget other


getMetricR :: Handler Html
getMetricR = do
  master <- getYesod
  langs <- languages
  let config = caminoAppConfig master
  let router = renderCaminoRoute config langs
  let messages = renderCaminoMsg config
  defaultLayout $ do
    setTitleI MsgMetricTitle
    toWidget ((metricWidget langs) messages router)

-- | Help for the languages that we have
metricWidget :: [Text] -> HtmlUrlI18n CaminoMsg CaminoRoute
metricWidget [] = metricWidget ["en"]
metricWidget ("en":_) = $(ihamletFile "templates/help/metric-en.hamlet")
metricWidget (_:other) = metricWidget other

getAboutR :: Handler Html
getAboutR = do
  master <- getYesod
  langs <- languages
  let config = caminoAppConfig master
  let router = renderCaminoRoute config langs
  let messages = renderCaminoMsg config
  defaultLayout $ do
    setTitleI MsgAboutTitle
    toWidget ((aboutWidget langs) messages router)

-- | About text for the languages that we have
aboutWidget :: [Text] -> HtmlUrlI18n CaminoMsg CaminoRoute
aboutWidget [] = aboutWidget ["en"]
aboutWidget ("en":_) = $(ihamletFile "templates/about/about-en.hamlet")
aboutWidget (_:other) = aboutWidget other

getHomeR :: Handler Html
getHomeR = do
  homeP

postPlanR :: Handler Html
postPlanR = do
  ((result, widget), enctype) <- runFormPost $ (stepForm ShowPreferencesStep blankHelp) Nothing
  case result of
      FormSuccess prefs -> do
         encodePreferences prefs
         planPage prefs
      FormFailure errs -> do
        setMessage [shamlet|
          $forall err <- errs
            <div .text-danger>#{err}
        |]
        stepPage ShowPreferencesStep Nothing blankHelp widget enctype
      _ ->
        getHomeR

postPlanKmlR :: Handler TypedContent
postPlanKmlR = do
  ((result, _widget), _enctype) <- runFormPost $ (stepForm ShowPreferencesStep blankHelp) Nothing
  case result of
      FormSuccess prefs -> do
         encodePreferences prefs
         planKml prefs
      _ ->
        invalidArgs ["Bad preferences data"]


getPreferencesR :: Handler Html
getPreferencesR = do
    master <- getYesod
    prefs <- decodePreferences
    let prefs' = Just $ maybe (defaultPreferenceData master) id prefs
    (embedded, help) <- helpPopup FitnessStep
    ((_result, widget), enctype) <- runFormPost $ chooseFitnessForm embedded prefs'
    stepPage FitnessStep Nothing help widget enctype

postPreferencesR :: Handler Html
postPreferencesR = do
  stepp <- lookupPostParam "_step"
  nextp <- lookupPostParam "_next"
  let step' = maybe Nothing (readMaybe . unpack) stepp
  let next' = maybe FitnessStep id $ maybe Nothing (readMaybe . unpack) nextp
  case step' of
    Nothing ->
      getPreferencesR
    Just step'' ->
      nextStep step'' next'

nextStep :: PreferenceStep -> PreferenceStep -> Handler Html
nextStep stepp nextp = do
  (embedded, help) <- helpPopup stepp
  ((result, widget), enctype) <- runFormPost $ (stepForm stepp embedded) Nothing
  case result of
      FormSuccess prefs -> do
         (embedded', help') <- helpPopup nextp
         (widget', enctype') <- generateFormPost $ (stepForm nextp embedded') (Just prefs)
         stepPage nextp (Just prefs) help' widget' enctype'
      FormFailure errs -> do
        setMessage [shamlet|
          $forall err <- errs
            <div .text-danger>#{err}
        |]
        stepPage stepp Nothing help widget enctype
      _ ->
        getHomeR

stepForm :: PreferenceStep -> Widget -> Maybe PreferenceData -> (Html -> MForm Handler (FormResult PreferenceData, Widget))
stepForm FitnessStep help prefs = chooseFitnessForm help prefs
stepForm RangeStep help prefs = chooseRangeForm help prefs
stepForm ServicesStep help prefs = chooseServicesForm help prefs
stepForm CaminoStep help prefs = chooseCaminoForm help prefs
stepForm RoutesStep help prefs = chooseRoutesForm help prefs
stepForm StartStep help prefs = chooseStartForm help prefs
stepForm StopsStep help prefs = chooseStopsForm help prefs
stepForm ShowPreferencesStep help prefs = confirmPreferencesForm help prefs

stepPage' :: CaminoAppMessage -> CaminoAppMessage -> Maybe CaminoAppMessage -> PreferenceStep -> PreferenceStep -> PreferenceStep -> Widget -> Widget -> Enctype -> Handler Html
stepPage' title top bottom stepp prevp nextp help widget enctype = do
  defaultLayout $ do
    setTitleI title
    toWidget help
    $(widgetFile "step")

stepPage :: PreferenceStep -> Maybe PreferenceData -> Widget -> Widget -> Enctype -> Handler Html
stepPage FitnessStep _ help widget enctype = stepPage' MsgFitnessTitle MsgFitnessText (Just MsgFitnessBottom) FitnessStep FitnessStep RangeStep help widget enctype
stepPage RangeStep _ help widget enctype = stepPage' MsgRangeTitle MsgRangeText Nothing RangeStep FitnessStep ServicesStep help widget enctype
stepPage ServicesStep _ help widget enctype = stepPage' MsgServicesTitle MsgServicesText Nothing ServicesStep RangeStep CaminoStep help widget enctype
stepPage CaminoStep _ help widget enctype = stepPage' MsgCaminoTitle MsgCaminoText Nothing CaminoStep ServicesStep RoutesStep help widget enctype
stepPage RoutesStep _ help widget enctype = stepPage' MsgRoutesTitle MsgRoutesText Nothing RoutesStep CaminoStep StartStep help widget enctype
stepPage StartStep _ help widget enctype = stepPage' MsgStartTitle MsgStartText Nothing StartStep RoutesStep StopsStep help widget enctype
stepPage StopsStep _ help widget enctype = stepPage' MsgStopsTitle MsgStopsText Nothing StopsStep StartStep ShowPreferencesStep help widget enctype
stepPage ShowPreferencesStep (Just prefs) help widget enctype = let
    preferences = travelPreferencesFrom prefs
    camino = caminoPreferencesFrom prefs
  in
    defaultLayout $ do
      master <- getYesod
      langs <- languages
      let config = caminoAppConfig master
      let router = renderCaminoRoute config langs
      let messages = renderCaminoMsg config
      setTitleI MsgShowPreferencesTitle
      toWidget help
      $(widgetFile "show-preferences")
stepPage ShowPreferencesStep _ help widget enctype = stepPage' MsgShowPreferencesTitle MsgShowPreferencesText Nothing ShowPreferencesStep StopsStep ShowPreferencesStep help widget enctype


-- For use where a help widget is required but not present
blankHelp :: Widget
blankHelp = [whamlet||]

helpPopup' :: PreferenceStep -> [Lang] -> Maybe (HtmlUrlI18n CaminoMsg CaminoRoute)
helpPopup' FitnessStep _ = Just $(ihamletFile "templates/help/fitness-help-en.hamlet")
helpPopup' RangeStep _ = Just $(ihamletFile "templates/help/range-help-en.hamlet")
helpPopup' ServicesStep _ = Just $(ihamletFile "templates/help/services-help-en.hamlet")
helpPopup' RoutesStep _ = Just $(ihamletFile "templates/help/routes-help-en.hamlet")
helpPopup' StartStep _ = Just $(ihamletFile "templates/help/start-help-en.hamlet")
helpPopup' StopsStep _ = Just $(ihamletFile "templates/help/stops-help-en.hamlet")
helpPopup' _ _ = Nothing

helpPopup :: PreferenceStep -> Handler (Widget, Widget)
helpPopup stepp = do
  master <- getYesod
  langs <- languages
  let config = caminoAppConfig master
  let router = renderCaminoRoute config langs
  let messages = renderCaminoMsg config
  let help' = (\h -> h messages router) <$> helpPopup' stepp langs
  return $ case help' of
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

addError :: Either Location Trip -> Handler ()
addError (Left loc) = do
  setMessage [shamlet|
    <div ..alert .alert-warning role="alert">
      Break at #{locationID loc} #{locationName loc}
    |]
  return ()
addError _ = do
  return ()

planPage :: PreferenceData -> Handler Html
planPage prefs = do
    master <- getYesod
    langs <- languages
    let tprefs = travelPreferencesFrom prefs
    let cprefs = caminoPreferencesFrom prefs
    let solution = planCamino tprefs cprefs
    let config = caminoAppConfig master
    let router = renderCaminoRoute config langs
    let messages = renderCaminoMsg config
    let html = (caminoHtmlBase config tprefs cprefs solution) messages router
    addError (solutionTrip solution)
    defaultLayout $ do
      setTitle [shamlet|#{locationName (preferenceStart cprefs)} - #{locationName (preferenceFinish cprefs)}|]
      (toWidget html)

-- | The MIME type for KML
kmlType :: ContentType
kmlType = "application/vnd.google-earth.kml+xml"

-- | Generate a file name for this
kmlFileName :: CaminoPreferences -> Maybe Trip -> Text
kmlFileName camino Nothing = (toFileName $ caminoName $ preferenceCamino camino) <> ".kml"
kmlFileName camino (Just trip) = (toFileName $ caminoName $ preferenceCamino camino) <> "-" <> (toFileName $ locationName $ start trip) <> "-" <> (toFileName $ locationName $ finish trip) <> ".kml"

planKml :: PreferenceData -> Handler TypedContent
planKml prefs = do
    master <- getYesod
    let tprefs = travelPreferencesFrom prefs
    let cprefs = caminoPreferencesFrom prefs
    let solution = planCamino tprefs cprefs
    let trip = either (const Nothing) (Just) (solutionTrip solution)
    let config = caminoAppConfig master
    let kml = createCaminoDoc config tprefs cprefs trip
    let result = renderLBS (def { rsPretty = True, rsUseCDATA = useCDATA }) kml
    addHeader "content-disposition" ("attachment; filename=\"" <> kmlFileName cprefs trip <> "\"")
    return $ TypedContent kmlType (toContent result)

runCaminoApp :: CaminoApp -> IO ()
runCaminoApp app = warp (caminoAppPort app) app