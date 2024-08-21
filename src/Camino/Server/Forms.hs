{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-|
Module      : Forms
Description : Forms for the De Calixtinus application
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Forms for the application.

The preferences gathering forms use a common data element and pass data via hidden fields
-}
module Camino.Server.Forms (
    PreferenceData(..)

  , caminoPreferencesFrom
  , confirmPreferencesForm
  , chooseCaminoForm
  , chooseFitnessForm
  , choosePoiForm
  , chooseRangeForm
  , chooseRoutesForm
  , chooseServicesForm
  , chooseStartForm
  , chooseStopsForm
  , defaultPreferenceData
  , findCaminoById
  , travelPreferencesFrom
) where

import Camino.Camino
import Camino.Preferences
import Camino.Util
import Camino.Display.Html (caminoAccommodationTypeIcon, caminoAccommodationTypeMsg, caminoComfortMsg, caminoFitnessMsg, caminoLocationTypeIcon, caminoLocationTypeLabel, caminoPoiCategoryLabel, caminoServiceIcon, caminoServiceMsg, caminoTravelMsg)
import Camino.Display.I18n (renderCaminoMsg)
import Camino.Display.Routes (renderCaminoRoute)
import Camino.Server.Fields
import Camino.Server.Foundation
import Data.Description (Description(..), descriptionSummary)
import Data.List (find, partition, singleton, sortOn)
import Data.Localised (Locale(..), Tagged(..), localise, localiseText)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isNothing)
import Data.Placeholder
import qualified Data.Set as S
import Data.Text (Text, pack)
import Data.Time.Calendar (Day)
import Text.Hamlet
import Yesod

-- Gathered result and widget data
data PreferenceDataFields = PreferenceDataFields {
    resEasyMode :: FormResult Bool
  , viewEasyMode :: FieldView CaminoApp
  , resPrevTravel :: FormResult Travel
  , viewPrevTravel :: FieldView CaminoApp
  , resTravel :: FormResult Travel 
  , viewTravel :: FieldView CaminoApp
  , resPrevFitness :: FormResult Fitness 
  , viewPrevFitness :: FieldView CaminoApp
  , resFitness :: FormResult Fitness 
  , viewFitness :: FieldView CaminoApp
  , resPrevComfort :: FormResult Comfort 
  , viewPrevComfort :: FieldView CaminoApp
  , resComfort :: FormResult Comfort 
  , viewComfort :: FieldView CaminoApp
  , resDistance :: FormResult (PreferenceRange Float)
  , viewDistance :: FieldView CaminoApp
  , resTime :: FormResult (PreferenceRange Float)
  , viewTime :: FieldView CaminoApp
  , resLocation :: FormResult (M.Map LocationType Penance)
  , viewLocation :: FieldView CaminoApp
  , resAccommodation :: FormResult (M.Map AccommodationType Penance)
  , viewAccommodation :: FieldView CaminoApp
  , resStopServices :: FormResult (M.Map Service Penance)
  , viewStopServices :: FieldView CaminoApp
  , resDayServices :: FormResult (M.Map Service Penance)
  , viewDayServices :: FieldView CaminoApp
  , resPoiCategories :: FormResult (S.Set PoiCategory)
  , viewPoiCategories :: FieldView CaminoApp
  , resPrevCamino :: FormResult Camino
  , viewPrevCamino :: FieldView CaminoApp
  , resCamino :: FormResult Camino 
  , viewCamino :: FieldView CaminoApp
  , resPrevRoutes :: FormResult (S.Set Camino.Camino.Route)
  , viewPrevRoutes :: FieldView CaminoApp
  , resRoutes :: FormResult (S.Set Camino.Camino.Route)
  , viewRoutes :: FieldView CaminoApp
  , resPrevStart :: FormResult Location 
  , viewPrevStart :: FieldView CaminoApp
  , resStart :: FormResult Location 
  , viewStart :: FieldView CaminoApp
  , resPrevFinish :: FormResult Location 
  , viewPrevFinish :: FieldView CaminoApp
  , resFinish :: FormResult Location 
  , viewFinish :: FieldView CaminoApp
  , resStops :: FormResult (S.Set Location)
  , viewStops :: FieldView CaminoApp
  , resExcluded :: FormResult (S.Set Location)
  , viewExcluded :: FieldView CaminoApp
  , resStartDate :: FormResult (Maybe Day)
  , viewStartDate :: FieldView CaminoApp
}

findCaminoById :: CaminoConfig -> Text -> Maybe Camino
findCaminoById config val = (caminoConfigLookup config) val

findRouteById :: FormResult Camino -> Text -> Maybe Camino.Camino.Route
findRouteById (FormSuccess camino) val = find (\r -> routeID r == val) (caminoRoutes camino)
findRouteById _ val = Just $ placeholder val

findLocationById :: FormResult Camino -> Text -> Maybe Location
findLocationById (FormSuccess camino) val = M.lookup val (caminoLocations camino)
findLocationById _ val = Just $ placeholder val

findSetById :: (Ord a) => FormResult Camino -> (FormResult Camino -> Text -> Maybe a) -> S.Set Text -> Maybe (S.Set a)
findSetById cres finder val = let
    results = map (finder cres) (S.toList val)
  in
    if any isNothing results then Nothing else Just $ S.fromList $ catMaybes results

fullRoutes :: FormResult Camino -> S.Set Camino.Camino.Route -> S.Set Camino.Camino.Route
fullRoutes (FormSuccess camino) routes = fst $ completeRoutes camino routes
fullRoutes _ routes = routes

descriptionText :: [Locale] -> Description -> Maybe Text
descriptionText locales description = plainText <$> localise locales (descriptionSummary description)


-- Make a default set of preference data fields with everything hidden
defaultPreferenceFields :: CaminoApp -> Maybe PreferenceData -> MForm Handler PreferenceDataFields
defaultPreferenceFields master prefs = do
    (emRes, emView) <- mreq hiddenField "" (prefEasyMode <$> prefs) -- Easy/detailed mode
    (trpRes, trpView) <- mreq hiddenField "" (prefTravel <$> prefs) -- Original values
    (trRes, trView) <- mreq hiddenField "" (prefTravel <$> prefs)
    (fipRes, fipView) <- mreq hiddenField "" (prefFitness <$> prefs)
    (fiRes, fiView) <- mreq hiddenField "" (prefFitness <$> prefs)
    (copRes, copView) <- mreq hiddenField "" (prefComfort <$> prefs)
    (coRes, coView) <- mreq hiddenField "" (prefComfort <$> prefs)
    (diRes, diView) <- mreq hiddenField "" (prefDistance <$> prefs)
    (tiRes, tiView) <- mreq hiddenField "" (prefTime <$> prefs)
    (loRes, loView) <- mreq hiddenField "" (prefLocation <$> prefs)
    (acRes, acView) <- mreq hiddenField "" (prefAccommodation <$> prefs)
    (ssRes, ssView) <- mreq hiddenField "" (prefStopServices <$> prefs)
    (dsRes, dsView) <- mreq hiddenField "" (prefDayServices <$> prefs)
    (pcRes, pcView) <- mreq hiddenField "" (prefPoiCategories <$> prefs)
    (cpRes, cpView) <- mreq (parsingHiddenField caminoId (findCaminoById (caminoAppCaminoConfig master))) "" (prefCamino <$> prefs)
    (cRes, cView) <- mreq (parsingHiddenField caminoId (findCaminoById (caminoAppCaminoConfig master))) "" (prefCamino <$> prefs)
    (ropRes, ropView) <- mreq (parsingHiddenField (S.map routeID) (\v -> fullRoutes cRes <$> findSetById cRes findRouteById v)) "" (prefRoutes <$> prefs)
    (roRes, roView) <- mreq (parsingHiddenField (S.map routeID) (\v -> fullRoutes cRes <$> findSetById cRes findRouteById v)) "" (prefRoutes <$> prefs)
    (sapRes, sapView) <- mreq (parsingHiddenField locationID (findLocationById cRes)) "" (prefStart <$> prefs)
    (saRes, saView) <- mreq (parsingHiddenField locationID (findLocationById cRes)) "" (prefStart <$> prefs)
    (fnpRes, fnpView) <- mreq (parsingHiddenField locationID (findLocationById cRes)) "" (prefFinish <$> prefs)
    (fnRes, fnView) <- mreq (parsingHiddenField locationID (findLocationById cRes)) "" (prefFinish <$> prefs)
    (spRes, spView) <- mreq (parsingHiddenField (S.map locationID) (findSetById cRes findLocationById)) "" (prefStops <$> prefs)
    (exRes, exView) <- mreq (parsingHiddenField (S.map locationID) (findSetById cRes findLocationById)) "" (prefExcluded <$> prefs)
    (sdRes, sdView) <- mreq hiddenField "" (prefStartDate <$> prefs)
    return PreferenceDataFields {
        resEasyMode = emRes
      , viewEasyMode = emView
      , resPrevTravel = trpRes
      , viewPrevTravel = trpView
      , resTravel = trRes
      , viewTravel = trView
      , resPrevFitness = fipRes
      , viewPrevFitness = fipView
      , resFitness = fiRes
      , viewFitness = fiView
      , resPrevComfort = copRes
      , viewPrevComfort = copView
      , resComfort = coRes
      , viewComfort = coView
      , resDistance = diRes
      , viewDistance = diView
      , resTime = tiRes
      , viewTime = tiView
      , resLocation = loRes
      , viewLocation = loView
      , resAccommodation = acRes
      , viewAccommodation = acView
      , resStopServices = ssRes
      , viewStopServices = ssView
      , resDayServices = dsRes
      , viewDayServices = dsView
      , resPoiCategories = pcRes
      , viewPoiCategories = pcView
      , resPrevCamino = cpRes
      , viewPrevCamino = cpView
      , resCamino = cRes
      , viewCamino = cView
      , resPrevRoutes = ropRes
      , viewPrevRoutes = ropView
      , resRoutes = roRes
      , viewRoutes = roView
      , resPrevStart = sapRes
      , viewPrevStart = sapView
      , resStart = saRes
      , viewStart = saView
      , resPrevFinish = fnpRes
      , viewPrevFinish = fnpView
      , resFinish = fnRes
      , viewFinish = fnView
      , resStops = spRes
      , viewStops = spView
      , resExcluded = exRes
      , viewExcluded = exView
      , resStartDate = sdRes
      , viewStartDate = sdView
    }

changed :: (Eq a) => FormResult a -> FormResult a -> Bool
changed (FormSuccess x) (FormSuccess y) = x /= y
changed _ _ = False

makePreferenceData :: CaminoApp -> PreferenceDataFields -> FormResult PreferenceData
makePreferenceData _master fields = let
    easy' = resEasyMode fields
    easy'' = case easy' of
      (FormSuccess e) -> e
      _ -> False
    travel' = resTravel fields
    fitness' = resFitness fields
    comfort' = resComfort fields
    changedTravel = (changed (resPrevTravel fields) travel') || (changed (resPrevFitness fields) fitness') || (changed (resPrevComfort fields) comfort')
    dtp = defaultTravelPreferences <$> travel' <*> fitness' <*> comfort'
    distance' = if easy'' || changedTravel then preferenceDistance <$> dtp else resDistance fields
    time' = if easy'' || changedTravel then preferenceTime <$> dtp else resTime fields
    location' = if easy'' || changedTravel then preferenceLocation <$> dtp else resLocation fields
    accommodation' = if easy'' || changedTravel then preferenceAccommodation <$> dtp else resAccommodation fields
    stopServices' = if easy'' || changedTravel then preferenceStopServices <$> dtp else resStopServices fields
    dayServices' = if easy'' || changedTravel then preferenceDayServices <$> dtp else resDayServices fields
    poiCategories' = if easy'' || changedTravel then preferencePoiCategories <$> dtp else resPoiCategories fields
    changedCamino = changed (caminoId <$> resPrevCamino fields) (caminoId <$> resCamino fields)
    camino' = resCamino fields
    dcp = defaultCaminoPreferences <$> camino'
    routes' = if changedCamino then preferenceRoutes <$> dcp else fullRoutes (resCamino fields) <$> (resRoutes fields)
    dcp' = withRoutes <$> dcp <*> routes'
    changedRoutes = changed (S.map routeID <$> resPrevRoutes fields) (S.map routeID <$> routes')
    start' = if changedRoutes then preferenceStart <$> dcp' else resStart fields
    finish' = if changedRoutes then preferenceFinish <$> dcp' else resFinish fields
    dcp'' = withStartFinish <$> dcp' <*> start' <*> finish'
    changedStart = changedRoutes || changed (locationID <$> resPrevStart fields) (locationID <$> start') || changed (locationID <$> resPrevFinish fields) (locationID <$> finish')
    stops' = if easy'' || changedStart then recommendedStops <$> dcp'' else resStops fields
    excluded' = if easy'' || changedStart then preferenceExcluded <$> dcp'' else resExcluded fields
    startDate' = resStartDate fields
  in
    PreferenceData
      <$> easy'
      <*> travel'
      <*> fitness'
      <*> comfort'
      <*> distance'
      <*> time'
      <*> location'
      <*> accommodation'
      <*> stopServices'
      <*> dayServices'
      <*> poiCategories'
      <*> camino'
      <*> routes'
      <*> start'
      <*> finish'
      <*> stops'
      <*> excluded'
      <*> startDate'

-- | Form to allow fitness settings to be chosen
chooseFitnessForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseFitnessForm help prefs extra = do
    master <- getYesod
    locales <- getLocales
    mRender <- getMessageRender
    let render = renderCaminoMsg (caminoAppConfig master) locales
    let  easyField =  extendedCheckboxField (toHtml . mRender) MsgEasyMode (Just MsgEasyModeText)
    let  travelField =  extendedRadioFieldList render (map (\f -> (pack $ show f, caminoTravelMsg f, f, Nothing)) travelEnumeration)
    let  fitnessField =  extendedRadioFieldList render (map (\f -> (pack $ show f, caminoFitnessMsg f, f, Nothing)) fitnessEnumeration)
    let  comfortField =  extendedRadioFieldList render (map (\f -> (pack $ show f, caminoComfortMsg f, f, Nothing)) comfortEnumeration)
    (emRes, emView) <- mreq easyField "" (prefEasyMode <$> prefs)
    (trRes, trView) <- mreq travelField (fieldSettingsLabel MsgSelectTravel) (prefTravel <$> prefs)
    (fRes, fView) <- mreq fitnessField (fieldSettingsLabel MsgSelectFitness) (prefFitness <$> prefs)
    (cRes, cView) <- mreq comfortField (fieldSettingsLabel MsgSelectComfort) (prefComfort <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resEasyMode = emRes,
      viewEasyMode = emView,
      resTravel = trRes,
      viewTravel = trView,
      resFitness = fRes,
      viewFitness = fView,
      resComfort = cRes,
      viewComfort = cView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewEasyMode fields
        <div .row .mb-3>
          <div .col>
            ^{fvInput view}
      $with view <- viewTravel fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      $with view <- viewFitness fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      $with view <- viewComfort fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewPrevComfort fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewLocation fields)}
      ^{fvInput (viewAccommodation fields)}
      ^{fvInput (viewStopServices fields)}
      ^{fvInput (viewDayServices fields)}
      ^{fvInput (viewPoiCategories fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewFinish fields)}
      ^{fvInput (viewStops fields)}
      ^{fvInput (viewExcluded fields)}
      ^{fvInput (viewStartDate fields)}
    |]
    return (res, widget)

-- | Form to allow preference range settings to be chosen
chooseRangeForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseRangeForm help prefs extra = do
    master <- getYesod
    let distanceRangeField = if maybe Walking prefTravel prefs == Cycling then rangeField 0.0 250.0 1.0 else rangeField 0.0 50.0 0.5
    let timeRangeField = rangeField 0.0 16.0 0.1
    (diRes, diView) <- mreq distanceRangeField (fieldSettingsLabel MsgDistancePreferencesLabel) (prefDistance <$> prefs)
    (tiRes, tiView) <- mreq timeRangeField (fieldSettingsLabel MsgTimePreferencesLabel) (prefTime <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resDistance = diRes,
      viewDistance = diView,
      resTime = tiRes,
      viewTime = tiView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewDistance fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      $with view <- viewTime fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{fvInput (viewEasyMode fields)}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewPrevComfort fields)}
      ^{fvInput (viewComfort fields)}
      ^{fvInput (viewLocation fields)}
      ^{fvInput (viewAccommodation fields)}
      ^{fvInput (viewStopServices fields)}
      ^{fvInput (viewDayServices fields)}
      ^{fvInput (viewPoiCategories fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewFinish fields)}
      ^{fvInput (viewStops fields)}
      ^{fvInput (viewExcluded fields)}
      ^{fvInput (viewStartDate fields)}
    |]
    return (res, widget)


-- | Form to allow service preferences to be chosen
chooseServicesForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseServicesForm help prefs extra = do
    master <- getYesod
    locales <- getLocales
    let config = caminoAppConfig master
    let router = renderCaminoRoute config locales
    let messages = renderCaminoMsg config locales
    let locationOptions = map (\v -> (v, [ihamlet|<span .location-type-sample>^{caminoLocationTypeIcon v}</span>&nbsp;_{caminoLocationTypeLabel v}|] messages router)) locationStopTypeEnumeration
    let accommodationOptions = map (\v -> (v, [ihamlet|^{caminoAccommodationTypeIcon v}&nbsp;_{caminoAccommodationTypeMsg v}|] messages router)) accommodationTypeEnumeration
    let stopServiceOptions = map (\v -> (v, [ihamlet|^{caminoServiceIcon v}&nbsp;_{caminoServiceMsg v}|] messages router)) serviceEnumeration
    let dayServiceOptions = map (\v -> (v, [ihamlet|^{caminoServiceIcon v}&nbsp;_{caminoServiceMsg v}|] messages router)) townServiceEnumeration
    (loRes, loView) <- mreq (penanceMapField True True locationOptions) (fieldSettingsLabel MsgLocationPreferencesLabel) (prefLocation <$> prefs)
    (acRes, acView) <- mreq (penanceMapField True True accommodationOptions) (fieldSettingsLabel MsgAccommodationPreferencesLabel) (prefAccommodation <$> prefs)
    (ssRes, ssView) <- mreq (penanceMapField False False stopServiceOptions) (fieldSettingsLabel MsgStopServicePreferencesLabel) (prefStopServices <$> prefs)
    (dsRes, dsView) <- mreq (penanceMapField False False dayServiceOptions) (fieldSettingsLabel MsgDayServicePreferencesLabel) (prefDayServices <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resLocation = loRes,
      viewLocation = loView,
      resAccommodation= acRes,
      viewAccommodation = acView,
      resStopServices = ssRes,
      viewStopServices = ssView,
      resDayServices = dsRes,
      viewDayServices = dsView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewLocation fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            $maybe tt <- fvTooltip view
              <div .form-text>
                ^{tt}
            ^{fvInput view}
      $with view <- viewAccommodation fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      $with view <- viewStopServices fields
         <div .row .mb-3>
           <div .col>
             <label for="#{fvId view}">
               ^{fvLabel view} ^{help}
             ^{fvInput view}
      $with view <- viewDayServices fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{fvInput (viewEasyMode fields)}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewPrevComfort fields)}
      ^{fvInput (viewComfort fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewPoiCategories fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewFinish fields)}
      ^{fvInput (viewStops fields)}
      ^{fvInput (viewExcluded fields)}
      ^{fvInput (viewStartDate fields)}
    |]
    return (res, widget)


-- | Form to allow Poi preferences to be chosen
choosePoiForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
choosePoiForm help prefs extra = do
    master <- getYesod
    locales <- getLocales
    let render = renderCaminoMsg (caminoAppConfig master) locales
    let  poiField = extendedCheckboxFieldList render (map (\f -> (pack $ show f, caminoPoiCategoryLabel f, f, Nothing)) poiCategoryEnumeration)
    (pcRes, pcView) <- mreq poiField (fieldSettingsLabel MsgSelectPoiCategories) (prefPoiCategories <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resPoiCategories = pcRes,
      viewPoiCategories = pcView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewPoiCategories fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{fvInput (viewEasyMode fields)}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewPrevComfort fields)}
      ^{fvInput (viewComfort fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewLocation fields)}
      ^{fvInput (viewAccommodation fields)}
      ^{fvInput (viewStopServices fields)}
      ^{fvInput (viewDayServices fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewFinish fields)}
      ^{fvInput (viewStops fields)}
      ^{fvInput (viewExcluded fields)}
      ^{fvInput (viewStartDate fields)}
    |]
    return (res, widget)

-- | Form to allow the camino to be chosen
chooseCaminoForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseCaminoForm help prefs extra = do
    master <- getYesod
    locales <- getLocales
    let localised c = localiseText locales c
    let  caminoField =  extendedRadioFieldList id (map (\c -> (caminoId c, toHtml $ localised $ caminoName c, c, toHtml <$> descriptionText locales (caminoDescription c))) (caminoAppCaminos master))
    (caRes, caView) <- mreq caminoField (fieldSettingsLabel MsgSelectCamino) (prefCamino <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resCamino = caRes,
      viewCamino = caView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewCamino fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{fvInput (viewEasyMode fields)}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewPrevComfort fields)}
      ^{fvInput (viewComfort fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewLocation fields)}
      ^{fvInput (viewAccommodation fields)}
      ^{fvInput (viewStopServices fields)}
      ^{fvInput (viewDayServices fields)}
      ^{fvInput (viewPoiCategories fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewFinish fields)}
      ^{fvInput (viewStops fields)}
      ^{fvInput (viewExcluded fields)}
      ^{fvInput (viewStartDate fields)}
    |]
    return (res, widget)


-- | Form to allow the routes to be chosen
chooseRoutesForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseRoutesForm help prefs extra = do
    master <- getYesod
    locales <- getLocales
    let localised r = localiseText locales r
    let camino = prefCamino <$> prefs
    let routes = maybe (Prelude.concat (map caminoRoutes (caminoAppCaminos master))) caminoRoutes camino
    let requirementClauses = maybe [] (\c -> Prelude.concat $ map createRequiresClauses (caminoRouteLogic c)) camino
    let allowedClauses = maybe [] (\c -> Prelude.concat $ map createAllowsClauses (caminoRouteLogic c)) camino
    let prohibitedClauses = maybe [] (\c -> Prelude.concat $ map createProhibitsClauses (caminoRouteLogic c)) camino
    let routeOptions = map (\r -> (routeID r, localised (routeName r), r, descriptionText locales (routeDescription r), maybe False (\c -> r == caminoDefaultRoute c) camino)) routes
    (roRes, roView) <- mreq (implyingCheckListField routeOptions requirementClauses allowedClauses prohibitedClauses) (fieldSettingsLabel MsgRoutePreferencesLabel) (prefRoutes <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resRoutes = roRes,
      viewRoutes = roView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewRoutes fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{fvInput (viewEasyMode fields)}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewPrevComfort fields)}
      ^{fvInput (viewComfort fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewLocation fields)}
      ^{fvInput (viewAccommodation fields)}
      ^{fvInput (viewStopServices fields)}
      ^{fvInput (viewDayServices fields)}
      ^{fvInput (viewPoiCategories fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewFinish fields)}
      ^{fvInput (viewStops fields)}
      ^{fvInput (viewExcluded fields)}
      ^{fvInput (viewStartDate fields)}
    |]
    return (res, widget)

makeOptions :: (Ord a) => (CaminoAppMessage -> Text) -> (a -> Text) -> (a -> Text) -> [a] -> [a] -> [(Text, [(Text, Text, a)])]
makeOptions render keyer labeler recommended options = let
    rset = S.fromList recommended
    other = filter (\v -> not $ S.member v rset) options
    mkOptions opts = map (\l -> (keyer l, labeler l, l)) opts
  in
    (render MsgSuggestedLabel, mkOptions recommended) : (map (\(m, ls) -> (m, mkOptions ls)) (Camino.Util.partition (categorise . labeler) other))

-- | Form to allow start and finish to be chosen
chooseStartForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseStartForm help prefs extra = do
    master <- getYesod
    render <- getMessageRender
    locales <- getLocales
    let camino = prefCamino <$> prefs
    let routes = prefRoutes <$> prefs
    let start' = prefStart <$> prefs
    let finish' = prefFinish <$> prefs
    let startDate' = prefStartDate <$> prefs
    let cprefs = CaminoPreferences <$> camino <*> start' <*> finish' <*> routes <*> pure S.empty <*> pure S.empty <*> startDate'
    let caminos = maybe (caminoAppCaminos master) singleton camino
    let allStops = Prelude.concat (map (M.elems . caminoLocations) caminos)
    let possibleStops = caminoRouteLocations <$> camino <*> routes
    let allowedStops = permittedStops <$> prefs <*> possibleStops
    let sortKey l = canonicalise $ localiseText locales (locationName l)
    let stops = sortOn sortKey $ maybe allStops S.toList allowedStops
    let rstarts = filter (\l -> maybe True (S.member l) possibleStops) $ maybe [] suggestedStarts cprefs
    let rfinishes = filter (\l -> maybe True (S.member l) possibleStops) $ maybe [] suggestedFinishes cprefs
    let startOptions = makeOptions render locationID ((localiseText locales) . locationName) rstarts stops
    let finishOptions = makeOptions render locationID ((localiseText locales) . locationName) rfinishes stops
    (stRes, stView) <- mreq (extendedSelectionField startOptions) (fieldSettingsLabel MsgStartLocationLabel) start'
    (fiRes, fiView) <- mreq (extendedSelectionField finishOptions) (fieldSettingsLabel MsgFinishLocationLabel) finish'
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resStart = stRes,
      viewStart = stView,
      resFinish = fiRes,
      viewFinish = fiView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewStart fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      $with view <- viewFinish fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{fvInput (viewEasyMode fields)}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewPrevComfort fields)}
      ^{fvInput (viewComfort fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewLocation fields)}
      ^{fvInput (viewAccommodation fields)}
      ^{fvInput (viewStopServices fields)}
      ^{fvInput (viewDayServices fields)}
      ^{fvInput (viewPoiCategories fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewStops fields)}
      ^{fvInput (viewExcluded fields)}
      ^{fvInput (viewStartDate fields)}
    |]
    return (res, widget)

-- | Form to allow the stops to be chosen
chooseStopsForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseStopsForm help prefs extra = do
    master <- getYesod
    render <- getMessageRender
    locales <- getLocales
    let localised l = localiseText locales $ locationName l
    let camino = prefCamino <$> prefs
    let routes = prefRoutes <$> prefs
    let start' = prefStart <$> prefs
    let finish' = prefFinish <$> prefs
    let cprefs = withRoutes <$> (defaultCaminoPreferences <$> camino) <*> routes
    let cprefs' = withStartFinish <$> cprefs <*> start' <*> finish'
    let caminos = maybe (caminoAppCaminos master) singleton camino
    let allStops = Prelude.concat (map (M.elems . caminoLocations) caminos)
    let possibleStops = reachableLocations <$> cprefs'
    let allowedStops = permittedStops <$> prefs <*> possibleStops
    let sortKey l = canonicalise $ localised l
    let stops = sortOn sortKey $ maybe allStops S.toList allowedStops
    let recommended = maybe S.empty recommendedStops cprefs'
    let (suggested, other) = Data.List.partition (\l -> S.member l recommended) stops
    let mkOptions locs = map (\l -> (locationID l, localised l, l)) locs
    let stopOptions = (render MsgSuggestedLabel, mkOptions suggested) : (map (\(m, ls) -> (m, mkOptions ls)) (Camino.Util.partition (categorise .localised) other))
    let exclOptions = (render MsgSuggestedLabel, []) : (map (\(m, ls) -> (m, mkOptions ls)) (Camino.Util.partition (categorise . localised) stops))
    let chosenStops = S.intersection <$> allowedStops <*> (prefStops <$> prefs)
    let chosenExcluded = S.intersection <$> allowedStops <*> (prefExcluded <$> prefs)
    (stRes, stView) <- mreq (clickSelectionField stopOptions) (fieldSettingsLabel MsgStopsLabel) chosenStops
    (exRes, exView) <- mreq (clickSelectionField exclOptions) (fieldSettingsLabel MsgExcludedLabel) chosenExcluded
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resStops = stRes,
      viewStops = stView,
      resExcluded = exRes,
      viewExcluded = exView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewStops fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      $with view <- viewExcluded fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{fvInput (viewEasyMode fields)}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewPrevComfort fields)}
      ^{fvInput (viewComfort fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewLocation fields)}
      ^{fvInput (viewAccommodation fields)}
      ^{fvInput (viewStopServices fields)}
      ^{fvInput (viewDayServices fields)}
      ^{fvInput (viewPoiCategories fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewFinish fields)}
      ^{fvInput (viewStartDate fields)}
    |]
    return (res, widget)
    

-- | Form to ensure preferences have been confirmed
confirmPreferencesForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
confirmPreferencesForm _help prefs extra = do
    master <- getYesod
    fields <- defaultPreferenceFields master prefs
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      ^{fvInput (viewEasyMode fields)}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewPrevComfort fields)}
      ^{fvInput (viewComfort fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewLocation fields)}
      ^{fvInput (viewAccommodation fields)}
      ^{fvInput (viewStopServices fields)}
      ^{fvInput (viewDayServices fields)}
      ^{fvInput (viewPoiCategories fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewFinish fields)}
      ^{fvInput (viewStops fields)}
      ^{fvInput (viewExcluded fields)}
      ^{fvInput (viewStartDate fields)}
    |]
    return (res, widget)
