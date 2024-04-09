{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# OPTIONS_GHC -Wno-orphans       #-}
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

  , acceptNoticeForm
  , caminoPreferencesFrom
  , confirmPreferencesForm
  , chooseCaminoForm
  , chooseFitnessForm
  , chooseRangeForm
  , chooseRoutesForm
  , chooseServicesForm
  , chooseStartForm
  , chooseStopsForm
  , defaultPreferenceData
  , travelPreferencesFrom
) where

import Camino.Camino
import Camino.Preferences
import Camino.Util
import Camino.Display.Html (caminoAccommodationTypeIcon, caminoAccommodationTypeMsg, caminoFitnessMsg, caminoServiceIcon, caminoServiceMsg, caminoTravelMsg)
import Camino.Display.I18n (renderCaminoMsg)
import Camino.Display.Routes (renderCaminoRoute)
import Camino.Server.Fields
import Camino.Server.Foundation
import Data.List (find, partition, singleton, sortOn)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Placeholder
import qualified Data.Set as S
import Data.Text (Text, concat, intercalate, pack, splitOn, unpack)
import Text.Hamlet
import Text.Read (readMaybe)
import Yesod

readValue :: (Read a) => Text -> Maybe a
readValue v = readMaybe $ unpack v

writeValue :: (Show a) => a -> Text
writeValue = pack . show

readMaybeValue :: (Read a) => Text -> Maybe (Maybe a)
readMaybeValue v = if v == "--" then Just Nothing else Just <$> readValue v

writeMaybeValue :: (Show a) => Maybe a -> Text
writeMaybeValue v = maybe "--" writeValue v

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

-- Gathered result and widget data
data PreferenceDataFields = PreferenceDataFields {
    resPrevTravel :: FormResult Travel 
  , viewPrevTravel :: FieldView CaminoApp
  , resTravel :: FormResult Travel 
  , viewTravel :: FieldView CaminoApp
  , resPrevFitness :: FormResult Fitness 
  , viewPrevFitness :: FieldView CaminoApp
  , resFitness :: FormResult Fitness 
  , viewFitness :: FieldView CaminoApp
  , resDistance :: FormResult (PreferenceRange Float)
  , viewDistance :: FieldView CaminoApp
  , resTime :: FormResult (PreferenceRange Float)
  , viewTime :: FieldView CaminoApp
  , resStop :: FormResult Penance 
  , viewStop :: FieldView CaminoApp
  , resAccommodation :: FormResult (M.Map AccommodationType Penance)
  , viewAccommodation :: FieldView CaminoApp
  , resStopServices :: FormResult (M.Map Service Penance)
  , viewStopServices :: FieldView CaminoApp
  , resDayServices :: FormResult (M.Map Service Penance)
  , viewDayServices :: FieldView CaminoApp
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
}

findCaminoById :: [Camino] -> Text -> Maybe Camino
findCaminoById caminos val = find (\c -> caminoId c == val') caminos where val' = unpack val

findRouteById :: FormResult Camino -> Text -> Maybe Camino.Camino.Route
findRouteById (FormSuccess camino) val = find (\r -> routeID r == val') (caminoRoutes camino) where val' = unpack val
findRouteById _ val = Just $ placeholder $ unpack val

findLocationById :: FormResult Camino -> Text -> Maybe Location
findLocationById (FormSuccess camino) val = M.lookup val' (caminoLocations camino) where val' = unpack val
findLocationById _ val = Just $ placeholder $ unpack val

findSetById :: (Ord a) => FormResult Camino -> (FormResult Camino -> Text -> Maybe a) -> S.Set Text -> Maybe (S.Set a)
findSetById cres finder val = let
    results = map (finder cres) (S.toList val)
  in
    if any isNothing results then Nothing else Just $ S.fromList $ catMaybes results

fullRoutes :: FormResult Camino -> S.Set Camino.Camino.Route -> S.Set Camino.Camino.Route
fullRoutes (FormSuccess camino) routes = fst $ completeRoutes camino routes
fullRoutes _ routes = routes


-- Make a default set of preference data fields with everything hidden
defaultPreferenceFields :: CaminoApp -> Maybe PreferenceData -> MForm Handler PreferenceDataFields
defaultPreferenceFields master prefs = do
    (trpRes, trpView) <- mreq hiddenField "" (prefTravel <$> prefs) -- Start with original values
    (trRes, trView) <- mreq hiddenField "" (prefTravel <$> prefs)
    (fipRes, fipView) <- mreq hiddenField "" (prefFitness <$> prefs)
    (fiRes, fiView) <- mreq hiddenField "" (prefFitness <$> prefs)
    (diRes, diView) <- mreq hiddenField "" (prefDistance <$> prefs)
    (tiRes, tiView) <- mreq hiddenField "" (prefTime <$> prefs)
    (stRes, stView) <- mreq hiddenField "" (prefStop <$> prefs)
    (acRes, acView) <- mreq hiddenField "" (prefAccommodation <$> prefs)
    (ssRes, ssView) <- mreq hiddenField "" (prefStopServices <$> prefs)
    (dsRes, dsView) <- mreq hiddenField "" (prefDayServices <$> prefs)
    (cpRes, cpView) <- mreq (parsingHiddenField (pack . caminoId) (findCaminoById (caminoAppCaminos master))) "" (prefCamino <$> prefs)
    (cRes, cView) <- mreq (parsingHiddenField (pack . caminoId) (findCaminoById (caminoAppCaminos master))) "" (prefCamino <$> prefs)
    (ropRes, ropView) <- mreq (parsingHiddenField (S.map (pack . routeID)) (\v -> fullRoutes cRes <$> findSetById cRes findRouteById v)) "" (prefRoutes <$> prefs)
    (roRes, roView) <- mreq (parsingHiddenField (S.map (pack . routeID)) (\v -> fullRoutes cRes <$> findSetById cRes findRouteById v)) "" (prefRoutes <$> prefs)
    (sapRes, sapView) <- mreq (parsingHiddenField (pack . locationID) (findLocationById cRes)) "" (prefStart <$> prefs)
    (saRes, saView) <- mreq (parsingHiddenField (pack . locationID) (findLocationById cRes)) "" (prefStart <$> prefs)
    (fnpRes, fnpView) <- mreq (parsingHiddenField (pack . locationID) (findLocationById cRes)) "" (prefFinish <$> prefs)
    (fnRes, fnView) <- mreq (parsingHiddenField (pack . locationID) (findLocationById cRes)) "" (prefFinish <$> prefs)
    (spRes, spView) <- mreq (parsingHiddenField (S.map (pack . locationID)) (findSetById cRes findLocationById)) "" (prefStops <$> prefs)
    (exRes, exView) <- mreq (parsingHiddenField (S.map (pack . locationID)) (findSetById cRes findLocationById)) "" (prefExcluded <$> prefs)
    return PreferenceDataFields {
        resPrevTravel = trpRes
      , viewPrevTravel = trpView
      , resTravel = trRes
      , viewTravel = trView
      , resPrevFitness = fipRes
      , viewPrevFitness = fipView
      , resFitness = fiRes
      , viewFitness = fiView
      , resDistance = diRes
      , viewDistance = diView
      , resTime = tiRes
      , viewTime = tiView
      , resStop = stRes
      , viewStop = stView
      , resAccommodation = acRes
      , viewAccommodation = acView
      , resStopServices = ssRes
      , viewStopServices = ssView
      , resDayServices = dsRes
      , viewDayServices = dsView
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
    }

changed :: (Eq a) => FormResult a -> FormResult a -> Bool
changed (FormSuccess x) (FormSuccess y) = x /= y
changed _ _ = False

makePreferenceData :: CaminoApp -> PreferenceDataFields -> FormResult PreferenceData
makePreferenceData _master fields = let
    travel' = resTravel fields
    fitness' = resFitness fields
    changedTravel = (changed (resPrevTravel fields) travel') || (changed (resPrevFitness fields) fitness')
    dtp = defaultTravelPreferences <$> resTravel fields <*> resFitness fields
    distance' = if changedTravel then preferenceDistance <$> dtp else resDistance fields
    time' = if changedTravel then preferenceTime <$> dtp else resTime fields
    stop' = if changedTravel then preferenceStop <$> dtp else resStop fields
    accommodation' = if changedTravel then preferenceAccommodation <$> dtp else resAccommodation fields
    stopServices' = if changedTravel then preferenceStopServices <$> dtp else resStopServices fields
    dayServices' = if changedTravel then preferenceDayServices <$> dtp else resDayServices fields
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
    stops' = if changedStart then recommendedStops <$> dcp'' else resStops fields
    excluded' = if changedStart then preferenceExcluded <$> dcp'' else resExcluded fields
  in
    PreferenceData
      <$> travel'
      <*> fitness'
      <*> distance'
      <*> time'
      <*> stop'
      <*> accommodation'
      <*> stopServices'
      <*> dayServices'
      <*> camino'
      <*> routes'
      <*> start'
      <*> finish'
      <*> stops'
      <*> excluded'

-- | Form to allow fitness settings to be chosen
chooseFitnessForm :: Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseFitnessForm prefs extra = do
    master <- getYesod
    let render = renderCaminoMsg (caminoAppConfig master)
    let  travelField =  extendedRadioFieldList render (map (\f -> (pack $ show f, caminoTravelMsg f, f, Nothing)) travelEnumeration)
    let  fitnessField =  extendedRadioFieldList render (map (\f -> (pack $ show f, caminoFitnessMsg f, f, Nothing)) fitnessEnumeration)
    (trRes, trView) <- mreq travelField (fieldSettingsLabel MsgSelectTravel) (prefTravel <$> prefs)
    (fRes, fView) <- mreq fitnessField (fieldSettingsLabel MsgSelectFitness) (prefFitness <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resTravel = trRes,
      viewTravel = trView,
      resFitness = fRes,
      viewFitness = fView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewTravel fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view}
            ^{fvInput view}
      $with view <- viewFitness fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view}
            ^{fvInput view}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewStop fields)}
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
    |]
    return (res, widget)

-- | Form to allow preference range settings to be chosen
chooseRangeForm :: Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseRangeForm prefs extra = do
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
              ^{fvLabel view}
            ^{fvInput view}
      $with view <- viewTime fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view}
            ^{fvInput view}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewStop fields)}
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
    |]
    return (res, widget)


-- | Form to allow service preferences to be chosen
chooseServicesForm :: Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseServicesForm prefs extra = do
    master <- getYesod
    langs <- languages
    let config = caminoAppConfig master
    let router = renderCaminoRoute config langs
    let messages = renderCaminoMsg config
    let accommodationOptions = map (\v -> (v, [ihamlet|^{caminoAccommodationTypeIcon v}&nbsp;_{caminoAccommodationTypeMsg v}|] messages router)) accommodationTypeEnumeration
    let stopServiceOptions = map (\v -> (v, [ihamlet|^{caminoServiceIcon v}&nbsp;_{caminoServiceMsg v}|] messages router)) serviceEnumeration
    let dayServiceOptions = map (\v -> (v, [ihamlet|^{caminoServiceIcon v}&nbsp;_{caminoServiceMsg v}|] messages router)) townServiceEnumeration
    (stRes, stView) <- mreq penanceField (fieldSettingsLabel MsgStopPreferencesLabel) (prefStop <$> prefs)
    (acRes, acView) <- mreq (penanceMapField accommodationOptions) (fieldSettingsLabel MsgAccommodationPreferencesLabel) (prefAccommodation <$> prefs)
    (ssRes, ssView) <- mreq (penanceMapField stopServiceOptions) (fieldSettingsLabel MsgStopServicePreferencesLabel) (prefStopServices <$> prefs)
    (dsRes, dsView) <- mreq (penanceMapField dayServiceOptions) (fieldSettingsLabel MsgDayServicePreferencesLabel) (prefDayServices <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resStop = stRes,
      viewStop = stView,
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
      $with view <- viewStop fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view}
            ^{fvInput view}
      $with view <- viewAccommodation fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view}
            ^{fvInput view}
      $with view <- viewStopServices fields
         <div .row .mb-3>
           <div .col>
             <label for="#{fvId view}">
               ^{fvLabel view}
             ^{fvInput view}
      $with view <- viewDayServices fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view}
            ^{fvInput view}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
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
    |]
    return (res, widget)


-- | Form to allow the camino to be chosen
chooseCaminoForm :: Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseCaminoForm prefs extra = do
    master <- getYesod
    let  caminoField =  extendedRadioFieldList id (map (\c -> (pack $ caminoId c, toHtml $ caminoName c, c, Just $ toHtml $ caminoDescription c)) (caminoAppCaminos master))
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
              ^{fvLabel view}
            ^{fvInput view}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewStop fields)}
      ^{fvInput (viewAccommodation fields)}
      ^{fvInput (viewStopServices fields)}
      ^{fvInput (viewDayServices fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewFinish fields)}
      ^{fvInput (viewStops fields)}
      ^{fvInput (viewExcluded fields)}
    |]
    return (res, widget)


-- | Form to allow the routes to be chosen
chooseRoutesForm :: Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseRoutesForm prefs extra = do
    master <- getYesod
    let camino = prefCamino <$> prefs
    let routes = maybe (Prelude.concat (map caminoRoutes (caminoAppCaminos master))) caminoRoutes camino
    let requirementClauses = maybe [] (\c -> Prelude.concat $ map createRequiresClauses (caminoRouteLogic c)) camino
    let allowedClauses = maybe [] (\c -> Prelude.concat $ map createAllowsClauses (caminoRouteLogic c)) camino
    let prohibitedClauses = maybe [] (\c -> Prelude.concat $ map createProhibitsClauses (caminoRouteLogic c)) camino
    let routeOptions = map (\r -> (pack $ routeID r, routeName r, r, Just $ routeDescription r, maybe False (\c -> r == caminoDefaultRoute c) camino)) routes
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
              ^{fvLabel view}
            ^{fvInput view}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewStop fields)}
      ^{fvInput (viewAccommodation fields)}
      ^{fvInput (viewStopServices fields)}
      ^{fvInput (viewDayServices fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewFinish fields)}
      ^{fvInput (viewStops fields)}
      ^{fvInput (viewExcluded fields)}
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
chooseStartForm :: Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseStartForm prefs extra = do
    master <- getYesod
    render <- getMessageRender
    let camino = prefCamino <$> prefs
    let routes = prefRoutes <$> prefs
    let start' = prefStart <$> prefs
    let finish' = prefFinish <$> prefs
    let cprefs = CaminoPreferences <$> camino <*> start' <*> finish' <*> routes <*> pure S.empty <*> pure S.empty
    let caminos = maybe (caminoAppCaminos master) singleton camino
    let allStops = Prelude.concat (map (M.elems . caminoLocations) caminos)
    let possibleStops = caminoRouteLocations <$> camino <*> routes
    let allowedStops = permittedStops <$> prefs <*> possibleStops
    let sortKey l = canonicalise $ locationName l
    let stops = sortOn sortKey $ maybe allStops S.toList allowedStops
    let rstarts = maybe [] suggestedStarts cprefs
    let rfinishes = maybe [] suggestedFinishes cprefs
    let startOptions = makeOptions render (pack . locationID) locationName rstarts stops
    let finishOptions = makeOptions render (pack . locationID) locationName rfinishes stops
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
              ^{fvLabel view}
            ^{fvInput view}
      $with view <- viewFinish fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view}
            ^{fvInput view}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewStop fields)}
      ^{fvInput (viewAccommodation fields)}
      ^{fvInput (viewStopServices fields)}
      ^{fvInput (viewDayServices fields)}
      ^{fvInput (viewPrevCamino fields)}
      ^{fvInput (viewCamino fields)}
      ^{fvInput (viewPrevRoutes fields)}
      ^{fvInput (viewRoutes fields)}
      ^{fvInput (viewPrevStart fields)}
      ^{fvInput (viewPrevFinish fields)}
      ^{fvInput (viewStops fields)}
      ^{fvInput (viewExcluded fields)}
    |]
    return (res, widget)

-- | Form to allow the stops to be chosen
chooseStopsForm :: Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseStopsForm prefs extra = do
    master <- getYesod
    render <- getMessageRender
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
    let sortKey l = canonicalise $ locationName l
    let stops = sortOn sortKey $ maybe allStops S.toList allowedStops
    let recommended = maybe S.empty recommendedStops cprefs'
    let (suggested, other) = Data.List.partition (\l -> S.member l recommended) stops
    let mkOptions locs = map (\l -> (pack $ locationID l, locationName l, l)) locs
    let stopOptions = (render MsgSuggestedLabel, mkOptions suggested) : (map (\(m, ls) -> (m, mkOptions ls)) (Camino.Util.partition (categorise . locationName) other))
    let exclOptions = (render MsgSuggestedLabel, []) : (map (\(m, ls) -> (m, mkOptions ls)) (Camino.Util.partition (categorise . locationName) stops))
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
              ^{fvLabel view}
            ^{fvInput view}
      $with view <- viewExcluded fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view}
            ^{fvInput view}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewStop fields)}
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
    |]
    return (res, widget)
    

-- | Form to ensure preferences have been confirmed
confirmPreferencesForm :: Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
confirmPreferencesForm prefs extra = do
    master <- getYesod
    fields <- defaultPreferenceFields master prefs
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      ^{fvInput (viewPrevTravel fields)}
      ^{fvInput (viewTravel fields)}
      ^{fvInput (viewPrevFitness fields)}
      ^{fvInput (viewFitness fields)}
      ^{fvInput (viewDistance fields)}
      ^{fvInput (viewTime fields)}
      ^{fvInput (viewStop fields)}
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
    |]
    return (res, widget)

acceptNoticeForm :: Maybe Bool -> Html -> MForm Handler (FormResult Bool, Widget)
acceptNoticeForm accept extra = do
  render <- getMessageRender
  (res, acceptView) <- mreq (extendedCheckboxField (\m -> toHtml $ render m) MsgAcceptDisclaimerLabel Nothing) "" accept
  let widget = [whamlet|
    #{extra}
    ^{fvInput acceptView}
  |]
  return (res, widget)
