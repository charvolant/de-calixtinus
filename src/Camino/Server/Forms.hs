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
  , choosePoiForm
  , chooseRangeForm
  , chooseRoutesForm
  , chooseRestServicesForm
  , chooseStockServicesForm
  , chooseStopServicesForm
  , chooseStartForm
  , chooseStopsForm
  , chooseTravelForm
  , defaultPreferenceData
  , findCaminoById
  , travelPreferencesFrom
) where

import Camino.Camino
import Camino.Preferences
import Data.Util
import Camino.Display.Html (caminoAccommodationTypeIcon, caminoAccommodationTypeMsg, caminoComfortMsg, caminoFitnessMsg, caminoLocationTypeIcon, caminoLocationTypeLabel, caminoPoiCategoryLabel, caminoServiceIcon, caminoServiceMsg, caminoTravelMsg, descriptionBlock)
import Camino.Display.I18n (renderCaminoMsg)
import Camino.Display.Routes (renderCaminoRoute)
import Camino.Server.Fields
import Camino.Server.Foundation
import Data.List (find, partition, singleton, sortOn)
import Data.Localised (Locale, localiseText)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, isNothing)
import Data.Placeholder
import qualified Data.Set as S
import Data.Summary
import Data.Text (Text, pack)
import Data.Time.Calendar (Day)
import Formatting
import Text.Hamlet
import Yesod
import Camino.Preferences (CaminoPreferences(preferenceRestPoints), recommendedRestPoints)

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
  , resRest :: FormResult (PreferenceRange Int)
  , viewRest :: FieldView CaminoApp
  , resRestPressure :: FormResult (Maybe Float)
  , viewRestPressure :: FieldView CaminoApp
  , resStopTransportLinks :: FormResult Bool
  , viewStopTransportLinks :: FieldView CaminoApp
  , resStopLocation :: FormResult (M.Map LocationType Penance)
  , viewStopLocation :: FieldView CaminoApp
  , resStopAccommodation :: FormResult (M.Map AccommodationType Penance)
  , viewStopAccommodation :: FieldView CaminoApp
  , resStopServices :: FormResult (M.Map Service Penance)
  , viewStopServices :: FieldView CaminoApp
  , resStopRouteServices :: FormResult (M.Map Service Penance)
  , viewStopRouteServices :: FieldView CaminoApp
  , resStockTransportLinks :: FormResult Bool
  , viewStockTransportLinks :: FieldView CaminoApp
  , resStockLocation :: FormResult (M.Map LocationType Penance)
  , viewStockLocation :: FieldView CaminoApp
  , resStockAccommodation :: FormResult (M.Map AccommodationType Penance)
  , viewStockAccommodation :: FieldView CaminoApp
  , resStockServices :: FormResult (M.Map Service Penance)
  , viewStockServices :: FieldView CaminoApp
  , resStockRouteServices :: FormResult (M.Map Service Penance)
  , viewStockRouteServices :: FieldView CaminoApp
  , resRestTransportLinks :: FormResult Bool
  , viewRestTransportLinks :: FieldView CaminoApp
  , resRestLocation :: FormResult (M.Map LocationType Penance)
  , viewRestLocation :: FieldView CaminoApp
  , resRestAccommodation :: FormResult (M.Map AccommodationType Penance)
  , viewRestAccommodation :: FieldView CaminoApp
  , resRestServices :: FormResult (M.Map Service Penance)
  , viewRestServices :: FieldView CaminoApp
  , resRestRouteServices :: FormResult (M.Map Service Penance)
  , viewRestRouteServices :: FieldView CaminoApp
  , resPrevPoiCategories :: FormResult (S.Set PoiCategory)
  , viewPrevPoiCategories :: FieldView CaminoApp
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
  , resRestPoints :: FormResult (S.Set Location)
  , viewRestPoints :: FieldView CaminoApp
  , resPois :: FormResult (S.Set PointOfInterest)
  , viewPois :: FieldView CaminoApp
  , resStartDate :: FormResult Day
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

findPointOfInterestById :: FormResult Camino -> Text -> Maybe PointOfInterest
findPointOfInterestById (FormSuccess camino) val = fst <$> M.lookup val (caminoPois camino)
findPointOfInterestById _ val = Just $ placeholder val

findSetById :: (Ord a) => FormResult Camino -> (FormResult Camino -> Text -> Maybe a) -> S.Set Text -> Maybe (S.Set a)
findSetById cres finder val = let
    results = map (finder cres) (S.toList val)
  in
    if any isNothing results then Nothing else Just $ S.fromList $ catMaybes results

findLocationByPoi :: Maybe Camino -> PointOfInterest -> Maybe Location
findLocationByPoi Nothing _poi = Nothing
findLocationByPoi (Just camino) poi = snd <$> M.lookup (poiID poi) (caminoPois camino)

fullRoutes :: FormResult Camino -> S.Set Camino.Camino.Route -> S.Set Camino.Camino.Route
fullRoutes (FormSuccess camino) routes = fst $ completeRoutes camino routes
fullRoutes _ routes = routes

fieldSettingsLabelName :: RenderMessage site msg => msg -> Text -> FieldSettings site
fieldSettingsLabelName msg name = FieldSettings (SomeMessage msg) Nothing Nothing (Just name) []

fieldSettingsName :: Text -> FieldSettings site
fieldSettingsName name = FieldSettings "" Nothing Nothing (Just name) []

-- Make a default set of preference data fields with everything hidden
defaultPreferenceFields :: CaminoApp -> Maybe PreferenceData -> MForm Handler PreferenceDataFields
defaultPreferenceFields master prefs = do
    (emRes, emView) <- mreq hiddenField (fieldSettingsName "easyMode") (prefEasyMode <$> prefs) -- Easy/detailed mode
    (trpRes, trpView) <- mreq hiddenField (fieldSettingsName "prevTravel") (prefTravel <$> prefs) -- Original values
    (trRes, trView) <- mreq hiddenField (fieldSettingsName "travel") (prefTravel <$> prefs)
    (fipRes, fipView) <- mreq hiddenField (fieldSettingsName "prevFitness") (prefFitness <$> prefs)
    (fiRes, fiView) <- mreq hiddenField (fieldSettingsName "fitness") (prefFitness <$> prefs)
    (copRes, copView) <- mreq hiddenField (fieldSettingsName "prevComfort") (prefComfort <$> prefs)
    (coRes, coView) <- mreq hiddenField (fieldSettingsName "comfort") (prefComfort <$> prefs)
    (diRes, diView) <- mreq hiddenField (fieldSettingsName "distance") (prefDistance <$> prefs)
    (tiRes, tiView) <- mreq hiddenField (fieldSettingsName "time") (prefTime <$> prefs)
    (reRes, reView) <- mreq hiddenField (fieldSettingsName "rest") (prefRest <$> prefs)
    (rpRes, rpView) <- mreq hiddenField (fieldSettingsName "restPressure") (prefRestPressure <$> prefs)
    (sptlRes, sptlView) <- mreq hiddenField (fieldSettingsName "stopTransportLinks") (stopTransportLinks <$> prefStop <$> prefs)
    (sploRes, sploView) <- mreq hiddenField (fieldSettingsName "stopLocation") (stopLocation <$> prefStop <$> prefs)
    (spacRes, spacView) <- mreq hiddenField (fieldSettingsName "stopAccommodation") (stopAccommodation <$> prefStop <$> prefs)
    (spssRes, spssView) <- mreq hiddenField (fieldSettingsName "stopServices") (stopServices <$> prefStop <$> prefs)
    (sprsRes, sprsView) <- mreq hiddenField (fieldSettingsName "stopRouteServices") (stopRouteServices <$> prefStop <$> prefs)
    (sttlRes, sttlView) <- mreq hiddenField (fieldSettingsName "stockStopTransportLinks") (stopTransportLinks <$> prefStockStop <$> prefs)
    (stloRes, stloView) <- mreq hiddenField (fieldSettingsName "stockStopLocation") (stopLocation <$> prefStockStop <$> prefs)
    (stacRes, stacView) <- mreq hiddenField (fieldSettingsName "stockStopAccommodation") (stopAccommodation <$> prefStockStop <$> prefs)
    (stssRes, stssView) <- mreq hiddenField (fieldSettingsName "stockStopServices") (stopServices <$> prefStockStop <$> prefs)
    (strsRes, strsView) <- mreq hiddenField (fieldSettingsName "stockStopRouteServices") (stopRouteServices <$> prefStockStop <$> prefs)
    (rstlRes, rstlView) <- mreq hiddenField (fieldSettingsName "restTransportLinks") (stopTransportLinks <$> prefRestStop <$> prefs)
    (rsloRes, rsloView) <- mreq hiddenField (fieldSettingsName "restLocation") (stopLocation <$> prefRestStop <$> prefs)
    (rsacRes, rsacView) <- mreq hiddenField (fieldSettingsName "restAccommodation") (stopAccommodation <$> prefRestStop <$> prefs)
    (rsssRes, rsssView) <- mreq hiddenField (fieldSettingsName "restServices") (stopServices <$> prefRestStop <$> prefs)
    (rsrsRes, rsrsView) <- mreq hiddenField (fieldSettingsName "restRouteServices") (stopRouteServices <$> prefRestStop <$> prefs)
    (pcpRes, pcpView) <- mreq hiddenField (fieldSettingsName "prevPoiCategories") (prefPoiCategories <$> prefs)
    (pcRes, pcView) <- mreq hiddenField (fieldSettingsName "poiCategories") (prefPoiCategories <$> prefs)
    (cpRes, cpView) <- mreq (parsingHiddenField caminoId (findCaminoById (caminoAppCaminoConfig master))) (fieldSettingsName "prevCamino") (prefCamino <$> prefs)
    (cRes, cView) <- mreq (parsingHiddenField caminoId (findCaminoById (caminoAppCaminoConfig master))) (fieldSettingsName "camino") (prefCamino <$> prefs)
    (ropRes, ropView) <- mreq (parsingHiddenField (S.map routeID) (\v -> fullRoutes cRes <$> findSetById cRes findRouteById v)) (fieldSettingsName "prevRoutes") (prefRoutes <$> prefs)
    (roRes, roView) <- mreq (parsingHiddenField (S.map routeID) (\v -> fullRoutes cRes <$> findSetById cRes findRouteById v)) (fieldSettingsName "routes") (prefRoutes <$> prefs)
    (sapRes, sapView) <- mreq (parsingHiddenField locationID (findLocationById cRes)) (fieldSettingsName "prevStart") (prefStart <$> prefs)
    (saRes, saView) <- mreq (parsingHiddenField locationID (findLocationById cRes)) (fieldSettingsName "start") (prefStart <$> prefs)
    (fnpRes, fnpView) <- mreq (parsingHiddenField locationID (findLocationById cRes)) (fieldSettingsName "prevFinish") (prefFinish <$> prefs)
    (fnRes, fnView) <- mreq (parsingHiddenField locationID (findLocationById cRes)) (fieldSettingsName "finish") (prefFinish <$> prefs)
    (spRes, spView) <- mreq (parsingHiddenField (S.map locationID) (findSetById cRes findLocationById)) (fieldSettingsName "stops") (prefStops <$> prefs)
    (exRes, exView) <- mreq (parsingHiddenField (S.map locationID) (findSetById cRes findLocationById)) (fieldSettingsName "excluded") (prefExcluded <$> prefs)
    (rspRes, rspView) <- mreq (parsingHiddenField (S.map locationID) (findSetById cRes findLocationById)) (fieldSettingsName "restPoints") (prefRestPoints <$> prefs)
    (poRes, poView) <- mreq (parsingHiddenField (S.map poiID) (findSetById cRes findPointOfInterestById)) (fieldSettingsName "pois") (prefPois <$> prefs)
    (sdRes, sdView) <- mreq hiddenField (fieldSettingsName "startDate") (prefStartDate <$> prefs)
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
      , resRest = reRes
      , viewRest = reView
      , resRestPressure = rpRes
      , viewRestPressure = rpView
      , resStopTransportLinks = sptlRes
      , viewStopTransportLinks = sptlView
      , resStopLocation = sploRes
      , viewStopLocation = sploView
      , resStopAccommodation = spacRes
      , viewStopAccommodation = spacView
      , resStopServices = spssRes
      , viewStopServices = spssView
      , resStopRouteServices = sprsRes
      , viewStopRouteServices = sprsView
      , resStockTransportLinks = sttlRes
      , viewStockTransportLinks = sttlView
      , resStockLocation = stloRes
      , viewStockLocation = stloView
      , resStockAccommodation = stacRes
      , viewStockAccommodation = stacView
      , resStockServices = stssRes
      , viewStockServices = stssView
      , resStockRouteServices = strsRes
      , viewStockRouteServices = strsView
      , resRestTransportLinks = rstlRes
      , viewRestTransportLinks = rstlView
      , resRestLocation = rsloRes
      , viewRestLocation = rsloView
      , resRestAccommodation = rsacRes
      , viewRestAccommodation = rsacView
      , resRestServices = rsssRes
      , viewRestServices = rsssView
      , resRestRouteServices = rsrsRes
      , viewRestRouteServices = rsrsView
      , resPrevPoiCategories = pcpRes
      , viewPrevPoiCategories = pcpView
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
      , resRestPoints = rspRes
      , viewRestPoints = rspView
      , resPois = poRes
      , viewPois = poView
      , resStartDate = sdRes
      , viewStartDate = sdView
    }

changed :: (Eq a) => FormResult a -> FormResult a -> Bool
changed (FormSuccess x) (FormSuccess y) = x /= y
changed _ _ = False

-- | For debugging
_showResultState :: FormResult a -> String
_showResultState (FormSuccess _) = "OK"
_showResultState (FormFailure msg) = show msg
_showResultState FormMissing = "Missing"

-- | For debugging
_showResultValue :: (Summary a) => FormResult a -> String
_showResultValue (FormSuccess v) = summaryString v
_showResultValue (FormFailure msg) = "Failure " ++ show msg
_showResultValue FormMissing = "Missing"

-- | For debugging
-- _traceResultValue :: (Summary a) => FormResult a -> FormResult a
-- _traceResultValue v = trace (_showResultValue v) v

makePreferenceData :: CaminoApp -> PreferenceDataFields -> FormResult PreferenceData
makePreferenceData _master fields = let
    easy' = resEasyMode fields
    easy'' = case easy' of
      (FormSuccess e) -> e
      _ -> False
    travel' = resTravel fields
    fitness' = resFitness fields
    comfort' = resComfort fields
    poiCategories' = resPoiCategories fields
    changedTravel = (changed (resPrevTravel fields) travel') || (changed (resPrevFitness fields) fitness') || (changed (resPrevComfort fields) comfort')
    changedPoiCategories = changed (resPrevPoiCategories fields) poiCategories'
    dtp = defaultTravelPreferences <$> travel' <*> fitness' <*> comfort' <*> (Just <$> poiCategories')
    distance' = if easy'' || changedTravel then preferenceDistance <$> dtp else resDistance fields
    time' = if easy'' || changedTravel then preferenceTime <$> dtp else resTime fields
    rest' = if easy'' || changedTravel then preferenceRest <$> dtp else resRest fields
    restpressure' = if easy'' || changedTravel then preferenceRestPressure <$> dtp else resRestPressure fields
    spTransportLinks' = if easy'' || changedTravel then stopTransportLinks <$> preferenceStop <$> dtp else resStopTransportLinks fields
    spLocation' = if easy'' || changedTravel then stopLocation <$> preferenceStop <$> dtp else resStopLocation fields
    spAccommodation' = if easy'' || changedTravel then stopAccommodation <$> preferenceStop <$> dtp else resStopAccommodation fields
    spServices' = if easy'' || changedTravel then stopServices <$> preferenceStop <$> dtp else resStopServices fields
    spRouteServices' = if easy'' || changedTravel then stopRouteServices <$> preferenceStop <$> dtp else resStopRouteServices fields
    stop' = StopPreferences <$> spTransportLinks' <*> spLocation' <*> spAccommodation' <*> spServices' <*> spRouteServices'
    stTransportLinks' = if easy'' || changedTravel then stopTransportLinks <$> preferenceStockStop <$> dtp else resStockTransportLinks fields
    stLocation' = if easy'' || changedTravel then stopLocation <$> preferenceStockStop <$> dtp else resStockLocation fields
    stAccommodation' = if easy'' || changedTravel then stopAccommodation <$> preferenceStockStop <$> dtp else resStockAccommodation fields
    stServices' = if easy'' || changedTravel then stopServices <$> preferenceStockStop <$> dtp else resStockServices fields
    stRouteServices' = if easy'' || changedTravel then stopRouteServices <$> preferenceStockStop <$> dtp else resStockRouteServices fields
    stopStock' = StopPreferences <$> stTransportLinks' <*> stLocation' <*> stAccommodation' <*> stServices' <*> stRouteServices'
    rsTransportLinks' = if easy'' || changedTravel then stopTransportLinks <$> preferenceRestStop <$> dtp else resRestTransportLinks fields
    rsLocation' = if easy'' || changedTravel then stopLocation <$> preferenceRestStop <$> dtp else resRestLocation fields
    rsAccommodation' = if easy'' || changedTravel then stopAccommodation <$> preferenceRestStop <$> dtp else resRestAccommodation fields
    rsServices' = if easy'' || changedTravel then stopServices <$> preferenceRestStop <$> dtp else resRestServices fields
    rsRouteServices' = if easy'' || changedTravel then stopRouteServices <$> preferenceRestStop <$> dtp else resRestRouteServices fields
    stopRest' = StopPreferences <$> rsTransportLinks' <*> rsLocation' <*> rsAccommodation' <*> rsServices' <*> rsRouteServices'
    changedCamino = changed (caminoId <$> resPrevCamino fields) (caminoId <$> resCamino fields)
    camino' = resCamino fields
    dcp = defaultCaminoPreferences <$> camino'
    routes' = if changedCamino then preferenceRoutes <$> dcp else fullRoutes (resCamino fields) <$> (resRoutes fields)
    dcp' = withRoutes <$> dcp <*> routes'
    changedRoutes = changedCamino || changed (S.map routeID <$> resPrevRoutes fields) (S.map routeID <$> routes')
    start' = if changedRoutes then preferenceStart <$> dcp' else resStart fields
    finish' = if changedRoutes then preferenceFinish <$> dcp' else resFinish fields
    dcp'' = withStartFinish <$> dcp' <*> start' <*> finish'
    changedStart = changedRoutes || changed (locationID <$> resPrevStart fields) (locationID <$> start') || changed (locationID <$> resPrevFinish fields) (locationID <$> finish')
    stops' = if easy'' || changedStart then recommendedStops <$> dcp'' else resStops fields
    excluded' = if easy'' || changedStart then preferenceExcluded <$> dcp'' else resExcluded fields
    restpoints' = if easy'' || changedStart then recommendedRestPoints <$> dcp'' else resRestPoints fields
    pois' = if easy'' || changedCamino || changedRoutes || changedPoiCategories then recommendedPois <$> dtp <*> dcp'' else resPois fields
    startDate' = resStartDate fields
  in
    -- trace (show $ [_showResultState easy', _showResultState travel', _showResultState fitness', _showResultState comfort', _showResultState distance', _showResultState time', _showResultState rest', _showResultState stop', _showResultState stopStock', _showResultState stopRest', _showResultState poiCategories', _showResultState camino', _showResultState routes', _showResultState start', _showResultState finish', _showResultState stops', _showResultState excluded', _showResultState pois', _showResultState startDate']) $
    PreferenceData
      <$> easy'
      <*> travel'
      <*> fitness'
      <*> comfort'
      <*> distance'
      <*> time'
      <*> rest'
      <*> restpressure'
      <*> stop'
      <*> stopStock'
      <*> stopRest'
      <*> poiCategories'
      <*> camino'
      <*> routes'
      <*> start'
      <*> finish'
      <*> stops'
      <*> excluded'
      <*> restpoints'
      <*> pois'
      <*> startDate'

-- | Fill out hidden fields
hiddenPreferences :: [Text] -> PreferenceDataFields -> Widget
hiddenPreferences exclude fields = [whamlet|
  $if notElem "EasyMode" exclude
    ^{fvInput (viewEasyMode fields)}
  $if notElem "PrevTravel" exclude
    ^{fvInput (viewPrevTravel fields)}
  $if notElem "Travel" exclude
    ^{fvInput (viewTravel fields)}
  $if notElem "PrevFitness" exclude
    ^{fvInput (viewPrevFitness fields)}
  $if notElem "Fitness" exclude
    ^{fvInput (viewFitness fields)}
  $if notElem "PrevComfort" exclude
    ^{fvInput (viewPrevComfort fields)}
  $if notElem "Comfort" exclude
    ^{fvInput (viewComfort fields)}
  $if notElem "Distance" exclude
    ^{fvInput (viewDistance fields)}
  $if notElem "Time" exclude
    ^{fvInput (viewTime fields)}
  $if notElem "Rest" exclude
    ^{fvInput (viewRest fields)}
  $if notElem "RestPressure" exclude
    ^{fvInput (viewRestPressure fields)}
  $if notElem "StopTransportLinks" exclude
    ^{fvInput (viewStopTransportLinks fields)}
  $if notElem "StopLocation" exclude
    ^{fvInput (viewStopLocation fields)}
  $if notElem "StopAccommodation" exclude
    ^{fvInput (viewStopAccommodation fields)}
  $if notElem "StopServices" exclude
    ^{fvInput (viewStopServices fields)}
  $if notElem "StopRouteServices" exclude
    ^{fvInput (viewStopRouteServices fields)}
  $if notElem "StockTransportLinks" exclude
    ^{fvInput (viewStockTransportLinks fields)}
  $if notElem "StockLocation" exclude
    ^{fvInput (viewStockLocation fields)}
  $if notElem "StockAccommodation" exclude
    ^{fvInput (viewStockAccommodation fields)}
  $if notElem "StockServices" exclude
    ^{fvInput (viewStockServices fields)}
  $if notElem "StockRouteServices" exclude
    ^{fvInput (viewStockRouteServices fields)}
  $if notElem "RestTransportLinks" exclude
    ^{fvInput (viewRestTransportLinks fields)}
  $if notElem "RestLocation" exclude
    ^{fvInput (viewRestLocation fields)}
  $if notElem "RestAccommodation" exclude
    ^{fvInput (viewRestAccommodation fields)}
  $if notElem "RestServices" exclude
    ^{fvInput (viewRestServices fields)}
  $if notElem "RestRouteServices" exclude
    ^{fvInput (viewRestRouteServices fields)}
  $if notElem "PrevPoiCategories" exclude
    ^{fvInput (viewPrevPoiCategories fields)}
  $if notElem "PoiCategories" exclude
    ^{fvInput (viewPoiCategories fields)}
  $if notElem "PrevCamino" exclude
    ^{fvInput (viewPrevCamino fields)}
  $if notElem "Camino" exclude
    ^{fvInput (viewCamino fields)}
  $if notElem "PrevRoutes" exclude
    ^{fvInput (viewPrevRoutes fields)}
  $if notElem "Routes" exclude
    ^{fvInput (viewRoutes fields)}
  $if notElem "PrevStart" exclude
    ^{fvInput (viewPrevStart fields)}
  $if notElem "Start" exclude
    ^{fvInput (viewStart fields)}
  $if notElem "PrevFinish" exclude
    ^{fvInput (viewPrevFinish fields)}
  $if notElem "Finish" exclude
    ^{fvInput (viewFinish fields)}
  $if notElem "Stops" exclude
    ^{fvInput (viewStops fields)}
  $if notElem "Excluded" exclude
    ^{fvInput (viewExcluded fields)}
  $if notElem "RestPoints" exclude
    ^{fvInput (viewRestPoints fields)}
  $if notElem "Pois" exclude
    ^{fvInput (viewPois fields)}
  $if notElem "StartDate" exclude
    ^{fvInput (viewStartDate fields)}
|]
-- | Form for basic travel preferences
chooseTravelForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseTravelForm help prefs extra = do
    master <- getYesod
    locales <- getLocales
    mRender <- getMessageRender
    let render = renderCaminoMsg (caminoAppConfig master) locales
    let easyField =  extendedCheckboxField (toHtml . mRender) MsgEasyMode (Just MsgEasyModeText)
    let travelField =  extendedRadioFieldList render (map (\f -> (pack $ show f, caminoTravelMsg f, f, Nothing)) travelEnumeration)
    let fitnessField =  extendedRadioFieldList render (map (\f -> (pack $ show f, caminoFitnessMsg f, f, Nothing)) fitnessEnumeration)
    let comfortField =  extendedRadioFieldList render (map (\f -> (pack $ show f, caminoComfortMsg f, f, Nothing)) comfortEnumeration)
    let poiField = extendedCheckboxFieldList render (map (\f -> (pack $ show f, caminoPoiCategoryLabel f, f, Nothing)) poiCategoryEnumeration)
    (emRes, emView) <- mreq easyField "" (prefEasyMode <$> prefs)
    (trRes, trView) <- mreq travelField (fieldSettingsLabelName MsgSelectTravel "travel") (prefTravel <$> prefs)
    (fRes, fView) <- mreq fitnessField (fieldSettingsLabelName MsgSelectFitness "fitness") (prefFitness <$> prefs)
    (cRes, cView) <- mreq comfortField (fieldSettingsLabelName MsgSelectComfort "comfort") (prefComfort <$> prefs)
    (pcRes, pcView) <- mreq poiField (fieldSettingsLabelName MsgSelectPoiCategories "poiCategories") (prefPoiCategories <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
        resEasyMode = emRes
      , viewEasyMode = emView
      , resTravel = trRes
      , viewTravel = trView
      , resFitness = fRes
      , viewFitness = fView
      , resComfort = cRes
      , viewComfort = cView
      , resPoiCategories = pcRes
      , viewPoiCategories = pcView
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
      $with view <- viewPoiCategories fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{hiddenPreferences ["EasyMode", "Travel", "Fitness", "Comfort", "PoiCategories"] fields}
    |]
    return (res, widget)

-- | Form to allow preference range settings to be chosen
chooseRangeForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseRangeForm help prefs extra = do
    master <- getYesod
    let distanceRangeField = if maybe Walking prefTravel prefs == Cycling then rangeField 0.0 250.0 1.0 else rangeField 0.0 50.0 0.5
    let timeRangeField = rangeField 0.0 16.0 0.1
    let restRangeField = rangeField 0 10 1
    let restPressureField = floatField 0 20 1
    (diRes, diView) <- mreq distanceRangeField (fieldSettingsLabelName MsgDistancePreferencesLabel "distance") (prefDistance <$> prefs)
    (tiRes, tiView) <- mreq timeRangeField (fieldSettingsLabelName MsgTimePreferencesLabel "time") (prefTime <$> prefs)
    (reRes, reView) <- mreq restRangeField (fieldSettingsLabelName MsgRestPreferencesLabel "rest") (prefRest <$> prefs)
    (rpRes, rpView) <- mopt restPressureField (fieldSettingsLabelName MsgRestPressureLabel "restPressure") (prefRestPressure <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
        resDistance = diRes
      , viewDistance = diView
      , resTime = tiRes
      , viewTime = tiView
      , resRest = reRes
      , viewRest = reView
      , resRestPressure = rpRes
      , viewRestPressure = rpView
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
      $with view <- viewRest fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      $with view <- viewRestPressure fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{hiddenPreferences ["Distance", "Time", "Rest", "RestPressure"] fields}
    |]
    return (res, widget)

-- | Form to allow service preferences to be chosen
chooseStopServicesForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseStopServicesForm help prefs extra = do
    master <- getYesod
    locales <- getLocales
    mRender <- getMessageRender
    let config = caminoAppConfig master
    let router = renderCaminoRoute config locales
    let messages = renderCaminoMsg config locales
    let transportLinksField =  extendedCheckboxField (toHtml . mRender) MsgTransportLinks (Just MsgTransportLinksText)
    let locationOptions = map (\v -> (v, [ihamlet|<span .location-type-sample>^{caminoLocationTypeIcon v}</span>&nbsp;_{caminoLocationTypeLabel v}|] messages router)) locationStopTypeEnumeration
    let accommodationOptions = map (\v -> (v, [ihamlet|^{caminoAccommodationTypeIcon v}&nbsp;_{caminoAccommodationTypeMsg v}|] messages router)) accommodationTypeEnumeration
    let stopServiceOptions = map (\v -> (v, [ihamlet|^{caminoServiceIcon v}&nbsp;_{caminoServiceMsg v}|] messages router)) serviceEnumeration
    let routeServiceOptions = map (\v -> (v, [ihamlet|^{caminoServiceIcon v}&nbsp;_{caminoServiceMsg v}|] messages router)) townServiceEnumeration
    (tlRes, tlView) <- mreq transportLinksField (fieldSettingsName "stopTransportLinks") (stopTransportLinks <$> prefStop <$> prefs)
    (loRes, loView) <- mreq (penanceMapField True True locationOptions) (fieldSettingsLabelName MsgLocationPreferencesLabel "stopLocation") (stopLocation <$> prefStop <$> prefs)
    (acRes, acView) <- mreq (penanceMapField True True accommodationOptions) (fieldSettingsLabelName MsgAccommodationPreferencesLabel "stopAccommodation") (stopAccommodation <$> prefStop <$> prefs)
    (ssRes, ssView) <- mreq (penanceMapField False False stopServiceOptions) (fieldSettingsLabelName MsgStopServicePreferencesLabel "stopServices") (stopServices <$> prefStop <$> prefs)
    (rsRes, rsView) <- mreq (penanceMapField False False routeServiceOptions) (fieldSettingsLabelName MsgDayServicePreferencesLabel "stopRouteServices") (stopRouteServices <$> prefStop <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resStopTransportLinks = tlRes,
      viewStopTransportLinks = tlView,
      resStopLocation = loRes,
      viewStopLocation = loView,
      resStopAccommodation= acRes,
      viewStopAccommodation = acView,
      resStopServices = ssRes,
      viewStopServices = ssView,
      resStopRouteServices = rsRes,
      viewStopRouteServices = rsView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewStopTransportLinks fields
        <div .row .mb-3>
          <div .col>
            ^{fvInput view}
      $with view <- viewStopLocation fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            $maybe tt <- fvTooltip view
              <div .form-text>
                ^{tt}
            ^{fvInput view}
      $with view <- viewStopAccommodation fields
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
      $with view <- viewStopRouteServices fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
     ^{hiddenPreferences ["StopTransportLinks", "StopLocation", "StopAccommodation", "StopServices", "StopRouteServices"] fields}
    |]
    return (res, widget)


-- | Form to allow stock-up day service preferences to be chosen
chooseStockServicesForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseStockServicesForm help prefs extra = do
    master <- getYesod
    locales <- getLocales
    mRender <- getMessageRender
    let config = caminoAppConfig master
    let router = renderCaminoRoute config locales
    let messages = renderCaminoMsg config locales
    let transportLinksField =  extendedCheckboxField (toHtml . mRender) MsgTransportLinks (Just MsgTransportLinksText)
    let locationOptions = map (\v -> (v, [ihamlet|<span .location-type-sample>^{caminoLocationTypeIcon v}</span>&nbsp;_{caminoLocationTypeLabel v}|] messages router)) locationStopTypeEnumeration
    let accommodationOptions = map (\v -> (v, [ihamlet|^{caminoAccommodationTypeIcon v}&nbsp;_{caminoAccommodationTypeMsg v}|] messages router)) accommodationTypeEnumeration
    let stopServiceOptions = map (\v -> (v, [ihamlet|^{caminoServiceIcon v}&nbsp;_{caminoServiceMsg v}|] messages router)) serviceEnumeration
    let routeServiceOptions = map (\v -> (v, [ihamlet|^{caminoServiceIcon v}&nbsp;_{caminoServiceMsg v}|] messages router)) townServiceEnumeration
    (tlRes, tlView) <- mreq transportLinksField (fieldSettingsName "stockTransportLinks") (stopTransportLinks <$> prefStockStop <$> prefs)
    (loRes, loView) <- mreq (penanceMapField True True locationOptions) (fieldSettingsLabelName MsgLocationPreferencesLabel "stockLocation") (stopLocation <$> prefStockStop <$> prefs)
    (acRes, acView) <- mreq (penanceMapField True True accommodationOptions) (fieldSettingsLabelName MsgAccommodationPreferencesLabel "stockAccommodation") (stopAccommodation <$> prefStockStop <$> prefs)
    (ssRes, ssView) <- mreq (penanceMapField False False stopServiceOptions) (fieldSettingsLabelName MsgStopServicePreferencesLabel "stockServices") (stopServices <$> prefStockStop <$> prefs)
    (rsRes, rsView) <- mreq (penanceMapField False False routeServiceOptions) (fieldSettingsLabelName MsgDayServicePreferencesLabel "stockRouteServices") (stopRouteServices <$> prefStockStop <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resStockTransportLinks = tlRes,
      viewStockTransportLinks = tlView,
      resStockLocation = loRes,
      viewStockLocation = loView,
      resStockAccommodation= acRes,
      viewStockAccommodation = acView,
      resStockServices = ssRes,
      viewStockServices = ssView,
      resStockRouteServices = rsRes,
      viewStockRouteServices = rsView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewStockTransportLinks fields
        <div .row .mb-3>
          <div .col>
            ^{fvInput view}
      $with view <- viewStockLocation fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            $maybe tt <- fvTooltip view
              <div .form-text>
                ^{tt}
            ^{fvInput view}
      $with view <- viewStockAccommodation fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      $with view <- viewStockServices fields
         <div .row .mb-3>
           <div .col>
             <label for="#{fvId view}">
               ^{fvLabel view} ^{help}
             ^{fvInput view}
      $with view <- viewStockRouteServices fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
     ^{hiddenPreferences ["StockTransportLinks", "StockLocation", "StockAccommodation", "StockServices", "StockRouteServices"] fields}
    |]
    return (res, widget)

-- | Form to allow rest day service preferences to be chosen
chooseRestServicesForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseRestServicesForm help prefs extra = do
    master <- getYesod
    locales <- getLocales
    mRender <- getMessageRender
    let config = caminoAppConfig master
    let router = renderCaminoRoute config locales
    let messages = renderCaminoMsg config locales
    let transportLinksField =  extendedCheckboxField (toHtml . mRender) MsgTransportLinks (Just MsgTransportLinksText)
    let locationOptions = map (\v -> (v, [ihamlet|<span .location-type-sample>^{caminoLocationTypeIcon v}</span>&nbsp;_{caminoLocationTypeLabel v}|] messages router)) locationStopTypeEnumeration
    let accommodationOptions = map (\v -> (v, [ihamlet|^{caminoAccommodationTypeIcon v}&nbsp;_{caminoAccommodationTypeMsg v}|] messages router)) accommodationTypeEnumeration
    let stopServiceOptions = map (\v -> (v, [ihamlet|^{caminoServiceIcon v}&nbsp;_{caminoServiceMsg v}|] messages router)) serviceEnumeration
    let routeServiceOptions = map (\v -> (v, [ihamlet|^{caminoServiceIcon v}&nbsp;_{caminoServiceMsg v}|] messages router)) townServiceEnumeration
    (tlRes, tlView) <- mreq transportLinksField (fieldSettingsName "stockTransportLinks") (stopTransportLinks <$> prefRestStop <$> prefs)
    (loRes, loView) <- mreq (penanceMapField True True locationOptions) (fieldSettingsLabelName MsgLocationPreferencesLabel "stockLocation") (stopLocation <$> prefRestStop <$> prefs)
    (acRes, acView) <- mreq (penanceMapField True True accommodationOptions) (fieldSettingsLabelName MsgAccommodationPreferencesLabel "stockAccommodation") (stopAccommodation <$> prefRestStop <$> prefs)
    (ssRes, ssView) <- mreq (penanceMapField False False stopServiceOptions) (fieldSettingsLabelName MsgStopServicePreferencesLabel "stockServices") (stopServices <$> prefRestStop <$> prefs)
    (rsRes, rsView) <- mreq (penanceMapField False False routeServiceOptions) (fieldSettingsLabelName MsgDayServicePreferencesLabel "stockRouteServices") (stopRouteServices <$> prefRestStop <$> prefs)
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resRestTransportLinks = tlRes,
      viewRestTransportLinks = tlView,
      resRestLocation = loRes,
      viewRestLocation = loView,
      resRestAccommodation= acRes,
      viewRestAccommodation = acView,
      resRestServices = ssRes,
      viewRestServices = ssView,
      resRestRouteServices = rsRes,
      viewRestRouteServices = rsView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewRestTransportLinks fields
        <div .row .mb-3>
          <div .col>
            ^{fvInput view}
      $with view <- viewRestLocation fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            $maybe tt <- fvTooltip view
              <div .form-text>
                ^{tt}
            ^{fvInput view}
      $with view <- viewRestAccommodation fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      $with view <- viewRestServices fields
         <div .row .mb-3>
           <div .col>
             <label for="#{fvId view}">
               ^{fvLabel view} ^{help}
             ^{fvInput view}
      $with view <- viewRestRouteServices fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
     ^{hiddenPreferences ["RestTransportLinks", "RestLocation", "RestAccommodation", "RestServices", "RestRouteServices"] fields}
    |]
    return (res, widget)

-- | Form to allow the camino to be chosen
chooseCaminoForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseCaminoForm help prefs extra = do
    master <- getYesod
    locales <- getLocales
    let config = caminoAppConfig master
    let router = renderCaminoRoute config locales
    let messages = renderCaminoMsg config locales
    let localised c = localiseText locales c
    let description d = Just $ (descriptionBlock False True d) messages router
    let  caminoField = extendedRadioFieldList id (map (\c -> (caminoId c, toHtml $ localised $ caminoName c, c, description $ caminoDescription c)) (caminoAppCaminos master))
    (caRes, caView) <- mreq caminoField (fieldSettingsLabelName MsgSelectCamino "camino") (prefCamino <$> prefs)
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
      ^{hiddenPreferences ["Camino"] fields}
    |]
    return (res, widget)


-- | Form to allow the routes to be chosen
chooseRoutesForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
chooseRoutesForm help prefs extra = do
    master <- getYesod
    locales <- getLocales
    let config = caminoAppConfig master
    let router = renderCaminoRoute config locales
    let messages = renderCaminoMsg config locales
    let localised r = localiseText locales r
    let description d = Just $ (descriptionBlock False True d) messages router
    let camino = prefCamino <$> prefs
    let routes = maybe (Prelude.concat (map caminoRoutes (caminoAppCaminos master))) caminoRoutes camino
    let requirementClauses = maybe [] (\c -> Prelude.concat $ map createRequiresClauses (caminoRouteLogic c)) camino
    let allowedClauses = maybe [] (\c -> Prelude.concat $ map createAllowsClauses (caminoRouteLogic c)) camino
    let prohibitedClauses = maybe [] (\c -> Prelude.concat $ map createProhibitsClauses (caminoRouteLogic c)) camino
    let routeOptions = map (\r -> (routeID r, toHtml $ localised $ routeName r, r, description $ routeDescription r, maybe False (\c -> r == caminoDefaultRoute c) camino)) routes
    (roRes, roView) <- mreq (implyingCheckListField id routeOptions requirementClauses allowedClauses prohibitedClauses) (fieldSettingsLabelName MsgRoutePreferencesLabel "routes") (prefRoutes <$> prefs)
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
      ^{hiddenPreferences ["Routes"] fields}
    |]
    return (res, widget)

makeOptions :: (Ord a) => (CaminoAppMessage -> Text) -> (a -> Text) -> (a -> Text) -> [a] -> [a] -> [(Text, [(Text, Text, a)])]
makeOptions render keyer labeler recommended options = let
    rset = S.fromList recommended
    other = filter (\v -> not $ S.member v rset) options
    mkOptions opts = map (\l -> (keyer l, labeler l, l)) opts
  in
    (render MsgSuggestedLabel, mkOptions recommended) : (map (\(m, ls) -> (m, mkOptions ls)) (Data.Util.partition (categorise . labeler) other))

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
    let cprefs = CaminoPreferences <$> camino <*> start' <*> finish' <*> routes <*> pure S.empty <*> pure S.empty <*> pure S.empty <*> pure S.empty <*> pure startDate'
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
    (stRes, stView) <- mreq (extendedSelectionField startOptions) (fieldSettingsLabelName MsgStartLocationLabel "start") start'
    (fiRes, fiView) <- mreq (extendedSelectionField finishOptions) (fieldSettingsLabelName MsgFinishLocationLabel "finish") finish'
    (sdRes, sdView) <- mreq (dateField) (fieldSettingsLabelName MsgStartDateLabel "startDate") startDate'
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resStart = stRes,
      viewStart = stView,
      resFinish = fiRes,
      viewFinish = fiView,
      resStartDate = sdRes,
      viewStartDate = sdView
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
      $with view <- viewStartDate fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{hiddenPreferences ["Start", "Finish", "StartDate"] fields}
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
    let allowedRests = permittedRestPoints <$> prefs <*> possibleStops
    let sortKey l = canonicalise $ localised l
    let stops = sortOn sortKey $ maybe allStops S.toList allowedStops
    let restpoints = sortOn sortKey $ maybe allStops S.toList allowedRests
    let recommended = maybe S.empty recommendedStops cprefs'
    let (suggested, other) = Data.List.partition (\l -> S.member l recommended) stops
    let recommendedRests = maybe S.empty recommendedRestPoints cprefs'
    let (suggestedRests, otherRests) = Data.List.partition (\l -> S.member l recommendedRests) restpoints
    let mkOptions locs = map (\l -> (locationID l, localised l, l)) locs
    let stopOptions = (render MsgSuggestedLabel, mkOptions suggested) : (map (\(m, ls) -> (m, mkOptions ls)) (Data.Util.partition (categorise .localised) other))
    let exclOptions = (render MsgSuggestedLabel, []) : (map (\(m, ls) -> (m, mkOptions ls)) (Data.Util.partition (categorise . localised) stops))
    let restOptions = (render MsgSuggestedLabel, mkOptions suggestedRests) : (map (\(m, ls) -> (m, mkOptions ls)) (Data.Util.partition (categorise .localised) otherRests))
    let chosenStops = S.intersection <$> allowedStops <*> (prefStops <$> prefs)
    let chosenExcluded = S.intersection <$> allowedStops <*> (prefExcluded <$> prefs)
    let chosenRestPoints = S.intersection <$> allowedRests <*> (prefRestPoints <$> prefs)
    (stRes, stView) <- mreq (clickSelectionField stopOptions) (fieldSettingsLabelName MsgStopsLabel "stops") chosenStops
    (exRes, exView) <- mreq (clickSelectionField exclOptions) (fieldSettingsLabelName MsgExcludedLabel "excluded") chosenExcluded
    (rspRes, rspView) <- mreq (clickSelectionField restOptions) (fieldSettingsLabelName MsgPreferredRestPointsLabel "restPoints") chosenRestPoints
    df <- defaultPreferenceFields master prefs
    let fields = df {
        resStops = stRes
      , viewStops = stView
      , resExcluded = exRes
      , viewExcluded = exView
      , resRestPoints = rspRes
      , viewRestPoints = rspView
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
      $with view <- viewRestPoints fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{hiddenPreferences ["Stops", "Excluded", "RestPoints"] fields}
    |]
    return (res, widget)

poiLabel :: [Locale] -> Maybe Location -> PointOfInterest -> Text
poiLabel locales location poi = poil
  <> (case locl of
    Nothing -> ""
    Just ll -> if ll == poil then "" else ", " <> ll
    )
  <> (case poiTime poi of
    Nothing -> ""
    Just time -> ", " <> sformat (fixed 1) time <> "hrs"
    )
  where
    poil = localiseText locales $ poiName poi
    locl = localiseText locales <$> locationName <$> location

-- | Form to allow Poi preferences to be chosen
choosePoiForm :: Widget -> Maybe PreferenceData -> Html -> MForm Handler (FormResult PreferenceData, Widget)
choosePoiForm help prefs extra = do
    master <- getYesod
    render <- getMessageRender
    locales <- getLocales
    let tprefs = travelPreferencesFrom <$> prefs
    let localised p = localiseText locales $ poiName p
    let camino = prefCamino <$> prefs
    let routes = prefRoutes <$> prefs
    let start' = prefStart <$> prefs
    let finish' = prefFinish <$> prefs
    let cprefs = withRoutes <$> (defaultCaminoPreferences <$> camino) <*> routes
    let cprefs' = withStartFinish <$> cprefs <*> start' <*> finish'
    let caminos = maybe (caminoAppCaminos master) singleton camino
    let allPois = S.unions (map (S.fromList . (map fst) . M.elems . caminoPois) caminos)
    let possiblePois = S.filter (\p -> isJust $ poiTime p) $ maybe allPois reachablePois cprefs'
    let sortKey p = canonicalise $ localised p
    let pois = sortOn sortKey $ S.toList possiblePois
    let recommended = maybe S.empty id $ recommendedPois <$> tprefs <*> cprefs'
    let (suggested, other) = Data.List.partition (\p -> S.member p recommended) pois
    let mkOptions ps = map (\p -> (poiID p, poiLabel locales (findLocationByPoi camino p) p, p)) ps
    let poiOptions = (render MsgSuggestedLabel, mkOptions suggested) : (map (\(m, ls) -> (m, mkOptions ls)) (Data.Util.partition (categorise .localised) other))
    let chosenPois = S.intersection possiblePois <$> (prefPois <$> prefs)
    (poRes, poView) <- mreq (clickSelectionField poiOptions) (fieldSettingsLabelName MsgPoisLabel "pois") chosenPois
    df <- defaultPreferenceFields master prefs
    let fields = df {
      resPois = poRes,
      viewPois = poView
    }
    let res = makePreferenceData master fields
    let widget = [whamlet|
      #{extra}
      $with view <- viewPois fields
        <div .row .mb-3>
          <div .col>
            <label for="#{fvId view}">
              ^{fvLabel view} ^{help}
            ^{fvInput view}
      ^{hiddenPreferences ["Pois"] fields}
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
     ^{hiddenPreferences [] fields}
    |]
    return (res, widget)
