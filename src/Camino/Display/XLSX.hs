{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}
{-|
Module      : XLSX
Description : Produce an excel spreadsheet showing the stages of the camino route.
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

A camino consists of a graph of legs that can be assembled in various ways.
The legs run between two locations, each with possible accommodation and service options.

Generally, it is expected that these models will be read from JSON files.

Note that XML-Hamlet remvoes path interpolation, so routes are not a thing here.
-}

module Camino.Display.XLSX (
  createCaminoXlsx
) where

import Camino.Camino
import Camino.Config
import Camino.Planner
import Camino.Preferences
import Camino.Display.Css
import Camino.Display.Html
import Camino.Display.I18n
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (filterM)
import Control.Monad.Reader
import Control.Monad.State (evalState)
import Data.Colour
import Data.Colour.Names
import Data.Description
import Data.Event
import Data.Event.Date
import Data.Foldable (foldlM, foldrM, toList)
import qualified Data.List as L
import Data.Localised
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Region
import qualified Data.Set as S
import Data.Text (Text, pack)
import qualified Data.Time.Calendar as C
import Data.Time.Format.ISO8601
import Data.Util
import Data.Xlsx
import Data.Description (Description(..))

-- What most of the sheets work in
type Cslab = Slab (Cell CaminoMsg)

titleFont :: Font
titleFont = baseFont
  & fontBold ?~ True
  & fontSize ?~ 18

-- Table headers
headingStyle :: Style
headingStyle = def
  & styleFont ?~ (baseFont & fontBold ?~ True)
  & styleAlignment ?~ (def & alignmentWrapText ?~ True)

head1Style :: Style
head1Style = def
  & styleFont ?~ (baseFont & fontBold ?~ True & fontSize ?~ 16)
  & styleWidth ?~ WidthExpandMax 20

head2Style :: Style
head2Style = def
  & styleFont ?~ (baseFont & fontBold ?~ True & fontSize ?~ 14)
  & styleWidth ?~ WidthExpandMax 20

head3Style :: Style
head3Style = def
  & styleFont ?~ (baseFont & fontBold ?~ True & fontSize ?~ 12)
  & styleWidth ?~ WidthExpandMax 20

head4Style :: Style
head4Style = def
  & styleFont ?~ (baseFont & fontBold ?~ True & fontItalic ?~ True)

minMaxStyle :: Style
minMaxStyle = def
  & styleFont ?~ (baseFont & fontColor ?~ (def & colorARGB ?~ toExcelColour warningRed))

targetStyle :: Style
targetStyle = def
  & styleFont ?~ (baseFont & fontColor ?~ (def & colorARGB ?~ toExcelColour successGreen))

rejectStyle :: Style
rejectStyle = def
  & styleFont ?~ (baseFont & fontColor ?~ (def & colorARGB ?~ toExcelColour warningRed))
  & styleAlignment ?~ (def & alignmentHorizontal ?~ CellHorizontalAlignmentCenter)

dateStyle :: Style
dateStyle = def
  & styleNumberFormat ?~ StdNumberFormat NfDMmmYy

longDateStyle :: Style
longDateStyle = def
  & styleNumberFormat ?~ UserNumberFormat "ddd dd-mmm-yy"
  & styleWidth ?~ WidthFixed 16

pilgrimageColour :: Color
pilgrimageColour = (def & colorARGB ?~ toExcelColour (blend 0.5 black caminoYellow))

pilgrimageFontColour :: Color
pilgrimageFontColour = (def & colorARGB ?~ toExcelColour white)

pilgrimageStyle :: Style
pilgrimageStyle = def
  & styleFont ?~ (baseFont & fontBold ?~ True & fontSize ?~ 14 & fontColor ?~ pilgrimageFontColour)
  & styleFill ?~ (def & fillPattern ?~ (def & fillPatternFgColor ?~ pilgrimageColour & fillPatternType ?~ PatternTypeSolid))

stageColour :: Color
stageColour = (def & colorARGB ?~ toExcelColour caminoYellow)

stageStyle :: Style
stageStyle = head3Style & styleFill ?~ (def & fillPattern ?~ (def & fillPatternFgColor ?~ stageColour & fillPatternType ?~ PatternTypeSolid))

dayColour :: Color
dayColour = (def & colorARGB ?~ toExcelColour (blend 0.5 white caminoYellow))

dayStyle :: Style
dayStyle = head4Style & styleFill ?~ (def & fillPattern ?~ (def & fillPatternFgColor ?~ dayColour & fillPatternType ?~ PatternTypeSolid))

servicesStyle :: Style
servicesStyle = def & styleWidth ?~ WidthExpandMax 25 & styleAlignment ?~ (def & alignmentWrapText ?~ True)

-- | Add a preference range to the preferences sheet
addPreferenceRange :: (a -> Double) -> CaminoMsg -> PreferenceRange a -> Cslab
addPreferenceRange convert label range =
  rowSlab [
      def & cellStyle ?~ head3Style & cellText ?~ label
    , def & cellStyle ?~ minMaxStyle & cellValue .~  (CellDouble <$> convert <$> rangeMinimum range)
    , def & cellValue ?~  (CellDouble $ convert $ rangeLower range)
    , def & cellStyle ?~ targetStyle & cellValue ?~  (CellDouble $ convert $ rangeTarget range)
    , def & cellValue ?~  (CellDouble $ convert $ rangeUpper range)
    , def & cellStyle ?~ minMaxStyle & cellValue .~  (CellDouble <$> convert <$> rangeMaximum range)
  ]

head2Label :: CaminoMsg -> Cell CaminoMsg
head2Label msg = def & cellStyle ?~ head2Style & cellText ?~ msg

head3Label :: CaminoMsg -> Cell CaminoMsg
head3Label msg = def & cellStyle ?~ head3Style & cellText ?~ msg

headingLabel :: CaminoMsg -> Cell CaminoMsg
headingLabel msg = def & cellStyle ?~ headingStyle & cellText ?~ msg

emptyCell :: Cell CaminoMsg
emptyCell = def

excelStartDate :: C.Day
excelStartDate = C.fromGregorian 1900 C.January 1

-- Convert haskell modified julian date to excel date
-- Haskell dates start on 0 = 1858-11-17, excel dates start on 1 = 1900-01-01, a difference of 15019 days
-- However, excel incorrectly classifies 1900 as a leap year, so it gets corrected further
dateCell :: C.Day -> Cell CaminoMsg
dateCell day = def & cellStyle ?~ dateStyle & cellValue ?~ CellDouble (realToFrac $ C.diffDays day excelStartDate + 2)

maybeDateCell :: Maybe C.Day -> Cell CaminoMsg
maybeDateCell Nothing = emptyCell
maybeDateCell (Just day) = dateCell day

longDateCell :: C.Day -> Cell CaminoMsg
longDateCell day = def & cellStyle ?~ longDateStyle & cellValue ?~ CellDouble (realToFrac $ C.diffDays day excelStartDate + 2)

maybeLongDateCell :: Maybe C.Day -> Cell CaminoMsg
maybeLongDateCell Nothing = emptyCell
maybeLongDateCell (Just day) = longDateCell day

booleanStyle :: Style
booleanStyle = def
  & styleAlignment ?~ (def & alignmentHorizontal ?~ CellHorizontalAlignmentCenter)
  & styleNumberFormat ?~ UserNumberFormat "\"\x2713\";;\"\x2717\""

-- Convert true into true and false into an empty cell
booleanCell :: Bool -> Cell CaminoMsg
booleanCell False = emptyCell
booleanCell True = def & cellValue ?~ CellDouble 1.0 & cellStyle ?~ booleanStyle

maybeBooleanCell :: Maybe Bool -> Cell CaminoMsg
maybeBooleanCell Nothing = emptyCell
maybeBooleanCell (Just bool) = booleanCell bool

-- Convert zero into nothing and another value into a double cell
doubleCell :: (Real a) => NumberFormat -> a -> Cell CaminoMsg
doubleCell f v = case (realToFrac v) of
  0.0 -> emptyCell
  d -> def & cellStyle ?~ (def & styleNumberFormat ?~ f) & cellValue ?~ CellDouble d

maybeDoubleCell :: (Real a) => NumberFormat -> Maybe a -> Cell CaminoMsg
maybeDoubleCell _f Nothing = emptyCell
maybeDoubleCell f (Just v) = doubleCell f v

integerFormat :: NumberFormat
integerFormat = StdNumberFormat NfZero

integerStyle :: Style
integerStyle = def & styleNumberFormat ?~ integerFormat

integerCell :: Int -> Cell CaminoMsg
integerCell = doubleCell integerFormat

distanceFormat :: NumberFormat
distanceFormat = UserNumberFormat "0.0"

distanceStyle :: Style
distanceStyle = def & styleNumberFormat ?~ distanceFormat

-- A double formatted as a distance
distanceCell :: (Real a) => a -> Cell CaminoMsg
distanceCell = doubleCell distanceFormat

maybeDistanceCell :: (Real a) => Maybe a -> Cell CaminoMsg
maybeDistanceCell = maybeDoubleCell distanceFormat

timeFormat :: NumberFormat
timeFormat = UserNumberFormat "0.0"

timeStyle :: Style
timeStyle = def & styleNumberFormat ?~ timeFormat

-- A double formatted as a time
timeCell :: (Real a) => a -> Cell CaminoMsg
timeCell = doubleCell timeFormat

maybeTimeCell :: (Real a) => Maybe a -> Cell CaminoMsg
maybeTimeCell = maybeDoubleCell timeFormat

heightFormat :: NumberFormat
heightFormat = UserNumberFormat "0"

heightStyle :: Style
heightStyle = def & styleNumberFormat ?~ heightFormat

-- A double formatted as a height
heightCell :: (Real a) => a -> Cell CaminoMsg
heightCell = doubleCell heightFormat

maybeHeightCell :: (Real a) => Maybe a -> Cell CaminoMsg
maybeHeightCell = maybeDoubleCell heightFormat

latLngFormat :: NumberFormat
latLngFormat = UserNumberFormat "0.00000"

-- Show a penance value as a double if possible or a symbol if not
penanceCell :: Bool -> Maybe Penance -> Cell CaminoMsg
penanceCell _show Nothing = emptyCell
penanceCell False (Just (Penance 0.0)) = emptyCell
penanceCell _show (Just (Penance p)) = def & cellStyle ?~ (def & styleNumberFormat ?~ distanceFormat) & cellValue ?~ CellDouble (realToFrac p)
penanceCell _show (Just Reject) = def & cellStyle ?~ rejectStyle & cellValue ?~ CellText rejectSymbol

localisedStyle :: Style
localisedStyle = def
  & styleWidth ?~ WidthExpandMax 25

-- Show a localised piece of text
localisedCell :: Localised TaggedText -> Cell CaminoMsg
localisedCell txt = def & cellText ?~ Txt txt & cellStyle ?~ localisedStyle

maybeLocalisedCell :: Maybe (Localised TaggedText) -> Cell CaminoMsg
maybeLocalisedCell Nothing = emptyCell
maybeLocalisedCell (Just txt) = localisedCell txt

-- Create a list of services
servicesCell :: S.Set Service -> Cell CaminoMsg
servicesCell services
  | S.null services = emptyCell
  | otherwise = def & cellText ?~ ListMsg (map caminoServiceMsg (toList services)) & cellStyle ?~ servicesStyle

-- Create a list of accommodation types
accommodationCell :: S.Set AccommodationType -> Cell CaminoMsg
accommodationCell accommodation
  | S.null accommodation = emptyCell
  | otherwise = def & cellText ?~ ListMsg (map caminoAccommodationTypeMsg (toList accommodation)) & cellStyle ?~ servicesStyle

-- Create a list of points of interest
poisCell :: [PointOfInterest] -> Cell CaminoMsg
poisCell pois
  | null pois = emptyCell
  | otherwise = def & cellText ?~ ListMsg (map (Txt . poiName) pois) & cellStyle ?~ servicesStyle

-- | Create a joined-up description suitable for placing in a cell
descriptionMsgs :: Maybe Description -> [CaminoMsg]
descriptionMsgs Nothing = []
descriptionMsgs (Just d) = let
    desc = maybe [] (\t -> [Txt t]) $ (descSummary d <|> descText d)
    notes = map (Txt . noteText) $ descNotes d
  in
    desc ++ notes

addStopList :: (Ord a) => CaminoMsg -> Bool -> (StopPreferences -> M.Map a Penance) -> (a -> CaminoMsg) -> [a] -> TravelPreferences -> Cslab
addStopList title showAll sub label list tprefs = let
    labels = rowSlab [headingLabel title]
    stop = foldl (\ss -> \s -> ss >>! rowSlab [
        def & cellText ?~ label s
      , penanceCell showAll (M.lookup s $ sub $ preferenceStop tprefs)
      , penanceCell showAll (M.lookup s $ sub $ preferenceStockStop tprefs)
      , penanceCell showAll (M.lookup s $ sub $ preferenceRestStop tprefs)
      ]) SEmpty list
  in
    labels >>! stop

addStopPreferences :: TravelPreferences -> Cslab
addStopPreferences tprefs = let
    header = rowSlab [
        head3Label StopPreferencesLabel
      , headingLabel StopLabel
      , headingLabel StockpointLabel
      , headingLabel RestpointLabel
      ]
    transportLinks = rowSlab [
        headingLabel TransportLinksLabel
      , booleanCell (stopTransportLinks $ preferenceStop tprefs)
      , booleanCell (stopTransportLinks $ preferenceStockStop tprefs)
      , booleanCell (stopTransportLinks $ preferenceRestStop tprefs)
      ]
    locations = addStopList LocationPreferencesLabel True stopLocation caminoLocationTypeLabel locationStopTypeEnumeration tprefs
    accommodation = addStopList AccommodationPreferencesLabel True stopAccommodation caminoAccommodationTypeMsg accommodationTypeEnumeration tprefs
    services = addStopList ServicesPreferencesLabel False stopServices caminoServiceMsg serviceEnumeration tprefs
    routeServices = addStopList RouteServicesPreferencesLabel False stopRouteServices caminoServiceMsg townServiceEnumeration tprefs
  in
    header >>! transportLinks >>! locations >>! accommodation >>! services >>! routeServices

addPoiCategories :: TravelPreferences -> Cslab
addPoiCategories tprefs = let
    categories = preferencePoiCategories tprefs
    cells = [head3Label PoisLabel]
      ++ map (\p -> headingLabel (caminoPoiCategoryLabel p)) poiCategoryEnumeration
      ++ [emptyCell & cellPos .~ (1, 0)]
      ++ map (\p -> booleanCell (S.member p categories)) poiCategoryEnumeration
  in
    rowSlab cells

-- | Add a table of travel preferences to the worksheet
addTravelPreferences :: TravelPreferences -> Cslab
addTravelPreferences tprefs = let
    pois = addPoiCategories tprefs
    basics = rowSlab [
        head3Label TravelLabel
      , def & cellText ?~ (caminoTravelMsg $ preferenceTravel tprefs)
      , head3Label FitnessLabel & cellPos .~ (1, 0)
      , def & cellText ?~ (caminoFitnessMsg $ preferenceFitness tprefs)
      , head3Label ComfortLabel & cellPos .~ (2, 0)
      , def & cellText ?~ (caminoComfortMsg $ preferenceComfort tprefs)
      ]
    distance = addPreferenceRange realToFrac DistancePreferencesLabel (preferenceDistance tprefs)
    time = addPreferenceRange realToFrac TimePreferencesLabel (preferenceTime tprefs)
    rest = addPreferenceRange fromIntegral RestPreferencesLabel (preferenceRest tprefs)
    stops = addStopPreferences tprefs
  in
    basics >>! distance >>! time >>! rest >>! pois >>! stops

addNameList :: (Foldable f) => CaminoMsg -> (a -> Localised TaggedText) -> f a -> Cslab
addNameList title name list =
  rowSlab $ (head3Label title):(map (\v -> localisedCell (name v)) (toList list))

addCaminoPreferences :: CaminoPreferences -> Cslab
addCaminoPreferences cprefs = let
    camino = rowSlab [
        head3Label CaminoLabel
      , localisedCell (caminoName $ preferenceCamino cprefs)
      ]
    routes = addNameList RouteLabel routeName (preferenceRoutes cprefs)
    tripStart = rowSlab [
        head3Label TripStartLabel
      , localisedCell (locationName $ preferenceStart cprefs)
      ]
    tripFinish = rowSlab [
        head3Label TripFinishLabel
      , localisedCell (locationName $ preferenceFinish cprefs)
      ]
    tripStartDate = rowSlab [
        head3Label StartDateLabel
      , maybeDateCell (preferenceStartDate cprefs)
      ]
    requires = addNameList RequiredStopsLabel locationName (preferenceStops cprefs)
    excludes = addNameList ExcludedStopsLabel locationName (preferenceExcluded cprefs)
    pois = addNameList PoisLabel poiName (preferencePois cprefs)
  in
    camino >>! routes >>! tripStart >>! tripFinish >>! tripStartDate >>! requires >>! excludes >>! pois

createPreferenceSheet :: TravelPreferences -> CaminoPreferences -> CellIDStream (Worksheet CaminoMsg)
createPreferenceSheet tprefs cprefs = let
    heading = rowSlab [head2Label PreferencesLabel]
    travels = addTravelPreferences tprefs
    caminos = addCaminoPreferences cprefs
  in do
    return $ Worksheet PreferencesLabel (heading >>! (travels >>- caminos))

headers :: (a -> CaminoMsg) -> CaminoMsg -> [a] -> Cslab
headers labeler title list = let
    category = rowSlab [head3Label title]
    values = rowSlab $ map (\v -> headingLabel (labeler v)) list
  in
    category >>! values

createLocationSlab :: S.Set Location -> S.Set Location -> S.Set Location -> S.Set Location -> Location -> Cslab
createLocationSlab rests stocks stops waypoints location = let
    isRest = S.member location rests
    isStockpoint = not isRest && S.member location stocks
    isStop = not isRest && not isStockpoint && S.member location stops
    isWaypoint = not isStop && not isStockpoint && not isRest && S.member location waypoints
    locs = rowSlab  [
        localisedCell (locationName location)
      , def & cellText ?~ (caminoLocationTypeLabel $ locationType location)
      , maybeLocalisedCell (regionName <$> locationRegion location)
      , booleanCell isWaypoint
      , booleanCell isStop
      , booleanCell isStockpoint
      , booleanCell isRest
      ]
    accom = locationAccommodationTypes location
    accomodation = rowSlab $ map (\s -> booleanCell (S.member s accom)) accommodationTypeEnumeration
    servs = locationServices location
    services = rowSlab $ map (\s -> booleanCell (S.member s servs)) townServiceEnumeration
    poiTypes = locationPoiTypes location
    pois = rowSlab $ map (\s -> booleanCell (S.member s poiTypes)) locationTypeEnumeration
    additional = rowSlab [
        booleanCell (not $ null $ locationEvents location)
      , booleanCell (locationCamping location)
      , booleanCell (locationAlwaysOpen location)
      , maybeDoubleCell latLngFormat (latitude <$> locationPosition location)
      , maybeDoubleCell latLngFormat (longitude <$> locationPosition location)
      , def & cellValue ?~ CellText (locationID location)
      ]
  in
    locs >>- accomodation >>- services >>- pois >>- additional

locationHeaders :: Cslab
locationHeaders = rowSlab [
    head3Label LocationLabel
  , headingLabel LocationLabel & cellPos .~ (1, 0)
  , headingLabel RegionLabel
  , headingLabel WaypointLabel
  , headingLabel StopLabel
  , headingLabel StockpointLabel
  , headingLabel RestpointLabel
  ]

locationAdditionalHeaders :: Cslab
locationAdditionalHeaders = rowSlab
  [   head3Label LocationLabel
    , headingLabel EventsLabel & cellPos .~ (1, 0)
    , headingLabel CampingTitle
    , headingLabel AlwaysOpenLabel
    , headingLabel LatitudeLabel
    , headingLabel LongitudeLabel
    , headingLabel IdentifierLabel
  ]


createLocationsSheet :: TravelPreferences -> CaminoPreferences -> Solution -> CellIDStream (Worksheet CaminoMsg)
createLocationsSheet _tprefs cprefs solution = let
    camino = preferenceCamino cprefs
    locationOrder a b = compare (canonicalise $ locationNameLabel a) (canonicalise $ locationNameLabel b)
    locationsSorted = L.sortBy locationOrder (caminoLocationList camino)
    (_trip, _jerrors, _perrors, rests, stocks, stops, waypoints, _usedLegs) = solutionElements camino (Just solution)
    headings = (
      locationHeaders
      >>- headers caminoAccommodationTypeMsg AccommodationLabel accommodationTypeEnumeration
      >>- headers caminoServiceMsg ServicesLabel townServiceEnumeration
      >>- headers caminoLocationTypeLabel PoisLabel locationTypeEnumeration
      >>- locationAdditionalHeaders
      )
    locations = foldl (\s -> \l -> s >>! createLocationSlab rests stocks stops waypoints l) headings locationsSorted
  in do
    return $ Worksheet LocationsLabel locations

legDescriptionCell :: Leg -> Cell CaminoMsg
legDescriptionCell leg = let
    lt = if (legType leg /= Road) then [caminoLegTypeLabel (legType leg)] else []
    legDesc = descriptionMsgs (legDescription leg)
    locDesc = descriptionMsgs (locationDescription $ legTo leg)
    msgs = lt ++ legDesc ++ locDesc
  in
    if null msgs then emptyCell else def & cellText ?~ ListMsg msgs

createLegSlab :: Leg -> CellIDStream (Cslab, CellID, CellID, CellID)
createLegSlab leg = do
    disid <- nextCellID "leg distance"
    asid <- nextCellID "leg ascent"
    deid <- nextCellID "leg descent"
    return $ (rowSlab [
        emptyCell
      , emptyCell
      , def & cellText ?~ Txt (locationName $ legTo leg)
      , (distanceCell $ legDistance leg) & cellID ?~ disid
      , (maybeTimeCell $ legTime leg)
      , emptyCell
      , (penanceCell False $ legPenance leg)
      , emptyCell
      , (heightCell $ legAscent leg) & cellID ?~ asid
      , (heightCell $ legDescent leg) & cellID ?~ deid
      , emptyCell
      , emptyCell
      , emptyCell
      , servicesCell (locationServices $ legTo leg)
      , accommodationCell (locationAccommodationTypes $ legTo leg)
      , poisCell (locationPois $ legTo leg)
      , emptyCell
      , legDescriptionCell leg
      ], disid, asid, deid)

startDateFormula :: CellID -> Maybe CellID -> Int -> Formula
startDateFormula prevID mrestID add = let
    date = FRef prevID
    date' = maybe date (\rid -> FApply "+" [date, FRef rid]) mrestID
    date'' = if add == 0 then date' else FApply "+" [date', FInt add]
  in
    date''

createDaySlab :: S.Set Location -> CellID -> M.Map Region Int -> CellID -> Maybe CellID -> Int -> Day -> CellIDStream (Cslab, CellID, CellID)
createDaySlab  stocks htable rtable sid mrdid add day = do
  (legs, disids, asids, deids) <- foldrM (\l -> \(s, dsids', asids', deids') -> do
      (ls, disid, asid, deid) <- createLegSlab l
      return (ls >>! s, disid:dsids', asid:asids', deid:deids')
    ) (SEmpty, [], [], []) (path day)
  let metrics = score day
  let region = locationRegion $ finish day
  dayid <- nextCellID "day"
  rdid' <- nextCellID "rest"
  let
    final = SRow dayStyle $ rowSlab [
        def
          & cellID ?~ dayid
          & cellStyle ?~ longDateStyle
          & cellFormula ?~ startDateFormula sid mrdid add
      , emptyCell
      , localisedCell (locationName $ start day)
      , localisedCell (locationName $ finish day)
      , def
          & cellStyle ?~ (def & styleNumberFormat ?~ distanceFormat)
          & cellFormula ?~ FApply "SUM" [FRef $ CellRangeID (head disids) (last disids)]
      , maybeTimeCell (metricsTime metrics)
      , maybeDistanceCell (metricsPerceivedDistance metrics)
      , penanceCell True $ Just $ metricsPenance metrics
      , def
          & cellStyle ?~ (def & styleNumberFormat ?~ heightFormat)
          & cellFormula ?~ FApply "SUM" [FRef $ CellRangeID (head asids) (last asids)]
      , def
          & cellStyle ?~ (def & styleNumberFormat ?~ heightFormat)
          & cellFormula ?~ FApply "SUM" [FRef $ CellRangeID (head deids) (last deids)]
      , maybeTimeCell (metricsPoiTime metrics)
      , integerCell (metricsRestDays metrics) & cellID ?~ rdid'
      , booleanCell (S.member (finish day) stocks)
      , emptyCell
      , emptyCell
      , emptyCell
      , def & cellText .~ ((Txt . regionName) <$> (locationRegion $ finish day))
      , def & cellFormula .~
           -- There is an idiocy here where later versions of Excel insert a @ implicit join operator and then complains about it.
           -- It also gets uset over LET. This works but I feel dirty.
          ((\rc -> let
              match = FApply "MATCH" [
                  FRef dayid
                , FRef $ CellSubrangeID Nothing Nothing Nothing (Just 1) htable
                , FInt 0
                ]
            in
              FApply "IF" [
                FApply "IF" [
                      FApply "ISNA" [ match ]
                    , FBool False
                    , FApply "=" [
                          FApply "INDEX" [
                              FRef htable
                            , match
                            , FInt (M.findWithDefault 1 rc rtable)
                            ]
                        , FInt 1
                        ]
                    ]
                , FApply "INDEX" [
                      FRef htable
                    , match
                    , FInt 1
                    ]
                , FText ""
                ]
            ) <$> region)
      ]
  return (legs >>! final, dayid, rdid')

createStageSlab :: S.Set Location -> CellID -> M.Map Region Int -> CellID -> Int -> Journey -> CellIDStream (Cslab, CellID)
createStageSlab stocks htable rtable ssid nd stage = do
  fsid <- nextCellID "stage final"
  (ss, fdid', _, mrdid', dids) <- foldlM (\(s, did, add, mrdid, dids') -> \d -> do
       (ds, did', rdid') <- createDaySlab stocks htable rtable did mrdid add d
       return $ (s >>! ds, did', 1, Just rdid', dids' ++ [did'])
    ) (SEmpty, ssid, nd, Nothing, []) (path stage)
  let
    final = SRow stageStyle $ rowSlab [
        def
          & cellStyle ?~ longDateStyle
          & cellFormula ?~ FRef ssid
          & cellID ?~ fsid
      , def
          & cellStyle ?~ longDateStyle
          & cellFormula ?~ startDateFormula fdid' mrdid' 0
      , localisedCell (locationName $ start stage)
      , localisedCell (locationName $ finish stage)
      , def & cellStyle ?~ distanceStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 4 c) dids)
      , def & cellStyle ?~ timeStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 5 c) dids)
      , def & cellStyle ?~ distanceStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 6 c) dids)
      , def & cellStyle ?~ distanceStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 7 c) dids)
      , def & cellStyle ?~ heightStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 8 c) dids)
      , def & cellStyle ?~ heightStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 9 c) dids)
      , def & cellStyle ?~ timeStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 10 c) dids)
      , def & cellStyle ?~ integerStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 11 c) dids)
      ]
  return $ (ss >>! final, CellOffsetID 0 1 fsid)

createPilgrimageSlab :: S.Set Location -> CellID -> M.Map Region Int -> Maybe Pilgrimage -> CellIDStream Cslab
createPilgrimageSlab _ _ _ Nothing = return $ SEmpty
createPilgrimageSlab stocks htable rtable (Just pilgrimage) = do
  sdid <- nextCellID "stage"
  let
    header = SRow stageStyle $ rowSlab [
       maybeLongDateCell (fst <$> metricsDate (score pilgrimage)) & cellID ?~ sdid
     , emptyCell
     , emptyCell
     , localisedCell (locationName $ start pilgrimage)
     ]
  (ps, fsid, _, sids) <- foldlM (\(s, did, ndid, sids) -> \st -> do
      (ss, sid') <- createStageSlab stocks htable rtable did ndid st
      return $ (s >>! ss, sid', 1, sids ++ [sid'])
    ) (header, sdid, 0, []) (path pilgrimage)
  let
    footer = SRow pilgrimageStyle $ rowSlab [
        def & cellStyle ?~ longDateStyle & cellFormula ?~ FRef sdid
      , def & cellStyle ?~ longDateStyle & cellFormula ?~ FRef fsid
      , localisedCell (locationName $ start pilgrimage)
      , localisedCell (locationName $ finish pilgrimage)
      , def & cellStyle ?~ distanceStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 4 c) sids)
      , def & cellStyle ?~ timeStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 5 c) sids)
      , def & cellStyle ?~ distanceStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 6 c) sids)
      , def & cellStyle ?~ distanceStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 7 c) sids)
      , def & cellStyle ?~ heightStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 8 c) sids)
      , def & cellStyle ?~ heightStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 9 c) sids)
      , def & cellStyle ?~ distanceStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 10 c) sids)
      , def & cellStyle ?~ integerStyle & cellFormula ?~ FApply "SUM" (map (\c -> FRef $ CellOffsetID 0 11 c) sids)
      ]
  return (ps >>! footer)

createPlanSheet :: CellID -> M.Map Region Int -> TravelPreferences -> CaminoPreferences -> Solution -> CellIDStream (Worksheet CaminoMsg)
createPlanSheet htable rtable _tprefs cprefs solution = let
    camino = preferenceCamino cprefs
    (trip, _jerrors, _perrors, _rests, stocks, _stops, _waypoints, _usedLegs) = solutionElements camino (Just solution)
    headings = rowSlab [
        headingLabel StartDateLabel
      , headingLabel FinishDateLabel
      , headingLabel StartLocationLabel
      , headingLabel FinishLocationLabel
      , headingLabel DistanceLabel
      , headingLabel HoursTitle
      , headingLabel PerceivedDistanceLabel
      , headingLabel PenanceSummaryLabel
      , headingLabel AscentLabel
      , headingLabel DescentLabel
      , headingLabel PoisLabel
      , headingLabel RestDaysLabel
      , headingLabel StockpointLabel
      , headingLabel ServicesLabel
      , headingLabel AccommodationLabel
      , headingLabel PoisLabel
      , headingLabel RegionLabel
      , headingLabel HolidaysLabel
      , headingLabel NotesLabel
      , emptyCell & cellPos .~ (1, 0)
      , emptyCell
      , emptyCell
      , emptyCell
      , def & cellStyle ?~ headingStyle & cellValue ?~ CellText "km"
      , def & cellStyle ?~ headingStyle & cellValue ?~ CellText "hrs"
      , def & cellStyle ?~ headingStyle & cellValue ?~ CellText "km"
      , def & cellStyle ?~ headingStyle & cellValue ?~ CellText "km"
      , def & cellStyle ?~ headingStyle & cellValue ?~ CellText "m"
      , def & cellStyle ?~ headingStyle & cellValue ?~ CellText "m"
      , def & cellStyle ?~ headingStyle & cellValue ?~ CellText "hrs"
      , def & cellStyle ?~ headingStyle & cellValue ?~ CellText "days"
      ]
  in do
    pilgrimage <- createPilgrimageSlab stocks htable rtable trip
    return $ Worksheet PlanLabel (headings >>! pilgrimage)

createHolidayMatrixSlab :: Config -> CaminoPreferences -> CellIDStream (Cslab, CellID, M.Map Region Int)
createHolidayMatrixSlab config cprefs = do
    let camino = preferenceCamino cprefs
    let regions = S.filter isHolidayRegion $ regionClosure $ caminoRegions camino
    let regionList = S.toList regions
    let rtable = M.fromList (zip regionList [3..])
    let holidayRegions = S.filter (\r -> not $ null $ getRegionalHolidays r) regions
    let holidayKeys = S.fold S.union S.empty $ S.map (\r -> S.fromList $ catMaybes $ map calendarKey $ regionHolidays r) holidayRegions
    let startDate = preferenceStartDate cprefs
    let mapDate k = (k, (\d -> runReader (calendarDateOnOrAfter c d) config) <$> startDate) where c = NamedCalendar k
    let holidays = L.sortOn snd $ map mapDate $ S.toList holidayKeys
    let isHoliday k region = elem (NamedCalendar k) (getRegionalHolidays region)
    let
      header = rowSlab $
          [ headingLabel HolidaysLabel, emptyCell ] ++
          map (\r -> localisedCell (regionName r) & cellStyle ?~ headingStyle) regionList
    (matrix, hids) <- foldlM (\(s, hids') -> \(h, d) -> do
        hid <- nextCellID "holiday"
        let
          hline = rowSlab $ [
              def
                & cellStyle ?~ (headingStyle & styleWidth ?~ WidthExpand)
                & cellText ?~ NamedCalendarLabel h
                & cellID ?~ hid
            , maybeDateCell d
            ] ++ map (\r -> booleanCell (isHoliday h r)) regionList
        return $ (s >>! hline, hids' ++ [hid])
      ) (SEmpty, []) holidays
    let htable = CellRangeID (toAbsolute $ head hids) (toAbsolute $ CellOffsetID 0 (length regions + 1) (last hids))
    return $ (header >>! matrix, htable, rtable)

-- Not used
_dateList :: (MonadReader env m, HasCalendarConfig env, HasRegionConfig env) => [Region] -> C.Day -> m [(C.Day, Region, [Text])]
_dateList regions startDate =
  foldrM (\r -> \ds -> let
      holidays = catMaybes $ map calendarKey $ getRegionalHolidays r
    in
      foldrM (\d -> \ds' -> do
          hols <- filterM (\h -> inCalendar (NamedCalendar h) d) holidays
          return $ if null hols then ds' else (d, r, hols):ds'
        ) ds [startDate..(C.addGregorianYearsClip 1 startDate)]
    ) [] regions

_createHolidayListSlab :: Config -> CaminoPreferences -> C.Day -> CellIDStream Cslab
_createHolidayListSlab config cprefs startDate = do
    let camino = preferenceCamino cprefs
    let regions = S.filter isHolidayRegion $ regionClosure $ caminoRegions camino
    let holidayRegions = S.filter (\r -> not $ null $ getRegionalHolidays r) regions
    let holidayDates = runReader (_dateList (S.toList holidayRegions) startDate) config
    let header = rowSlab [ headingLabel DateLabel, headingLabel HolidaysLabel ]
    (dates, _tids) <- foldlM (\(s, rids) -> \(d, r, hs) -> do
        rid <- nextCellID "region/holiday"
        let
          hline = rowSlab [
              def
                & cellValue ?~ CellText (regionID r <> ":" <> (pack $ iso8601Show d))
                & cellID ?~ rid
            , def
                & cellText ?~ ListMsg (map NamedCalendarLabel hs)
            ]
        return $ (s >>! hline, rids ++ [rid])
      ) (SEmpty, []) holidayDates
    return $ header >>! dates

createHolidaySheet :: Config -> CaminoPreferences -> CellIDStream (Worksheet CaminoMsg, CellID, M.Map Region Int)
createHolidaySheet config cprefs = do
    (holidays, htable, rtable) <- createHolidayMatrixSlab config cprefs
    return $ (Worksheet HolidaysLabel holidays, htable, rtable)

--  Create an excel spreadsheet with the planned camino
createCaminoXlsx :: Config -- ^ The dislay configuration
  -> Renderer CaminoMsg -- ^ The message renderer
  -> TravelPreferences -- ^ The selected travel preferences
  -> CaminoPreferences -- ^ The selected camino references
  -> Solution -- ^ The planning solution
  -> Xlsx -- ^ The resulting spreadsheet
createCaminoXlsx config renderer tprefs cprefs solution =
    evalState (do
      (holidays, htable, rtable) <- createHolidaySheet config cprefs
      plan <- createPlanSheet htable rtable tprefs cprefs solution
      locations <- createLocationsSheet tprefs cprefs solution
      preferences <- createPreferenceSheet tprefs cprefs
      return $ createXlsx renderer [plan, locations, holidays, preferences]
    ) 1
