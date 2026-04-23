{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-
Module      : I18n
Description : Common Internationalisation for Camino display
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Common Internationalisation for Camino display
-}
module Camino.Display.I18n (
  -- * Internationalisation
    CaminoMsg(..)
  , renderCaminoMsg
  , renderCaminoMsgText

  -- Formatting
  , formatDistance
  , formatMaybeDistance
  , formatMaybeHours
  , formatPenance
  , formatPenancePlain
  , formatHeight
  , formatHours
  , formatStages
  , rejectSymbol
) where

import Camino.Camino
import Camino.Config
import qualified Camino.Planner as P
import qualified Camino.Units as U
import Data.Localised
import Data.Region
import Data.Text
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Time.Calendar as C
import Data.Time.Format
import Data.Time.LocalTime
import Data.Util (commaJoin)
import Formatting (fixed, int, sformat)
import Language.Haskell.TH
import Text.Blaze.Html (text, text, toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Internal as TB
import Text.Hamlet
import Text.MessageCatalogue

-- | A symbol indicating something beyond a preference range or specifically rejected
rejectSymbol :: Text
rejectSymbol = "\x25c6"

thinSpace :: Text
thinSpace = "\x2009"

halfSymbol :: Text
halfSymbol = "\x00bd"

quarterSymbol :: Text
quarterSymbol = "\x00bc"

threeQuarterSymbol :: Text
threeQuarterSymbol = "\x00be"

-- | Format a value for a system of units.
--
--   SI units are simple @0.0@ decimal values.
--   US units tend to split things into quarters.
formatForSystemOfUnits U.SIUnits v = sformat (fixed 1) v
formatForSystemOfUnits U.USUnits v = whole <> part where
  (n, f) = properFraction v
  (n', f') = if f > 0.875 then (n + 1, 0.0) else (n, f)
  whole = if n' == 0 then if f' < 0.25 then "0" else "" else sformat int n'
  part = if f' < 0.125 then ""
    else if f' < 0.365 then quarterSymbol
    else if f' < 0.625 then halfSymbol
    else threeQuarterSymbol

-- | Format a penance value
--
-- Note the use of direct blaze scans in the following code, rather than shamlet.
-- Hamlet seems to generate tags as chunks of content, rather than proper blaze HTML,
-- so the direct formatting allows renderCaminoMsgText to strip tags properly.
formatPenance :: U.SystemOfUnits -> Bool -> Penance -> Html
formatPenance _ _ Reject = H.span H.! HA.class_ "penance rejected" H.! HA.title "Rejected" $ text rejectSymbol
formatPenance sou showZero (Penance p) = if showZero || p /= 0.0 then H.span H.! HA.class_ "penance" $ text $ p'' <> thinSpace <> symbol else text ""
  where
    unit = U.preferredUnit sou U.Distance
    p' = U.convertAmount U.Kilometre unit p
    p'' = formatForSystemOfUnits sou p'
    symbol = pack $ U.unitSymbol unit

formatPenancePlain :: U.SystemOfUnits -> Bool -> Penance -> Html
formatPenancePlain _ _ Reject = "Rejected"
formatPenancePlain sou showZero (Penance p) = if showZero || p /= 0.0 then text $ p'' <> symbol else text ""
  where
    unit = U.preferredUnit sou U.Distance
    p' = U.convertAmount U.Kilometre unit p
    p'' = formatForSystemOfUnits sou p'
    symbol = pack $ U.unitSymbol unit

formatDistance :: (RealFrac a) => U.SystemOfUnits -> a -> Html
formatDistance sou d = H.span H.! HA.class_ "distance" $ text $ d'' <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Distance
    d' = U.convertAmount U.Kilometre unit d
    d'' = formatForSystemOfUnits sou d'
    symbol = pack $ U.unitSymbol unit


formatMaybeDistance :: (RealFrac a) => U.SystemOfUnits -> Maybe a -> Html
formatMaybeDistance sou Nothing = formatPenance sou True Reject
formatMaybeDistance sou (Just d) = formatDistance sou d

formatHours :: (RealFrac a) => U.SystemOfUnits -> a -> Html
formatHours sou t = H.span H.! HA.class_ "time" $ text $ sformat (fixed 1) t <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Time
    t' = U.convertAmount U.Hour unit t
    symbol = pack $ U.unitSymbol unit

formatDays :: U.SystemOfUnits -> Int -> Html
formatDays sou d = H.span H.! HA.class_ "days" $ text $  sformat int d' <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Calendar
    d' = round $ U.convertAmount U.Day unit (fromIntegral d :: Float)
    symbol = pack $ U.unitSymbol unit

formatStages :: Int -> Html
formatStages s = H.span H.! HA.class_ "stages" $ text $  sformat  int s <> thinSpace <> "stages"

formatMaybeHours :: (RealFrac a) => U.SystemOfUnits -> Maybe a -> Html
formatMaybeHours sou Nothing = formatPenance sou True Reject
formatMaybeHours sou (Just t) = formatHours sou t

formatHeight :: (RealFrac a) => U.SystemOfUnits -> a -> Html
formatHeight sou h = H.span H.! HA.class_ "height" $ text $  sformat  (fixed 0) h' <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Elevation
    h' = U.convertAmount U.Metre unit h
    symbol = pack $ U.unitSymbol unit

hasLocalisedText :: (Tagged a) => [Locale] -> Localised a -> Bool
hasLocalisedText locales locd = maybe False (\t -> plainText t /= "") $ localise locales locd

renderLocalisedText :: (Tagged a) => [Locale] -> Bool -> Bool -> Bool -> Localised a -> Html
renderLocalisedText locales plain attr js locd = let
    elt = localise locales locd
    txt = maybe "" plainText elt
    txt' = if attr then replace "\"" "'" txt else txt
    txt'' = if js then replace "'" "\\'" txt' else txt'
    loc = maybe rootLocale locale elt
    lng = localeLanguageTag loc
  in
    if plain || attr || Data.Text.null lng then
      toHtml txt''
    else
      H.span H.! HA.lang (toValue lng) $ Text.Blaze.Html.text txt''

renderLocalisedDate :: (FormatTime t) => U.SystemOfUnits -> Bool -> [Locale] -> t -> Html
renderLocalisedDate sou weekDay [] day = renderLocalisedDate sou weekDay [rootLocale] day
renderLocalisedDate _sou weekDay (loc:_) day = toHtml $ if weekDay then dwf ++ df else df
  where
    tl = localeTimeLocale loc
    df = formatTime tl (dateFmt tl) day
    dwf = formatTime tl "%a " day

renderLocalisedTime :: (FormatTime t) => U.SystemOfUnits -> [Locale] -> String -> t -> Html
renderLocalisedTime sou [] fmt t = renderLocalisedTime sou [rootLocale] fmt t
renderLocalisedTime _sou (loc:_) fmt t = toHtml $ formatTime (localeTimeLocale loc) fmt t

renderLocalisedMonth :: U.SystemOfUnits -> [Locale] -> C.MonthOfYear -> Html
renderLocalisedMonth sou [] t = renderLocalisedMonth sou [rootLocale] t
renderLocalisedMonth _sou (loc:_) t = toHtml $ snd $ (months $ localeTimeLocale loc) !! t

renderLocalisedOrdinal :: U.SystemOfUnits -> [Locale] -> Int -> Html
renderLocalisedOrdinal sou [] o = renderLocalisedOrdinal sou [rootLocale] o
renderLocalisedOrdinal _sou (loc:_) o = toHtml $ (localeOrdinalRender loc) o

renderLocalisedBeforeAfter :: Int -> Text -> Text -> Html
renderLocalisedBeforeAfter n before after = toHtml (if n < 0 then before else after)

renderLocalisedWeekOfMonth :: U.SystemOfUnits -> [Locale] -> WeekOfMonth -> Html
renderLocalisedWeekOfMonth sou [] wom = renderLocalisedWeekOfMonth sou [rootLocale] wom
renderLocalisedWeekOfMonth _sou (loc:_) wom = toHtml $ (localeWeekOfMonthRender loc) wom

renderLocalisedDayOfWeek :: U.SystemOfUnits -> [Locale] -> C.DayOfWeek -> Html
renderLocalisedDayOfWeek sou [] dow = renderLocalisedDayOfWeek sou [rootLocale] dow
renderLocalisedDayOfWeek _sou (loc:_) dow = let
    idx = (fromEnum dow) - 1
    names = wDays $ localeTimeLocale loc
    name = names !! idx
  in
    toHtml $ snd name

renderLocalisedRegion :: Config -> U.SystemOfUnits -> [Locale] -> Text -> Html
renderLocalisedRegion config closed locs rid = maybe (toHtml rid) (\r -> renderLocalisedText locs False False False (regionName r)) region
  where
    region = (regionConfigLookup $ getRegionConfig config) rid

mkMessageCatalogue (Just ''Config) (mkName "CaminoMsg") ''Locale [("config", ''Config), ("sou", ''U.SystemOfUnits)] "messages" "en" False

-- | Render a camino message as un-markuped text
renderCaminoMsgText :: Config -> U.SystemOfUnits -> [Locale] -> CaminoMsg -> Text
renderCaminoMsgText config sou locales (ListMsg msgs) = commaJoin $ Prelude.map (renderCaminoMsgText config sou locales) msgs
renderCaminoMsgText config sou locales msg = renderMarkupToText $ renderCaminoMsg config sou locales msg

