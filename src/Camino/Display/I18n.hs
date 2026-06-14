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
  -- * Localisation Functions
  , hasLocalisedText
  , renderLocalisedText
  , renderLocalisedDate
  , renderLocalisedMonth
  , renderLocalisedWeekOfMonth
  , renderLocalisedDayOfWeek
  , renderLocalisedRegion
  , renderLocalisedBeforeAfter
  , renderLocalisedOrdinal
  -- * Formatting
  , formatDays
  , formatDistance
  , formatMaybeDistance
  , formatMaybeHours
  , formatPenance
  , formatPenancePlain
  , formatHeight
  , formatHours
  , formatStages
  -- * Symbols
  , rejectSymbol
  , thinSpace
  , halfSymbol
  , quarterSymbol
  , threeQuarterSymbol
) where

import Camino.Camino
import Camino.Config
import qualified Camino.Units as U
import Data.Localised
import Data.Region
import Data.Text
import qualified Data.Time.Calendar as C
import Data.Time.Format
import Data.Time.LocalTime
import Data.Util (commaJoin)
import Formatting (fixed, int, sformat)
import Language.Haskell.TH
import Text.Blaze.Html (text, text, toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Hamlet
import Text.MessageCatalogue

-- | A symbol (◆) indicating something beyond a preference range or specifically rejected
rejectSymbol :: Text
rejectSymbol = "\x25c6"

-- | The sort of thin space one might want to use for separating an amount and a unit
thinSpace :: Text
thinSpace = "\x2009"

-- | The 1/2 (½) fraction
halfSymbol :: Text
halfSymbol = "\x00bd"

-- | The 1/4 (¼) fraction
quarterSymbol :: Text
quarterSymbol = "\x00bc"

-- | The 3/4 (¾) fraction
threeQuarterSymbol :: Text
threeQuarterSymbol = "\x00be"

-- | Format a value for a system of units.
--
--   SI units are simple @0.0@ decimal values.
--   US units tend to split things into quarters.
--
-- >>> formatForSystemOfUnits SIUnits 12.7
-- 12.7
-- >>> formatForSystemOfUnits USUnits 12.7
-- 12¾
formatForSystemOfUnits :: (RealFrac a) => U.SystemOfUnits -> a -> Text
formatForSystemOfUnits U.SIUnits v = sformat (fixed 1) v
formatForSystemOfUnits U.USUnits v = whole <> part where
  (n, f) = properFraction v
  (n', f') = if f > 0.875 then (fromIntegral n + 1 :: Int, 0.0) else (fromInteger n :: Int, realToFrac f :: Float)
  whole = if n' == 0 then if f' < 0.25 then "0" else "" else sformat int n'
  part = if f' < 0.125 then ""
    else if f' < 0.365 then quarterSymbol
    else if f' < 0.625 then halfSymbol
    else threeQuarterSymbol

-- | Format a penance value
--
--   This encloses the penance value in a span that can be used with CSS

-- Note the use of direct blaze scans in the following code, rather than shamlet.
-- Hamlet seems to generate tags as chunks of content, rather than proper blaze HTML,
-- so the direct formatting allows renderCaminoMsgText to strip tags properly.
--
-- >>> renderHtml $ formatPenance def True (Penance 12.1)
-- "<span class=\"penance\">12.1\226\128\137km</span>"
formatPenance :: U.SystemOfUnits -> Bool -> Penance -> Html
formatPenance _ _ Reject = H.span H.! HA.class_ "penance rejected" H.! HA.title "Rejected" $ text rejectSymbol
formatPenance sou showZero (Penance p) = if showZero || p /= 0.0 then H.span H.! HA.class_ "penance" $ text $ p'' <> thinSpace <> symbol else text ""
  where
    unit = U.preferredUnit sou U.Distance
    p' = U.convertAmount U.Kilometre unit p
    p'' = formatForSystemOfUnits sou p'
    symbol = pack $ U.unitSymbol unit

-- | Format a penance (see `formatPenance`) without extensive markup
-- >>> renderHtml $ formatPenancePlain def True (Penance 12.1)
-- "12.1km"
formatPenancePlain :: U.SystemOfUnits -> Bool -> Penance -> Html
formatPenancePlain _ _ Reject = "Rejected"
formatPenancePlain sou showZero (Penance p) = if showZero || p /= 0.0 then text $ p'' <> symbol else text ""
  where
    unit = U.preferredUnit sou U.Distance
    p' = U.convertAmount U.Kilometre unit p
    p'' = formatForSystemOfUnits sou p'
    symbol = pack $ U.unitSymbol unit

-- | Format a distance
--
--   Distances are always in kilometres.
--   The system of units specified will convert the amount and display format appropriately
--
-- >>> renderHtml $ formatDistance def 12.6
-- "<span class=\"distance\">12.6\226\128\137km</span>"
-- >>> renderHtml $ formatDistance USUnits 12.6
-- "<span class=\"distance\">7\194\190\226\128\137mi</span>"
formatDistance :: (RealFrac a) => U.SystemOfUnits -> a -> Html
formatDistance sou d = H.span H.! HA.class_ "distance" $ text $ d'' <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Distance
    d' = U.convertAmount U.Kilometre unit d
    d'' = formatForSystemOfUnits sou d'
    symbol = pack $ U.unitSymbol unit


-- | Format an optional distance
--
--   If there isn't a distance then it is displayed as a `invalid` value.
--   See `formatDistance`
formatMaybeDistance :: (RealFrac a) => U.SystemOfUnits -> Maybe a -> Html
formatMaybeDistance sou Nothing = formatPenance sou True Reject
formatMaybeDistance sou (Just d) = formatDistance sou d

-- | Format a time interval
--
--   Periods are always in hours.
--   The system of units specified will convert the amount and display format appropriately
--
-- >>> renderHtml $ formatHours def 12.4
-- "<span class=\"time\">12.4\226\128\137hr</span>"
formatHours :: (RealFrac a) => U.SystemOfUnits -> a -> Html
formatHours sou t = H.span H.! HA.class_ "time" $ text $ sformat (fixed 1) t' <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Time
    t' = U.convertAmount U.Hour unit t
    symbol = pack $ U.unitSymbol unit

-- | Format a large time interval
--
--   Periods are always in days.
--   The system of units specified will convert the amount and display format appropriately
--
-- >>> renderHtml $ formatDays def 7
-- "<span class=\"days\">7\226\128\137days</span>"

formatDays :: U.SystemOfUnits -> Int -> Html
formatDays sou d = H.span H.! HA.class_ "days" $ text $  sformat int d' <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Calendar
    d' = (round $ U.convertAmount U.Day unit (fromIntegral d :: Float)) :: Int
    symbol = pack $ U.unitSymbol unit

-- | Format a number of multi-day stages
--
-- >>> renderHtml $ formatStages 2
-- "<span class=\"stages\">2\226\128\137stages</span>"
formatStages :: Int -> Html
formatStages s = H.span H.! HA.class_ "stages" $ text $  sformat  int s <> thinSpace <> "stages"

-- | Format an optional time period
--
--   If there isn't a period then it is displayed as a `invalid` value.
--   See `formatHours`
formatMaybeHours :: (RealFrac a) => U.SystemOfUnits -> Maybe a -> Html
formatMaybeHours sou Nothing = formatPenance sou True Reject
formatMaybeHours sou (Just t) = formatHours sou t

-- | Format a height
--
--   Heights are always in metres.
--   The system of units specified will convert the amount and display format appropriately
--
-- >>> renderHtml $ formatHeight def 125
-- "<span class=\"height\">125\226\128\137m</span>"
-- >>> renderHtml $ formatHeight USUnits 125
-- "<span class=\"height\">410\226\128\137ft</span>"
formatHeight :: (RealFrac a) => U.SystemOfUnits -> a -> Html
formatHeight sou h = H.span H.! HA.class_ "height" $ text $  sformat  (fixed 0) h' <> thinSpace <> symbol
  where
    unit = U.preferredUnit sou U.Elevation
    h' = U.convertAmount U.Metre unit h
    symbol = pack $ U.unitSymbol unit

-- | Does this have a localisable piece of text for a locale?
hasLocalisedText :: (Tagged a) => [Locale] -> Localised a -> Bool
hasLocalisedText locales locd = maybe False (\t -> plainText t /= "") $ localise locales locd

-- | Display the correct piece of localised text, based on a list of preferred locales and what is available.
--
--   Anything that is not a plain string is embedded in a @\<span>@ with an appropriate @lang@ attribute.
renderLocalisedText :: (Tagged a) => [Locale] -- ^ The list of locales
  -> Bool -- ^ Render as plain text without additional markup
  -> Bool -- ^ Render in a form sutable for a markup attribute value
  -> Bool -- ^ Render in a form suiable for javascript strings
  -> Localised a -- ^ The localisable item
  -> Html -- ^ The resulting markup
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

-- | Display a date in a locale-specific format
--
-- >>> renderHtml $ renderLocalisedDate def False ["fr"] (fromGregorian 2024 April 17)
-- "2024-04-17"
renderLocalisedDate :: (FormatTime t) => U.SystemOfUnits -- ^ Preferred system of units
  -> Bool -- ^ Include the weekday name
  -> [Locale] -- ^ A list of potential locales
  -> t -- ^ The date to format
  -> Html -- ^ The formatted date
renderLocalisedDate sou weekDay [] day = renderLocalisedDate sou weekDay [rootLocale] day
renderLocalisedDate _sou weekDay (loc:_) day = toHtml $ if weekDay then dwf ++ df else df
  where
    tl = localeTimeLocale loc
    df = formatTime tl (dateFmt tl) day
    dwf = formatTime tl "%a " day

-- | Display a time in a locale-specific format
--
--   This function displays a time according to a format specification, substituting locale-specific elements
--   where appropriate. See `formatTime` for the format specification
--
--   renderHtml $ renderLocalisedDate def ["fr"] "%a" (fromGregorian 2024 April 17)
renderLocalisedTime :: (FormatTime t) => U.SystemOfUnits -- ^ Preferred system of units
  -> [Locale] -- ^ A list of potential locales
  -> String -- ^ The time format
  -> t -- ^ The time to format
  -> Html -- ^ The formatted time
renderLocalisedTime sou [] fmt t = renderLocalisedTime sou [rootLocale] fmt t
renderLocalisedTime _sou (loc:_) fmt t = toHtml $ formatTime (localeTimeLocale loc) fmt t

-- | Display a month in a localised format
--
-- >>> renderHtml $ renderLocalisedMonth def ["en"] 1
-- "Feb"
renderLocalisedMonth :: U.SystemOfUnits -- ^ Preferred system of units
  -> [Locale] -- ^ A list of potential locales
  -> C.MonthOfYear -- ^ The month
  -> Html -- ^ The formatted month
renderLocalisedMonth sou [] t = renderLocalisedMonth sou [rootLocale] t
renderLocalisedMonth _sou (loc:_) t = toHtml $ snd $ (months $ localeTimeLocale loc) !! t

-- | Display an ordinal number (first, second, third etc.) in a localised format.
--
--   >>> renderHtml $ renderLocalisedOrdinal def ["en"] 1
--   "1st"
--   >>> renderHtml $ renderLocalisedOrdinal def ["fr"] 1
--   "1re"
renderLocalisedOrdinal :: U.SystemOfUnits -> [Locale] -> Int -> Html
renderLocalisedOrdinal sou [] o = renderLocalisedOrdinal sou [rootLocale] o
renderLocalisedOrdinal _sou (loc:_) o = toHtml $ (localeOrdinalRender loc) o

-- | Display a "before" or "after" as appropriate for dates\/times
--
--   >>> renderHtml $ renderLocalisedBeforeAfter 1 "before" "after"
--   "after"
--   >>> renderHtml $ renderLocalisedBeforeAfter (-1) "before" "after"
--   "before"
renderLocalisedBeforeAfter :: Int -> Text -> Text -> Html
renderLocalisedBeforeAfter n before after = toHtml (if n < 0 then before else after)

-- | Display a week of a month month in a localised format
--
-- >>> renderHtml $ renderLocalisedWeekOfMonth def ["en"] Fourth
-- "fourth"
renderLocalisedWeekOfMonth :: U.SystemOfUnits -> [Locale] -> WeekOfMonth -> Html
renderLocalisedWeekOfMonth sou [] wom = renderLocalisedWeekOfMonth sou [rootLocale] wom
renderLocalisedWeekOfMonth _sou (loc:_) wom = toHtml $ (localeWeekOfMonthRender loc) wom

-- | Display a day of a week in a localised format
--
-- >>> renderHtml $ renderLocalisedDayOfWeek def ["en"] Monday
-- "Mon"
renderLocalisedDayOfWeek :: U.SystemOfUnits -> [Locale] -> C.DayOfWeek -> Html
renderLocalisedDayOfWeek sou [] dow = renderLocalisedDayOfWeek sou [rootLocale] dow
renderLocalisedDayOfWeek _sou (loc:_) dow = let
    idx = (fromEnum dow) - 1
    names = wDays $ localeTimeLocale loc
    name = names !! idx
  in
    toHtml $ snd name

-- | Display a region name in a localised format
--
-- >>> renderHtml $ renderLocalisedRegion config def ["es"] "ES"
-- "España"
renderLocalisedRegion :: Config -- ^ The configuration containing the region data
 -> U.SystemOfUnits -- ^ The system of units in use
 -> [Locale] -- ^ A list of desired locales
 -> Text -- ^ The region identifier (see `regionID`)
 -> Html -- ^ The resulting region name
renderLocalisedRegion config _sou locs rid = maybe (toHtml rid) (\r -> renderLocalisedText locs False False False (regionName r)) region
  where
    region = (regionConfigLookup $ getRegionConfig config) rid

mkMessageCatalogue True (mkName "CaminoMsg") ''Locale [("config", ''Config), ("sou", ''U.SystemOfUnits)] "messages" "en" Nothing

-- | Render a camino message as un-markuped text
renderCaminoMsgText :: Config -> U.SystemOfUnits -> [Locale] -> CaminoMsg -> Text
renderCaminoMsgText config sou locales (ListMsg msgs) = commaJoin $ Prelude.map (renderCaminoMsgText config sou locales) msgs
renderCaminoMsgText config sou locales msg = renderMarkupToText $ renderCaminoMsg config sou locales msg

