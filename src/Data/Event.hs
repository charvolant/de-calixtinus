{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Event
Description : Modelling for events that occur on particular days, months, etc
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Allows the specification of an event calendar for events that occur on particular days,
days of week, months, etc.

Eventually, the modelling will improve
-}
module Data.Event (
    CalendarConfig(..)
  , CalendarConfigEntry(..)
  , EventCalendar(..)
  , EventHours(..)
  , EventTime(..)
  , HasCalendarConfig(..)
  , OpenHours(..)
  
  , calendarKey
  , createCalendarConfig
  , getHoliday
  , getNamedCalendar
  , getNamedCalendarName
  , hasCalendar
  , hasHours
  , isAlwaysOpen
  , isDaily
) where

import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import Data.Localised
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, intercalate, pack, splitOn, unpack)
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import Formatting

-- | A calendar configuration entry
data CalendarConfigEntry = CalendarConfigEntry {
    ceKey :: Text
  , ceName :: Localised TaggedText
  , ceCalendar :: EventCalendar
} deriving (Show, Eq)

instance ToJSON CalendarConfigEntry where
  toJSON (CalendarConfigEntry key' name' calendar') = object [ "key" .= key', "name" .= name', "calendar" .= calendar' ]

instance FromJSON CalendarConfigEntry where
  parseJSON (Object v) = do
    key' <- v .: "key"
    name' <- v .: "name"
    calendar' <- v .: "calendar"
    return $ CalendarConfigEntry key' name' calendar'
  parseJSON v = typeMismatch "calendar entry object" v
  
-- | Top-level calendar configureation for named calendars.
--   This is intended to be embedded in a ReaderT so that readers and writers can access common calendar defintions
data CalendarConfig = CalendarConfig {
    calendarConfigCalendars :: [CalendarConfigEntry]
  , calendarConfigLookup :: Text -> Maybe CalendarConfigEntry
}

instance Show CalendarConfig where
  show config = showString "CalendarConfig: " $ showList (calendarConfigCalendars config) ""

instance Eq CalendarConfig where
  config1 == config2 = (calendarConfigCalendars config1) == (calendarConfigCalendars config2)
  
instance ToJSON CalendarConfig where
  toJSON config = toJSON $ calendarConfigCalendars config

instance FromJSON CalendarConfig where
  parseJSON v@(Array _) = do
    calendars' <- parseJSONList v :: Parser [CalendarConfigEntry]
    let calendarMap = M.fromList $ map (\e -> (ceKey e, e)) calendars'
    return $ CalendarConfig calendars' (\k -> M.lookup k calendarMap)
  parseJSON v = typeMismatch "array of calendar entries" v

class HasCalendarConfig a where
  getCalendarConfig :: a -> CalendarConfig

instance HasCalendarConfig CalendarConfig where
  getCalendarConfig = id

-- | Create a simple calendar config from a list of triples for testing purposes
createCalendarConfig :: [(Text, Localised TaggedText, EventCalendar)] -> CalendarConfig
createCalendarConfig entries = let
    ces = map (\(k, n, c) -> CalendarConfigEntry k n c) entries
    cesm = M.fromList $ map (\e -> (ceKey e, e)) ces
  in
    CalendarConfig ces (\k -> M.lookup k cesm)
    
-- | Get a named calendar from an environment.
--   Throws an error if the named calendar is not found
getNamedCalendar :: (MonadReader env m, HasCalendarConfig env) => Text -> m EventCalendar
getNamedCalendar key = do
  env <- ask
  let mcal = (calendarConfigLookup $ getCalendarConfig env) key
  let cal = maybe (error ("Can't find named calendar with key " ++ show key)) ceCalendar mcal
  return cal

-- | Get a public holiday named calendar from an envonment
--   Throws an error if the named calendar is not found or if this is not a public holiday
getHoliday :: (MonadReader env m, HasCalendarConfig env) => EventCalendar -> m CalendarConfigEntry
getHoliday (NamedCalendar key) = do
  env <- ask
  let mcal = (calendarConfigLookup $ getCalendarConfig env) key
  let cal = maybe (error ("Can't find named calendar with key " ++ show key)) id mcal
  return cal
getHoliday cal = error ("Can't find calendar " ++ show cal)

-- | Get a named calendar from an environment.
--   Throws an error if the named calendar is not found
getNamedCalendarName :: (MonadReader env m, HasCalendarConfig env) => Text -> m (Localised TaggedText)
getNamedCalendarName key = do
  env <- ask
  let mcal = (calendarConfigLookup $ getCalendarConfig env) key
  let name = maybe (error ("Can't find named calendar with key " ++ show key)) ceName mcal
  return name

-- | A calendar for an event.
--   The calendar is intended to allow the building of 
data EventCalendar =
    Daily -- ^ Occurs daily
  | Weekly (S.Set DayOfWeek)  -- ^ Occurs on certain days of the week
  | Monthly (S.Set DayOfMonth)  -- ^ Occurs on certain days of the month
  | Yearly (S.Set MonthOfYear) -- ^ Occurs on certain months of the year
  | DayOfYear (S.Set (MonthOfYear, DayOfMonth)) -- ^ Occurs on specific days in the year
  | RangeCalendar EventCalendar EventCalendar -- ^ On or between a range of dates
  | UnionCalendar [EventCalendar] -- ^ Union of other calendars (or)
  | IntersectionCalendar [EventCalendar] -- ^ Intersection of other calendars (and)
  | InvertedCalendar EventCalendar -- ^ The inverse of a calendar (except)
  | NthDayAfter Int EventCalendar -- ^ Eg 27 days before Easter
  | NthWeekday WeekOfMonth DayOfWeek -- ^ Eg first Tuesday of the month with 5 = last
  | NthWeekdayAfter Int DayOfWeek EventCalendar -- ^ Eg 3rd Sunday after Easter, negative for before
  | ListCalendar (S.Set Day) -- ^ A specific list of Julian Days
  | NamedCalendar Text  -- ^ A calendar with a specific key (eg Easter) that has a definition supplied by a calendar configuration
  | PublicHoliday Text -- ^ A regional public holiday
  | ClosedDay Text -- ^ A regional closed day (eg Sundays)
  | Conditional EventCalendar (Localised TaggedText) -- ^ Occurs at complex times specified by a note
  deriving (Show)
  
instance Eq EventCalendar where
  Daily == Daily = True
  Weekly dow1 == Weekly dow2 = dow1 == dow2
  Monthly dom1 == Monthly dom2 = dom1 == dom2
  Yearly moy1 == Yearly moy2 = moy1 == moy2
  DayOfYear dates1 == DayOfYear dates2 = dates1 == dates2
  RangeCalendar rfrom1 rto1 == RangeCalendar rfrom2 rto2 = rfrom1 == rfrom2 && rto1 == rto2
  UnionCalendar calendars1 == UnionCalendar calendars2 = calendars1 == calendars2
  IntersectionCalendar calendars1 == IntersectionCalendar calendars2 = calendars1 == calendars2
  InvertedCalendar calendar1 == InvertedCalendar calendar2 = calendar1 == calendar2
  NthDayAfter days1 calendar1 == NthDayAfter days2 calendar2 = days1 == days2 && calendar1 == calendar2
  NthWeekday nth1 week1 == NthWeekday nth2 week2 = nth1 == nth2 && week1 == week2
  NthWeekdayAfter nth1 day1 calendar1 == NthWeekdayAfter nth2 day2 calendar2 = nth1 == nth2 && day1 == day2 && calendar1 == calendar2
  ListCalendar dates1 == ListCalendar dates2 = dates1 == dates2
  NamedCalendar key1 == NamedCalendar key2 = key1 == key2
  PublicHoliday region1 == PublicHoliday region2 = region1 == region2
  ClosedDay region1 == ClosedDay region2 = region1 == region2
  Conditional cal1 cond1 == Conditional cal2 cond2 = cal1 == cal2 && localiseDefault cond1 == localiseDefault cond2
  _ == _ = False 

instance ToJSON EventCalendar where
  toJSON Daily = object [
      "type" .= ("daily" :: Text)
    ]
  toJSON (Weekly days') = object [
      "type" .= ("weekly" :: Text)
    , "days" .= days'
    ]
  toJSON (Monthly days') = object [
      "type" .= ("monthly" :: Text)
    , "days" .= days'
    ]
  toJSON (Yearly months') = object [
      "type" .= ("yearly" :: Text)
    , "months" .= months'
    ]
  toJSON (DayOfYear days') = object [
      "type" .= ("day-of-year" :: Text)
    , "days" .= S.map (\(m, d) -> format (left 2 '0') m  <> "-" <> format (left 2 '0') d) days'
    ]
  toJSON (RangeCalendar from' to') = object [
      "type" .= ("range" :: Text)
    , "from" .= from'
    , "to" .= to'
    ]
  toJSON (UnionCalendar calendars') = object [
      "type" .= ("union" :: Text)
    , "calendars" .= calendars'
    ]
  toJSON (IntersectionCalendar calendars') = object [
      "type" .= ("intersection" :: Text)
    , "calendars" .= calendars'
    ]
  toJSON (InvertedCalendar calendar') = object [
      "type" .= ("except" :: Text)
    , "calendar" .= calendar'
    ]
  toJSON (NthDayAfter nth' calendar') = object [
      "type" .= ("nth-day-after" :: Text)
    , "nth" .= nth'
    , "calendar" .= calendar'
    ]
  toJSON (NthWeekday nth' day') = object [
      "type" .= ("nth-weekday" :: Text)
    , "nth" .= nth'
    , "day" .= day'
    ]
  toJSON (NthWeekdayAfter nth' day' calendar') = object [
      "type" .= ("nth-weekday-after" :: Text)
    , "nth" .= nth'
    , "day" .= day'
    , "calendar" .= calendar'
    ]
  toJSON (ListCalendar dates') = object [
      "type" .= ("list" :: Text)
    , "dates" .= S.map (formatTime rootTimeLocale "%F") dates'
    ]
  toJSON (NamedCalendar key') = object [
      "type" .= ("named" :: Text)
    , "key" .= key'
    ]
  toJSON (PublicHoliday region') = object [
      "type" .= ("public-holiday" :: Text)
    , "region" .= region'
    ]
  toJSON (ClosedDay region') = object [
      "type" .= ("closed-day" :: Text)
    , "region" .= region'
    ]
  toJSON (Conditional calendar' condition') = object [
      "type" .= ("conditional" :: Text)
    , "calendar" .= calendar'
    , "condition" .= condition'
    ]
 
parseMonthDay :: Text -> (MonthOfYear, DayOfMonth)
parseMonthDay day = let
    md = splitOn "-" day
  in
    case md of
      [moy, dom] -> (read (unpack moy), read (unpack dom))
      _ -> error "Expecting month-day format"
  
instance FromJSON EventCalendar where
  parseJSON (Object v) = do
    type' <- v .: "type"
    case type' of
      "daily" -> return Daily
      "weekly" -> do
        dow' <- v .: "days"
        return $ Weekly dow'
      "monthly" -> do
        dom' <- v .: "days"
        return $ Monthly dom'
      "yearly" -> do
        months' <- v .: "months"
        return $ Yearly months'
      "day-of-year" -> do
        days' <- v .: "days"
        let days'' = map parseMonthDay days'
        return $ DayOfYear (S.fromList days'')
      "range" -> do
        from' <- v .: "from"
        to' <- v .: "to"
        return $ RangeCalendar from' to'
      "union" -> do
        calendars' <- v .: "calendars"
        return $ UnionCalendar calendars'
      "intersection" -> do
        calendars' <- v .: "calendars"
        return $ IntersectionCalendar calendars'
      "except" -> do
        calendar' <- v .: "calendar"
        return $ InvertedCalendar calendar'
      "nth-day-after" -> do
        nth' <- v .: "nth"
        calendar' <- v .: "calendar"
        return $ NthDayAfter nth' calendar'
      "nth-weekday" -> do
        nth' <- v .: "nth"
        day' <- v .: "day"
        return $ NthWeekday nth' day'
      "nth-weekday-after" -> do
        nth' <- v .: "nth"
        day' <- v .: "day"
        calendar' <- v .: "calendar"
        return $ NthWeekdayAfter nth' day' calendar'
      "list" -> do
        dates' <- v .: "dates"
        return $ ListCalendar (S.fromList $ map (\d -> parseTimeOrError True rootTimeLocale "%F" d :: Day) dates')
      "named" -> do
        key' <- v .: "key"
        return $ NamedCalendar key'
      "public-holiday" -> do
        region' <- v .: "region"
        return $ PublicHoliday region'
      "closed-day" -> do
        region' <- v .: "region"
        return $ ClosedDay region'
      "conditional" -> do
        calendar' <- v .: "calendar"
        condition' <- v .: "condition"
        return $ Conditional calendar' condition'
      invalid' -> typeMismatch "expecting event type" invalid'
  parseJSON v = typeMismatch "expecting object" v

-- | Is this calendar daily (ie. no specific dates)
isDaily :: EventCalendar -> Bool
isDaily Daily = True
isDaily _ = False

-- | Get the calendar key for a calendar
--   Returns either just the key or nothing for a non-named calendar
calendarKey :: EventCalendar -> Maybe Text
calendarKey (NamedCalendar key) = Just key
calendarKey _ = Nothing

-- | The times an event can occur during a day  
data EventTime =
    EventClosed -- ^ Indicates a null event time
  | EventOpen -- ^ Indicates soemthing open 24/7
  | EventTime [(TimeOfDay, TimeOfDay)]
  deriving (Show, Eq)

instance ToJSON EventTime where
  toJSON EventClosed = "closed"
  toJSON EventOpen = "open"
  toJSON (EventTime times') = toJSON $ intercalate ", " $ map (\(from', to') -> fmt from' <> "-" <> fmt to') times'
    where
      fmt t = pack $ formatTime rootTimeLocale "%H%M" t

-- | Parse a HHMM-HHMM time range
parseTimeRange v = case splitOn "-" v of
  [from', to'] -> let
      from'' = parseTimeM True rootTimeLocale "%H%M" (unpack from') :: Maybe TimeOfDay
      to'' = parseTimeM True rootTimeLocale "%H%M" (unpack to') :: Maybe TimeOfDay
    in case (from'', to'') of
      (Just from''', Just to''') -> (from''', to''')
      _ -> error ("Can't parse time range " ++ unpack v)
  _ -> error ("Can't parse time range " ++ unpack v)

instance FromJSON EventTime where
  parseJSON (String v) = do
    return $ case v of
      "closed" -> EventClosed
      "open" -> EventOpen
      _ -> EventTime times'' where
        times' = splitOn "," v
        times'' = map parseTimeRange times'
  parseJSON v = typeMismatch "expecting string" v
  
-- | Is this open 24/7
isAlwaysOpen :: EventTime -> Bool
isAlwaysOpen EventOpen = True
isAlwaysOpen _ = False


-- | A combination of calendar and hours (eg. open on Fridays, 10-12)
data EventHours = EventHours EventCalendar EventTime
    deriving (Show, Eq)

instance ToJSON EventHours where
  toJSON (EventHours calendar' hours') = object [
      "calendar" .= calendar'
    , "hours" .= hours'
    ]

instance FromJSON EventHours where
  parseJSON v@(String _) = do
    hours' <- parseJSON v
    return $ EventHours Daily  hours'
  parseJSON (Object v) = do
    calendar' <- v .:? "calendar" .!= Daily
    hours' <- v .:? "hours" .!= EventOpen
    return $ EventHours calendar' hours'
  parseJSON v = typeMismatch "expecting string or object" v

-- | The opening hours of an establishment
--   Opening hours match in order, so it is possible to have something like "closed on holidays, otherwise open from 10-5"
data OpenHours = OpenHours [EventHours]
    deriving (Show, Eq)

instance ToJSON OpenHours where
  toJSON (OpenHours [hours']) = toJSON hours'
  toJSON (OpenHours hours') = toJSON $ map toJSON hours'

instance FromJSON OpenHours where
  parseJSON Null = return $ OpenHours [EventHours Daily EventOpen]  
  parseJSON v@(String _) = do
    hours' <- parseJSON v
    return $ OpenHours [EventHours Daily hours']
  parseJSON v@(Array _) = do
    hours' <- parseJSONList v
    return $ OpenHours hours'
  parseJSON v@(Object _) = do
    hours' <- parseJSON v
    return $ OpenHours [hours']
  parseJSON v = typeMismatch "expecting string, object or array" v

-- | Has at least one entry that gives a calendar of some sort
hasCalendar :: OpenHours -> Bool
hasCalendar (OpenHours hours) = any (\(EventHours cal _time) -> not $ isDaily cal) hours


-- | Has at least one entry that is not 24/7
hasHours :: OpenHours -> Bool
hasHours (OpenHours hours) = any (\(EventHours _cal time) -> not $ isAlwaysOpen time) hours