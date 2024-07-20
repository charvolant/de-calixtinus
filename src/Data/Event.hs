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
  , EventCalendar(..)
  , EventHours(..)
  , EventTime(..)
  , HasCalendarConfig
  , OpenHours(..)
  , WeekOfMonth(..)
  
  , calendarDateOnOrAfter
  , calendarDateOnOrBefore
  , getNamedCalendar
  , inCalendar
  , nthWeekOnOf
) where

import GHC.Generics
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
import Data.Time.Calendar.MonthDay (monthLength)

-- For making statements like "last Tuesday of the month" etc.
data WeekOfMonth =
    First
  | Second
  | Third
  | Fourth
  | Fifth
  | Last
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON WeekOfMonth
instance ToJSON WeekOfMonth

-- | Calculate the nth weekday of a month for a day
--   In cases of the fifth day-of-week of the month, if it overflows it becomes the fourth day-of-week of the month
nthWeekOnOf :: WeekOfMonth -- ^ The week of the month
  -> DayOfWeek -- ^ The day of the week
  -> Day -- ^ The target date
  -> Day -- ^ The nth day-of-week for the month of the target date
nthWeekOnOf Last dow day = let
    (year', month', _day') = toGregorian day
    isLeap = isLeapYear year'
    lastDay = fromGregorian year' month' (monthLength isLeap month')
    after = firstDayOfWeekOnAfter dow lastDay
    (_, month'', _) = toGregorian after
  in 
    if month' == month'' then after else addDays (-7) after
nthWeekOnOf nth dow day = let
      (year', month', _day') = toGregorian day
      firstDay = fromGregorian year' month' 1
      skip = case nth of
        First -> 0
        Second -> 7
        Third -> 14
        Fourth -> 21
        Fifth -> 28
      after = firstDayOfWeekOnAfter dow (addDays skip firstDay)
      (_, month'', _) = toGregorian after
    in 
      if month' == month'' then after else addDays (-7) after

-- | Top-level calendar configureation for named calendars.
--   This is intended to be embedded in a ReaderT so that readers and writers can access common calendar defintions
data CalendarConfig = CalendarConfig {
  configCalendars :: M.Map Text EventCalendar
} deriving (Show, Eq)

instance ToJSON CalendarConfig where
  toJSON config = toJSON $ M.assocs $ configCalendars config

instance FromJSON CalendarConfig where
  parseJSON v@(Array _) = do
    calendars' <- parseJSONList v :: Parser [(Text, EventCalendar)]
    let calendars'' = M.fromList calendars'
    return $ CalendarConfig calendars''
  parseJSON v = typeMismatch "calendar" v

class HasCalendarConfig a where
  getCalendarConfig :: a -> CalendarConfig

instance HasCalendarConfig CalendarConfig where
  getCalendarConfig = id

-- | Get a named calendar from an environment.
--   Throws an error if the named calendar is not found
getNamedCalendar :: (MonadReader env m, HasCalendarConfig env) => Text -> m EventCalendar
getNamedCalendar name = do
  env <- ask
  let cal = (configCalendars $ getCalendarConfig env) M.! name
  return cal
   
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
  | NthDayAfter Integer EventCalendar -- ^ Eg 27 days before Easter
  | NthWeekday WeekOfMonth DayOfWeek -- ^ Eg first Tuesday of the month with 5 = last
  | NthWeekdayAfter Integer DayOfWeek EventCalendar -- ^ Eg 3rd Sunday after Easter, negative for before
  | ListCalendar (S.Set Day) -- ^ A specific list of Julian Days
  | NamedCalendar Text -- ^ A calendar with a specific name (eg Easter) that is supplied by a calendar configuration
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
  NamedCalendar name1 == NamedCalendar name2 = name1 == name2
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
    , "days" .= days'
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
    , "dates" .= S.map toGregorian dates'
    ]
  toJSON (NamedCalendar name') = object [
      "type" .= ("named" :: Text)
    , "name" .= name'
    ]
  toJSON (Conditional calendar' condition') = object [
      "type" .= ("conditional" :: Text)
    , "calendar" .= calendar'
    , "condition" .= condition'
    ]
  
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
        return $ DayOfYear days'
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
        return $ ListCalendar (S.fromList $ map (\(y', m', d') -> fromGregorian y' m' d') dates')
      "named" -> do
        name' <- v .: "name"
        return $ NamedCalendar name'
      "conditional" -> do
        calendar' <- v .: "calendar"
        condition' <- v .: "condition"
        return $ Conditional calendar' condition'
      invalid' -> typeMismatch "expecting event type" invalid'
  parseJSON v = typeMismatch "expecting object" v

-- Get the closest calendar date before the given date
calendarDateOnOrBefore :: (MonadReader env m, HasCalendarConfig env) => EventCalendar -- ^ The calendar to test against
  -> Day -- ^ The day to use
  -> m Day -- ^ The closest calendar date
calendarDateOnOrBefore Daily day = return day
calendarDateOnOrBefore c@(Weekly dow) day = calendarDateOnOrBefore'' c $ maximum (S.map (\d -> firstDayOfWeekOnAfter d day') dow) where day' = addDays (-6) day
calendarDateOnOrBefore c@(Monthly dom) day = do
    let g@(_year', _month', day') = toGregorian day
    let befores = S.filter (<= day') dom
    let afters = S.filter (> day') dom
    let prevMonth = S.null befores
    let (year'', month'', _) = if prevMonth then toGregorian (addGregorianMonthsRollOver (-1) day) else g
    let before = if prevMonth then maximum afters else maximum befores
    calendarDateOnOrBefore'' c $ fromGregorian year'' month'' before
calendarDateOnOrBefore c@(Yearly moy) day = do
    let (year', month', _day') = toGregorian day
    let befores = S.filter (<= month') moy
    let afters = S.filter (> month') moy
    let prevYear = S.null befores
    let before = if prevYear then maximum afters else maximum befores
    let year'' = if prevYear then year' - 1 else year'
    calendarDateOnOrBefore'' c $ fromGregorian year'' before 1
calendarDateOnOrBefore c@(DayOfYear dates) day = do
    let (year', month', day') = toGregorian day
    let befores = S.filter (\(m', d') -> m' < month' || (m' == month' && d' <= day')) dates
    let afters = S.filter (\(m', d') -> m' > month' || (m' == month' && d' > day')) dates
    let prevYear = S.null befores
    let (beforem, befored) = if prevYear then maximum afters else maximum befores
    calendarDateOnOrBefore'' c $ fromGregorian (if prevYear then year' - 1 else year') beforem befored
calendarDateOnOrBefore (RangeCalendar rfrom _rto) day = calendarDateOnOrBefore rfrom day
calendarDateOnOrBefore c@(UnionCalendar calendars) day = do
  days <- mapM (\cc -> calendarDateOnOrBefore cc day) calendars
  calendarDateOnOrBefore'' c $ maximum days
calendarDateOnOrBefore c@(IntersectionCalendar _calendars) day = do
  before <- calendarDateOnOrBefore' c day
  calendarDateOnOrBefore'' c before
calendarDateOnOrBefore c@(InvertedCalendar _calendar) day = calendarDateOnOrBefore' c day
calendarDateOnOrBefore c@(NthDayAfter days calendar) day = do
 before <- (calendarDateOnOrBefore calendar (addDays (negate days) day))
 calendarDateOnOrBefore'' c (addDays days before)
calendarDateOnOrBefore (NthWeekday nth dow) day = return $ if nth' <= day then nth' else nth''
  where
    nth' = nthWeekOnOf nth dow day
    nth'' = nthWeekOnOf nth dow (addGregorianMonthsRollOver (-1) day)
calendarDateOnOrBefore c@(NthWeekdayAfter _nth _dow _calendar) day = calendarDateOnOrBefore' c day
calendarDateOnOrBefore c@(ListCalendar dates) day = calendarDateOnOrBefore'' c $ if S.null befores then day else maximum befores where befores = S.filter (<= day) dates
calendarDateOnOrBefore (NamedCalendar name) day = do
  calendar <- getNamedCalendar name
  calendarDateOnOrBefore calendar day
calendarDateOnOrBefore (Conditional calendar _note) day = calendarDateOnOrBefore calendar day

-- Simple version for when we just have to search backwards
calendarDateOnOrBefore' calendar day = do
  incal <- inCalendar calendar day
  if incal then
    return day
  else
    calendarDateOnOrBefore' calendar (addDays (-1) day)

-- Wind back until we reach the start of a sequence
calendarDateOnOrBefore'' calendar day = do
  let prev = addDays (-1) day
  incal <- inCalendar calendar prev
  if not incal then
    return day
  else
    calendarDateOnOrBefore'' calendar prev

-- Get the closest calendar date after the given date
calendarDateOnOrAfter :: (MonadReader env m, HasCalendarConfig env) => EventCalendar -- ^ The calendar to test against
  -> Day -- ^ The day to use
  -> m Day -- ^ The closest calendar date
calendarDateOnOrAfter Daily day = return day
calendarDateOnOrAfter c@(Weekly dow) day = calendarDateOnOrAfter'' c $ minimum (S.map (\d -> firstDayOfWeekOnAfter d day) dow)
calendarDateOnOrAfter c@(Monthly dom) day = do
    let g@(_year', _month', day') = toGregorian day
    let befores = S.filter (< day') dom
    let afters = S.filter (>= day') dom
    let nextMonth = S.null afters
    let (year'', month'', _) = if nextMonth then toGregorian (addGregorianMonthsRollOver 1 day) else g
    let after = if nextMonth then minimum befores else minimum afters
    calendarDateOnOrAfter'' c $ fromGregorian year'' month'' after
calendarDateOnOrAfter c@(Yearly moy) day = do
    let (year', month', _day') = toGregorian day
    let befores = S.filter (< month') moy
    let afters = S.filter (>= month') moy
    let nextYear = S.null afters
    let after = if nextYear then minimum befores else minimum afters
    let year'' = if nextYear then year' + 1 else year'
    let leapYeap = isLeapYear year''
    let lastDay = monthLength leapYeap after
    calendarDateOnOrAfter'' c $ fromGregorian year'' after lastDay
calendarDateOnOrAfter c@(DayOfYear dates) day = do
    let (year', month', day') = toGregorian day
    let befores = S.filter (\(m', d') -> m' < month' || (m' == month' && d' < day')) dates
    let afters = S.filter (\(m', d') -> m' > month' || (m' == month' && d' >= day')) dates
    let nextYear = S.null afters
    let (afterm, afterd) = if nextYear then minimum befores else minimum afters
    calendarDateOnOrAfter'' c $ fromGregorian (if nextYear then year' + 1 else year') afterm afterd
calendarDateOnOrAfter (RangeCalendar rfrom rto) day = do
  from' <- calendarDateOnOrAfter rfrom day
  to' <- calendarDateOnOrAfter rto from'
  return $ min from' to'
calendarDateOnOrAfter c@(UnionCalendar calendars) day = do
  days <- mapM (\cc -> calendarDateOnOrAfter cc day) calendars
  calendarDateOnOrAfter'' c $ minimum days
calendarDateOnOrAfter c@(IntersectionCalendar _calendars) day = do
  after <- calendarDateOnOrAfter' c day
  calendarDateOnOrAfter'' c after
calendarDateOnOrAfter c@(InvertedCalendar _calendar) day = calendarDateOnOrAfter' c day
calendarDateOnOrAfter c@(NthDayAfter days calendar) day = do
  after1 <- calendarDateOnOrAfter calendar (addDays (negate days) day)
  after2 <- calendarDateOnOrAfter calendar day
  let after1' = addDays days after1
  let after2' = addDays days after2
  calendarDateOnOrAfter'' c $ if after2' <= after1' then after2' else after1'
calendarDateOnOrAfter (NthWeekday nth dow) day = return $ if nth' >= day then nth' else nth''
  where
    nth' = nthWeekOnOf nth dow day
    nth'' = nthWeekOnOf nth dow (addGregorianMonthsRollOver 1 day)
calendarDateOnOrAfter c@(NthWeekdayAfter _nth _dow _calendar) day = calendarDateOnOrAfter' c day
calendarDateOnOrAfter c@(ListCalendar dates) day = calendarDateOnOrAfter'' c $ if S.null afters then day else minimum afters where afters = S.filter (>= day) dates
calendarDateOnOrAfter (NamedCalendar name) day = do
  calendar <- getNamedCalendar name
  calendarDateOnOrAfter calendar day
calendarDateOnOrAfter (Conditional calendar _note) day = calendarDateOnOrAfter calendar day


-- Simple version for when we just have to search forwards
calendarDateOnOrAfter' calendar day = do
  incal <- inCalendar calendar day
  if incal then
    return day
  else
    calendarDateOnOrAfter' calendar (addDays 1 day)

-- Wind forward until we reach the end of a sequence
calendarDateOnOrAfter'' calendar day = do
  let next = addDays 1 day
  incal <- inCalendar calendar next
  if not incal then
    return day
  else
    calendarDateOnOrAfter'' calendar next

-- | See if a particular day is in a calendar
inCalendar :: (MonadReader env m, HasCalendarConfig env) => EventCalendar -- ^ The calendar to test against
  -> Day -- ^ The day to test 
  -> m Bool -- ^ True of the day fits the calendar
inCalendar Daily _ = return True
inCalendar (Weekly dow) day = return $ S.member (dayOfWeek day) dow
inCalendar (Monthly dom) day = return $ S.member ((\(_, _, d) -> d) $ toGregorian day) dom
inCalendar (Yearly moy) day = return $ S.member ((\(_, m, _) -> m) $ toGregorian day) moy
inCalendar (DayOfYear dates) day = return $ elem ((\(_, m, d) -> (m, d)) $ toGregorian day) dates
inCalendar (RangeCalendar rfrom rto) day = do
  from' <- calendarDateOnOrBefore rfrom day
  to' <- calendarDateOnOrAfter rto from'
  return $ from' <= day && day <= to'
inCalendar (UnionCalendar calendars) day = do
  matches <- mapM (\c -> inCalendar c day) calendars
  return $ any id matches
inCalendar (IntersectionCalendar calendars) day = do
  matches <- mapM (\c -> inCalendar c day) calendars
  return $ all id matches            
inCalendar (InvertedCalendar calendar) day = do
  incal <- inCalendar calendar day 
  return $ not incal
inCalendar (NthDayAfter days calendar) day = inCalendar calendar (addDays (negate days) day)
inCalendar (NthWeekday nth dow) day = return $ dow == dow' && day == nth'
  where
    dow' = dayOfWeek day
    nth' = nthWeekOnOf nth dow day
inCalendar (NthWeekdayAfter nth dow calendar) day = do
  nd <- mapM (\os -> inCalendar calendar (addDays ((-7) * nth + os) day)) [0..6]
  return $ dayOfWeek day == dow && any id nd
inCalendar (ListCalendar dates) day = return $ S.member day dates
inCalendar (NamedCalendar name) day = do
  calendar <- getNamedCalendar name
  inCalendar calendar day
inCalendar (Conditional calendar _note) day = inCalendar calendar day

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

