{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Data.Event.Date
Description : Calculate dates according to calendars
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Find the nearest date in a calendar for a specific date
-}
module Data.Event.Date (
    calendarDateOnOrAfter
  , calendarDateOnOrBefore
  , inCalendar
  , nthWeekOnOf
) where

import Control.Monad.Reader
import Data.Event
import Data.Localised
import Data.Region
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Calendar.MonthDay


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


-- Get the closest calendar date before the given date
calendarDateOnOrBefore :: (MonadReader env m, HasCalendarConfig env, HasRegionConfig env) => EventCalendar -- ^ The calendar to test against
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
 before <- (calendarDateOnOrBefore calendar (addDays (toInteger $ negate days) day))
 calendarDateOnOrBefore'' c (addDays (toInteger days) before)
calendarDateOnOrBefore (NthWeekday nth dow) day = return $ if nth' <= day then nth' else nth''
  where
    nth' = nthWeekOnOf nth dow day
    nth'' = nthWeekOnOf nth dow (addGregorianMonthsRollOver (-1) day)
calendarDateOnOrBefore c@(NthWeekdayAfter _nth _dow _calendar) day = calendarDateOnOrBefore' c day
calendarDateOnOrBefore c@(ListCalendar dates) day = calendarDateOnOrBefore'' c $ if S.null befores then day else maximum befores where befores = S.filter (<= day) dates
calendarDateOnOrBefore (NamedCalendar name) day = do
  calendar <- getNamedCalendar name
  calendarDateOnOrBefore calendar day
calendarDateOnOrBefore (PublicHoliday region) day = do
  region' <- getRegion region
  calendarDateOnOrBefore (getRegionalHolidays region') day
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
calendarDateOnOrAfter :: (MonadReader env m, HasCalendarConfig env, HasRegionConfig env) => EventCalendar -- ^ The calendar to test against
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
  after1 <- calendarDateOnOrAfter calendar (addDays (toInteger $ negate days) day)
  after2 <- calendarDateOnOrAfter calendar day
  let days' = toInteger days
  let after1' = addDays days' after1
  let after2' = addDays days' after2
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
calendarDateOnOrAfter (PublicHoliday region) day = do
  region' <- getRegion region
  calendarDateOnOrAfter (getRegionalHolidays region') day
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
inCalendar :: (MonadReader env m, HasCalendarConfig env, HasRegionConfig env) => EventCalendar -- ^ The calendar to test against
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
inCalendar (NthDayAfter days calendar) day = inCalendar calendar (addDays (toInteger $ negate days) day)
inCalendar (NthWeekday nth dow) day = return $ dow == dow' && day == nth'
  where
    dow' = dayOfWeek day
    nth' = nthWeekOnOf nth dow day
inCalendar (NthWeekdayAfter nth dow calendar) day = do
  nd <- mapM (\os -> inCalendar calendar (addDays (toInteger $ (-7) * nth + os) day)) [0..6]
  return $ dayOfWeek day == dow && any id nd
inCalendar (ListCalendar dates) day = return $ S.member day dates
inCalendar (NamedCalendar name) day = do
  calendar <- getNamedCalendar name
  inCalendar calendar day
inCalendar (PublicHoliday region) day = do
  region' <- getRegion region
  inCalendar (getRegionalHolidays region') day
inCalendar (Conditional calendar _note) day = inCalendar calendar day


