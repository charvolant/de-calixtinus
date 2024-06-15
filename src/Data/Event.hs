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
    EventCalendar(..)
) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Localised
import qualified Data.Set as S
import Data.Text
import Data.Time.Calendar
import Data.Time.LocalTime

-- | A calendar for an event.
--   The calendar is intended to allow the building of 
data EventCalendar =
    Daily -- ^ Occurs daily
  | Weekly (S.Set DayOfWeek)  -- ^ Occurs on certain days of the week
  | Monthly (S.Set DayOfMonth)  -- ^ Occurs on certain days of the month
  | Yearly (S.Set MonthOfYear) -- ^ Occurs on certain months of the year
  | Conditional EventCalendar (Localised TaggedText) -- ^ Occurs at complex times specified by a note

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
      "conditional" -> do
        calendar' <- v .: "calendar"
        condition' <- v .: "condition"
        return $ Conditional calendar' condition'
      invalid' -> typeMismatch "expecting event type" invalid'
  parseJSON v = typeMismatch "expecting object" v
  
data EventTime = EventTime TimeOfDay TimeOfDay
