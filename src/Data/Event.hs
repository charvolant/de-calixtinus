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
  , EventTime(..)
) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Localised
import qualified Data.Set as S
import Data.Text (Text, intercalate, pack, splitOn, unpack)
import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime

-- | A calendar for an event.
--   The calendar is intended to allow the building of 
data EventCalendar =
    Daily -- ^ Occurs daily
  | Weekly (S.Set DayOfWeek)  -- ^ Occurs on certain days of the week
  | Monthly (S.Set DayOfMonth)  -- ^ Occurs on certain days of the month
  | Yearly (S.Set MonthOfYear) -- ^ Occurs on certain months of the year
  | Conditional EventCalendar (Localised TaggedText) -- ^ Occurs at complex times specified by a note
  deriving (Show)
  
instance Eq EventCalendar where
  Daily == Daily = True
  Weekly dow1 == Weekly dow2 = dow1 == dow2
  Monthly dom1 == Monthly dom2 = dom1 == dom2
  Yearly moy1 == Yearly moy2 = moy1 == moy2
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
      
-- | The times an event can occur during a day  
data EventTime = EventTime [(TimeOfDay, TimeOfDay)]
  deriving (Show, Eq)

instance ToJSON EventTime where
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
    let times' = splitOn "," v
    let times'' = map parseTimeRange times'
    return $ EventTime times''
  parseJSON v = typeMismatch "expecting string" v
    