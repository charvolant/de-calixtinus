{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Summary
Description : Show a summary of complicated information
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

A class that allows summary information, rather than a detailed show dump of a piece of data
-}
module Data.Summary (
    Summary(..)

  , joinSummaries
  , traceSummaryId
) where

import Data.Maybe (catMaybes)
import qualified Data.Set as S (Set, null, toList)
import Data.Text (Text, intercalate, pack, unpack)
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (ISO8601, iso8601Show)
import Debug.Trace (trace)

class Summary a where
  -- | Generate a summary of a.
  --   A summary is generally something that can be easily printed for debugging without causing over-sharing
  summary :: a -> Text
  -- | Generate a string-like summary
  summaryString :: a -> String
  summaryString v = unpack $ summary v
  -- | If a summary is not empty, return it
  --   By default, this just returns the summary.
  summaryIfNotNull :: a -> Maybe Text
  summaryIfNotNull v = Just $ summary v
  -- | If a summary is not empty, add a label
  labelledSummaryIfNotNull :: Text -> a -> Maybe Text
  labelledSummaryIfNotNull l v = (\sv -> l <> "=" <> sv) <$> summaryIfNotNull v

instance Summary Text where
  summary v = v

instance (Summary a) => Summary [a] where
  summary v = "[" <> intercalate ", " (map summary v) <> "]"
  summaryIfNotNull v = if null v then Nothing else Just $ summary v

instance (Summary a) => Summary (S.Set a) where
  summary v = "{" <> intercalate ", " (map summary (S.toList v)) <> "}"
  summaryIfNotNull v = if S.null v then Nothing else Just $ summary v

instance (Summary a) => Summary (Maybe a) where
  summary Nothing = "-"
  summary (Just v) = summary v
  summaryIfNotNull v = summary <$> v

instance (Summary a, Summary b) => Summary (a, b) where
  summary (x, y) = "(" <> summary x <> ", " <> summary y <> ")"

instance (Summary a, Summary b, Summary c) => Summary (a, b, c) where
  summary (x, y, z) = "(" <> summary x <> ", " <> summary y <> ", " <> summary z <> ")"

instance Summary Float where
  summary n = pack $ show n
  summaryIfNotNull v = if v == 0.0 then Nothing else Just $ summary v

instance Summary Int where
  summary n = pack $ show n
  summaryIfNotNull v = if v == 0 then Nothing else Just $ summary v

instance Summary Bool where
  summary False = "F"
  summary True = "T"
  summaryIfNotNull False = Nothing
  summaryIfNotNull True = Just "T"

instance Summary Day where
  summary d = pack $ iso8601Show d

joinSummaries :: [Maybe Text] -> Text
joinSummaries ss = "<" <> intercalate ", " (catMaybes ss) <> ">"

-- | Create a trace out of a summarisable object
traceSummaryId :: (Summary a) => a -> a
traceSummaryId v = trace (summaryString v) v