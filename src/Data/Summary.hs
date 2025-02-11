{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
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
) where

import Data.Set (Set, toList)
import Data.Text (Text, intercalate, unpack)

class Summary a where
  -- | Generate a summary of a.
  --   A summary is generally something that can be easily printed for debugging without causing over-sharing
  summary :: a -> Text
  -- | Generate a string-like summary
  summaryString :: a -> String
  summaryString v = unpack $ summary v

instance Summary Text where
  summary v = v

instance (Summary a) => Summary [a] where
  summary v = "[" <> intercalate ", " (map summary v) <> "]"

instance (Summary a) => Summary (Set a) where
  summary v = "{" <> intercalate ", " (map summary (toList v)) <> "}"

instance (Summary a) => Summary (Maybe a) where
  summary Nothing = "--"
  summary (Just v) = summary v

instance (Summary a, Summary b) => Summary (a, b) where
  summary (x, y) = "(" <> summary x <> ", " <> summary y <> ")"

instance (Summary a, Summary b, Summary c) => Summary (a, b, c) where
  summary (x, y, z) = "(" <> summary x <> ", " <> summary y <> ", " <> summary z <> ")"
