{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE InstanceSigs #-}
{-|
Module      : Attributes
Description : Aribtary collections of data, possibly with a schema
Copyright   : (c) Doug Palmer, 2026
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Formatted data that can be either completely arbitrary or modelled by a schema
-}
module Data.Attributes (
    Attributes(..)
  , AttributeName(..)
  , AttributeValue(..)
  -- * Construction
  , emptyAttributes
  -- * Query
  , hasAttribute
  , lookupAttribute
  -- * Update
  , addAttribute
  -- * Display
  , valueToText
  , attributesToTextList
) where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (unexpected)
import qualified Data.Time.Calendar as C
import qualified Data.Time.Clock as CL
import Data.Time.Format.ISO8601
import qualified Data.Time.LocalTime as LT
import qualified Data.Map as M
import Data.Scientific
import Data.String
import Data.Text (Text, intercalate, pack)

-- | An attribute name
newtype AttributeName = AttributeName { unName :: Text } deriving (Eq, Ord, Show, Generic)

instance FromJSON AttributeName

instance ToJSON AttributeName

instance FromJSONKey AttributeName

instance ToJSONKey AttributeName

instance NFData AttributeName

instance IsString AttributeName where
  fromString s = AttributeName $ pack s

-- | The value of an attribute
--
--   Dates, times and datetimes require a schema to be correctly interpreted
data AttributeValue =
    NullV -- ^ An empty value
  | BooleanV Bool -- ^ A boolean value
  | IntegerV Integer -- ^ An integer value
  | DecimalV Double -- ^ A floating point value
  | StringV Text -- ^ A String (text)
  | DateV C.Day -- ^ A date
  | TimeV LT.TimeOfDay -- ^ A local time of day
  | DateTimeV CL.UTCTime -- ^ A date-time combination
  | ListV [AttributeValue] -- ^ A list of, potentially mixed, attribute value
  | ObjectV Attributes -- A sub-attribute
  deriving (Eq, Show, Generic)

instance FromJSON AttributeValue where
  parseJSON Null = return NullV
  parseJSON (Bool v) = return $ BooleanV v
  parseJSON (Number v) = return $ either DecimalV IntegerV (floatingOrInteger v)
  parseJSON (String v) = return $ Data.Attributes.StringV v
  parseJSON v@(Array _) = do
    v' <- parseJSONList v
    return $ ListV v'
  parseJSON v@(Object _) = do
    a <- parseJSON v
    return $ ObjectV a

instance ToJSON AttributeValue where
  toJSON :: AttributeValue -> Value
  toJSON NullV = Null
  toJSON (BooleanV v) = Bool v
  toJSON (IntegerV v) = Number $ fromInteger v
  toJSON (DecimalV v) = Number $ fromFloatDigits v
  toJSON (StringV v) = String v
  toJSON (DateV v) = String $ pack $ iso8601Show v
  toJSON (TimeV v) = String $ pack $ iso8601Show v
  toJSON (DateTimeV v) = String $ pack $ iso8601Show v
  toJSON (ListV v) = toJSONList v
  toJSON (ObjectV v) = toJSON v

instance NFData AttributeValue

-- | Convert attribute values to text for display
valueToText :: AttributeValue -> Text
valueToText NullV = ""
valueToText (BooleanV v) = if v then "true" else "false"
valueToText (IntegerV v) = pack $ show v
valueToText (DecimalV v) = pack $ show v
valueToText (StringV v) = v
valueToText (DateV v) = pack $ iso8601Show v
valueToText (TimeV v) = pack $ iso8601Show v
valueToText (DateTimeV v) = pack $ iso8601Show v
valueToText (ListV v) = intercalate ", " $ map valueToText v
valueToText (ObjectV v) = intercalate ", " $ map (\(k, v') -> k <> ": " <> v') $ attributesToTextList v

-- | Attributes, a list of name - value pairs
data Attributes = Attributes (M.Map AttributeName AttributeValue)
  deriving (Eq, Show, Generic)

instance FromJSON Attributes where
  parseJSON (Data.Aeson.Object v) = do
    let ks' = map (AttributeName . K.toText) (KM.keys v)
    vs' <- mapM parseJSON $ KM.elems v
    return $ Attributes $ M.fromList $ zip ks' vs'
  parseJSON v = unexpected v

instance ToJSON Attributes where
  toJSON (Attributes v) = toJSON $ M.mapKeys unName v

instance NFData Attributes

-- | Convert attributes into lists of name, value text pairs, suitable for display
attributesToTextList:: Attributes -> [(Text, Text)]
attributesToTextList (Attributes v) = map (\(k, v) -> (unName k, valueToText v)) $ M.toList v

-- | Is this attrribute present
hasAttribute :: AttributeName -> Attributes -> Bool
hasAttribute n (Attributes attrs) = M.member n attrs

-- | Get an attribute value
lookupAttribute :: Text -> Attributes -> Maybe AttributeValue
lookupAttribute key (Attributes v) = M.lookup (AttributeName key) v

-- | Empty attributes
emptyAttributes = Attributes M.empty

-- | Add an attrribute to an attrribute set
addAttribute ::AttributeName -> AttributeValue -> Attributes -> Attributes
addAttribute n v (Attributes attrs) = Attributes $ M.insert n v attrs