{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Region
Description : Modelling for geographical regions
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Simple regional information, including locales and holidays
-}
module Data.Region (
    HasRegionConfig(..)
  , Region(..)
  , RegionConfig(..)
  , RegionType(..)

  , createRegionConfig
  , getRegionalHolidays
  , getRegion
) where

import GHC.Generics
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Description
import Data.Event
import Data.Localised
import qualified Data.Map as M
import Data.Placeholder
import qualified Data.Set as S
import Data.Text (Text)

-- | Broad region types, which help to classify regions and check for anomalies
data RegionType =
    Planet -- ^ The entire world
  | Continent -- A land continent
  | Ocean -- An ocean
  | Terrestrial -- A large terrestrial region (eg. Western Europe)
  | Marine -- A large marine region (eg. Humboldt Current)
  | Country -- ^ A country (eg. Portugal)
  | Sea -- ^ A sea or smaller marine resion (eg. South China Sea)
  | Province -- ^ A sub-national division (eg. Galacia)
  | Island -- ^ An island or group of islands (eg. Macquarie Island)
  | OtherRegion -- ^ None of the above
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON RegionType
instance ToJSON RegionType

data Region = Region {
    regionID :: Text -- ^ The region identifier
  , regionName :: Localised TaggedText -- ^ The region name, allowing language-specific variations
  , regionType :: RegionType
  , regionDescription :: Maybe Description
  , regionParent :: Maybe Region -- ^ The primary parent region, if there is one
  , regionMember :: S.Set Region -- ^ Other regions that this region is a member of.
  , regionLocale :: Maybe Locale -- ^ The locale information for the region
  , regionHolidays :: [EventCalendar] -- ^ Local holidays
} deriving (Show)

instance Eq Region where
  r1 == r2 = regionID r1 == regionID r2

instance Ord Region where
  r1 `compare` r2 = regionID r1 `compare` regionID r2

instance Placeholder Text Region where
  placeholderID = regionID
  placeholder id' = Region {
      regionID = id'
    , regionName = wildcardText ("Placedholder for " <> id')
    , regionType = OtherRegion
    , regionDescription = Nothing
    , regionParent = Nothing
    , regionMember = S.empty
    , regionLocale = Nothing
    , regionHolidays = []
    }
  internalReferences region = maybe S.empty S.singleton (regionParent region) `S.union` regionMember region

instance Normaliser Text Region (ReferenceMap Text Region) where
  normalise references region = region {
      regionParent = fmap finder (regionParent region)
    , regionMember = S.map finder (regionMember region)
    }
    where
      finder item = maybe item id (M.lookup (placeholderID item) references)
 
instance ToJSON Region where
  toJSON (Region id' name' type' description' parent' member' locale' holidays') = object [
      "id" .= id'
    , "name" .= name'
    , "type" .= type'
    , "description" .= description'
    , "parent" .= (regionID <$> parent')
    , "member" .= member'
    , "locale" .= (localeID <$> locale')
    , "holidays" .= holidays'
    ]

instance FromJSON Region where
  parseJSON (Object v) = do
    id' <- v .: "id"
    name' <- v .: "name"
    type' <- v .: "type"
    description' <- v .:? "description"
    parent' <- v .:? "parent"
    let parent'' = placeholder <$> parent'
    member' <- v .:? "member" .!= S.empty
    let member'' = S.map placeholder member'
    locale' <- v .:? "locale"
    let locale'' = localeFromIDOrError <$> locale'
    holidays' <- v .:? "holidays" .!= []
    return $ Region id' name' type' description' parent'' member'' locale'' holidays'
  parseJSON v = typeMismatch "object" v

-- | Get all the holidays associated with a region, working up through the region hierarchy for parent holidays
getRegionalHolidays :: Region -> EventCalendar
getRegionalHolidays region = UnionCalendar $ getRegionalHolidays' region

getRegionalHolidays' :: Region -> [EventCalendar]
getRegionalHolidays' region = regionHolidays region ++ (maybe [] getRegionalHolidays' $ regionParent region)

-- | A region configuration, allowing regions to be looked up easily
data RegionConfig = RegionConfig {
    regionConfigRegions :: [Region]
  , regionConfigMap :: M.Map Text Region
} deriving (Show)

instance ToJSON RegionConfig where
  toJSON (RegionConfig regions' _) = toJSON regions'

instance FromJSON RegionConfig where
  parseJSON v@(Array _) = do
    regions' <- parseJSONList v :: Parser [Region]
    return $ createRegionConfig regions'
  parseJSON v = typeMismatch "array" v


class HasRegionConfig a where
  getRegionConfig :: a -> RegionConfig

instance HasRegionConfig RegionConfig where
  getRegionConfig = id
    
-- | Get a region from an environment.
--   Throws an error if the region id is not found
getRegion :: (MonadReader env m, HasRegionConfig env) => Text -> m Region
getRegion key = do
  env <- ask
  let mregion = M.lookup key (regionConfigMap $ getRegionConfig env)
  let region = maybe (error ("Can't find region with key " ++ show key)) id mregion
  return region

-- | Create a region config from a set of regions, normalising as we go
createRegionConfig :: [Region] -> RegionConfig
createRegionConfig regions = RegionConfig regions' (M.fromList $ map (\r -> (placeholderID r, r)) regions')
  where
    regions' = normaliseReferences regions
