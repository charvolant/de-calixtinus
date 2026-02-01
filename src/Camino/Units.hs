{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Units
Description : Simple unit system for the camino
Copyright   : (c) Doug Palmer, 2026
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Enough handling of units and systems of units to allow the planner to do unit conversions.
This is the skeleton of a proper unit system.
-}

module Camino.Units (
    Quantity(..)
  , SystemOfUnits(..)
  , Unit(..)

  , convertAmount
  , preferredUnit
  , symbolUnit
  , systemOfUnitsEnumeration
  , unitSymbol
) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isAlpha, isSpace)

-- | The types of quantities that we wish to deal with
data Quantity =
    Dimensionless -- ^ A quantity without a physical interpretation
  | Distance -- ^ Distance travelled
  | Elevation -- ^ Elevation ascended or descended
  | Time -- ^ Elapsed time
  | Calendar -- ^ Calendar day
  deriving (Generic, Eq, Ord, Enum)

instance NFData Quantity

instance FromJSON Quantity

instance ToJSON Quantity

-- | The systems of units that we wish to deal with
data Unit =
    Unit -- ^ A simple number
  | Metre -- ^ A metre (meter)
  | Kilometre -- ^ A kilometre
  | Mile -- ^ A mile
  | Foot -- ^ A foot
  | Hour -- ^ Time in hours
  | Day -- ^ Time in days
  deriving (Generic, Eq, Ord, Enum)

instance Show Unit where
   showsPrec _d unit s = unitSymbol unit ++ s

instance Read Unit where
  readsPrec _d r = let
    r' = dropWhile isSpace r
    (u, r'') = span isAlpha r'
    eu = symbolUnit u
    r''' = dropWhile isSpace r''
    in
      either (const []) (\u' -> [(u', r''')]) eu

instance NFData Unit

instance FromJSON Unit

instance ToJSON Unit

-- | The symbol for each unit
unitSymbol :: Unit -> String
unitSymbol Unit = ""
unitSymbol Metre = "m"
unitSymbol Kilometre = "km"
unitSymbol Mile = "mi"
unitSymbol Foot = "ft"
unitSymbol Hour = "hr"
unitSymbol Day = "day"

symbolUnit :: String -> Either String Unit
symbolUnit "" = Right Unit
symbolUnit "m" = Right Metre
symbolUnit "km" = Right Kilometre
symbolUnit "mi" = Right Mile
symbolUnit "ft" = Right Foot
symbolUnit "hr" = Right Hour
symbolUnit "day" = Right Day
symbolUnit symbol = Left symbol

-- | Convert an amount between two units 
convertAmount :: (RealFrac a) => Unit -> Unit -> a -> a
convertAmount Unit Unit v = v 
convertAmount Metre Metre v = v 
convertAmount Metre Kilometre v = v / 1000.0 
convertAmount Metre Mile v = v / 1609.344
convertAmount Metre Foot v = v * 3.280839939501
convertAmount Kilometre Metre v = v * 1000.0
convertAmount Kilometre Kilometre v = v  
convertAmount Kilometre Mile v = v / 1.609344
convertAmount Kilometre Foot v = v * 3280.839939501
convertAmount Mile Metre v = v * 1609.344 
convertAmount Mile Kilometre v = v * 1.609344
convertAmount Mile Mile v = v
convertAmount Mile Foot v = v * 5280.0
convertAmount Foot Metre v = v / 3.280839939501
convertAmount Foot Kilometre v = v / 3280.839939501
convertAmount Foot Mile v = v / 5280.0
convertAmount Foot Foot v = v
convertAmount Hour Hour v = v
convertAmount Hour Day v = v / 24.0
convertAmount Day Day v = v
convertAmount Day Hour v = v * 24.0
convertAmount f t _v = error ("Unable to convert unit " ++ show f ++ " to " ++ show t)

-- | a system of units
data SystemOfUnits =
    SIUnits -- ^ International system of units
  | USUnits -- ^ US customary units
    deriving (Generic, Eq, Ord, Enum, Bounded, Show, Read)

instance NFData SystemOfUnits

instance FromJSON SystemOfUnits

instance ToJSON SystemOfUnits

-- | Provide an enumeration of all system of units
systemOfUnitsEnumeration :: [SystemOfUnits]
systemOfUnitsEnumeration = [minBound .. maxBound]

-- | Get the preferred unit for a specific system of units and quantity
preferredUnit :: SystemOfUnits -> Quantity -> Unit
preferredUnit SIUnits Dimensionless = Unit
preferredUnit SIUnits Distance = Kilometre
preferredUnit SIUnits Elevation = Metre
preferredUnit SIUnits Time = Hour
preferredUnit SIUnits Calendar = Day
preferredUnit USUnits Dimensionless = Unit
preferredUnit USUnits Distance = Mile
preferredUnit USUnits Elevation = Foot
preferredUnit USUnits Time = Hour
preferredUnit USUnits Calendar = Day
