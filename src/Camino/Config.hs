{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Camino.Config where


import Data.Aeson
import Data.Yaml as Y
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Either
import Data.List.Split
import qualified Data.Map as M
import Data.Text (Text, pack, unpack, toUpper, take, null)
import qualified Data.Text.Read as TR
import Data.Aeson.Types (parseMaybe, unexpected)
import qualified Data.ByteString as B (ByteString, readFile)

-- A configuration object, either a value pair or a map from keynames to further config information
data Config = Value Text | Config (M.Map String Config) deriving (Show)

instance FromJSON Config where
  parseJSON (String v) = do
    return $ Value v
  parseJSON (Object v) = do
    return $ Config (M.fromList pairs)
    where
      fj v = let s = fromJSON v in case s of
        Success v' -> v'
        Error e -> error ("Unable to parse " ++ show v ++ " " ++ e)
      kmap = KM.toList v
      pairs = map (\(k, v) -> (K.toString k, fj v :: Config)) kmap
  parseJSON v = unexpected v

instance ToJSON Config where
  toJSON (Value v) = String v
  toJSON (Config vs) = object $ map (\(k, v) -> K.fromString k .= v) (M.toList vs)

readConfig :: B.ByteString -> Config
readConfig source = either (\e -> error $ "Can't read config " ++ show e) id (Y.decodeEither' source)

readConfigFile :: FilePath -> IO Config
readConfigFile path = do
  file <- B.readFile path
  return $ readConfig file

getValue' [] (Value v) = Just v
getValue' (key:keys) (Config vs) = let
    cv = vs M.!? key
  in
    maybe Nothing (getValue' keys) cv
getValue' _ _ = Nothing

getConfigValue :: (Configurable c) => String -> c -> Config -> c
getConfigValue key dflt config = let
    mv = getValue' (splitOn "." key) config
  in
    maybe dflt convert mv

-- | Something that can be read from a configuration
class Configurable c where
  -- Convert a config entry into a value
  convert :: Text -> c

instance Configurable Bool where
  convert v = initial == "T" || initial == "t"
    where
      initial = if Data.Text.null v then "F" else Data.Text.take 1 v

instance Configurable String where
  convert = unpack

instance Configurable Int where
  convert v = either (\e -> error ("Can't convert " <> unpack v <> " to integer: " <> e)) fst (TR.decimal v)

instance Configurable Double where
  convert v = either (\e -> error ("Can't convert " <> unpack v <> " to double: " <> e)) fst (TR.double v)

instance Configurable Text where
  convert = id