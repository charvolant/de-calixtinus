{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Camino.Camino
import Graph.Graph
import Camino.Planner
import Camino.KML
import Data.Text.Lazy (unpack)
import qualified Data.Set as S
import Options.Applicative
import Data.List.Split
import Text.XML

data Plan = Plan {
  camino :: String,
  preferences :: String,
  begin :: String,
  end :: String,
  require :: String,
  exclude :: String
}

arguments :: Parser Plan
arguments =  Plan
    <$> (argument str (metavar "CAMINO-FILE"))
    <*> (argument str (metavar "PREFERENCES-FILE"))
    <*> (argument str (metavar "FROM"))
    <*> (argument str (metavar "TO"))
    <*> (strOption (long "require" <> short 'r' <> value "" <> metavar "LOCATIONIDS" <> help "Require stopping at a comma-separated list of location ids"))
    <*> (strOption (long "exclude" <> short 'x' <> value "" <> metavar "LOCATIONIDS" <> help "Exclude stopping at a comma-separated list of location ids"))

readCamino :: String -> IO Camino
readCamino file = do
  cf <- B.readFile file
  let decoded = eitherDecode cf :: Either String Camino
  return $ case decoded of
    Left msg -> error msg
    Right camino' -> camino'

readPreferences :: String -> IO Preferences
readPreferences file = do
  cf <- B.readFile file
  let decoded = eitherDecode cf :: Either String Preferences
  return $ case decoded of
    Left msg -> error msg
    Right prefs' -> prefs'

plan :: Plan -> IO ()
plan opts = do
    camino' <- readCamino (camino opts)
    preferences' <- readPreferences (preferences opts)
    let begin' = vertex camino' (begin opts)
    let end' = vertex camino' (end opts)
    let required' = if require opts == "" then S.empty else S.fromList $ map placeholderLocation (splitOn "," (require opts))
    let excluded' = if exclude opts == "" then S.empty else S.fromList $ map placeholderLocation (splitOn "," (exclude opts))
    let preferences'' = normalisePreferences camino' (preferences' { preferenceRequired = required', preferenceExcluded = excluded' })
    print preferences''
    let solution = planCamino preferences'' camino' begin' end'
    putStr $ unpack $ showTrip preferences'' camino' solution
    let kml = createCaminoDoc preferences' camino' solution
    B.putStr $ renderLBS (def { rsPretty = True }) kml
    
main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Plan a camino")
    plan opts