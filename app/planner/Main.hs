{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : Generate a camino plan and output it as a collection of static files
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Main (main) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Camino.Camino
import Camino.Preferences
import Graph.Graph
import Camino.Planner
import Camino.Display.KML
import Camino.Display.Html
import Camino.Display.I18n
import Camino.Display.Routes
import Camino.Config
import qualified Data.Set as S
import Options.Applicative
import Data.List.Split
import Text.XML
import qualified Data.Text as ST (unpack)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import System.FilePath
import System.Directory

data Plan = Plan {
  camino :: String,
  preferences :: String,
  begin :: String,
  end :: String,
  routes :: String,
  stops :: String,
  exclude :: String,
  config :: FilePath,
  output :: FilePath
}

arguments :: Parser Plan
arguments =  Plan
    <$> (argument str (metavar "CAMINO-FILE"))
    <*> (argument str (metavar "PREFERENCES-FILE"))
    <*> (argument str (metavar "FROM"))
    <*> (argument str (metavar "TO"))
    <*> (strOption (long "routes" <> short 'r' <> value "" <> metavar "ROUTEIDS" <> help "Use the following comma-separated route variants - the default is always used"))
    <*> (strOption (long "stops" <> short 's' <> value "" <> metavar "LOCATIONIDS" <> help "Require stopping at a comma-separated list of location ids"))
    <*> (strOption (long "exclude" <> short 'x' <> value "" <> metavar "LOCATIONIDS" <> help "Exclude stopping at a comma-separated list of location ids"))
    <*> (strOption (long "config" <> short 'c' <> value "./config.yaml" <> metavar "CONFIG" <> help "Configuration file"))
    <*> (strOption (long "output" <> short 'o' <> value "./plan" <> metavar "OUTPUTDIR" <> help "Output directory"))

readPreferences :: String -> IO TravelPreferences
readPreferences file = do
  cf <- B.readFile file
  let decoded = eitherDecode cf :: Either String TravelPreferences
  return $ case decoded of
    Left msg -> error msg
    Right prefs' -> prefs'

plan :: Plan -> IO ()
plan opts = do
    camino' <- readCamino (camino opts)
    preferences' <- readPreferences (preferences opts)
    config' <- readConfigFile (config opts)
    let output' = output opts
    let begin' = vertex camino' (begin opts)
    let end' = vertex camino' (end opts)
    let routes' = if routes opts == "" then S.empty else S.fromList $ map placeholderRoute (splitOn "," (routes opts))
    let stops' =  if stops opts == "" then S.empty else S.fromList $ map placeholderLocation (splitOn "," (stops opts))
    let excluded' = if exclude opts == "" then S.empty else S.fromList $ map placeholderLocation (splitOn "," (exclude opts))
    let caminoPrefs' = withRoutes (defaultCaminoPreferences camino') routes'
    let caminoPrefs'' = normalisePreferences [camino'] $ caminoPrefs' {
       preferenceStart = begin',
       preferenceFinish = end'
    }
    let caminoPrefs''' = normalisePreferences [camino'] $ caminoPrefs'' {
       preferenceStops = if S.null stops' then recommendedStops caminoPrefs'' else stops',
       preferenceExcluded = excluded'
    }
    let router = renderCaminoRoute config' ["en", ""]
    let messages = renderCaminoMsg config'
    let solution = planCamino preferences' caminoPrefs'''
    let solution' = either (\v -> error ("Unable to find solution, break at " ++ identifier v ++ " " ++ (ST.unpack $ locationName v))) Just solution
    createDirectoryIfMissing True output'
    let kml = createCaminoDoc config' preferences' caminoPrefs''' solution'
    let kmlFile = output' </> "camino.kml"
    B.writeFile kmlFile (renderLBS (def { rsPretty = True, rsUseCDATA = useCDATA }) kml)
    let html = caminoHtml config' preferences' caminoPrefs''' solution'
    let indexFile = output' </> "index.html"
    B.writeFile indexFile (renderHtml (html messages router))

main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Plan a camino")
    plan opts