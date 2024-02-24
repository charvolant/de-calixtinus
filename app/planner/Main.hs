{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Camino.Camino
import Camino.Preferences
import Graph.Graph
import Camino.Planner
import Camino.Display.Css
import Camino.Display.KML
import Camino.Display.Html
import Camino.Display.I18n
import Camino.Display.Routes
import Camino.Config
import Control.Monad (when)
import qualified Data.Set as S
import Options.Applicative
import Data.List.Split
import Text.Cassius
import Text.Hamlet
import Text.XML
import qualified Data.Text as ST (Text, unpack)
import qualified Data.Text.Lazy as T (concat)
import qualified Data.Text.Lazy.IO as TIO (writeFile)
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
  output :: FilePath,
  static :: Bool
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
    <*> (switch (long "static" <> short 'z' <> help "Generate static files"))

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

createCss :: Config -> Camino -> (CaminoRoute -> [(ST.Text, ST.Text)] -> ST.Text) -> String -> IO ()
createCss config camino router output = let
    css = caminoCss config camino
    cssFile = output </> "camino.css"
  in
    TIO.writeFile cssFile $ T.concat (map (\c -> renderCss $ c router) css)

createHelp :: Config -> (CaminoRoute -> [(ST.Text, ST.Text)] -> ST.Text) -> (CaminoMsg -> Html) -> String -> IO ()
createHelp config router messages output = let
    help = helpHtml config
    helpFile = output </> "help-en.html"
  in
    B.writeFile helpFile (renderHtml (help messages router))

plan :: Plan -> IO ()
plan opts = do
    camino' <- readCamino (camino opts)
    preferences' <- readPreferences (preferences opts)
    let preferences'' = normalisePreferences camino' preferences'
    config' <- readConfigFile (config opts)
    let output' = output opts
    let begin' = vertex camino' (begin opts)
    let end' = vertex camino' (end opts)
    let routes' = if routes opts == "" then S.empty else S.fromList $ map placeholderRoute (splitOn "," (routes opts))
    let preferences''' = normalisePreferences camino' (preferences'' { preferenceRoutes = routes' })
    let stops' = if stops opts == "" then (recommendedStops preferences''' camino') else S.fromList $ map placeholderLocation (splitOn "," (stops opts))
    let excluded' = if exclude opts == "" then S.empty else S.fromList $ map placeholderLocation (splitOn "," (exclude opts))
    let preferences'''' = normalisePreferences camino' (preferences''' { preferenceRoutes = routes', preferenceStops = stops', preferenceExcluded = excluded' })
    let static' = static opts
    let router = renderCaminoRoute config' ["en", ""]
    let messages = renderCaminoMsg config'
    let solution = planCamino preferences'''' camino' begin' end'
    let solution' = either (\v -> error ("Unable to find solution, break at " ++ identifier v ++ " " ++ (ST.unpack $ locationName v))) Just solution
    createDirectoryIfMissing True output'
    let kml = createCaminoDoc config' preferences'''' camino' solution'
    let kmlFile = output' </> "camino.kml"
    B.writeFile kmlFile (renderLBS (def { rsPretty = True }) kml)
    let html = caminoHtml config' preferences'''' camino' solution'
    let indexFile = output' </> "index.html"
    B.writeFile indexFile (renderHtml (html messages router))
    when static' (createCss config' camino' router output')
    when static' (createHelp config' router messages output')
    let css = caminoCss config' camino'
    let cssFile = output' </> "camino.css"
    TIO.writeFile cssFile $ T.concat (map (\c -> renderCss $ c router) css)

main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Plan a camino")
    plan opts