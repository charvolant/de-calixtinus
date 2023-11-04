{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Camino.Camino
import Camino.Preferences
import Graph.Graph
import Camino.Planner
import Camino.Display.Css
import Camino.Display.KML
import Camino.Display.Html
import Camino.Display.Routes
import Camino.Config
import qualified Data.Set as S
import Options.Applicative
import Data.List.Split
import Text.Cassius
import Text.XML
import Data.Text.Lazy as T (concat)
import Data.Text.Lazy.IO as TIO (writeFile)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import System.FilePath
import System.Directory

data Plan = Plan {
  camino :: String,
  preferences :: String,
  begin :: String,
  end :: String,
  require :: String,
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
    <*> (strOption (long "require" <> short 'r' <> value "" <> metavar "LOCATIONIDS" <> help "Require stopping at a comma-separated list of location ids"))
    <*> (strOption (long "exclude" <> short 'x' <> value "" <> metavar "LOCATIONIDS" <> help "Exclude stopping at a comma-separated list of location ids"))
    <*> (strOption (long "config" <> short 'c' <> value "./config.yaml" <> metavar "CONFIG" <> help "Configuration file"))
    <*> (strOption (long "output" <> short 'o' <> value "./plan" <> metavar "OUTPUTDIR" <> help "Output directory"))

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
    config' <- readConfigFile (config opts)
    let output' = output opts
    let begin' = vertex camino' (begin opts)
    let end' = vertex camino' (end opts)
    let required' = if require opts == "" then S.empty else S.fromList $ map placeholderLocation (splitOn "," (require opts))
    let excluded' = if exclude opts == "" then S.empty else S.fromList $ map placeholderLocation (splitOn "," (exclude opts))
    let preferences'' = normalisePreferences camino' (preferences' { preferenceRequired = required', preferenceExcluded = excluded' })
    let renderer = renderCaminoRoute config'
    let solution = planCamino preferences'' camino' begin' end'
    createDirectoryIfMissing True output'
    let kml = createCaminoDoc config' preferences'' camino' solution
    let kmlFile = output' </> "camino.kml"
    B.writeFile kmlFile (renderLBS (def { rsPretty = True }) kml)
    let html = caminoHtml config' preferences'' camino' solution
    let indexFile = output' </> "index.html"
    B.writeFile indexFile (renderHtml (html renderer))
    let css = caminoCss config' camino'
    let cssFile = output' </> "camino.css"
    TIO.writeFile cssFile $ T.concat (map (\c -> renderCss $ c renderer) css)

main :: IO ()
main = do
    opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "Plan a camino")
    plan opts