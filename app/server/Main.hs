{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : De Calixtinus web server
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
import Data.Event
import Data.Region
import Data.Text (pack)
import Camino.Camino
import Camino.Config
import Camino.Server.Application
import Camino.Server.Foundation
import Network.HTTP.Simple()
import Options.Applicative
import Yesod.Static

data Server = Server {
    config :: FilePath
  , staticDir :: FilePath
  , featureDir :: FilePath
  , imageDir :: FilePath
  , devel :: Bool
  , root :: String
  , port :: Int
}

arguments :: Parser Server
arguments = Server
    <$> strOption (long "config" <> short 'c' <> value "./config.yaml" <> metavar "CONFIG-FILE" <> showDefault <> help "Configuration file")
    <*> strOption (long "static" <> short 's' <> value "./static" <> metavar "DIR" <> showDefault <> help "The directory holding static files")
    <*> strOption (long "feature" <> short 'f' <> value "./features" <> metavar "DIR" <> showDefault <> help "The directory holding feature data")
    <*> strOption (long "image" <> short 'i' <> value "./images" <> metavar "DIR" <> showDefault <> help "The directory holding images")
    <*> flag False True (long "devel" <> short 'd' <> help "True if in development mode")
    <*> strOption (long "root" <> short 'r' <> value "http://localhost:3000" <> metavar "URL" <> showDefault <> help "The root URL for links")
    <*> option auto (long "port" <> short 'p' <> value 3000 <> metavar "PORT" <> showDefault <> help "The port to listen on")

loadCamino :: AssetConfig -> IO Camino
loadCamino asset = do
  result <- readAsset asset
  return $ readCamino result

main :: IO ()
main = do
  opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "De Calixtinus")
  config' <- readConfigFile (config opts)
  let config'' = withRoot config' (pack $ root opts)
  caminos' <- mapM loadCamino (getCaminos config'')
  let cconfig' = createCaminoConfig (getCalendarConfig config'') (getRegionConfig config'') caminos'
  static' <- (if devel opts then staticDevel else static) (staticDir opts)
  feature' <- (if devel opts then staticDevel else static) (featureDir opts)
  putStrLn $ "Feature = " ++ (featureDir opts)
  image' <- (if devel opts then staticDevel else static) (imageDir opts)
  putStrLn $ "Image = " ++ (imageDir opts)
  cache' <- createCache config'' "plans"
  let app = CaminoApp {
      caminoAppPort = port opts
    , caminoAppDevel = devel opts
    , caminoAppStatic = static'
    , caminoAppFeature = feature'
    , caminoAppImage = image'
    , caminoAppPlans = cache'
    , caminoAppConfig = config''
    , caminoAppCaminoConfig = cconfig'
  }
  runCaminoApp app
