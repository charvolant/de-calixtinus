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
  , devel :: Bool
  , root :: String
  , port :: Int
}

arguments :: Parser Server
arguments = Server
    <$> strOption (long "config" <> short 'c' <> value "./config.yaml" <> metavar "CONFIG-FILE" <> showDefault <> help "Configuration file")
    <*> strOption (long "static" <> short 's' <> value "./static" <> metavar "DIR" <> showDefault <> help "The directory holding static files")
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
  caminos' <- mapM loadCamino (configCaminos config')
  let config'' = createCaminoConfig (getCalendarConfig config') (getRegionConfig config') caminos'
  static' <- (if devel opts then staticDevel else static) (staticDir opts)
  runCaminoApp (CaminoApp (pack $ root opts) (port opts) (devel opts) static' config' config'')
