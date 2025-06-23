{-|
Module      : Main
Description : De Calixtinus web server
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
import Data.Cache
import Data.Event
import Data.IORef
import Data.Region
import Data.Text (Text, pack, unpack)
import Camino.Camino
import Camino.Config
import Camino.Planner (Solution)
import Camino.Server.Application
import Camino.Server.Foundation
import Network.HTTP.Simple()
import Options.Applicative
import Yesod.Static

data Server = Server {
    config :: FilePath
  , staticDir :: FilePath
  , storeDir :: FilePath
  , devel :: Bool
  , root :: String
  , port :: Int
}

arguments :: Parser Server
arguments = Server
    <$> strOption (long "config" <> short 'c' <> value "./config.yaml" <> metavar "CONFIG-FILE" <> showDefault <> help "Configuration file")
    <*> strOption (long "static" <> short 's' <> value "./static" <> metavar "DIR" <> showDefault <> help "The directory holding static files")
    <*> strOption (long "store" <> short 'f' <> value "./store" <> metavar "DIR" <> showDefault <> help "The directory holding stored files and solutions")
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
  let primaryCache' = defaultMemCache 10 :: MemCache Text Solution
  let secondaryCache' = defaultFileCacheWithNamer (storeDir opts) (30 * 24 * 3600) unpack :: FileCache Text Solution
  let cache' = CompositeCache primaryCache' secondaryCache'
  cache'' <- newIORef cache'
  runCaminoApp (CaminoApp (port opts) (devel opts) static' cache'' config'' cconfig')
