{-|
Module      : Main
Description : De Calixtinus web server
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Camino.Camino
import Camino.Config
import Camino.Server.Application
import Camino.Server.Foundation
import Control.Monad (when)
import Options.Applicative
import System.FilePath

data Server = Server {
  config :: FilePath,
  port :: Int,
  caminos :: [FilePath]
}

arguments :: Parser Server
arguments = Server
    <$> strOption (long "config" <> short 'c' <> value "./config.yaml" <> metavar "CONFIG-FILE" <> showDefault <> help "Configuration file")
    <*> option auto (long "port" <> short 'p' <> value 3000 <> metavar "PORT" <> showDefault <> help "The port to listen on")
    <*> some (argument str (metavar "CAMINO-FILE"))

main :: IO ()
main = do
  opts <- execParser $ info (arguments <**> helper) (fullDesc <> progDesc "De Calixtinus")
  caminos' <- mapM readCamino (caminos opts)
  config' <- readConfigFile (config opts)
  runCaminoApp (CaminoApp (port opts) config' caminos')
