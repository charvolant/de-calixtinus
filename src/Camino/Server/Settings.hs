module Camino.Server.Settings (
    widgetFile
) where

import Data.Default.Class (def)
import Language.Haskell.TH.Syntax  (Exp, Name, Q)
import Yesod.Default.Util (WidgetFileSettings, widgetFileNoReload, widgetFileReload)

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

widgetFile :: String -> Q Exp
widgetFile = (if True then widgetFileReload else widgetFileNoReload) widgetFileSettings


