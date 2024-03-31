module Camino.Server.Settings (
    widgetFile
) where

import Data.Default.Class (def)
import Language.Haskell.TH.Syntax  (Exp, Q)
import Yesod.Default.Util (WidgetFileSettings, widgetFileNoReload, widgetFileReload)

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

widgetFile :: String -> Q Exp
widgetFile = (if False then widgetFileReload else widgetFileNoReload) widgetFileSettings


