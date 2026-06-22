{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE InstanceSigs #-}
{-|
Module      : Functions
Description : Useful functions for forms.
Copyright   : (c) Doug Palmer, 2026
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

-}
module Camino.Server.Forms.Functions where

import Text.MessageCatalogue
import Yesod
import Yesod.Form.I18n.Chinese
import Yesod.Form.I18n.Czech
import Yesod.Form.I18n.Dutch
import Yesod.Form.I18n.English
import Yesod.Form.I18n.French
import Yesod.Form.I18n.German
import Yesod.Form.I18n.Japanese
import Yesod.Form.I18n.Korean
-- import Yesod.Form.I18n.Norwegian
import Yesod.Form.I18n.Portuguese
import Yesod.Form.I18n.Romanian
import Yesod.Form.I18n.Russian
import Yesod.Form.I18n.Spanish
import Yesod.Form.I18n.Swedish

instance RenderCatalogueMessage master FormMessage where
    renderMessageText _master [] = defaultFormMessage
    renderMessageText _master ("zh":_) = chineseFormMessage
    renderMessageText _master ("cz":_) = czechFormMessage
    renderMessageText _master ("nl":_) = dutchFormMessage
    renderMessageText _master ("en":_) = englishFormMessage
    renderMessageText _master ("es":_) = spanishFormMessage
    renderMessageText _master ("fr":_) = frenchFormMessage
    renderMessageText _master ("de":_) = germanFormMessage
    renderMessageText _master ("jp":_) = japaneseFormMessage
    renderMessageText _master ("ko":_) = koreanFormMessage
    -- renderMessageText _master ("no":_) = norwegianBokmålFormMessage
    renderMessageText _master ("pt":_) = portugueseFormMessage
    renderMessageText _master ("ro":_) = romanianFormMessage
    renderMessageText _master ("ru":_) = russianFormMessage
    renderMessageText _master ("sv":_) = swedishFormMessage
    renderMessageText master (_:rest) = renderMessageText master rest
