{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE InstanceSigs #-}
{-|
Module      : Functions
Description : Functions for forms that use the `CatalogueMessage` message types.
Copyright   : (c) Doug Palmer, 2026
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Forms for the application.

The preferences gathering forms use a common data element and pass data via hidden fields
-}
module Camino.Server.Forms.Functions (
    CatalogueFieldSettings(..)

  , mreqr
  , mreqrMsg
  , moptr
) where

import Control.Monad (join)
import Control.Monad.Trans.RWS (ask, tell)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
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
import Yesod.Form.I18n.Norwegian
import Yesod.Form.I18n.Portuguese
import Yesod.Form.I18n.Romanian
import Yesod.Form.I18n.Russian
import Yesod.Form.I18n.Spanish
import Yesod.Form.I18n.Swedish

-- | Field settings for use with `CatalogueMessage`
data CatalogueFieldSettings master message = CatalogueFieldSettings
    { cfsLabel :: message
    , cfsTooltip :: Maybe message
    , cfsId :: Maybe Text
    , cfsName :: Maybe Text
    , cfsAttrs :: [(Text, Text)]
    }

instance RenderCatalogueMessage0 master FormMessage where
    renderMessageText0 :: master -> [Lang] -> FormMessage -> Text
    renderMessageText0 site langs msg = renderMessage site langs msg

instance RenderMessage master FormMessage where
    renderMessage _master [] = defaultFormMessage
    renderMessage _master ("zh":_) = chineseFormMessage
    renderMessage _master ("cz":_) = czechFormMessage
    renderMessage _master ("nl":_) = dutchFormMessage
    renderMessage _master ("en":_) = englishFormMessage
    renderMessage _master ("es":_) = spanishFormMessage
    renderMessage _master ("fr":_) = frenchFormMessage
    renderMessage _master ("de":_) = germanFormMessage
    renderMessage _master ("jp":_) = japaneseFormMessage
    renderMessage _master ("ko":_) = koreanFormMessage
    -- renderMessage _master ("no":_) = norwegianBokmålFormMessage
    renderMessage _master ("pt":_) = portugueseFormMessage
    renderMessage _master ("ro":_) = romanianFormMessage
    renderMessage _master ("ru":_) = russianFormMessage
    renderMessage _master ("sv":_) = swedishFormMessage
    renderMessage master (_:rest) = renderMessage master rest

-- | Monadic form field with an explicit renderer for a required field and a default missing message.
--
--   @see `mreq`
mreqr :: (HandlerSite m ~ master, MonadHandler m)
  => Renderer master message -- ^ The message renderer
  -> Field m a -- ^ form field
  -> CatalogueFieldSettings master message -- ^ settings for this field
  -> Maybe a -- ^ optional default value
  -> MForm m (FormResult a, FieldView master)
mreqr render field fs mdef = mreqrMsg render field fs MsgValueRequired mdef

-- | Monadic form field with an explicit renderer for a required field.
--
--   @see `mreqMsq`
mreqrMsg :: (RenderMessage master rmessage, HandlerSite m ~ master, MonadHandler m)
  => Renderer master message -- ^ The message renderer
  -> Field m a -- ^ form field
  -> CatalogueFieldSettings master message -- ^ settings for this field
  -> rmessage -- ^ The 'required' message
  -> Maybe a -- ^ optional default value
  -> MForm m (FormResult a, FieldView master)
mreqrMsg render field fs msg mdef = mrhelper render field fs mdef formFailure FormSuccess True
  where formFailure m l = FormFailure [renderMessage m l msg]

-- | Monadic form field with an explicit renderer for an optional field.
--
--   @see `mopt`
moptr :: (master ~ HandlerSite m, MonadHandler m)
   => Renderer master message -- ^ The message renderer
   -> Field m a -- ^ form field
   -> CatalogueFieldSettings master message  -- ^ settings for this field
   -> Maybe (Maybe a) -- ^ Optional default value
   -> MForm m (FormResult (Maybe a), FieldView master)
moptr render field fs mdef = mrhelper render field fs (join mdef) (const $ const $ FormSuccess Nothing) (FormSuccess . Just) False

-- Derived from `Yesod.Form.Functions.mhelper`
mrhelper render field fs mdef onMissing onFound isReq = do
  tell (fieldEnctype field)
  mp <- askParams
  name <- maybe newFormIdent return (cfsName fs)
  theId <- lift $ maybe newIdent return (cfsId fs)
  (_, site, langs) <- ask
  let mr2 = render site langs
  (res, val) <-
    case mp of
      Nothing -> return (FormMissing, maybe (Left "") Right mdef)
      Just p -> do
        mfs <- askFiles
        let mvals = fromMaybe [] $ M.lookup name p
            files = fromMaybe [] $ mfs >>= M.lookup name
        emx <- lift $ (fieldParse field) mvals files
        return $ case emx of
          Left (SomeMessage e) -> (FormFailure [renderMessage site langs e], maybe (Left "") Left (listToMaybe mvals))
          Right mx ->
            case mx of
              Nothing -> (onMissing site langs, Left "")
              Just x -> (onFound x, Right x)
  return (res, FieldView {
      fvLabel = mr2 (cfsLabel fs)
    , fvTooltip = fmap mr2 (cfsTooltip fs)
    , fvId = theId
    , fvInput = (fieldView field) theId name (cfsAttrs fs) val isReq
    , fvErrors =
        case res of
          FormFailure [e] -> Just $ toHtml e
          _ -> Nothing
    , fvRequired = isReq
    })

