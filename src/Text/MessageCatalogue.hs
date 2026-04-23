{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Text.MessageCatalogue
Description : Message catalogues that allow hamlet-stle expressions
Copyright   : (c) Doug Palmer, 2026
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

The message catalogue module allows a user to define HTML message catalogues.

Message files reside in a common directory and have names like en.msg or pt-BR.msg.
A message file consists of a number of lines:

Comments have an initial #, eg.

@
# This next message doesn't make sense
@

Messages have an initial constructor and potential arguments, followed by a colon and the message body.
As a simple example:

@
PageTitle: The Page Title
@

In this case, the constructor, for use in @_{PageTitle}@ is @PageTitle@ and the substituted text is @The Page Title@

Constructors with arguments use \@ notation, eg

@
HelloMsg n\@Text lc@Int: Hello #{n}, you have logged-in #{lc} times
@

Similar to Yesod messages, types need only be specified in the default message file.
Types in other message files can be left off entirely or must match the default message type.
Variable names must start with a lower-case latter and may then be followed by any combination of letters or digits.
Types start with a capital letter and can use namespaces, such as @LB.ByteString@

A simple type name can be simply included after the \@. symbol.
More complex types need to be enclosed in parentheses.
Compound types, without type variables, can be declared.
For example @(Maybe String)@, @(Int -> String)@, @(M.Map Int (String -> [T.Text]))@ are all acceptable.

Messages use @#{...}@ interpolation and @^{...}@ interploation in the same manner as `shamlet`.
Similarly @.class-name@ and @#ident@ also work, as do structures like @$if@ or @$maybe@.
Care needs to be taken to ensure indentation works properly, since the first line implicitly has no leading spaces.

Since hamlet uses indentation to process HTML tags, spaces or other hamlet markup at the start of messages in the message file are
significant and used to denote multi-line hamlet.
An example multi-line message is

@
Ascent a@Float:
<span .ascent .distance>
  #{sformat int a}m
$if a > 20.0 then
  <span .problem>
    Oh dear!
@

when given  @Ascent 45.0@ this would produce

@
\<span class="ascent distance"\>45m\</span\> \<span class="problem"\>Oh dear!\</span\>
@

-}
module Text.MessageCatalogue (
    Text.MessageCatalogue.Internal.MessageException(..)

  , mkMessageCatalogueSimple
  , mkMessageCatalogue
  , Text.MessageCatalogue.Internal.renderMarkupToText
) where

import Text.MessageCatalogue.Internal
import Control.Exception (throw)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory (getDirectoryContents)
import Text.Shakespeare.I18N (Lang)

-- | Create a simple message catalogue.
--   This follows the structure of the Yesod `Yesod.Message.mkMessage` with the following differences:
--   * The output is `Html`, rather than `Text`
--   * Message constructors do not have a "Msg" appended to them
--
--   The result is a message data type called @/dt/Message@ and a rendering function called @render/dt/Message@ where
--   /dt/ is the application data type.
--   The rendering function takes two arguments, which can be used in the message body:
--   @master@ which is the application data and @langs@ which is a
--   complete list of the requested languages.
--   The requested languages can be used for further language resolution, if required.
--   E.g. @mkMessage "MyApp" "messages" "en"@ will create the message data type @MyAppMessage@ and a rendering
--   function @renderMyAppMessage :: MyApp -> [Lang] -> MyAppMessage -> Html@
--
--   The render function is an instance of `Text.Shakespeare.I18N.RenderMessage`

mkMessageCatalogueSimple :: String -- ^ The application data type
          -> FilePath -- ^ The folder containing the language-tagged message files
          -> Lang -- ^ The default language
          -> Q [Dec]
mkMessageCatalogueSimple dt folder lang = let
    aname = mkName dt
    mname = mkName $ dt ++ "Message"
    params = [("master", aname)]
  in
    mkMessageCatalogue (Just aname) mname ''Lang params folder lang True

-- | Create a message catalogue.
--
--   This allows considerably more customisation than `mkMessageCatalogueSimple`.
--
--   A typical invocation is
--   @
--     mkMessageCatalogue (Just ''MyApp) (mkName "MyAppMsg") ''Lang [("sub", ''MyApp), ("region", ''Region) "messages" "en" True
--   @
--
--   In this case:
--
--   * @''MyApp@ is the master application data type. This is optional and, if Nothing, will result in q "masterless" message catalogue
--   * @"MyAppMsg"@ is the message data type. This will be created by the function
--   * @''Lang@ is the locale data type. This is the standard Lang type used by Yesod but can be any locale type that is an instance of `Data.String.IsString` and `Eq`
--   * @("sub", ''MyApp)@ is a contextual parameter that provides the master application. This needs to be specified, as well as the data type in the first argument if you want to pass the application context to the messages.
--   * @("region", ''Region)@ is a contextual parameter that provides an additional region parameter to the message. Non master application types need to be an instance of `Data.Default.Class.Default`
--   * @"messages"@ is the directory that holds the message catalogue files
--   * @"en"@ is the base message catalogue
--
--   This will produce a main render function @renderMyAppMsg :: MyApp -> Region -> [Lang] -> MyAppMsg -> Html@ and
--   an instance of `Text.Shakespeare.I18N.RenderMessage` that uses `def` for the region.
mkMessageCatalogue :: Maybe Name -- ^ The optional application data type
  -> Name -- ^ The message type
  -> Name -- ^ The type of the language/locale
  -> [(String, Name)] -- ^ The names and types of additional context parameters (see above for information about the application data)
  -> FilePath -- ^ The folder containing the language-tagged message files
  -> Lang -- ^ The default language
  -> Bool -- ^ Create a default RenderMessage instance
  -> Q [Dec]
mkMessageCatalogue mdtype mtype ltype params folder lang rm = do
  files <- qRunIO $ getDirectoryContents folder
  contents <- qRunIO $ fmap catMaybes $ mapM (loadLang folder) files
  let base = maybe (throw $ MessageException "Did not find main language file" (Just lang) Nothing Nothing) id $ L.find (\c -> caLang c == lang) contents
  mapM_ (checkLang base) contents
  let ctx = MessageContext {
      mcRender = mkName $ "render" ++ nameBase mtype
    , mcAppType = ConT <$> mdtype
    , mcMsg = Param (mkName "msg") (ConT mtype)
    , mcLocale = Param (mkName "lang") (ConT ltype)
    , mcLocales = Param (mkName "langs") (ListT `AppT` ConT ltype)
    , mcMarkupType = htmlType
    , mcContext = map (\(n, t) -> Param (mkName n) (ConT t)) params
    , mcBase = base
  }
  datadec <- makeDataDec ctx
  msgdecs <- mapM (makeCatalogueDec ctx) contents
  renderdec <- makeRenderDec ctx contents
  instancedec <- if rm then makeInstanceDec ctx else return []
  let decs = datadec ++ concat msgdecs ++ renderdec ++ instancedec
  return decs
