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

== Message Files

Message files reside in a common directory and have names like @en.msg@ or @pt-BR.msg@.
A message file consists of a number of lines:

Comments have an initial @#@, eg.

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
HelloMsg n\@Text lc\@Int: Hello #{n}, you have logged-in #{lc} times
@

Similar to Yesod messages, types need only be specified in the default message file.
Types in other message files can be left off entirely or must match the default message type.
Variable names must start with a lower-case latter and may then be followed by any combination of letters or digits.
Types start with a capital letter and can use namespaces, such as @LB.ByteString@, although multi-level module names,
such as @Data.Maybe.Just@ won't work.

A simple type name can be simply included after the \@. symbol.
More complex types need to be enclosed in parentheses.
Compound types, without type variables, can be declared.
For example @(Maybe String)@, @(Int -> String)@, @(M.Map Int (String -> [T.Text]))@ are all acceptable.

Messages use @#{...}@ interpolation and @^{...}@ interploation in the same manner as `shamlet`.
Similarly @.class-name@ and @#ident@ also work, as do structures like @$if@ or @$maybe@.
For multi-line messages, care needs to be taken to ensure indentation works properly, since the first line implicitly has no leading spaces.

Since hamlet uses indentation to process HTML tags, spaces or other hamlet markup at the start of messages in the message file are
significant and used to denote multi-line hamlet.
An example multi-line message is

@
Ascent a@Float:
\<span .ascent .distance>
  #{sformat int a}m
$if a > 20.0 then
  \<span .problem>
    Oh dear!
@

when given  @Ascent 45.0@ this would produce

@
\<span class="ascent distance"\>45m\<\/span\> \<span class="problem"\>Oh dear!\<\/span\>
@

== Message Parameters

Parameters passed in to any message are the named parameters specified in `mkMessageCatalogue` or @master@ for the
master context in `mkMessageCatalogueSimple`.
The complete list of locales is passed in using @locales@.
Locales can be anything that implements `IsString` and `Eq`, and matches language codes.
For simple applications, this will be the Yesod `Lang` type, but fancier implementations can be used, if you want
to propagate additional localisation information into the message.
As an example, if the message catalogue is created with

@
mkMessageCatalogue (mkName "MyAppMsg") ''Lang [("sub", ''MyApp), ("region", ''Region) "messages" "en" (Just ''MyApp)
@

then a message of the sort

@
Nearby: Nearby attractions ^{showAttractions sub region locales}
@

will work as expected and the @showAttractions@ function can use the supplied locales to do its own bit of localisation, if needed.

== Pragmas

Generally, you will need to in add @{-# LANGUAGE OverloadedStrings #-}@ and @{-# LANGUAGE TemplateHaskell #-}@ pragmas to the source file.
If the render function is being generated, you may need to include @{-# LANGUAGE MultiParamTypeClasses #-}@ in the source file, since
`RenderMessage` takes two type parameters.

== Differences to Yesod message catalogues

`mkMessageCatalogueSimple` works similarly to the Yesod `mkMessage`, with a few differences:

* The message constructor is "as-is" without an additional @Msg@ suffix.
* The master application context is @master@ rather than @sub@
* Output is HTML, rather than text. `renderMarkupToText` can be used to convert the resulting HTML to plain text, if needed.

-}
module Text.MessageCatalogue (
    mkMessageCatalogueSimple
  , mkMessageCatalogue
  , mkMessageCatalogueRender
  , Text.MessageCatalogue.Internal.RenderCatalogueMessage0(..)
  , Text.MessageCatalogue.Internal.RenderCatalogueMessage1(..)
  , Text.MessageCatalogue.Internal.RenderCatalogueMessage2(..)
  , Text.MessageCatalogue.Internal.Renderer
  , Text.MessageCatalogue.Internal.renderMarkupToText
) where

import Control.Monad (unless)
import Text.MessageCatalogue.Internal
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Text (Text, unpack)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory (getDirectoryContents)
import Text.Shakespeare.I18N (Lang)

-- | Create a simple message catalogue.
--   This follows the structure of the Yesod `Yesod.Message.mkMessage` with the differences specified above.
--
--   The result is a message data type called @/dt/Message@ and a rendering function called @render/dt/Message@ where
--   /dt/ is the application data type.
--   The rendering function takes two arguments, which can be used in the message body:
--   @master@ which is the application data and @locales@ which is a
--   complete list of the requested languages.
--   The requested languages can be used for further language resolution, if required.
--   E.g. @mkMessage "MyApp" "messages" "en"@ will create the message data type @MyAppMessage@ and a rendering
--   function @renderMyAppMessage :: MyApp -> [Lang] -> MyAppMessage -> Html@
--
--   The render function is an instance of `Text.Shakespeare.I18N.RenderMessage`

mkMessageCatalogueSimple :: Bool -- ^ Include message files as dependencies
  -> String -- ^ The application data type
  -> FilePath -- ^ The folder containing the language-tagged message files
  -> Lang -- ^ The default language
  -> Q [Dec]
mkMessageCatalogueSimple deps dt folder lang = do
  let
    aname = mkName dt
    mname = mkName $ dt ++ "Message"
    params = [("master", aname)]
  catalogue <- mkMessageCatalogue deps mname ''Lang params folder lang
  instances <- mkMessageCatalogueRender mname ''Lang Nothing aname params
  return $ catalogue ++ instances


-- | Create a message catalogue.
--
--   This allows considerably more customisation than `mkMessageCatalogueSimple`.
--   In particular, it allows an arbitrary list of contextual parameters and choice over the locale specification.
--
--   A typical invocation is
--   @
--     mkMessageCatalogue (mkName "MyAppMsg") ''Lang [("sub", ''MyApp), ("region", ''Region) "messages" "en" (Just ''MyApp)
--   @
--
--   In this case:
--
--   * @"MyAppMsg"@ is the message data type. This will be created by @mkMessageCatalogue@, so @''MyAppMsg@ won't work.
--   * @''Lang@ is the locale data type. This is the standard Lang type used by Yesod but can be any locale type that is an instance of `Data.String.IsString` and `Eq`
--   * @("sub", ''MyApp)@ is a contextual parameter that provides the master application. This needs to be specified, as well as the data type in the first argument if you want to pass the application context to the messages.
--   * @("region", ''Region)@ is a contextual parameter that provides an additional region parameter to the message. Non master application types need to be an instance of `Data.Default.Class.Default`
--   * @"messages"@ is the directory that holds the message catalogue files
--   * @"en"@ is the base message catalogue
--   * @''MyApp@ is the master application data type. This is optional and, if Nothing, will result in a "masterless" message catalogue
--
--   This will produce a main render function @renderMyAppMsg :: MyApp -> Region -> [Lang] -> MyAppMsg -> Html@ and
--   an instance of `Text.Shakespeare.I18N.RenderMessage` that uses `def` for the region.
mkMessageCatalogue :: Bool -- ^ Include message files as a dependency
  -> Name -- ^ The message type
  -> Name -- ^ The type of the language\/locale (the list of requested languages\/locales will be placed in the reserved @locales@ parameter)
  -> [(String, Name)] -- ^ The names and types of additional context parameters (see below for information about distinguished application data)
  -> FilePath -- ^ The folder containing the language-tagged message files
  -> Text -- ^ The default language, using the string representation
  -> Q [Dec]
mkMessageCatalogue deps mtype ltype params folder lang = do
  files <- qRunIO $ getDirectoryContents folder
  contents <- fmap catMaybes $ mapM (loadLang deps folder) files
  base <- findBase lang contents
  let ctx = MessageContext {
      mcRender = mkName $ "render" ++ nameBase mtype
    , mcAppType = Nothing
    , mcMsg = Param (mkName "msg") (ConT mtype)
    , mcLocale = Param (mkName "locale") (ConT ltype)
    , mcLocales = Param (mkName "locales") (ListT `AppT` ConT ltype)
    , mcLocaleLookup = Nothing
    , mcMarkupType = htmlType
    , mcContext = map (\(n, t) -> Param (mkName n) (ConT t)) params
    , mcBase = base
  }
  ok <- checkMessageDeclaration ctx contents
  unless ok (fail "Error validating language files")
  datadec <- makeDataDec ctx
  msgdecs <- mapM (makeCatalogueDec ctx) contents
  renderdec <- makeRenderDec ctx contents
  let decs = datadec ++ concat msgdecs ++ renderdec
  return decs

findBase :: Lang -> [Catalogue] -> Q Catalogue
findBase lang catalogues = do
  case L.find (\c -> caLang c == lang) catalogues of
    Nothing ->
      fail ("Can't find main catalogue of language " ++ unpack lang)
    (Just catalogue) ->
      return catalogue

-- | Create a message catalogue render instances.
--
--   This creates instances of `MessageCatalogueRender` and `MessageRender` for the
--
--   A typical invocation is
--   @
--     mkMessageCatalogue (mkName "MyAppMsg") ''Lang [("sub", ''MyApp), ("region", ''Region) "messages" "en" (Just ''MyApp)
--   @
--
--   In this case:
--
--   * @"MyAppMsg"@ is the message data type. This will be created by @mkMessageCatalogue@, so @''MyAppMsg@ won't work.
--   * @''Lang@ is the locale data type. This is the standard Lang type used by Yesod but can be any locale type that is an instance of `Data.String.IsString` and `Eq`
--   * @("sub", ''MyApp)@ is a contextual parameter that provides the master application. This needs to be specified, as well as the data type in the first argument if you want to pass the application context to the messages.
--   * @("region", ''Region)@ is a contextual parameter that provides an additional region parameter to the message. Non master application types need to be an instance of `Data.Default.Class.Default`
--   * @"messages"@ is the directory that holds the message catalogue files
--   * @"en"@ is the base message catalogue
--   * @''MyApp@ is the master application data type. This is optional and, if Nothing, will result in a "masterless" message catalogue
--
--   This will produce a main render function @renderMyAppMsg :: MyApp -> Region -> [Lang] -> MyAppMsg -> Html@ and
--   an instance of `Text.Shakespeare.I18N.RenderMessage` that uses `def` for the region.
mkMessageCatalogueRender :: Name -- ^ The message type
  -> Name -- ^ The type of the language\/locale (the list of requested languages\/locales will be placed in the reserved @locales@ parameter)
  -> Maybe Name -- ^ An optional name of the function that traslates a `Lang` into a Maybe locale
  -> Name -- ^ The type of the master application
  -> [(String, Name)] -- ^ The names and types of additional context parameters (see below for information about distinguished application data)
  -> Q [Dec]
mkMessageCatalogueRender mtype ltype mlookup stype params = do
  let ctx = MessageContext {
      mcRender = mkName $ "render" ++ nameBase mtype
    , mcAppType = Just (ConT stype)
    , mcMsg = Param (mkName "msg") (ConT mtype)
    , mcLocale = Param (mkName "locale") (ConT ltype)
    , mcLocales = Param (mkName "locales") (ListT `AppT` ConT ltype)
    , mcLocaleLookup = mlookup
    , mcMarkupType = htmlType
    , mcContext = map (\(n, t) -> Param (mkName n) (ConT t)) params
    , mcBase = Catalogue "*" []
  }
  makeInstanceDec ctx
