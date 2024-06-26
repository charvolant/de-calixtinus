{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Localised
Description : Locale-specific text and formatting
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Handle the need to have language-specific text and formatting for things like text, dates and numbers.
Or, at least it would if things like @Formatting@ were locale-friendly.

This module is intended to be JSON-friendly, with a simple JSON string having an "obvious" interpretation
as a wild-card localisation.
-}

module Data.Localised (
    Hyperlink(..)
  , Locale(..)
  , Localised(..)
  , Tagged(..)
  , TaggedLink(..)
  , TaggedText(..)
  , TaggedURL(..)

  , appendText
  , elements
  , invalidLink
  , localeFromID
  , localeFromIDOrError
  , localeLanguageTag
  , localeSeparator
  , localeTimeLocale
  , localise
  , localiseDefault
  , localiseText
  , parseTagged
  , rootLocale
  , rootTimeLocale
  , uriToText
  , wildcardText
) where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Char (isAlpha)
import Data.List (find, singleton, uncons)
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Text (Text, breakOnEnd, dropEnd, intercalate, isInfixOf, null, pack, splitOn, takeWhile, toLower, unpack)
import Data.Time.Format
import Data.Time.LocalTime
import Network.URI


-- | Convert a URI to text
uriToText :: URI -> Text
uriToText uri = pack $ (uriToString id uri) ""

-- | A locale specification
data Locale = Locale {
    localeParent :: Maybe Locale
  , localeID :: Text -- ^ The IETF specifier for the locale
  , localeMatches :: [Text] -- ^ Alternative IETF specifier that match the locale
  , localeLanguage :: Maybe [Text] -- ^ The language codes
  , localeCountry :: Maybe [Text] -- ^ The country codes
  , localeTime :: Maybe TimeLocale -- ^ The time locale to use
} deriving (Show)

instance Eq Locale where
  a == b = localeID a == localeID b
  
instance Ord Locale where
  a `compare` b = (localeID a) `compare` (localeID b)

localeLanguageTag :: Locale -> Text
localeLanguageTag loc = if Prelude.null langs then
    maybe "" localeLanguageTag (localeParent loc)
  else
    head langs
  where
    langs = maybe [] id (localeLanguage loc)

rootTimeLocale :: TimeLocale
rootTimeLocale = TimeLocale {
    wDays =
        [ ("Sunday", "Sun")
        , ("Monday", "Mon")
        , ("Tuesday", "Tue")
        , ("Wednesday", "Wed")
        , ("Thursday", "Thu")
        , ("Friday", "Fri")
        , ("Saturday", "Sat")
        ]
    , months =
        [ ("January", "Jan")
        , ("February", "Feb")
        , ("March", "Mar")
        , ("April", "Apr")
        , ("May", "May")
        , ("June", "Jun")
        , ("July", "Jul")
        , ("August", "Aug")
        , ("September", "Sep")
        , ("October", "Oct")
        , ("November", "Nov")
        , ("December", "Dec")
        ]
    , amPm = ("AM", "PM")
    , dateTimeFmt = "%Y-%m-%d %H:%M:%S %Z %Y"
    , dateFmt = "%Y-%m-%d"
    , timeFmt = "%H:%M:%S"
    , time12Fmt = "%I:%M:%S %p"
    , knownTimeZones =
        [ TimeZone 0 False "UT"
        , TimeZone 0 False "GMT"
        , TimeZone (1 * 60) False "CET"
        , TimeZone (2 * 60) True "CEST"
       ]
    }

portugueseTimeLocale :: TimeLocale
portugueseTimeLocale = rootTimeLocale {
    wDays =
        [ ("domingo", "dom")
        , ("segunda-feira", "seg")
        , ("terça-feira", "ter")
        , ("quarta-feira", "qua")
        , ("quinta-feira", "qui")
        , ("sexta-feira", "sex")
        , ("sábado", "sáb")
        ]
    , months =
        [ ("janeiro", "jan")
        , ("fevereiro", "fev")
        , ("março", "março")
        , ("abril", "abril")
        , ("maio", "maio")
        , ("junho", "junho")
        , ("julho", "julho")
        , ("agosto", "agosto")
        , ("setembro", "set")
        , ("outubro", "out")
        , ("novembro", "nov")
        , ("dezembro", "dez")
        ]
    , amPm = ("da manhã", "da tarde")
    , knownTimeZones =
        [ TimeZone 0 False "WET"
        , TimeZone 0 False "UT"
        , TimeZone 0 False "GMT"
        , TimeZone (1 * 60) True "WEST"
        ]
    }

spanishTimeLocale :: TimeLocale
spanishTimeLocale = rootTimeLocale {
    wDays =
        [ ("domingo", "dom")
        , ("lunes", "lun")
        , ("martes", "mar")
        , ("miércoles", "mié")
        , ("jueves", "jue")
        , ("viernes", "bie")
        , ("sábado", "sáb")
        ]
    , months =
        [ ("enero", "enero")
        , ("febrero", "feb")
        , ("marzo", "marzo")
        , ("abril", "abr")
        , ("mayo", "mayo")
        , ("junio", "jun")
        , ("julio", "jul")
        , ("agosto", "agosto")
        , ("septiembre", "sept")
        , ("octobre", "okt")
        , ("noviembre", "nov")
        , ("deciembre", "dic")
        ]
    , amPm = ("de la mañana", "de la tarde")
    }

frenchTimeLocale :: TimeLocale
frenchTimeLocale = rootTimeLocale {
    wDays =
        [ ("dimanche", "dim")
        , ("lundi", "lun")
        , ("mardi", "mar")
        , ("mercredi", "mer")
        , ("jeudi", "jeu")
        , ("vendredi", "ven")
        , ("samedi", "sam")
        ]
    , months =
        [ ("janvier", "janv")
        , ("fécrier", "févr")
        , ("mars", "mars")
        , ("avril", "avril")
        , ("mai", "mai")
        , ("juin", "juni")
        , ("juillet", "juil")
        , ("août", "août")
        , ("septembre", "sept")
        , ("octobre", "oct")
        , ("novembre", "nov")
        , ("décembre", "déc")
        ]
    , amPm = ("du matin", "de l'après-midi")
    }

galacianTimeLocale :: TimeLocale
galacianTimeLocale = rootTimeLocale {
    wDays =
        [ ("domingo", "dom")
        , ("luns", "lun")
        , ("martes", "mar")
        , ("mércores", "mié")
        , ("xoves", "jue")
        , ("venres", "bie")
        , ("sábado", "sáb")
        ]
    , months =
        [ ("xaneiro", "xan")
        , ("febreiro", "feb")
        , ("marzo", "marzo")
        , ("abril", "abril")
        , ("maio", "maio")
        , ("xuño", "xuño")
        , ("xullo", "xullo")
        , ("agosto", "agosto")
        , ("setembro", "set")
        , ("outubro", "out")
        , ("novembro", "nov")
        , ("decembro", "dec")
        ]
    , amPm = ("da mañá", "a tarde")
    }

basqueTimeLocale :: TimeLocale
basqueTimeLocale = rootTimeLocale {
    wDays =
        [ ("igande", "iga")
        , ("astelehen", "ahn")
        , ("astearte", "ate")
        , ("asteazken", "azn")
        , ("ortzegun", "org")
        , ("ortzirale", "orz")
        , ("larunbat", "lar")
        ]
    , months =
        [ ("urtarrila", "urt")
        , ("otsaila", "ots")
        , ("martxoa", "mar")
        , ("apirila", "api")
        , ("maiatza", "mai")
        , ("ekaina", "eka")
        , ("uztaila", "uzt")
        , ("agorrila", "ago")
        , ("iraila", "ira")
        , ("urria", "urr")
        , ("azaroa", "aza")
        , ("abendua", "abe")
        ]
    , amPm = ("goizean", "arratsaldean")
    }

-- | The base, wildcard locale
rootLocale :: Locale
rootLocale = Locale Nothing "*" ["root"] (Just []) (Just []) (Just rootTimeLocale)

englishLocale = Locale (Just rootLocale) "en" ["eng"] (Just ["en", "eng"]) (Just []) Nothing
englishUSLocale = Locale (Just englishLocale) "en-US" ["eng-US", "en_US", "eng_US"] Nothing (Just ["US"]) Nothing
englishUKLocale = Locale (Just englishLocale) "en-UK" ["eng-UK", "en_UK", "eng_UK", "en-GB", "eng-GB", "en_GB", "eng_GB"] Nothing (Just ["UK"]) Nothing
frenchLocale = Locale (Just rootLocale) "fr" ["fra", "fre" ] (Just ["fr", "fra", "fre"]) Nothing (Just frenchTimeLocale)
galacianLocale = Locale (Just rootLocale) "ga" [ "glg" ] (Just ["ga", "glg"]) Nothing (Just galacianTimeLocale)
portugueseLocale = Locale (Just rootLocale) "pt" [ "por" ] (Just ["pt", "por"]) Nothing (Just portugueseTimeLocale)
spanishLocale = Locale (Just rootLocale) "es" ["spa" ] (Just ["es", "spa"]) Nothing (Just spanishTimeLocale)
basqueLocale = Locale (Just rootLocale) "eu" ["eus", "baq" ] (Just ["eu", "eus", "baq"]) Nothing (Just basqueTimeLocale)

-- | Decode a locale identifier into a locale specification
--   If the locale cannot be identifier, the @rootLocale@ is returned
--   The current languages/regions are supported, corresponding to those encountered on the Camino:
--   Any(*), English, English/US, English/UK, French, Galacian, Portuguese, Spanish
localeFromID :: Text -- ^ The locale identifier 
  -> Maybe Locale -- ^ The resulting locale, if there is one
localeFromID "" = Just rootLocale
localeFromID "*" = Just rootLocale
localeFromID "eu" = Just basqueLocale
localeFromID "eus" = Just basqueLocale
localeFromID "baq" = Just basqueLocale
localeFromID "en" = Just englishLocale
localeFromID "eng" = Just englishLocale
localeFromID "en-GB" = Just englishUKLocale
localeFromID "en_GB" = Just englishUKLocale
localeFromID "eng-GB" = Just englishUKLocale
localeFromID "eng_GB" = Just englishUKLocale
localeFromID "en-UK" = Just englishUKLocale
localeFromID "en_UK" = Just englishUKLocale
localeFromID "eng-UK" = Just englishUKLocale
localeFromID "eng_UK" = Just englishUKLocale
localeFromID "en-US" = Just englishUSLocale
localeFromID "en_US" = Just englishUSLocale
localeFromID "eng-US" = Just englishUSLocale
localeFromID "eng_US" = Just englishUSLocale
localeFromID "fr" = Just frenchLocale
localeFromID "fra" = Just frenchLocale
localeFromID "fre" = Just frenchLocale
localeFromID "ga" = Just galacianLocale
localeFromID "glg" = Just galacianLocale
localeFromID "pt" = Just portugueseLocale
localeFromID "por" = Just portugueseLocale
localeFromID "es" = Just spanishLocale
localeFromID "spa" = Just spanishLocale
localeFromID v = let 
    lang = Data.Text.takeWhile (isAlpha) v
  in 
    if lang == v then 
      Nothing 
    else 
      localeFromID (toLower lang)  

-- | Get a locale and throw a fit if it's not found
--   Useful when reading from JSON
localeFromIDOrError :: Text -> Locale
localeFromIDOrError v = maybe
  (error $ "Invalid locale " ++ unpack v)
  id
  (localeFromID v)
  
-- | The separator in text that indicates a locale tagged onto the end of the string
localeSeparator :: Text
localeSeparator = "@"

-- | Get the time locale for this locale, working up the parent structure if not immediately found
localeTimeLocale :: Locale -> TimeLocale
localeTimeLocale (Locale _ _ _ _ _ (Just tl)) = tl
localeTimeLocale (Locale Nothing _ _ _ _ Nothing) = rootTimeLocale -- Should never happen
localeTimeLocale (Locale (Just parent) _ _ _ _ Nothing) = localeTimeLocale parent

-- | Parse a piece of text with an optional locale tage at the end into a locale/text pair
--   For exampele @"Hello@fr"@ becomes @(french, "Hello")@ and @"Nothing"@ becomes @(root, "Nothing")@
parseTagged :: Text -> (Locale, Text)
parseTagged txt = (maybe rootLocale id locale'', txt'')
  where
    (txt', locale') = breakOnEnd localeSeparator txt
    locale'' = if Data.Text.null txt' || Data.Text.null locale' then Nothing else (localeFromID locale')
    txt'' = if isNothing locale'' then txt else dropEnd 1 txt'

-- | Information that is tagged with a locale
class Tagged a where
  -- | The locale associated with the tagged object
  locale :: a -> Locale
  -- | A plain text label for the tagged object
  plainText :: a -> Text
  -- | Construct a wildcard version from a piece of plain text
  fromText :: Text -> a
  -- | Append a piece of text to the plain text element in this tagged object
  addText :: a -> Text -> a


-- | Information that contains a link
class TaggedLink a where
  -- | The link
  link :: a -> URI
  -- | The link as text
  linkText :: a -> Text
  linkText tu = uriToText $ link tu
  -- | Resolve this link against a base path, if the link URL is relative
  resolveLink :: Text -> a -> Text
  resolveLink base tl = if uriIsAbsolute (link tl) then url' else base <> "/" <> url' where url' = linkText tl

-- | A piece of localised text tgged by a locale specification
--   In JSON, localised text can be written as @Text\@Locale@ eg "Hello@en", "Hola@es"
data TaggedText = TaggedText Locale Text 
  deriving (Show)

instance Tagged TaggedText where
  locale (TaggedText loc _) = loc
  plainText (TaggedText _ txt) = txt
  fromText txt = TaggedText rootLocale txt
  addText (TaggedText loc txt) txt' = TaggedText loc (txt <> txt')
  
instance FromJSON TaggedText where
  parseJSON (String v) = do
    let (locale', text') = parseTagged v
    return $ TaggedText locale' text'
  parseJSON v@(Array _) = do
    txts' <- parseJSONList v :: Parser [Text]
    let (locale', txts'') = maybe ("", txts') id (uncons txts')
    let (locale'', txts''') = maybe (rootLocale, txts') (\l -> (l, txts'')) (localeFromID locale')
    return $ TaggedText locale'' (intercalate "\n" txts''')
  parseJSON v = typeMismatch "string or array" v

instance ToJSON TaggedText where
  toJSON (TaggedText locale' text') = if isInfixOf "\n" text' then
      toJSON $ (localeID locale'):(splitOn "\n" text')
    else
      toJSON $ if locale' == rootLocale then text' else text' <> localeSeparator <> (localeID locale')

-- | A URL with an optional title
data Hyperlink = Hyperlink URI (Maybe Text)
  deriving (Show)

-- | A URL with potential localisation and title
--   In JSON, localised text can be written as @Text\@Locale@ eg "Hello@en", "Hola@es"
data TaggedURL = TaggedURL Locale Hyperlink
  deriving (Show)

instance Tagged TaggedURL where
  locale (TaggedURL loc _) = loc
  plainText (TaggedURL _ (Hyperlink _ title)) = maybe "" id title
  fromText txt = TaggedURL rootLocale (Hyperlink (fromJust $ parseURI $ unpack $ txt) Nothing)
  addText (TaggedURL loc (Hyperlink uri title)) txt' = TaggedURL loc (Hyperlink uri (Just $ maybe "" id title <> txt'))

instance TaggedLink TaggedURL where
  link (TaggedURL _ (Hyperlink url _)) = url

-- A link to an invalid or 404 page
invalidLink :: TaggedURL
invalidLink = TaggedURL rootLocale (Hyperlink (fromJust $ parseURI "invalid") (Just "Invalid"))

instance FromJSON TaggedURL where
  parseJSON (String v) = do
    let (locale', url') = parseTagged v
    return $ TaggedURL locale' (Hyperlink (fromJust $ parseURI $ unpack url') Nothing)
  parseJSON (Object v) = do
    locale' <- v .: "locale"
    url' <- v .: "url"
    title' <- v .:? "title"
    return $ TaggedURL (localeFromIDOrError locale') (Hyperlink (fromJust $ parseURI url') title')
  parseJSON v = typeMismatch "string or object" v

instance ToJSON TaggedURL where
  toJSON (TaggedURL locale' (Hyperlink url' Nothing)) = toJSON $
      if locale' == rootLocale then url'' else url'' <> localeSeparator <> (localeID locale')
      where
        url'' = uriToText url'
  toJSON (TaggedURL locale' (Hyperlink url' (Just title'))) = object [
        "locale" .= localeID locale'
      , "url" .= uriToText url'
      , "title" .= title'
    ]

-- | A localised object containing (potentially) multiple localised instances of something
data (Tagged a) => Localised a = Localised [a]
    deriving (Show)

instance (Tagged a, FromJSON a) => FromJSON (Localised a) where
  parseJSON v@(String _) = Localised <$> (singleton <$> parseJSON v)
  parseJSON v@(Array _) = Localised <$> (parseJSONList v)
  parseJSON v = typeMismatch "string or array" v

instance (Tagged a, ToJSON a) => ToJSON (Localised a) where
  toJSON (Localised []) = toJSON (""::Text)
  toJSON (Localised [elt]) = toJSON elt
  toJSON (Localised elts) = toJSON (map toJSON elts)

-- | Get the elements of a localised list
elements :: (Tagged a) => Localised a -> [a]
elements (Localised elts) = elts

-- | Append some text to localised text
appendText :: (Tagged a) => Localised a -> Text -> Localised a
appendText (Localised elts) txt = Localised (map (\elt -> elt `addText` txt) elts)

-- | Choose the most appropriately localised piece of text for a list of locales
--   If there is no matching text and the 
--   If there is only one piece of text, then that is used regardless
localise :: (Tagged a) => [Locale] -> Localised a -> Maybe a
localise _ (Localised []) = Nothing -- ^ Empty list of tags
localise _ (Localised [elt]) = Just elt -- Default case for a singleton
localise [] lt@(Localised (tt:_)) = case localise' [rootLocale] lt of
  Nothing -> Just tt
  tt' -> tt'
localise locales lt = case localise' locales lt of
  Nothing -> localise rlocales lt where rlocales = catMaybes (map localeParent locales)
  tt' -> tt'

localise' :: (Tagged a) => [Locale] -> Localised a -> Maybe a
localise' [] _ = Nothing
localise' (l:lr) lt@(Localised elts) = case find (\elt -> locale elt == l) elts of
  Nothing -> localise' lr lt
  mtt' -> mtt'
  
-- | Choose the most appropriately localised piece of text for a list of locales
--   See @localise@
localiseText :: (Tagged a) => [Locale] -> Localised a -> Text
localiseText locales lt = maybe "" id (plainText <$> localise locales lt)

-- | Choose the default text
--   See @localiseText@
localiseDefault :: (Tagged a) => Localised a -> Text
localiseDefault lt = localiseText [] lt

-- | Create a localised instance from a piece of text
wildcardText :: (Tagged a) => Text -> Localised a
wildcardText txt = Localised [fromText txt]
