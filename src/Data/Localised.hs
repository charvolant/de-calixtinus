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
    Description(..)
  , Locale(..)
  , Localised(..)
  , Tagged(..)
  , TaggedText(..)
  
  , appendText
  , elements
  , localeFromID
  , localeFromIDOrError
  , localeLanguageTag
  , localise
  , localiseDefault
  , localiseText
  , rootLocale
  , uriToUrl
  , wildcardText
) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Char (isAlpha)
import Data.List (find, singleton)
import Data.Maybe (catMaybes)
import Data.Text (Text, breakOnEnd, dropEnd, null, pack, takeWhile, toLower, unpack)
import Network.URI

-- | A locale specification
data Locale = Locale {
    localeParent :: Maybe Locale
  , localeID :: Text -- ^ The IETF specifier for the locale
  , localeMatches :: [Text] -- ^ Alternative IETF specifier that match the locale
  , localeLanguage :: Maybe [Text] -- ^ The language codes
  , localeCountry :: Maybe [Text] -- ^ The country codes
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
    
-- | The base, wildcard locale
rootLocale :: Locale
rootLocale = Locale Nothing "*" ["root"] (Just []) (Just [])

englishLocale = Locale (Just rootLocale) "en" ["eng"] (Just ["en", "eng"]) (Just [])
englishUSLocale = Locale (Just englishLocale) "en-US" ["eng-US", "en_US", "eng_US"] Nothing (Just ["US"])
englishUKLocale = Locale (Just englishLocale) "en-UK" ["eng-UK", "en_UK", "eng_UK", "en-GB", "eng-GB", "en_GB", "eng_GB"] Nothing (Just ["UK"])
frenchLocale = Locale (Just rootLocale) "fr" ["fra", "fre" ] (Just ["fr", "fra", "fre"]) Nothing
galacianLocale = Locale (Just rootLocale) "ga" [ "glg" ] (Just ["ga", "glg"]) Nothing
portugueseLocale = Locale (Just rootLocale) "pt" [ "por" ] (Just ["pt", "por"]) Nothing
spanishLocale = Locale (Just rootLocale) "es" ["spa" ] (Just ["es", "spa"]) Nothing
basqueLocale = Locale (Just rootLocale) "eu" ["eus", "baq" ] (Just ["eu", "eus", "baq"]) Nothing

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
    let (text', locale') = breakOnEnd localeSeparator v
    let locale'' = if Data.Text.null text' || Data.Text.null locale' then rootLocale else (localeFromIDOrError locale')
    let text'' = if Data.Text.null text' then v else dropEnd 1 text'
    return $ TaggedText locale'' text''
  parseJSON v = typeMismatch "string" v

instance ToJSON TaggedText where
  toJSON (TaggedText locale' text') = toJSON $
      if locale' == rootLocale then text' else text' <> localeSeparator <> (localeID locale')


-- | A full description of something, inclusing links and other bits and pieces
data Description = Description {
      descLocale :: Locale -- ^ The locale associated with this description
    , descText :: Maybe Text -- ^ Any descriptive text
    , descNotes :: [Text] -- ^ Additional notes and comments
    , descAbout :: Maybe URI -- ^ A referencing URI, if resolvable then this can be make into a link
    , descImage :: Maybe URI -- ^ A link to an image
  }   
  deriving (Show)

instance Tagged Description where
   locale (Description loc _ _ _ _) = loc
   plainText (Description _ txt _ _ _) = maybe "" id txt
   fromText txt = Description rootLocale (Just txt) [] Nothing Nothing
   addText d@(Description loc _ notes about image) txt' = Description loc (Just (plainText d <> txt')) notes about image

instance FromJSON Description where
  parseJSON (String v) = do
    let (text', locale') = breakOnEnd localeSeparator v
    let locale'' = if Data.Text.null text' || Data.Text.null locale' then rootLocale else (localeFromIDOrError locale')
    let text'' = if Data.Text.null text' then v else dropEnd 1 text'
    return $ Description locale'' (Just text'') [] Nothing Nothing
  parseJSON (Object v) = do
    locale' <- v .: "locale"
    text' <- v .:? "text"
    notes' <- v .:? "notes" .!= []
    about' <- v .:? "about"
    let about'' = maybe Nothing parseURI about'
    image' <- v .:? "image"
    let image'' = maybe Nothing parseURI image'
    return $ Description (localeFromIDOrError locale') text' notes' about'' image''
  parseJSON v = typeMismatch "string or object" v

instance ToJSON Description where
  toJSON (Description locale' Nothing [] Nothing Nothing) = toJSON $ localeSeparator <> (localeID locale')
  toJSON (Description locale' (Just text') [] Nothing Nothing) = toJSON $
    if locale' == rootLocale then text' else text' <> localeSeparator <> (localeID locale')
  toJSON (Description locale' text' notes' about' image') = object [ 
        "locale" .= localeID locale'
      , "text" .= text'
      , "notes" .= if Prelude.null notes' then Nothing else Just notes'
      , "about" .= about''
      , "image" .= image''
    ] where
      about'' = fmap (\v -> (uriToString id v) "") about'
      image'' = fmap (\v -> (uriToString id v) "") image'

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

-- | Convert a URI into a text URL
uriToUrl :: Text -- ^ The URL base for relative URIs
  -> URI -- ^ The URI to resolve
  -> Text -- ^ The resolved URI
uriToUrl base uri = let
    urit = pack $ (uriToString id uri) ""
  in
    if uriIsAbsolute uri then urit else base <> "/" <> urit
    
-- | Choose the most appropriately localised piece of text for a list of locales
--   If there is no matching text and the 
--   If there is only one piece of text, then that is used regardless
localise :: (Tagged a) => [Locale] -> Localised a -> a
localise _ (Localised []) = fromText "" -- ^ Empty list of tags
localise _ (Localised [elt]) = elt -- Default case for a singleton
localise [] lt@(Localised (tt:_)) = case localise' [rootLocale] lt of
  Nothing -> tt
  (Just tt') -> tt'
localise locales lt = case localise' locales lt of
  Nothing -> localise rlocales lt where rlocales = catMaybes (map localeParent locales)
  (Just tt') -> tt'

localise' :: (Tagged a) => [Locale] -> Localised a -> Maybe a
localise' [] _ = Nothing
localise' (l:lr) lt@(Localised elts) = case find (\elt -> locale elt == l) elts of
  Nothing -> localise' lr lt
  mtt' -> mtt'
  
-- | Choose the most appropriately localised piece of text for a list of locales
--   See @localise@
localiseText :: (Tagged a) => [Locale] -> Localised a -> Text
localiseText locales lt = plainText $ localise locales lt

-- | Choose the default text
--   See @localiseText@
localiseDefault :: (Tagged a) => Localised a -> Text
localiseDefault lt = plainText $ localise [] lt

-- | Create a localised instance from a piece of text
wildcardText :: (Tagged a) => Text -> Localised a
wildcardText txt = Localised [fromText txt]