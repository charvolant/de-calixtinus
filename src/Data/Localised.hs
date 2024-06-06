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
    Locale(..)
  , LocalisedText(..)
  , TaggedText(..)
  
  , localeFromID
  , localeLanguageTag
  , localise
  , rootLocale
) where

import Data.Aeson
import Data.Char (isAlpha)
import Data.List (find, singleton)
import Data.Maybe (catMaybes)
import Data.Text (Text, breakOnEnd, dropEnd, null, takeWhile, toLower)
import Data.Aeson.Types (typeMismatch)

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
localeLanguageTag locale = if Prelude.null langs then
    maybe "" localeLanguageTag (localeParent locale)
  else
    head langs
  where
    langs = maybe [] id (localeLanguage locale)
    
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

-- | Decode a locale identifier into a locale specification
--   If the locale cannot be identifier, the @rootLocale@ is returned
--   The current languages/regions are supported, corresponding to those encountered on the Camino:
--   Any(*), English, English/US, English/UK, French, Galacian, Portuguese, Spanish
localeFromID :: Text -- ^ The locale identifier 
  -> Locale -- ^ The resulting locale.
localeFromID "" = rootLocale
localeFromID "*" = rootLocale
localeFromID "en" = englishLocale
localeFromID "eng" = englishLocale
localeFromID "en-GB" = englishUKLocale
localeFromID "en_GB" = englishUKLocale
localeFromID "eng-GB" = englishUKLocale
localeFromID "eng_GB" = englishUKLocale
localeFromID "en-UK" = englishUKLocale
localeFromID "en_UK" = englishUKLocale
localeFromID "eng-UK" = englishUKLocale
localeFromID "eng_UK" = englishUKLocale
localeFromID "en-US" = englishUSLocale
localeFromID "en_US" = englishUSLocale
localeFromID "eng-US" = englishUSLocale
localeFromID "eng_US" = englishUSLocale
localeFromID "fr" = frenchLocale
localeFromID "fra" = frenchLocale
localeFromID "fre" = frenchLocale
localeFromID "ga" = galacianLocale
localeFromID "glg" = galacianLocale
localeFromID "pt" = portugueseLocale
localeFromID "por" = portugueseLocale
localeFromID "es" = spanishLocale
localeFromID "spa" = spanishLocale
localeFromID v = let 
    lang = Data.Text.takeWhile (isAlpha) v
  in 
    if lang == v then 
      rootLocale 
    else 
      localeFromID (toLower lang)  

-- | The separator in text that indicates a locale tagged onto the end of the string
localeSeparator :: Text
localeSeparator = "@"

-- | A piece of localised text tgged by a locale specification
--   In JSON, localised text can be written as @Text\@Locale@ eg "Hello@en", "Hola@es"
data TaggedText = TaggedText {
    ttText :: Text -- ^ The text
  , ttLocale :: Locale -- ^ The locale specification
} deriving (Show)

instance FromJSON TaggedText where
  parseJSON (String v) = do
    let (text', locale') = breakOnEnd localeSeparator v
    let locale'' = if Data.Text.null text' || Data.Text.null locale' then rootLocale else (localeFromID locale')
    return $ if Data.Text.null text' then TaggedText v locale'' else TaggedText (dropEnd 1 text') locale''
  parseJSON v = typeMismatch "string" v

instance ToJSON TaggedText where
  toJSON (TaggedText text' locale') = toJSON $
      if locale' == rootLocale then text' else text' <> localeSeparator <> (localeID locale')

-- | A piece of localised text
data LocalisedText = LocalisedText {
  ltTexts :: [TaggedText] -- ^ The list of tagged text instances
  }  deriving (Show)

instance FromJSON LocalisedText where
  parseJSON v@(String _) = LocalisedText <$> (singleton <$> parseJSON v)
  parseJSON v@(Array _) = LocalisedText <$> (parseJSONList v)
  parseJSON v = typeMismatch "string or array" v

instance ToJSON LocalisedText where
  toJSON (LocalisedText []) = toJSON (""::Text)
  toJSON (LocalisedText [tt]) = toJSON tt
  toJSON (LocalisedText texts) = toJSON (map toJSON texts)

-- | Choose the most appropriately localised piece of text for a list of locales
--   If there is no matching text and the 
--   If there is only one piece of text, then that is used regardless
localise :: [Locale] -> LocalisedText -> TaggedText
localise _ (LocalisedText []) = TaggedText "" rootLocale -- ^ Empty list of tags
localise _ (LocalisedText [tt]) = tt -- Default case for a singleton
localise [] lt@(LocalisedText (tt:_)) = case localise' [rootLocale] lt of
  Nothing -> tt
  (Just tt') -> tt'
localise locales lt = case localise' locales lt of
  Nothing -> localise rlocales lt where rlocales = catMaybes (map localeParent locales)
  (Just tt') -> tt'

localise' :: [Locale] -> LocalisedText -> Maybe TaggedText
localise' [] _ = Nothing
localise' (l:lr) lt@(LocalisedText tts) = case find (\tt -> ttLocale tt == l) tts of
  Nothing -> localise' lr lt
  mtt' -> mtt'