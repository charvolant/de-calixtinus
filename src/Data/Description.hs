{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Description
Description : Descriptive information
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Handle detailed, potentially localised, descriptive information
-}
module Data.Description (
    Description(..)
  , FormattedText(..)
  , Image(..)
  , Note(..)
  , NoteType(..)
  , TaggedFormattedText(..)
  , TextType(..)

  , descriptionSummary
  , firstLine
  , firstLineTagged
  , imageAttribution
  , wildcardDescription
  , wildcardFormattedText
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.!=), (.:), (.:?), (.=), object)
import Data.Aeson.Types (typeMismatch)
import Data.DublinCore
import Data.Localised
import Data.Maybe (fromJust)
import Data.Metadata
import Data.Text
import Network.URI

-- | The format for (potentially) complex text
data TextType =
    PlainText
  | HtmlText
  | MarkdownText
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
  
instance ToJSON TextType where
  toJSON PlainText = "plain"
  toJSON HtmlText = "html"
  toJSON MarkdownText = "markdown"

instance FromJSON TextType where
  parseJSON (String v) = do
    return $ case v of
      "plain" -> PlainText
      "html" -> HtmlText
      "markdown" -> MarkdownText
      _ -> error ("Uknown text type " ++ unpack v)
  parseJSON v = typeMismatch "expecting string" v
  
-- | Simple formatted text that can be embedded in a description and selectively used or laid out
data FormattedText = FormattedText TextType [Text]
  deriving (Show)

instance ToJSON FormattedText where
  toJSON (FormattedText PlainText [txt]) = toJSON txt
  toJSON (FormattedText fmt txts) = object [ "format" .= fmt, "text" .= txts ]
  
instance FromJSON FormattedText where
  parseJSON (String v) = do
    return $ FormattedText PlainText [v]
  parseJSON v@(Array _) = do
    txts' <- parseJSONList v
    return $ FormattedText PlainText txts'
  parseJSON (Object v) = do
    fmt' <- v .:? "format" .!= PlainText
    txts' <- v .: "text" 
    return $ FormattedText fmt' txts'
  parseJSON v = typeMismatch "expecting string, array or object" v

-- Doesn't account for format
plainFormattedText :: FormattedText -> Text
plainFormattedText (FormattedText _fmt' txts') = intercalate " " txts'

-- Doesn't account for format
appendFormattedText :: FormattedText -> Text -> FormattedText
appendFormattedText (FormattedText fmt' txts') txt = FormattedText fmt' (txts' ++ [txt])

-- Get the first line from a piece of formatted text
firstLine :: FormattedText -> Text
firstLine (FormattedText _ []) = ""
firstLine (FormattedText _ [one]) = one
firstLine (FormattedText _ (one:_)) = one

-- | A piece of formatted text tagged by a locale specification
data TaggedFormattedText = TaggedFormattedText Locale FormattedText 
  deriving (Show)

instance Tagged TaggedFormattedText where
  locale (TaggedFormattedText loc _) = loc
  plainText (TaggedFormattedText _ ftxt) = plainFormattedText ftxt
  fromText txt = TaggedFormattedText rootLocale (FormattedText PlainText [txt])
  addText (TaggedFormattedText loc ftxt) txt' = TaggedFormattedText loc (appendFormattedText ftxt txt')
  
instance FromJSON TaggedFormattedText where
  parseJSON (String v) = do
    let (locale', txt') = parseTagged v
    return $ TaggedFormattedText locale' (FormattedText PlainText [txt'])
  parseJSON (Object v) = do
    locale' <- v .:? "locale" .!= "*"
    ftxt' <- v .: "text"
    return $ TaggedFormattedText (localeFromIDOrError locale') ftxt'
  parseJSON v = typeMismatch "string or object" v

instance ToJSON TaggedFormattedText where
  toJSON (TaggedFormattedText locale' (FormattedText PlainText [txt'])) = toJSON $
      if locale' == rootLocale then txt' else txt' <> localeSeparator <> (localeID locale')
  toJSON (TaggedFormattedText locale' ftxt') = object [ "locale" .= localeID locale', "text" .= ftxt' ]

-- Construct a simple tagged formatted text from text
wildcardFormattedText :: Text -> TaggedFormattedText
wildcardFormattedText txt = TaggedFormattedText rootLocale (FormattedText PlainText [txt])

-- Get tagged formatted text that just contains the first line
firstLineTagged :: TaggedFormattedText -> TaggedText
firstLineTagged (TaggedFormattedText locale' ftxt) = TaggedText locale' (firstLine ftxt)

-- A type of note
data NoteType =
    Information -- ^ A generic piece of information
  | Warning -- ^ A warning about the thing being described
  | Address -- ^ An address
  | Directions -- ^ How to get there etc.
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON NoteType
instance ToJSON NoteType

-- | A note with a type and detailed information
data Note = Note {
    noteType :: NoteType -- ^ The type of note
  , noteText :: Localised TaggedFormattedText -- ^ The note substance
} deriving (Show)

instance FromJSON Note where
  parseJSON (String v) = do
    let (locale', txt') = parseTagged v
    return $ Note Information (Localised [TaggedFormattedText locale' (FormattedText PlainText [txt'])])
  parseJSON (Object v) = do
      type' <- v .:? "type" .!= Information
      txt' <- v .: "text"
      return $ Note type' txt'
  parseJSON v = typeMismatch "string or object" v

instance ToJSON Note where
  toJSON (Note Information (Localised [TaggedFormattedText locale' (FormattedText PlainText [txt'])])) = toJSON $ txt' <> "@" <> localeID locale'
  toJSON (Note type' txt') = object [ "type" .= type', "text" .= txt' ]
 
-- | An image descriptor
data Image = Image {
      imageSource :: URI
    , imageTitle :: Localised TaggedText
    , imageMetadata :: Maybe Metadata
  } deriving (Show)

instance TaggedLink Image where
  link = imageSource

instance FromJSON Image where
  parseJSON (Object v) = do
    source' <- v .: "source"
    title' <- v .: "title"
    metadata' <- v .:? "metadata"
    return $ Image (fromJust $ parseURI source') title' metadata'
  parseJSON v = typeMismatch "object" v

instance ToJSON Image where
  toJSON (Image source' title' metadata') = object [
        "source" .= uriToText source'
      , "title" .= title'
      , "metadata" .= metadata'
     ]

-- Construct an attribution statement
imageAttribution :: Image -> Text
imageAttribution image = let
        metadata = imageMetadata image
        rights = maybe Nothing (localisedValue dctermsRights [rootLocale]) metadata
        licence = maybe Nothing (localisedValue dctermsLicense [rootLocale]) metadata
        rights' = maybe "" plainText rights
        licence' = maybe "" plainText licence
      in
        replace "\"" "'" $ strip $ rights' <> " " <> licence'


-- | A full description of something, including links and other bits and pieces
--   The sumamry is intended for use instead of, rather than as well as, the descriptive text
--   However, if one or the other is missing, 
data Description = Description {
      descSummary :: Maybe (Localised TaggedText) -- ^ A short, single line summary
    , descText :: Maybe (Localised TaggedFormattedText) -- ^ Any descriptive text
    , descNotes :: [Note] -- ^ Additional notes and comments
    , descAbout :: Maybe (Localised TaggedURL) -- ^ A referencing URI, if resolvable then this can be make into a link
    , descImage :: Maybe Image -- ^ A link to an image
  }   
  deriving (Show)

instance FromJSON Description where
  parseJSON (String v) = do
    let (locale', text') = parseTagged v
    let localised' = Localised [TaggedFormattedText locale' (FormattedText PlainText [text'])]
    return $ Description Nothing (Just localised') [] Nothing Nothing
  parseJSON (Object v) = do
    summary' <- v .:? "summary"
    text' <- v .:? "text"
    notes' <- v .:? "notes" .!= []
    about' <- v .:? "about"
    image' <- v .:? "image"
    return $ Description summary' text' notes' about' image'
  parseJSON v = typeMismatch "string or object" v

instance ToJSON Description where
  toJSON (Description Nothing Nothing [] Nothing Nothing) = ""
  toJSON (Description Nothing (Just (Localised [text'])) [] Nothing Nothing) = toJSON text'
  toJSON (Description summary' text' notes' about' image') = object [
        "summary" .= summary'
      , "text" .= text'
      , "notes" .= if Prelude.null notes' then Nothing else Just notes'
      , "about" .= about'
      , "image" .= image'
    ]

-- | Create a localised instance from a piece of text
wildcardDescription :: Text -> Description
wildcardDescription txt = Description Nothing (Just (Localised [wildcardFormattedText txt])) [] Nothing Nothing

-- | Get a summary of the description
--   This is either the explicit summary, or the first line of the description text.
--   The result is an appropriate localised collection
descriptionSummary :: Description -> Localised TaggedText
descriptionSummary (Description Nothing Nothing _ _ _) = wildcardText ""
descriptionSummary (Description (Just summary) _ _ _ _) = summary
descriptionSummary (Description Nothing (Just (Localised ftexts)) _ _ _) = Localised (Prelude.map firstLineTagged ftexts)