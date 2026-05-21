{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Description
Description : Descriptive information
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Descriptive information

Handle detailed, potentially localised, descriptive information.
This information is so common that it gets its own collection.
-}
module Data.Description (
  -- * Descriptive Information
    Description(..)
  , descriptionSummary
  , wildcardDescription
  -- ** Images
  , Image(..)
  , imageAttribution
  , imageOrigin
  , imageToLink
  -- ** Notes
  , Note(..)
  , NoteType(..)
) where

import GHC.Generics (Generic)
import Control.DeepSeq
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), (.?=), (.!=), (.:), (.:?), (.=), object, pairs)
import Data.Aeson.Types (typeMismatch)
import Data.DublinCore
import Data.Localised
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Metadata
import Data.Text
import Network.URI

-- | The type of note
--
--   The note type is intended to give a quick guide as to how important a note is and its general content
data NoteType =
    Information -- ^ A generic piece of information
  | Warning -- ^ A warning about the thing being described
  | Barrier -- ^ Something that means that you might not be able to get to something or do something (more serious than a warning)
  | Address -- ^ An address
  | Directions -- ^ How to get there etc.
  | Access -- ^ How to access something
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded)

instance FromJSON NoteType
instance ToJSON NoteType
instance NFData NoteType

-- | A note with a type and detailed information
--
--   Notes represent optional pieces of additional information that can be displayed to provide more detail
data Note = Note {
    noteType :: NoteType -- ^ The type of note
  , noteText :: Localised TaggedText -- ^ The note substance
} deriving (Show, Generic)

instance FromJSON Note where
  parseJSON (String v) = do
    let (locale', txt') = parseTagged v
    return $ Note Information (Localised [TaggedText locale' txt'])
  parseJSON (Object v) = do
      type' <- v .:? "type" .!= Information
      txt' <- v .: "text"
      return $ Note type' txt'
  parseJSON v = typeMismatch "string or object" v

instance ToJSON Note where
  toJSON (Note Information (Localised [TaggedText locale' txt'])) =
    toJSON $ txt' <> (if locale' == rootLocale then "" else "@" <> localeID locale')
  toJSON (Note type' txt') =
    object [
        "type" .= type'
       , "text" .= txt'
       ]
  toEncoding (Note Information (Localised [TaggedText locale' txt'])) =
    toEncoding $ txt' <> "@" <> localeID locale'
  toEncoding (Note type' txt') =
    pairs $
        "type" .= type'
      <> "text" .= txt'

instance NFData Note

-- | An image description and location
--
--   Images come as web links to external graphics.
--   Good practice is to supply metadata with acknowledgements, licensing and other information that can be displayed alongside the image.
data Image = Image {
      imageSource :: URI -- ^ The main image location
    , imageThumbnail :: Maybe URI -- ^ A thumbnail image location
    , imageTitle :: Localised TaggedText -- ^ A localised title
    , imageMetadata :: Maybe Metadata -- ^ Optional metadata giving ownership, licence etc.
  } deriving (Show, Generic)

instance FromJSON Image where
  parseJSON (Object v) = do
    source' <- v .: "source"
    thumbnail' <- v .:? "thumbnail"
    title' <- v .: "title"
    metadata' <- v .:? "metadata"
    return $ Image (textToUri source') (textToUri <$> thumbnail') title' metadata'
  parseJSON v = typeMismatch "object" v

instance ToJSON Image where
  toJSON (Image source' thumbnail' title' metadata') =
    object [
        "source" .= uriToText source'
      , "thumbnail" .= (uriToText <$> thumbnail')
      , "title" .= title'
      , "metadata" .= metadata'
      ]
  toEncoding (Image source' thumbnail' title' metadata') =
    pairs $
        "source" .= uriToText source'
      <> "thumbnail" .?= (uriToText <$> thumbnail')
      <> "title" .= title'
      <> "metadata" .?= metadata'

instance NFData Image

joinText :: Text -> Maybe TaggedText -> Maybe Text -> Maybe Text
joinText sep mtt mtxt = maybe mtxt (\tt -> Just $ maybe (plainText tt) (\txt -> txt <> sep <> plainText tt) mtxt) mtt

-- | Construct an attribution statement, if there is suitable data in the image metadata
imageAttribution :: Image -> Maybe Text
imageAttribution image = let
      mmetadata = imageMetadata image
     in
      if isNothing mmetadata then
        Nothing
      else let
          metadata = fromJust mmetadata
          citation = localisedValue [dctermsBibliographicCitation] [rootLocale] metadata
          rights = localisedValue [dctermsRights, dcRights] [rootLocale] metadata
          creator = localisedValue [dctermsCreator, dcCreator] [rootLocale] metadata
          rightsHolder = localisedValue [dctermsRightsHolder] [rootLocale] metadata
          licence = localisedValue [dctermsLicense] [rootLocale] metadata
          created = localisedValue [dctermsCreated] [rootLocale] metadata
        in
          if isJust citation then
            plainText <$> citation
          else if isJust rights then
            plainText <$> rights
          else
            joinText ", " created $ joinText " via " rightsHolder $ joinText ", " licence $ joinText "" creator Nothing

-- | Get the origin of the image, if available, from the metadata
imageOrigin :: Image -> Maybe Text
imageOrigin image = plainText <$> maybe Nothing (localisedValue [dctermsSource, dcSource] [rootLocale]) (imageMetadata image)

-- | Convert an image to a hyperlink
imageToLink :: Bool -> Image -> TaggedURL
imageToLink thumb image = let
    url = if
        thumb
      then
        maybe (imageSource image) id (imageThumbnail image)
      else
        imageSource image
  in
    TaggedURL rootLocale (Hyperlink url Nothing)

-- | A full description of something, including links and other bits and pieces
--
--   The summary is intended for use instead of, rather than as well as, the descriptive text
--   However, if one or the other is missing, the summary or full text will be constructed.
data Description = Description {
      descSummary :: Maybe (Localised TaggedText) -- ^ A short, single line summary
    , descText :: Maybe (Localised TaggedText) -- ^ Any descriptive text
    , descNotes :: [Note] -- ^ Additional notes and comments
    , descAbout :: Maybe (Localised TaggedURL) -- ^ A referencing URI, if resolvable then this can be made into a link
    , descImage :: Maybe Image -- ^ A link to an image that provides additional graphical support for the description
  }   
  deriving (Show, Generic)

instance FromJSON Description where
  parseJSON (String v) = do
    let (locale', text') = parseTagged v
    let localised' = Localised [TaggedText locale' text']
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
  toJSON (Description Nothing Nothing [] Nothing Nothing) = String ""
  toJSON (Description Nothing (Just (Localised [text'])) [] Nothing Nothing) = toJSON text'
  toJSON (Description summary' text' notes' about' image') =
    object [
        "summary" .= summary'
      , "text" .= text'
      , "notes" .= (if Prelude.null notes' then Nothing else Just notes')
      , "about" .= about'
      , "image" .= image'
      ]
  toEncoding (Description Nothing Nothing [] Nothing Nothing) = ""
  toEncoding (Description Nothing (Just (Localised [text'])) [] Nothing Nothing) = toEncoding text'
  toEncoding (Description summary' text' notes' about' image') =
    pairs $
        "summary" .?= summary'
      <> "text" .?= text'
      <> "notes" .?= (if Prelude.null notes' then Nothing else Just notes')
      <> "about" .?= about'
      <> "image" .?= image'

instance NFData Description

-- | Create a description from a piece of text
--
--   The resulting description just has a piece of wildcard text.
wildcardDescription :: Text -> Description
wildcardDescription txt = Description Nothing (Just (wildcardText txt)) [] Nothing Nothing

-- | Get a summary of the description
--
--   This is either the explicit summary, or the first line of the description text.
--   The result is an appropriate localised collection.
--
--  TODO The summary should just be the first sentence of the description text, if constructed.
descriptionSummary :: Description -> Localised TaggedText
descriptionSummary (Description Nothing Nothing _ _ _) = wildcardText ""
descriptionSummary (Description (Just summary) _ _ _ _) = summary
descriptionSummary (Description Nothing (Just txt) _ _ _) = txt