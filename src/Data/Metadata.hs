{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Metadata
Description : Dublin-core style metadata for data sources
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

A module that handles lists of metadata-style statements using URI-based terms.
The expection is that this can be embedded in other data types that use JSON-style
-}
module Data.Metadata (
  Metadata(..),
  Namespace(..),
  Statement(..),

  decodeTerm,
  defaultMetadata,
  encodeTerm,
  statementLabel,
  statementLang,
  statementTerm,
  statementValue
) where

import Data.Aeson
import Data.List (find)
import Data.Maybe (fromJust)
import qualified Data.Text as T (Text, concat, isPrefixOf, null, split, stripPrefix, stripSuffix, unpack, pack)
import Network.URI

data Namespace = Namespace {
  namespacePrefix :: T.Text, -- ^ The prefix to use
  namespaceUri :: T.Text -- ^ The corresponding namespace URI
} deriving (Show)

instance FromJSON Namespace where
  parseJSON (Object v) = do
    prefix' <- v .: "prefix"
    namespace' <- v .: "namespace"
    return (Namespace (T.concat [prefix', ":"]) namespace')
  parseJSON v = error ("Unable to parse namespace " ++ show v)

instance ToJSON Namespace where
  toJSON (Namespace prefix' namespace') = object [ "prefix" .= (fromJust $ T.stripSuffix ":" prefix'), "namespace" .= namespace' ]

decodeTerm :: [Namespace] -> T.Text -> Maybe URI
decodeTerm namespaces term =
  let
    namespace = find (\ns -> T.isPrefixOf (namespacePrefix ns) term) namespaces
    normalised = maybe term (\ns -> T.concat [(namespaceUri ns), (fromJust $ T.stripPrefix (namespacePrefix ns) term)]) namespace
  in
    parseURI $ T.unpack normalised

encodeTerm :: [Namespace] -> URI -> T.Text
encodeTerm namespaces term =
  let
    term' = T.pack $ (uriToString id term) ""
    namespace = find (\ns -> T.isPrefixOf (namespaceUri ns) term') namespaces
  in
    maybe term' (\ns -> T.concat [(namespacePrefix ns), (fromJust $ T.stripPrefix (namespaceUri ns) term')]) namespace

data Statement =
  Statement URI T.Text (Maybe T.Text) -- ^ A proper statement
  | RawStatement T.Text T.Text (Maybe T.Text) -- ^ A statement that needs to be decoded
  deriving (Show)

instance FromJSON Statement where
  parseJSON (Object v) = do
    term' <- v .: "term"
    value' <- v .: "value"
    lang' <- v .:? "lang" .!= Nothing
    return (RawStatement term' value' lang')
  parseJSON v = error ("Unable to parse namespace " ++ show v)

instance ToJSON Statement where
  toJSON (RawStatement term' value' lang') = object [ "term" .= term', "value" .= value', "lang" .= lang' ]
  toJSON (Statement term' value' lang') = object [ "term" .= (uriToString id term') "", "value" .= value', "lang" .= lang' ]

fromRawStatement _namespaces s@(Statement _ _ _) = s
fromRawStatement namespaces (RawStatement term value lang) = Statement (fromJust $ decodeTerm namespaces term) value lang

toRawStatement :: [Namespace] -> Statement -> Statement
toRawStatement _namespaces r@(RawStatement _ _ _) = r
toRawStatement namespaces (Statement term value lang) = RawStatement (encodeTerm namespaces term) value lang

statementTerm :: Statement -> URI
statementTerm (RawStatement term _value _lang) = maybe nullURI id (parseURI $ T.unpack term)
statementTerm (Statement term _value _lang) = term

statementValue :: Statement -> T.Text
statementValue (RawStatement _term value _lang) = value
statementValue (Statement _term value _lang) = value

statementLang :: Statement -> Maybe T.Text
statementLang (RawStatement _term _value lang) = lang
statementLang  (Statement _term _value lang) = lang

statementLabel :: Statement -> T.Text
statementLabel statement =
  let
    url = case statement of
      (RawStatement term _ _) -> term
      (Statement term _ _) -> T.pack $ (uriToString id term) ""
    broken = dropWhile T.null (T.split (\c -> c == '#' || c == '/') url)
  in
    if length broken == 0 then url else last broken

data Metadata = Metadata {
  metadataNamespaces :: [Namespace], -- ^ The list of namespaces
  metadataStatements :: [Statement] -- ^ The list of metadata statements
} deriving (Show)

instance FromJSON Metadata where
  parseJSON (Object v) = do
    namespaces' <- v .:? "namespaces" .!= []
    statements' <- v .: "statements"
    let statements'' =  map (fromRawStatement namespaces') statements'
    return (Metadata { metadataNamespaces = namespaces', metadataStatements = statements'' })
  parseJSON v = error ("Unable to parse metadata object " ++ show v)

instance ToJSON Metadata where
  toJSON (Metadata namespaces' statements') =
    let
      statements'' = map (toRawStatement namespaces') statements'
    in
      object [ "namespaces" .= namespaces', "statements" .= statements'' ]


-- | A default metadata instance with common namespaces
defaultMetadata :: Metadata
defaultMetadata = Metadata {
    metadataNamespaces = [
      Namespace "dc:" "http://purl.org/dc/elements/1.1/",
      Namespace "dcterms:" "http://purl.org/dc/terms/"
    ],
    metadataStatements = []
  }
