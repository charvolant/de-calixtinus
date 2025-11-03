{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : JSON
Description : JSON formatting utilities
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX
-}
module Camino.Display.JSON (
    caminoPrintOptions
  , feature3PrintOptions
  , feature2PrintOptions
  , geojsonPrintOptions
) where

import Control.Lens
import Data.Aeson.Formatting
import Data.Default.Class
import Data.Aeson.Formatting (inlineAlways)

-- See https://www.dublincore.org/specifications/dublin-core/dcmi-terms
dctermsFieldOptions :: [FieldOptions]
dctermsFieldOptions = [
    def & foPath .~ "type" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "id" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "identifier" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "title"  & foRemove ?~ removeNullEmpty
  , def & foPath .~ "name"  & foRemove ?~ removeNullEmpty
  , def & foPath .~ "creator" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "contributor" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "date"  & foRemove ?~ removeNullEmpty
  , def & foPath .~ "created"  & foRemove ?~ removeNullEmpty
  , def & foPath .~ "modified"  & foRemove ?~ removeNullEmpty
  , def & foPath .~ "version" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "abstract"  & foRemove ?~ removeNullEmpty
  , def & foPath .~ "description" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "publisher" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "bibliographicCitation" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "language" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "source" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "rights" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "license" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "accessRights" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "rightsHolder" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "provenance" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "subject" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "spatial" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "temporal" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "tableOfContents" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "dateCopyrighted" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "dateSubmitted" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "dateAccepted" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "issed" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "valid" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "extent" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "format" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "medium" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "hasFormat" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "isFormatOf" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "conformsTo" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "coverage" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "audience" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "mediator" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "educationLevel" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "instructionalMethod" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "relation" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "alternative" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "hasPart" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "isPartOf" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "references" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "isReferencedBy" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "requires" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "isRequiredBy" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "hasVersion" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "isVersionOf" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "replaces" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "isReplacedBy" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "accrualMethod" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "accrualPeriodicity" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "accrualPolicy" & foRemove ?~ removeNullEmpty
  ]

latLongFieldOptions :: [FieldOptions]
latLongFieldOptions = [
  def & foPath .~ "latitude" & foNumberFormat ?~ fieldFixedFormat 5
  , def & foPath .~ "longitude" & foNumberFormat ?~ fieldFixedFormat 5
  , def & foPath .~ "elevation" & foRemove ?~ removeNull & foNumberFormat ?~ fieldIntFormat
  ]


legFieldOptions :: [FieldOptions]
legFieldOptions = [
    def & foPath .~ "type" & foRemove ?~ removeNull
  , def & foPath .~ "description" & foRemove ?~ removeNull
  , def & foPath .~ "from"
  , def & foPath .~ "to"
  , def & foPath .~ "distance" & foNumberFormat ?~ fieldFixedFormat 2
  , def & foPath .~ "time"  & foRemove ?~ removeNull & foNumberFormat ?~ fieldFixedFormat 1
  , def & foPath .~ "ascent" & foNumberFormat ?~ fieldIntFormat
  , def & foPath .~ "descent" & foNumberFormat ?~ fieldIntFormat
  , def & foPath .~ "penance"  & foRemove ?~ removeNull
  , def & foPath .~ "waypoints"  & foRemove ?~ removeNullEmpty & foChildren .~ [
      def & foPath .~ "[]" & foInline ?~ inlineAlways & foChildren .~ latLongFieldOptions
    ]
  ]

caminoPrintOptions :: PrintOptions
caminoPrintOptions = buildPrintOptions [
    def & foPath .~ "id" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "name"
  , def & foPath .~ "type" & foRemove ?~ removeNull
  , def & foPath .~ "locale" & foRemove ?~ removeNull
  , def & foPath .~ "summary" & foRemove ?~ removeNull
  , def & foPath .~ "title" & foRemove ?~ removeNull
  , def & foPath .~ "text" & foRemove ?~ removeNull
  , def & foPath .~ "description" & foRemove ?~ removeNull
  , def & foPath .~ "notes" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "about" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "url" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "uri" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "image" & foRemove ?~ removeNull
  , def & foPath .~ "position" & foRemove ?~ removeNull & foInline ?~ inlineAlways & foChildren .~ latLongFieldOptions
  , def & foPath .~ "srs" & foRemove ?~ removeNull
  , def & foPath .~ "region" & foRemove ?~ removeNull
  , def & foPath .~ "source" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "thumbnail" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "title" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "metadata" & foRemove ?~ removeNull
  , def & foPath .~ "namespaces" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "prefix"
  , def & foPath .~ "namespace"
  , def & foPath .~ "fragment" & foRemove ?~ removeNullFalse
  , def & foPath .~ "imports" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "locations" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "legs" & foChildren .~ legFieldOptions
  , def & foPath .~ "links" & foRemove ?~ removeNullEmpty & foChildren .~ legFieldOptions
  , def & foPath .~ "routes" & foChildren .~ [
      def & foPath .~ "id"
    , def & foPath .~ "name"
    , def & foPath .~ "description" & foRemove ?~ removeNull
    , def & foPath .~ "locations" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways & foWrap ?~ 10
    ]
  , def & foPath .~ "route-logic"
  , def & foPath .~ "default-route"
  , def & foPath .~ "major" & foRemove ?~ removeNullFalse
  , def & foPath .~ "stops" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways & foWrap ?~ 10
  , def & foPath .~ "condition" & foRemove ?~ removeNull & foChildren .~ [
      def & foPath .~ "*" & foInline ?~ inlineWhenLiterals & foWrap ?~ 10
    ]
  , def & foPath .~ "requires" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways & foWrap ?~ 10
  , def & foPath .~ "allows" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways & foWrap ?~ 10
  , def & foPath .~ "prohibits" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways & foWrap ?~ 10
  , def & foPath .~ "include" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways & foWrap ?~ 10
  , def & foPath .~ "exclude" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways & foWrap ?~ 10
  , def & foPath .~ "rest-points" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways & foWrap ?~ 10
  , def & foPath .~ "starts" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways & foWrap ?~ 10
  , def & foPath .~ "finishes" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways & foWrap ?~ 10
  , def & foPath .~ "suggested-pois" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways & foWrap ?~ 10
  , def & foPath .~ "palette"
  , def & foPath .~ "colour"
  , def & foPath .~ "text-colour" & foRemove ?~ removeNull
  , def & foPath .~ "features" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "dummy" & foRemove ?~ removeNullFalse
  , def & foPath .~ "geometry" & foRemove ?~ removeNull
  , def & foPath .~ "categories" & foRemove ?~ removeNull & foInline ?~ inlineAlways
  , def & foPath .~ "calendar" & foRemove ?~ removeNull
  , def & foPath .~ "months" & foRemove ?~ removeNull & foInline ?~ inlineAlways & foNumberFormat ?~ fieldIntFormat
  , def & foPath .~ "days" & foRemove ?~ removeNull & foInline ?~ inlineAlways
  , def & foPath .~ "hours" & foRemove ?~ removeNull
  , def & foPath .~ "time" & foRemove ?~ removeNull
  , def & foPath .~ "services" & foInline ?~ inlineAlways
  , def & foPath .~ "accommodation"
  , def & foPath .~ "pois" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "events" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "camping" & foRemove ?~ removeNull
  , def & foPath .~ "always-open" & foRemove ?~ removeNull
  , def & foPath .~ "sleeping" & foInline ?~ inlineAlways
  , def & foPath .~ "multi-day" & foRemove ?~ removeNull
  ]

-- Generic geojson, includes some foreign members that are useful
geojsonPrintOptions :: PrintOptions
geojsonPrintOptions = buildPrintOptions $ dctermsFieldOptions ++ [
    def & foPath .~ "type" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "id" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "name"  & foRemove ?~ removeNullEmpty
  , def & foPath .~ "properties" & foRemove ?~ removeNullEmpty & foChildren .~ dctermsFieldOptions
  , def & foPath .~ "crs" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "bbox" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways
  , def & foPath .~ "features" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "geometry" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "coordinates" & foRemove ?~ removeNullEmpty & foChildren .~ [
        def & foPath .~ "[]" & foInline ?~ inlineWhenLiterals & foChildren .~ [
            def & foPath .~ "[0, 1]" & foNumberFormat ?~ fieldFixedFormat 5
          , def & foPath .~ "[2, 3]" & foNumberFormat ?~ fieldIntFormat
          ]
      ]
  ]


-- Geojson as used by camino features (three layers deep), includes some foreign members that are useful
feature3PrintOptions :: PrintOptions
feature3PrintOptions = buildPrintOptions $ [
    def & foPath .~ "type" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "id" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "name"  & foRemove ?~ removeNullEmpty
  , def & foPath .~ "properties" & foRemove ?~ removeNullEmpty & foChildren .~ dctermsFieldOptions
  , def & foPath .~ "crs" & foRemove ?~ removeAlways
  , def & foPath .~ "bbox" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways
  , def & foPath .~ "features" & foRemove ?~ removeNull
  , def & foPath .~ "geometry" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "coordinates" & foRemove ?~ removeNullEmpty & foChildren .~ [
        def & foPath .~ "[]" & foInline ?~ inlineWhenLiterals & foChildren .~ [
          def & foPath .~ "[]" & foInline ?~ inlineWhenLiterals & foChildren .~ [
            def & foPath .~ "[0, 1]" & foNumberFormat ?~ fieldFixedFormat 5
          , def & foPath .~ "[2 .. 20]" & foRemove ?~ removeAlways
          ]
        ]
      ]
  ]


-- Geojson as used by camino features (two layers deep), includes some foreign members that are useful
feature2PrintOptions :: PrintOptions
feature2PrintOptions = buildPrintOptions $ [
    def & foPath .~ "type" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "id" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "name"  & foRemove ?~ removeNullEmpty
  , def & foPath .~ "properties" & foRemove ?~ removeNullEmpty & foChildren .~ dctermsFieldOptions
  , def & foPath .~ "crs" & foRemove ?~ removeAlways
  , def & foPath .~ "bbox" & foRemove ?~ removeNullEmpty & foInline ?~ inlineAlways
  , def & foPath .~ "features" & foRemove ?~ removeNull
  , def & foPath .~ "geometry" & foRemove ?~ removeNullEmpty
  , def & foPath .~ "coordinates" & foRemove ?~ removeNullEmpty & foChildren .~ [
      def & foPath .~ "[]" & foInline ?~ inlineWhenLiterals & foChildren .~ [
          def & foPath .~ "[0, 1]" & foNumberFormat ?~ fieldFixedFormat 5
        , def & foPath .~ "[2 .. 20]" & foRemove ?~ removeAlways
        ]
      ]
  ]