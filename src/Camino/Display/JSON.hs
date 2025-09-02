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
) where

import Control.Lens
import Data.Aeson.Formatting
import Data.Default.Class

caminoPrintOptions :: PrintOptions
caminoPrintOptions = buildPrintOptions [
    def & foName .~ "id" & foRemove .~ removeNullEmpty
  , def & foName .~ "name"
  , def & foName .~ "type" & foRemove .~ removeNull
  , def & foName .~ "locale" & foRemove .~ removeNull
  , def & foName .~ "summary" & foRemove .~ removeNull
  , def & foName .~ "title" & foRemove .~ removeNull
  , def & foName .~ "text" & foRemove .~ removeNull
  , def & foName .~ "description" & foRemove .~ removeNull
  , def & foName .~ "notes" & foRemove .~ removeNullEmpty
  , def & foName .~ "about" & foRemove .~ removeNullEmpty
  , def & foName .~ "url" & foRemove .~ removeNullEmpty
  , def & foName .~ "uri" & foRemove .~ removeNullEmpty
  , def & foName .~ "image" & foRemove .~ removeNull
  , def & foName .~ "position" & foRemove .~ removeNull & foInline .~ inlineAlways
  , def & foName .~ "latitude" & foNumberFormat .~ fieldFixedFormat 5
  , def & foName .~ "longitude" & foNumberFormat .~ fieldFixedFormat 5
  , def & foName .~ "elevation" & foRemove .~ removeNull & foNumberFormat .~ fieldIntFormat
  , def & foName .~ "srs" & foRemove .~ removeNull
  , def & foName .~ "region" & foRemove .~ removeNull
  , def & foName .~ "source" & foRemove .~ removeNullEmpty
  , def & foName .~ "thumbnail" & foRemove .~ removeNullEmpty
  , def & foName .~ "title" & foRemove .~ removeNullEmpty
  , def & foName .~ "metadata" & foRemove .~ removeNull
  , def & foName .~ "namespaces" & foRemove .~ removeNullEmpty
  , def & foName .~ "prefix"
  , def & foName .~ "namespace"
  , def & foName .~ "fragment" & foRemove .~ removeNullFalse
  , def & foName .~ "imports" & foRemove .~ removeNullEmpty
  , def & foName .~ "locations" & foRemove .~ removeNullEmpty & foInline .~ inlineWhenLiterals & foWrap ?~ 10
  , def & foName .~ "legs"
  , def & foName .~ "links" & foRemove .~ removeNullEmpty
  , def & foName .~ "routes"
  , def & foName .~ "route-logic"
  , def & foName .~ "default-route"
  , def & foName .~ "condition"
  , def & foName .~ "requires" & foRemove .~ removeNullEmpty & foInline .~ inlineAlways & foWrap ?~ 10
  , def & foName .~ "allows" & foRemove .~ removeNullEmpty & foInline .~ inlineAlways & foWrap ?~ 10
  , def & foName .~ "prohibits" & foRemove .~ removeNullEmpty & foInline .~ inlineAlways & foWrap ?~ 10
  , def & foName .~ "include" & foRemove .~ removeNullEmpty & foInline .~ inlineAlways & foWrap ?~ 10
  , def & foName .~ "exclude" & foRemove .~ removeNullEmpty & foInline .~ inlineAlways & foWrap ?~ 10
  , def & foName .~ "major" & foRemove .~ removeNullFalse
  , def & foName .~ "stops" & foRemove .~ removeNullEmpty & foInline .~ inlineAlways & foWrap ?~ 10
  , def & foName .~ "rest-points" & foRemove .~ removeNullEmpty & foInline .~ inlineAlways & foWrap ?~ 10
  , def & foName .~ "starts" & foRemove .~ removeNullEmpty & foInline .~ inlineAlways & foWrap ?~ 10
  , def & foName .~ "finishes" & foRemove .~ removeNullEmpty & foInline .~ inlineAlways & foWrap ?~ 10
  , def & foName .~ "suggested-pois" & foRemove .~ removeNullEmpty & foInline .~ inlineAlways & foWrap ?~ 10
  , def & foName .~ "palette"
  , def & foName .~ "colour"
  , def & foName .~ "text-colour" & foRemove .~ removeNull
  , def & foName .~ "from"
  , def & foName .~ "to"
  , def & foName .~ "distance" & foNumberFormat .~ fieldFixedFormat 2
  , def & foName .~ "time"  & foRemove .~ removeNull & foNumberFormat .~ fieldFixedFormat 1
  , def & foName .~ "ascent" & foNumberFormat .~ fieldIntFormat
  , def & foName .~ "descent" & foNumberFormat .~ fieldIntFormat
  , def & foName .~ "penance"  & foRemove .~ removeNull
  , def & foName .~ "waypoints"  & foRemove .~ removeNullEmpty & foInlineChildren .~ inlineAlways
  , def & foName .~ "categories" & foRemove .~ removeNull & foInline .~ inlineAlways
  , def & foName .~ "calendar" & foRemove .~ removeNull
  , def & foName .~ "months" & foRemove .~ removeNull & foInline .~ inlineAlways & foNumberFormat .~ fieldIntFormat
  , def & foName .~ "days" & foRemove .~ removeNull & foInline .~ inlineAlways
  , def & foName .~ "hours" & foRemove .~ removeNull
  , def & foName .~ "time" & foRemove .~ removeNull
  , def & foName .~ "services" & foInline .~ inlineAlways
  , def & foName .~ "accommodation" & foInline .~ inlineWhenLiterals & foWrap ?~ 10
  , def & foName .~ "pois" & foRemove .~ removeNullEmpty
  , def & foName .~ "events" & foRemove .~ removeNullEmpty
  , def & foName .~ "camping" & foRemove .~ removeNull
  , def & foName .~ "always-open" & foRemove .~ removeNull
  , def & foName .~ "sleeping" & foInline .~ inlineAlways
  , def & foName .~ "multi-day" & foRemove .~ removeNull
  ]