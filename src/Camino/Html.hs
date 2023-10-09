{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : Html
Description : Produce HTML descriptions map of caminos and trips
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Generate HTML descriptions of 
-}
module Camino.Html where

import Camino.Camino
import Camino.Planner (Trip(..), Day(..), Metrics(..))
import Graph.Programming (score)
import Text.Hamlet
import qualified Data.Text as T (intercalate, null, pack)
import Formatting

daySummary :: Preferences -> Camino -> Maybe Trip -> Day -> Html
daySummary _preferences _camino _trip day = [shamlet|
    <p>#{locationName $ start day} to #{locationName $ finish day} #{time}hrs #{distance}km (feels like #{perceivedDistance}km)
    <p>#{T.intercalate ", " (map locationName ((map legFrom $ path day) ++ [finish day]))}
  |]
  where
    time = format (fixed 1) (metricsTime $ score day)
    distance = format (fixed 1) (metricsDistance $ score day)
    perceivedDistance = format (fixed 1) (metricsPerceivedDistance $ score day)


tripSummary :: Preferences -> Camino -> Trip -> Html
tripSummary preferences camino trip = [shamlet|
    <h1>From #{locationName $ start trip} to #{locationName $ finish trip}
    <h2>Stages
    <ul>
    $forall day <- path trip
      <ol>#{locationName $ start day} - #{locationName $ finish day}
  |]

locationSummary :: Preferences -> Camino -> Location -> Html
locationSummary _preferences _camino location = [shamlet|
    $if not $ T.null services
      <p>Services: #{services}
    $if not $ T.null accommodation
      <p>Accomodation: #{accommodation}
  |]
  where 
    services = T.intercalate ", " (map (\s -> T.pack $ show s) (locationServices location))
    accommodation = T.intercalate ", " (map (\a -> T.pack $ show $ accommodationType a) (locationAccommodation location))
  