# Changelog for `de-calixtinus`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.9 - 2026-01-03

### Added

* Vía de la Plata
* Camino Sanabres
* Support for detailed trails via GeoJSON (major change)
* Airport location type
* Farm location type
* Station location type
* Industrial location type
* Access note type
* Half-board service type

### Changed

* More detailed software and data licence information
* Link to licences in about screen
* Key shows different trail types

## 0.8 - 2025-09-12

### Added

* Camino Invierno
* Elevation profiles
* Rest pressure for location selection
* Additional description for accommodation options
* Intermediate waypoints for wiggly legs
* Elevation and checking tools for camino builders

### Changed 

* Wharf location type

## 0.7 - 2025-07-04

### Added

* Camino del Norte
* Download spreadsheet version of camino
* Make caminos persistent with unique identifiers, so that they can be revisited and shared
* Map of all caminos

### Changed

* Map icons embedded in background to make them more obvious
  * Improved town and village icons
* Information point location type
* Casa rural accommodation type
* Labels for caminos/
* Direct rest day preferences towards required stops
* Move to lts-23.24 / GHC 9.8.4 stack

## 0.6 - 2025-03-23

### Added

* Rest days and pre-holiday "stock-up" days
  * This is still a work in progress. The system works by building days and then dividing the days into stages.
    A better solution would be to choose days based on the need for rest days.
    However, this turns out to be quite hard.
  * Also includes proposed travel dates so that Sundays and holidays can be tracked.
* Camino Primitivo
* Imports of camino segments

### Changed

* Added transport links to Camino Portuguese and more links to Bondi-Manly caminos
* Moved camino definitions to configuration file
* Region and holiday display
* Small graphic and layout improvements

### Fixed

* Fix to PoI category bug

## 0.5 - 2024-11-22

### Added

* Bondi-Manly walk
* Calendars, regions and locality-based public holidays.
* Allow users to use public transport to switch to places where
  they can get accommodation.
  This violates the spirit of the Camino but lots of people do it
  and it's necessary for the Bondi-Manly walk.
* Include time spent at points of interest during planning.
* Allow the user to pick points of interest from a menu.

### Changed

* Reworked opening hours to take account of holidays
* Additional location types for localities and points of interest

## 0.4 - 2024-07-07

### Added

* Camino Ingles
* Extended support for localisation within the camino data files and for dates and times
* Support an "Easy Mode" where you don't get asked detailed questions #[7](https://github.com/charvolant/de-calixtinus/issues/7)
* Support for Points of Interest and Events
* Detailed support for descriptive information

### Changed

* Multiple new location types to support points of interest
* Fisterra camino now includes Faro de Fisterra

## 0.3 - 2024-06-04

### Added

* Camino Frances
* Include Gîtes d'Étape accomodation type
* Show summary pages of the various caminos

### Changed

* MunicipalAlbergue is now PilgrimAlbergue
* Update ansible scripts for a development/testing server

## 0.2 - 2024-04-28

### Added

* Camino Fisterra
* Show chosen accommodation and penance for all locations
* Add a comfort level to condition accommodation and service choice #[4](https://github.com/charvolant/de-calixtinus/issues/4)

### Changed

* Route CSS is now embedded in final plan rather than added to the general camino.css
* Improved location of help and metric conversion
* Allow/Disallow camping either by configuration or by default #[5](https://github.com/charvolant/de-calixtinus/issues/5)
* Handling of stop location preferences in the same way as accommodation (see below) #[6](https://github.com/charvolant/de-calixtinus/issues/6)
* Make incoming/outgoing legs more obvious on location display

### Fixed

* Municipal albergues being ignored #[2](https://github.com/charvolant/de-calixtinus/issues/2)
* Consistent handling of accommodation/service preferences #[3](https://github.com/charvolant/de-calixtinus/issues/3)

## 0.1 - 2024-04-11

