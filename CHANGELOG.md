# Changelog for `de-calixtinus`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

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

### Chnaged

* MunicipalAlbergue is now PilgrimAlbergue
* Update ansible scripts for a development/testing server

## 0.2 - 2024-04-28

### Added

* Camino Fisterra
* Show chosen accommodation and penance for all locations
* Add a comfort level to condition accommodation and servoce choice #[4](https://github.com/charvolant/de-calixtinus/issues/4)

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

