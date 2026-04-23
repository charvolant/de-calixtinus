{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Xlsx
Description : A model-based interface to the Xlsx codec
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

A more programmer-friendly interface to the `Codec.Xlsx` module.

Styles and cells can be accumulated on an ad-hoc basis and then converted into a complete spreadsheet.
This uses shadow data structures for Style, Formula, Cell and Worksheet, allowing them to be grouped together in
relative terms in `Slab`s.
The slabs can then be positioned relative to each other to contstruct a layout.

Since cell positions are not known during construction, formulas use `CellID`s to encode references  to other cells.
These are resolved during spreadsheet construction.
-}

module Data.Xlsx (
  module Codec.Xlsx
  -- * Slabs
  , Slab(..)
  , Positionable(..)
  , (>>!)
  , (>>-)
  , columnSlab
  , rowSlab
  , slabAddStyle
  -- * Cells
  , Cell(..)
  , CellWidth(..)
  , cellComment
  , cellFormula
  , cellID
  , cellPos
  , cellStyle
  , cellText
  , cellValue
  -- ** Cell identifiers
  , CellID(..)
  , CellIDStream
  , nextCellID
  , toAbsolute
  , toOffset
  -- ** Cell Elements
  , Comment(..)
  -- * Formulas
  , Formula(..)
  -- * Styling
  , Style(..)
  , baseFont
  , styleAlignment
  , styleBorder
  , styleFill
  , styleFont
  , styleNumberFormat
  , styleSheetFromMap
  , styleWidth
  -- * Worksheets
  , Renderer
  , Worksheet(..)
  , createXlsx
) where

import Codec.Xlsx hiding (Cell(..), Comment(..), Formula(..), Worksheet(..), cellStyle, cellValue, cellComment, cellFormula)
import Data.Xlsx.Internal
