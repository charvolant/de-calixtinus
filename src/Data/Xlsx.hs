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

Since cell positions are not know during construction, formulas use `CellID`s to encode references  to other cells.
These are resolved during spreadsheet construction.
-}

module Data.Xlsx (
    module Codec.Xlsx
  , module Data.Xlsx.Internal
) where

import Codec.Xlsx hiding (Cell(..), Comment(..), Formula(..), Worksheet(..), cellStyle, cellValue, cellComment, cellFormula)
import Data.Xlsx.Internal (
    Cell(..)
  , CellID(..)
  , CellIDStream
  , CellWidth(..)
  , Comment(..)
  , Formula(..)
  , Positionable(..)
  , Renderer
  , Slab(..)
  , Style(..)
  , Worksheet(..)

  , baseFont
  , cellComment
  , cellFormula
  , cellID
  , cellPos
  , cellStyle
  , cellText
  , cellValue
  , columnSlab
  , createXlsx
  , nextCellID
  , rowSlab
  , slabAddStyle
  , styleAlignment
  , styleBorder
  , styleFill
  , styleFont
  , styleNumberFormat
  , styleSheetFromMap
  , styleWidth
  , toAbsolute
  , toOffset

  , (>>!)
  , (>>-)
  )
