{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-|
Module      : Data.Xlsx.Internal
Description : Implementation of Data.Xslx
Copyright   : (c) Doug Palmer, 2025
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Implementation of the `Data.Xlsx` module

Styles and cells can be accumulated on an ad-hoc basis and then converted into a
complete
-}

module Data.Xlsx.Internal where

import GHC.Generics (Generic)
import Codec.Xlsx hiding (Cell(..), Comment(..), Formula(..), Worksheet(..), cellStyle, cellValue, cellComment, cellFormula)
import qualified Codec.Xlsx as X (Cell(..), Comment(..), Worksheet(..), cellStyle, cellValue, cellComment, cellFormula)
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.State
import Data.Default.Class
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, isNothing, mapMaybe)
import Data.Text (Text, intercalate, null, pack)
import qualified Data.Text as T (length)
import Data.Util (unique, maybeMin, headWithDefault)

-- | Render a cell message type into text
type Renderer t = t -> Text

-- | A positionable item.
--   Positionable items can be placed in matrices, with a (row, column) pair indicating where the element goes.
--   Positions are relative to an enclosing item and start at (0, 0)
class Positionable a where
  -- | Get the relative position of an item
  pos :: a -> (Int, Int)

  -- Test to see if this is the null (0, 0) position
  nullPos :: a -> Bool
  nullPos v = pos v == (0, 0)

  -- | Compute the absolute (row, column) pair
  absolute :: Int -- ^ The base row position
    -> Int -- ^ The base column position
    -> a -- ^ The item to position
    -> (RowIndex, ColumnIndex) -- ^ The absolute position relative to the base
  absolute row column v = (RowIndex $ row + fst p, ColumnIndex $ column + snd p) where p = pos v

-- | Set a cell width
data CellWidth =
    WidthNone -- ^ The default fit, asccepting any
  | WidthFixed Int -- ^ Fixed width (a minimum, given other widths in a column)
  | WidthExpand -- ^ Expands from the base width to fit the expected size
  | WidthExpandMax Int -- ^ Expands to a maximum width, based on the number of standard text characters
  deriving (Eq, Ord, Show, Generic)

-- | Style information.
--   Styles obey semigroup rules, where `a <> b` replaces any style in a with one in b if present
data Style = Style {
    _styleAlignment :: Maybe Alignment
  , _styleBorder :: Maybe Border
  , _styleFill :: Maybe Fill
  , _styleFont :: Maybe Font
  , _styleNumberFormat :: Maybe NumberFormat
  , _styleWidth :: Maybe CellWidth
} deriving (Eq, Ord, Show, Generic)

instance Default Style where
    def = Style Nothing Nothing Nothing Nothing Nothing Nothing

joinAlignment :: Alignment -> Alignment -> Alignment
joinAlignment a b =
  Alignment {
      _alignmentHorizontal = _alignmentHorizontal b <|> _alignmentHorizontal a
    , _alignmentIndent = _alignmentIndent b <|> _alignmentIndent a
    , _alignmentJustifyLastLine = _alignmentJustifyLastLine b <|> _alignmentJustifyLastLine a
    , _alignmentReadingOrder = _alignmentReadingOrder b <|> _alignmentReadingOrder a
    , _alignmentRelativeIndent = _alignmentRelativeIndent b <|> _alignmentRelativeIndent a
    , _alignmentShrinkToFit = _alignmentShrinkToFit b <|> _alignmentShrinkToFit a
    , _alignmentTextRotation = _alignmentTextRotation b <|> _alignmentTextRotation a
    , _alignmentVertical = _alignmentVertical b <|> _alignmentVertical a
    , _alignmentWrapText = _alignmentWrapText b <|> _alignmentWrapText a
  }

joinBorder :: Border -> Border -> Border
joinBorder a b =
  Border {
      _borderDiagonalDown = _borderDiagonalDown b <|> _borderDiagonalDown a
    , _borderDiagonalUp = _borderDiagonalUp b <|> _borderDiagonalUp a
    , _borderOutline = _borderOutline b <|> _borderOutline a
    , _borderBottom = _borderBottom b <|> _borderBottom a
    , _borderDiagonal = _borderDiagonal b <|> _borderDiagonal a
    , _borderEnd = _borderEnd b <|> _borderEnd a
    , _borderHorizontal = _borderHorizontal b <|> _borderHorizontal a
    , _borderLeft = _borderLeft b <|> _borderLeft a
    , _borderRight = _borderRight b <|> _borderRight a
    , _borderStart = _borderStart b <|> _borderStart a
    , _borderTop = _borderTop b <|> _borderTop a
    , _borderVertical = _borderVertical b <|> _borderVertical a
  }

joinFill :: Fill -> Fill -> Fill
joinFill a b =
  Fill {
    _fillPattern = _fillPattern b <|> _fillPattern a
  }

joinFont :: Font -> Font -> Font
joinFont a b =
  Font {
      _fontBold = _fontBold b <|> _fontBold a
    , _fontCharset = _fontCharset b <|> _fontCharset a
    , _fontColor = _fontColor b <|> _fontColor a
    , _fontCondense = _fontCondense b <|> _fontCondense a
    , _fontExtend = _fontExtend b <|> _fontExtend a
    , _fontFamily = _fontFamily b <|> _fontFamily a
    , _fontItalic = _fontItalic b <|> _fontItalic a
    , _fontName = _fontName b <|> _fontName a
    , _fontOutline = _fontOutline b <|> _fontOutline a
    , _fontScheme = _fontScheme b <|> _fontScheme a
    , _fontShadow = _fontShadow b <|> _fontShadow a
    , _fontStrikeThrough = _fontStrikeThrough b <|> _fontStrikeThrough a
    , _fontSize = _fontSize b <|> _fontSize a
    , _fontUnderline = _fontUnderline b <|> _fontUnderline a
    , _fontVertAlign = _fontVertAlign b <|> _fontVertAlign a
  }

instance Semigroup Style where
  a <> b = Style {
     _styleAlignment = js joinAlignment (_styleAlignment a) (_styleAlignment b)
   , _styleBorder = js joinBorder (_styleBorder a) (_styleBorder b)
   , _styleFill = js joinFill (_styleFill a) (_styleFill b)
   , _styleFont = js joinFont (_styleFont a) (_styleFont b)
   , _styleNumberFormat = _styleNumberFormat b <|> _styleNumberFormat a
   , _styleWidth = _styleWidth b <|> _styleWidth a
  } where
    js _ Nothing Nothing = Nothing
    js _ av@(Just _) Nothing = av
    js _ Nothing bv@(Just _) = bv
    js joiner (Just av) (Just bv) = Just $ joiner av bv

instance Monoid Style where
  mempty = def

makeLenses ''Style

data StyleMap = StyleMap {
    styleMapCells :: [CellXf]
  , styleMapCellMap :: M.Map Style Int
  , styleMapBorders :: [Border]
  , styleMapBorderMap :: M.Map Border Int
  , styleMapFills :: [Fill]
  , styleMapFillMap :: M.Map Fill Int
  , styleMapFonts :: [Font]
  , styleMapFontMap :: M.Map Font Int
  , styleMapFormats :: [NumberFormat]
  , styleMapFormatMap :: M.Map NumberFormat Int
  , styleMapNumFmts :: M.Map Int FormatCode
}

data CellID =
    CellID Text Int -- ^ Base cell identifier
  | CellRangeID CellID CellID -- ^ A range between two cells
  | CellAbsoluteID CellID -- ^ Absolute version of a cell identifier
  | CellOffsetID Int Int CellID -- ^ Offset version of a cell identifier
  | CellSubrangeID (Maybe Int) (Maybe Int) (Maybe Int) (Maybe Int) CellID -- ^ A subrange of cell IDs (offset row, offset col, row width, col width)
  deriving (Eq, Ord, Show)

type CellIDStream a = State Int a

-- | Get the next relative cell identifier from a `CellIDStream`
nextCellID :: Text -> CellIDStream CellID
nextCellID label = do
  v <- get
  modify (+1)
  return (CellID label v)

toAbsolute :: CellID -> CellID
toAbsolute cid = CellAbsoluteID cid

toOffset :: Int -> Int -> CellID -> CellID
toOffset ro co cid = CellOffsetID ro co cid

toColumn :: Int -> CellID -> CellID
toColumn co cid = CellSubrangeID Nothing (Just co) Nothing (Just 1) cid

-- A comment, based on a list of messages messages
data Comment t = Comment (Maybe Text) [t]
  deriving (Show)

-- | A formula, based on cell references
data Formula =
    FVar Text -- ^ A variable name (for LETs and the like)
  | FBool Bool -- ^ A literal boolean value
  | FInt Int -- ^ A literal integer value
  | FDouble Double -- ^ A literal double value
  | FText Text -- ^ A literal text/string value
  | FRef CellID -- ^ A reference to a cell
  | FApply Text [Formula] -- ^ Function application
  deriving (Show, Eq)

-- | An expanded cell that can be converted into the sort of index-based positioning that a @Slab needs
--   Cells can contain a message type, based on Yesod i18n, that can later be expanded to text.
--   The message type allows you to internationalise building a spreadsheet, rendering it into locale-appropriate
--   text once the sheet structure has been built.
data Cell t = Cell {
    _cellPos :: (Int, Int) -- ^ The cell position, see @Positionable
  , _cellID :: Maybe CellID
  , _cellStyle :: Maybe Style -- ^ Any style information
  , _cellValue :: Maybe CellValue -- ^ The cell value
  , _cellText :: Maybe t -- ^ I18n renderable text
  , _cellComment :: Maybe (Comment t) -- ^ A comment (currently not working)
  , _cellFormula :: Maybe Formula -- ^ A formula
} deriving (Show, Generic)

instance Positionable (Cell t) where
  pos = _cellPos

instance Default (Cell t) where
    def = Cell (0, 0) Nothing Nothing Nothing Nothing Nothing Nothing

makeLenses ''Cell

-- Test to see if a cell is empty, containing no values or styling or styling but nothing to style
nullCell :: Cell t -> Bool
nullCell (Cell _ Nothing Nothing Nothing Nothing Nothing Nothing) = True
nullCell (Cell _ Nothing (Just (Style _ Nothing Nothing _ _ _)) Nothing Nothing Nothing Nothing) = True
nullCell _ = False

-- | A slab of cells
-- These can be used to compose groups of cells into larger layouts
data Slab c =
    SEmpty -- ^ An empty slab of cells, usefull for accumulation
  | SCells Int Int [c] -- ^ A sequence of cells, assumed to have a size and a (0, 0) origin
  | SMargin Int Int (Slab c) -- ^ A slab of cells with a margin offset
  | SSize Int Int (Slab c) -- ^ Ensure a slab of cells has a specific size
  | SRight (Slab c) (Slab c) -- ^ Two slabs with the second immediately to the right of the first
  | SBelow (Slab c) (Slab c) -- ^ Two slabs with the second immediately below the first
  | SRow Style (Slab c) -- ^ A row with a default row style
  deriving (Show)


instance Functor Slab where
  -- | Map the elements of a slab onto a slab with a similar structure
  fmap :: (a -> b) -- ^ The transformation
   -> Slab a -- ^ The source slab
   -> Slab b -- ^ The resulting slab
  fmap _f SEmpty = SEmpty
  fmap f (SCells rs cs cells) = SCells rs cs $ map f cells
  fmap f (SMargin ro co slab) = SMargin ro co $ fmap f slab
  fmap f (SSize rs cs slab) = SSize rs cs $ fmap f slab
  fmap f (SRight slab1 slab2) = SRight (fmap f slab1) (fmap f slab2)
  fmap f (SBelow slab1 slab2) = SBelow (fmap f slab1) (fmap f slab2)
  fmap f (SRow style slab) = SRow style $ fmap f slab

-- | Above operator, argument 1 is above argument 2
infixr 1 >>!
(>>!) :: Slab c -> Slab c -> Slab c
SEmpty >>! slab = slab
slab >>! SEmpty = slab
slab1 >>! slab2 = SBelow slab1 slab2

-- | Left of operator, argument 1 is left of argument 2
infixr 1 >>-
(>>-) :: Slab c -> Slab c -> Slab c
SEmpty >>- slab = slab
slab >>- SEmpty = slab
slab1 >>- slab2 = SRight slab1 slab2

slabSize :: (Positionable c) => Slab c -> (Int, Int)
slabSize SEmpty = (0, 0)
slabSize (SCells rs cs _) = (rs, cs)
slabSize (SMargin ro co slab) = let
    (rs, cs) = slabSize slab
  in
    (rs + ro, cs + co)
slabSize (SSize rs cs slab) = let
    (rs', cs') = slabSize slab
  in
    (max rs rs', max cs cs')
slabSize (SRight slab1 slab2) = let
    (rs1, cs1) = slabSize slab1
    (rs2, cs2) = slabSize slab2
  in
    (max rs1 rs2, cs1 + cs2)
slabSize (SBelow slab1 slab2) = let
    (rs1, cs1) = slabSize slab1
    (rs2, cs2) = slabSize slab2
  in
    (rs1 + rs2, max cs1 cs2)
slabSize (SRow _style slab) = slabSize slab

-- | Collect the properties of a slab
collect :: (Positionable c) => (Int -> Int -> b -> Slab c -> ([a], b)) -> Int -> Int -> b -> Slab c -> [a]
collect collector row column context s@SEmpty = let
    (c1, _ctxt1) = collector row column context s
  in
    c1
collect collector row column context s@(SCells _ _ _) = let
    (c1, _ctxt1) = collector row column context s
  in
    c1
collect collector row column context s@(SMargin ro co slab) = let
    (c1, ctxt1) = collector row column context s
    c2 = collect collector (row + ro) (column + co) ctxt1 slab
  in
    c1 ++ c2
collect collector row column context s@(SSize _ _ slab) = let
    (c1, ctxt1) = collector row column context s
    c2 = collect collector row column ctxt1 slab
  in
    c1 ++ c2
collect collector row column context s@(SRight slab1 slab2) = let
    (_rs1, cs1) = slabSize slab1
    (c1, ctxt1) = collector row column context s
    c2 = collect collector row column ctxt1 slab1
    c3 = collect collector row (column + cs1) ctxt1 slab2
  in
    c1 ++ c2 ++ c3
collect collector row column context s@(SBelow slab1 slab2) = let
    (rs1, _cs1) = slabSize slab1
    (c1, ctxt1) = collector row column context s
    c2 = collect collector row column ctxt1 slab1
    c3 = collect collector (row + rs1) column ctxt1 slab2
  in
    c1 ++ c2 ++ c3
collect collector row column context s@(SRow _ slab) = let
    (c1, ctxt1) = collector row column context s
    c2 = collect collector row column ctxt1 slab
  in
    c1 ++ c2

-- | Create a slab out of a row of unpositioned cells.
--   If a cell has a specific position, then that position is used and other cells follow.
--   Otherwise, the next cell will be one over from the current cell.
--   To gernerate more than one row, set an explicit position in a cell with a row greater than zero.
--   Empty cells are removed.
rowSlab :: [Cell t] -> Slab (Cell t)
rowSlab [] = SEmpty
rowSlab cells = let
  (cells', rsz, csz) = foldl (\(cs, ro, co) -> \c ->
      if nullCell c then
        if nullPos c then
          (cs, ro, co + 1)
        else let
          (ro', co') = pos c
        in
          (cs, ro', co' + 1)
      else if nullPos c then
        ((c & cellPos .~ (ro, co)):cs, ro, co + 1)
      else let
          (ro', co') = pos c
        in
          (c:cs, ro', co' + 1)
    ) ([], 0, 0) cells
  in
    SCells (rsz + 1) csz (reverse cells')

-- | Create a slab out of a column of unpositioned cells.
--   If a cell has a specific position, then that position is used and other cells follow.
--   Otherwise, the next cell will be one down from the current cell.
--   To gernerate more than one row, set an explicit position in a cell with a row greater than zero.
--   Empty cells are removed.
columnSlab :: [Cell t] -> Slab (Cell t)
columnSlab [] = SEmpty
columnSlab cells = let
  (cells', rsz, csz) = foldl (\(cs, ro, co) -> \c ->
      if nullCell c then
        if nullPos c then
          (cs, ro + 1, co)
        else let
          (ro', co') = pos c
        in
          (cs, ro' + 1, co')
      else if nullPos c then
        ((c & cellPos .~ (ro, co)):cs, ro + 1, co)
      else let
          (ro', co') = pos c
        in
          (c:cs, ro' + 1, co')
    ) ([], 0, 0) cells
  in
    SCells rsz (csz + 1) (reverse cells')

toRowAbsolute :: RowCoord -> RowCoord
toRowAbsolute (RowRel r) = RowAbs r
toRowAbsolute rc = rc

toColumnAbsolute :: ColumnCoord -> ColumnCoord
toColumnAbsolute (ColumnRel r) = ColumnAbs r
toColumnAbsolute cc = cc

toCellAbsolute :: CellCoord -> CellCoord
toCellAbsolute (rc, cc) = (toRowAbsolute rc, toColumnAbsolute cc)

addRowCoord :: RowCoord -> Int -> RowCoord
addRowCoord rc 0 = rc
addRowCoord (RowRel r) ro = RowRel (r + RowIndex ro)
addRowCoord (RowAbs r) ro = RowAbs (r + RowIndex ro)

addColumnCoord :: ColumnCoord -> Int -> ColumnCoord
addColumnCoord cc 0 = cc
addColumnCoord (ColumnRel c) co = ColumnRel (c + ColumnIndex co)
addColumnCoord (ColumnAbs c) co = ColumnAbs (c + ColumnIndex co)

maxRowCoord :: RowCoord -> RowCoord -> RowCoord
maxRowCoord r1@(RowRel ri1) r2@(RowRel ri2) = if ri2 < ri1 then r1 else r2
maxRowCoord r1@(RowRel ri1) r2@(RowAbs ri2) = if ri2 < ri1 then r1 else r2
maxRowCoord r1@(RowAbs ri1) r2@(RowRel ri2) = if ri2 < ri1 then r1 else r2
maxRowCoord r1@(RowAbs ri1) r2@(RowAbs ri2) = if ri2 < ri1 then r1 else r2

maxColumnCoord :: ColumnCoord -> ColumnCoord -> ColumnCoord
maxColumnCoord r1@(ColumnRel ri1) r2@(ColumnRel ri2) = if ri2 < ri1 then r1 else r2
maxColumnCoord r1@(ColumnRel ri1) r2@(ColumnAbs ri2) = if ri2 < ri1 then r1 else r2
maxColumnCoord r1@(ColumnAbs ri1) r2@(ColumnRel ri2) = if ri2 < ri1 then r1 else r2
maxColumnCoord r1@(ColumnAbs ri1) r2@(ColumnAbs ri2) = if ri2 < ri1 then r1 else r2

maxCellCoord :: CellCoord -> CellCoord -> CellCoord
maxCellCoord (rc1, cc1) (rc2, cc2) = (maxRowCoord rc1 rc2, maxColumnCoord cc1 cc2)

minRowCoord :: RowCoord -> RowCoord -> RowCoord
minRowCoord r1@(RowRel ri1) r2@(RowRel ri2) = if ri2 > ri1 then r1 else r2
minRowCoord r1@(RowRel ri1) r2@(RowAbs ri2) = if ri2 > ri1 then r1 else r2
minRowCoord r1@(RowAbs ri1) r2@(RowRel ri2) = if ri2 > ri1 then r1 else r2
minRowCoord r1@(RowAbs ri1) r2@(RowAbs ri2) = if ri2 > ri1 then r1 else r2

minColumnCoord :: ColumnCoord -> ColumnCoord -> ColumnCoord
minColumnCoord r1@(ColumnRel ri1) r2@(ColumnRel ri2) = if ri2 > ri1 then r1 else r2
minColumnCoord r1@(ColumnRel ri1) r2@(ColumnAbs ri2) = if ri2 > ri1 then r1 else r2
minColumnCoord r1@(ColumnAbs ri1) r2@(ColumnRel ri2) = if ri2 > ri1 then r1 else r2
minColumnCoord r1@(ColumnAbs ri1) r2@(ColumnAbs ri2) = if ri2 < ri1 then r1 else r2

minCellCoord :: CellCoord -> CellCoord -> CellCoord
minCellCoord (rc1, cc1) (rc2, cc2) = (minRowCoord rc1 rc2, minColumnCoord cc1 cc2)

cellIDLookup' :: M.Map CellID (Text, RowIndex, ColumnIndex) -> Int -> Int -> Maybe Int -> Maybe Int -> CellID -> (Text, CellCoord, Maybe CellCoord)
cellIDLookup' cmap ro co _mrsz _mcsz cid@(CellID _ _) = let
    (sheet', ri', ci') = maybe (error ("Can't map cell " ++ show cid)) id (M.lookup cid cmap)
    ri'' = ri' + RowIndex ro
    ci'' = ci' + ColumnIndex co
    coords = (RowRel ri'', ColumnRel ci'')
  in
    (sheet', coords, Nothing)
cellIDLookup' cmap ro co mrsz mcsz (CellRangeID cid1 cid2) = let
    (sheet', tl@(tlrc, tlcc), _) = cellIDLookup' cmap ro co mrsz mcsz cid1
    (blrc, blcc) = case cellIDLookup' cmap ro co mrsz mcsz cid2 of
      (_, br'', Nothing) -> br''
      (_, _, Just br'') -> br''
    blrc' = maybe blrc (\rsz -> addRowCoord tlrc (rsz - 1)) mrsz
    blcc' = maybe blcc (\csz -> addColumnCoord tlcc (csz - 1)) mcsz
    br = (blrc', blcc')
    tl' = minCellCoord tl br
    br' = maxCellCoord tl br
  in
    (sheet', tl', Just br')
cellIDLookup' cmap ro co mrsz mcsz (CellAbsoluteID cid) = (sheet, toCellAbsolute tl, toCellAbsolute <$> mbr)
  where
    (sheet, tl, mbr) = cellIDLookup' cmap ro co mrsz mcsz cid
cellIDLookup' cmap ro co mrsz mcsz (CellOffsetID ro' co' cid) =
  cellIDLookup' cmap (ro + ro') (co + co') mrsz mcsz cid
cellIDLookup' cmap ro co mrsz mcsz (CellSubrangeID mro mco mrw mcw cid) =
  cellIDLookup' cmap (ro + maybe 0 id mro) (co + maybe 0 id mco) (maybeMin mrsz mrw) (maybeMin mcsz mcw) cid


cellIDLookup :: M.Map CellID (Text, RowIndex, ColumnIndex) -> Text -> CellID -> CellRef
cellIDLookup cmap sheet cid = let
    (sheet', tl, mbr) = cellIDLookup' cmap 0 0 Nothing Nothing cid
  in
    if sheet == sheet' || Data.Text.null sheet' then
      case mbr of
        Nothing -> singleCellRef' tl
        Just br -> mkRange' tl br
    else
      case mbr of
        Nothing -> mkForeignSingleCellRef sheet' tl
        Just br -> mkForeignRange sheet' tl br

createCellMap''' :: Text -> Int -> Int -> Cell t -> [(CellID, (Text, RowIndex, ColumnIndex))] -> [(CellID, (Text, RowIndex, ColumnIndex))]
createCellMap''' title row column cell cm = maybe
  cm
  (\cid -> let
      (r, c) = absolute row column cell
    in
      (cid, (title, r, c)):cm
    )
  (cell ^. cellID)

createCellMap'' :: Int -> Int -> Text -> Slab (Cell t) -> ([(CellID, (Text, RowIndex, ColumnIndex))], Text)
createCellMap'' row column title (SCells _ _ cells) = (foldr (createCellMap''' title row column) [] cells, title)
createCellMap'' _ _ title _ = ([], title)

createCellMap' :: Renderer t -> Worksheet t -> M.Map CellID (Text, RowIndex, ColumnIndex)
createCellMap' renderer worksheet = M.fromList $ collect createCellMap'' 1 1 (renderer $ _worksheetName worksheet) (_worksheetSlab worksheet)

createCellMap :: Renderer t -> [Worksheet t] -> (Text -> CellID -> CellRef)
createCellMap renderer worksheets = let
    cmap = foldr (\ws -> \m -> m `M.union` createCellMap' renderer ws) M.empty worksheets
  in
    cellIDLookup cmap


-- | Add a style, using the `<>` operator to a slab of cells
slabAddStyle :: Style -> Slab (Cell a) -> Slab (Cell a)
slabAddStyle style slab = fmap (\cell -> cell & cellStyle <>~ Just style) slab

_showCValue :: Show a => Cell a -> String
_showCValue (Cell _ _ _ Nothing (Just msg) _ _) = show msg
_showCValue (Cell _ _ _ (Just (CellBool v)) _ _ _) = show v
_showCValue (Cell _ _ _ (Just (CellDouble v)) _ _ _) = show v
_showCValue (Cell _ _ _ (Just (CellText v)) _ _ _) = show v
_showCValue _ = "???"

-- | Debugging
_slabShowLayout' :: (Positionable c) => (c -> String) -> String -> Int -> Int -> Slab c -> [String]
_slabShowLayout' _render indent row column SEmpty =
    [indent ++ "Empty @" ++ show (row, column)]
_slabShowLayout' render indent row column (SCells rs cs cells) =
    (indent ++ "Cells (" ++ show (rs, cs) ++ ") @" ++ show (row, column))
      : map (\c -> indent ++ "  " ++ show (pos c) ++ " = " ++ render c) cells
_slabShowLayout' render indent row column (SMargin ro co slab) =
    (indent ++ "Margin +" ++ show ro ++ ", +" ++ show co)
      : _slabShowLayout' render ("  " ++ indent) (row + ro) (column + co) slab
_slabShowLayout' render indent row column (SSize rs cs slab) =
    (indent ++ "Size " ++ show rs ++ ", " ++ show cs)
      : _slabShowLayout' render ("  " ++ indent) row column slab
_slabShowLayout' render indent row column (SRight slab1 slab2) =
    (indent ++ "Right @" ++ show (row, column))
      : _slabShowLayout' render ("  " ++ indent) row column slab1
      ++ _slabShowLayout' render ("  " ++ indent) row (column + cs1) slab2
  where
    (_rs1, cs1) = slabSize slab1
_slabShowLayout' render indent row column (SBelow slab1 slab2) =
    (indent ++ "Below @" ++ show (row, column))
      : _slabShowLayout' render ("  " ++ indent) row column slab1
      ++ _slabShowLayout' render ("  " ++ indent) (row + rs1) column slab2
  where
    (rs1, _cs1) = slabSize slab1
_slabShowLayout' render indent row column (SRow _style slab) =
    (indent ++ "Row @" ++ show (row, column))
      : _slabShowLayout' render ("  " ++ indent) row column slab

_slabShowLayout :: (Positionable c) => Slab c -> (c -> String) -> String
_slabShowLayout slab render = unlines (_slabShowLayout' render "" 1 1 slab)

-- | A named worksheet
data Worksheet t = Worksheet {
      _worksheetName :: t -- ^ The name of the worksheet
    , _worksheetSlab :: Slab (Cell t) -- ^ The slab of cells that describes the worksheet
}

isOperator :: Text -> Bool
isOperator "+" = True
isOperator "-" = True
isOperator "*" = True
isOperator "/" = True
isOperator "&" = True
isOperator "^" = True
isOperator "=" = True
isOperator "<>" = True
isOperator "<" = True
isOperator ">" = True
isOperator "<=" = True
isOperator ">=" = True
isOperator _ = False

operatorPrec :: Text -> Int
operatorPrec "+" = 5
operatorPrec "-" = 5
operatorPrec "*" = 6
operatorPrec "/" = 6
operatorPrec "&" = 4
operatorPrec "^" = 3
operatorPrec "=" = 2
operatorPrec "<>" = 2
operatorPrec "<" = 2
operatorPrec ">" = 2
operatorPrec "<=" = 2
operatorPrec ">=" = 2
operatorPrec _ = 1

formulaFromFormula' :: (CellID -> CellRef) -> Int -> Formula -> Text
formulaFromFormula' _cells _prec (FVar v) = v
formulaFromFormula' _cells _prec (FBool v) = if v then "TRUE" else "FALSE"
formulaFromFormula' _cells _prec (FInt v) = pack $ show v
formulaFromFormula' _cells _prec (FDouble v) = pack $ show v
formulaFromFormula' _cells _prec (FText v) = "\"" <> v <> "\""
formulaFromFormula' cells _prec (FRef ref) = unCellRef $ cells ref
formulaFromFormula' cells prec (FApply func args) =
  if isOperator func then let
    prec' = operatorPrec func
    prefix = if prec' < prec then "(" else ""
    arg1 = if length args > 0 then formulaFromFormula' cells prec' (args !! 0) else ""
    arg2 = if length args > 1 then formulaFromFormula' cells prec' (args !! 1) else ""
    suffix = if prec' < prec then ")" else ""
    in
      case length args of
        0 -> prefix <> func <> suffix
        1 -> prefix <> func <> " " <> arg1 <> suffix
        2 -> prefix <> arg1 <> " " <> func <> " " <> arg2 <> suffix
        _ -> error ("Invalid arguments for inline function " ++ show args)
  else
    func <> "(" <> intercalate ", " (map (formulaFromFormula' cells 0) args) <> ")"

formulaFromFormula :: (CellID -> CellRef) -> Formula -> CellFormula
formulaFromFormula cells formula = simpleCellFormula $ formulaFromFormula' cells 0 formula

commentFromComment :: Renderer t -> Comment t -> X.Comment
commentFromComment renderer (Comment base msgs) = let
    notes = foldr (\m -> \ls -> (renderer m):ls) (maybe [] L.singleton base) msgs
    txt = intercalate "\n" notes
  in
    X.Comment (XlsxText txt) "de-calixtinus" False

cellFromCell :: Renderer t -> StyleMap -> (CellID -> CellRef) -> Style -> Cell t -> X.Cell
cellFromCell renderer styles cells style cell = let
    style' = maybe style (\s -> style <> s) (cell ^. cellStyle)
    msid = case style' of
      (Style Nothing Nothing Nothing Nothing Nothing _) -> Nothing
      s -> M.lookup s (styleMapCellMap styles)
  in
    def
      & X.cellStyle .~ msid
      & X.cellValue .~ ((cell ^. cellValue) <|> (CellText . renderer <$> cell ^. cellText))
      & X.cellComment .~ (commentFromComment renderer <$> cell ^. cellComment)
      & X.cellFormula .~ (formulaFromFormula cells <$> cell ^. cellFormula)

-- | The minimum width a cell can be before it's just not needed
minimumWidth :: Int
minimumWidth = 10

cellWidth' :: Maybe NumberFormat -> Maybe CellValue -> Maybe Int
cellWidth' (Just (UserNumberFormat fmt)) Nothing = Just $ T.length fmt + 1 -- Formula
cellWidth' _ Nothing = Nothing
cellWidth' _ (Just (CellBool _))  = Nothing
cellWidth' (Just (UserNumberFormat fmt)) (Just (CellDouble _)) = Just $ T.length fmt + 1
-- Standard number formats all, pretty much, fit into an ordinary cell
cellWidth' _ (Just (CellDouble _)) = Nothing
cellWidth' _ (Just (CellError _)) = Nothing
cellWidth' _ (Just (CellText txt)) = Just $ T.length txt + 1
cellWidth' _ (Just (CellRich rt)) = Just $ (sum $ map (\r -> T.length (r ^. richTextRunText)) rt) + 1

cellWidth :: Maybe CellWidth -> Maybe NumberFormat -> X.Cell -> Maybe Int
cellWidth Nothing _ _ = Nothing
cellWidth (Just WidthNone) _ _ = Nothing
cellWidth (Just (WidthFixed width)) _ _ = Just $ width
cellWidth (Just WidthExpand) mfmt xcell = cellWidth' mfmt (xcell ^. X.cellValue)
cellWidth (Just (WidthExpandMax mx)) mfmt xcell = (min mx) <$> cellWidth' mfmt (xcell ^. X.cellValue)

worksheetSetColumnWidth :: X.Worksheet -> Int -> Int -> X.Worksheet
worksheetSetColumnWidth sheet column width = let
    makeCp col dw = ColumnsProperties col col (Just dw) Nothing False False False
    cps = sheet ^. wsColumnsProperties
    mexisting = L.find (\cp -> column >= cpMin cp && column <= cpMax cp) cps
    dwidth = realToFrac width
    sheet' = if isNothing mexisting then
        sheet & wsColumnsProperties .~ (makeCp column dwidth:cps)
      else
        let
          existing = fromJust mexisting
        in
          if dwidth <= maybe 0.0 id (cpWidth existing) then
           sheet
          else
           sheet & wsColumnsProperties .~ (makeCp column dwidth:L.delete existing cps)
  in
    sheet'

worksheetFromCell :: Renderer t -> StyleMap -> (CellID -> CellRef) -> X.Worksheet -> Style -> Int -> Int -> Cell t -> X.Worksheet
worksheetFromCell renderer styles cells sheet style row column cell = let
  xcell = cellFromCell renderer styles cells style cell
  p@(_ro, co) = absolute row column cell
  style' = cell ^. cellStyle
  width = maybe Nothing (\s -> cellWidth (s ^. styleWidth) (s ^. styleNumberFormat) xcell) style'
  sheet' = sheet & atCell p ?~ xcell
  sheet'' = maybe sheet' (\w -> if w <= minimumWidth then sheet' else worksheetSetColumnWidth sheet' (unColumnIndex co) w) width
  in
    sheet''


worksheetFromSlab :: Renderer t -> StyleMap -> (CellID -> CellRef) -> X.Worksheet -> Style -> Int -> Int -> Slab (Cell t) -> X.Worksheet
worksheetFromSlab _renderer _styles _cells sheet _style _row _column SEmpty = sheet
worksheetFromSlab renderer styles cells sheet style row column (SCells _ _ cs) =
  foldl (\s -> \c -> if nullCell c then s else worksheetFromCell renderer styles cells s style row column c) sheet cs
worksheetFromSlab renderer styles cells sheet style row column (SMargin ro co slab) =
  worksheetFromSlab renderer styles cells sheet style (row + ro) (column + co) slab
worksheetFromSlab renderer styles cells sheet style row column (SSize _ _ slab) =
  worksheetFromSlab renderer styles cells sheet style row column slab
worksheetFromSlab renderer styles cells sheet style row column (SRight slab1 slab2) = let
    (_rs1, cs1) = slabSize slab1
    sheet' = worksheetFromSlab renderer styles cells sheet style row column slab1
    sheet'' = worksheetFromSlab renderer styles cells sheet' style row (column + cs1) slab2
  in
    sheet''
worksheetFromSlab renderer styles cells sheet style row column (SBelow slab1 slab2) = let
    (rs1, _cs1) = slabSize slab1
    sheet' = worksheetFromSlab renderer styles cells sheet style row column slab1
    sheet'' = worksheetFromSlab renderer styles cells sheet' style (row + rs1) column slab2
  in
    sheet''
worksheetFromSlab renderer styles cells sheet style row column (SRow style' slab) = let
    style'' = style <> style'
    styleMap = styleMapCellMap styles
    rowStyles = sheet ^. wsRowPropertiesMap
    (srs, _scs) = slabSize slab
    rstyle = RowProps Nothing (M.lookup style'' styleMap) False
    rowStyles' = foldr (\r -> \s -> M.insert (RowIndex $ row + r) rstyle s) rowStyles [0..(srs - 1)]
    sheet' = sheet & wsRowPropertiesMap .~ rowStyles'
    sheet'' = worksheetFromSlab renderer styles cells sheet' style'' row column slab
  in
    sheet''

worksheetFromWorksheet :: Renderer t -> StyleMap -> (Text -> CellID -> CellRef) -> Worksheet t -> X.Worksheet
worksheetFromWorksheet renderer styles cells worksheet =
  worksheetFromSlab
    renderer
    styles
    (cells (renderer $ _worksheetName worksheet))
    def
    def
    1
    1
    (_worksheetSlab worksheet)

isUserNumberFormat :: NumberFormat -> Bool
isUserNumberFormat (UserNumberFormat _code) = True
isUserNumberFormat _ = False

makeCellXf :: M.Map Border Int -> M.Map Fill Int -> M.Map Font Int -> M.Map NumberFormat Int -> Style -> CellXf
makeCellXf borderMap fillMap fontMap formatMap (Style alignment border fill font format _width) =
  def
    & cellXfApplyAlignment .~ check alignment
    & cellXfAlignment .~ alignment
    & cellXfApplyBorder .~ check border
    & cellXfBorderId .~ ((\b -> M.lookup b borderMap) =<< border)
    & cellXfApplyFill .~ check fill
    & cellXfFillId .~  ((\f -> M.lookup f fillMap) =<< fill)
    & cellXfApplyFont .~ check font
    & cellXfFontId .~  ((\f -> M.lookup f fontMap) =<< font)
    & cellXfApplyNumberFormat .~ check format
    & cellXfNumFmtId .~  ((\f -> M.lookup f formatMap) =<< format)
  where
    check a = True <$ a

styleMap' :: Int -> Int -> Style -> Slab (Cell t) -> ([Style], Style)
styleMap' _ _ context (SCells _ _ cells) = (map (\s -> context <> s) $ catMaybes $ map _cellStyle cells, context)
styleMap' _ _ context (SRow style _slab) = ([style], context <> style)
styleMap' _ _ context _ = ([], context)

createStyleMap :: [Worksheet t] -> StyleMap
createStyleMap worksheets = let
    base = minimalStyleSheet
    slabs = map _worksheetSlab worksheets
    styles = concatMap (\s -> collect styleMap' 1 1 def s) slabs
    ustyles = unique styles
    borders = unique $ base ^. styleSheetBorders ++ catMaybes (map _styleBorder ustyles)
    borderMap = M.fromList $ zip borders [0..]
    fills = unique $ base ^. styleSheetFills ++ catMaybes (map _styleFill ustyles)
    fillMap = M.fromList $ zip fills [0..]
    fonts = unique $ base ^. styleSheetFonts ++ catMaybes (map _styleFont ustyles)
    fontMap = M.fromList $ zip fonts [0..]
    existingFormats = base ^. styleSheetNumFmts
    startNumFmtId = max firstUserNumFmtId ((if M.null existingFormats then 0 else fst $ M.findMax existingFormats) + 1)
    formats = unique $ mapMaybe _styleNumberFormat ustyles
    userFormats = filter isUserNumberFormat formats
    userFormatMap = M.fromList $ zip userFormats [startNumFmtId ..]
    formatMap = M.fromList $ map (\f -> (f, case f of
      StdNumberFormat nf -> stdNumberFormatId nf
      _ -> userFormatMap M.! f
      )) formats
    fmtMap f@(UserNumberFormat code) = (formatMap M.! f, code)
    fmtMap f = error ("Unexpected number format " ++ show f)
    numFmts = M.union existingFormats (M.fromList $ map fmtMap userFormats)
    existingCellxfs = base ^. styleSheetCellXfs
    cellxfs = existingCellxfs ++ map (makeCellXf borderMap fillMap fontMap formatMap) ustyles
    cellxfMap = M.fromList $ zip ustyles [(length existingCellxfs) ..]
  in
    StyleMap {
        styleMapCells = cellxfs
      , styleMapCellMap = cellxfMap
      , styleMapBorders = borders
      , styleMapBorderMap = borderMap
      , styleMapFills = fills
      , styleMapFillMap = fillMap
      , styleMapFonts = fonts
      , styleMapFontMap = fontMap
      , styleMapFormats = formats
      , styleMapFormatMap = formatMap
      , styleMapNumFmts = numFmts
    }

styleSheetFromMap :: StyleMap -> StyleSheet
styleSheetFromMap styles = def
      & styleSheetBorders .~ styleMapBorders styles
      & styleSheetFonts   .~ styleMapFonts styles
      & styleSheetFills   .~ styleMapFills styles
      & styleSheetCellXfs .~ styleMapCells styles
      & styleSheetNumFmts .~ styleMapNumFmts styles

-- | Create a full workbook from a list of worksheet descriptions
createXlsx :: (Renderer t) -> [Worksheet t] -> Xlsx
createXlsx renderer worksheets = let
    styles = createStyleMap worksheets
    cells = createCellMap renderer worksheets
    xlsx = def & xlStyles .~ renderStyleSheet (styleSheetFromMap styles)
    xlsx' = foldl (\xl -> \ws -> xl & atSheet (renderer $ _worksheetName ws) ?~ worksheetFromWorksheet renderer styles cells ws) xlsx worksheets
  in
    xlsx'

-- | A useful utility to get at the minimal base font before adjusting it
baseFont :: Font
baseFont = headWithDefault def $ minimalStyleSheet ^. styleSheetFonts

