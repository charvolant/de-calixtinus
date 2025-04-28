{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module XlsxSpec(testXlsx) where

import Test.HUnit
import Codec.Xlsx hiding (Cell(..), Comment(..), Formula(..), Worksheet(..), cellStyle, cellValue, cellComment, cellFormula)
import qualified Codec.Xlsx as X
import Control.Exception
import Control.Lens
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Xlsx.Internal


testXlsx :: Test
testXlsx = TestList [
    TestLabel "Style" testStyle
  , TestLabel "CellID" testCellID
  , TestLabel "Formula" testFormula
  , TestLabel "Cell" testCell
  , TestLabel "Slab" testSlab
  , TestLabel "Worksheet" testWorksheet
  , TestLabel "CreateXlsx" testCreateXlsx
  ]
  
testStyle = TestList [
    TestLabel "JoinAlignment" testJoinAlignment
  , TestLabel "JoinBorder" testJoinBorder
  , TestLabel "JoinFill" testJoinFill
  , TestLabel "JoinFont" testJoinFont
  , TestLabel "StyleMonoid" testStyleMonoid
  , TestLabel "StyleMap" testStyleMap
  ]

testJoinAlignment = TestList [
  testJoinAlignment1, testJoinAlignment2, testJoinAlignment3
  ]

align1 = def & alignmentHorizontal ?~ CellHorizontalAlignmentDistributed & alignmentIndent ?~ 1 & alignmentWrapText ?~ True

align2 = def & alignmentHorizontal ?~ CellHorizontalAlignmentCenter & alignmentJustifyLastLine ?~ True

align3 = def & alignmentIndent ?~ 2 & alignmentShrinkToFit ?~ True

testJoinAlignment1 = let
    align = joinAlignment align1 align2
  in
    TestCase (do
      assertEqual "JoinAlignment 1 1" (Just CellHorizontalAlignmentCenter) (align ^. alignmentHorizontal)
      assertEqual "JoinAlignment 1 2" (Just 1) (align ^. alignmentIndent)
      assertEqual "JoinAlignment 1 3" (Just True) (align ^. alignmentJustifyLastLine)
      assertEqual "JoinAlignment 1 4" Nothing (align ^. alignmentReadingOrder)
      assertEqual "JoinAlignment 1 5" Nothing (align ^. alignmentRelativeIndent)
      assertEqual "JoinAlignment 1 6" Nothing (align ^. alignmentShrinkToFit)
      assertEqual "JoinAlignment 1 7" Nothing (align ^. alignmentTextRotation)
      assertEqual "JoinAlignment 1 8" Nothing (align ^. alignmentVertical)
      assertEqual "JoinAlignment 1 9" (Just True) (align ^. alignmentWrapText)
   )

testJoinAlignment2 = let
    align = joinAlignment align2 align1
  in
    TestCase (do
      assertEqual "JoinAlignment 2 1" (Just CellHorizontalAlignmentDistributed) (align ^. alignmentHorizontal)
      assertEqual "JoinAlignment 2 2" (Just 1) (align ^. alignmentIndent)
      assertEqual "JoinAlignment 2 3" (Just True) (align ^. alignmentJustifyLastLine)
      assertEqual "JoinAlignment 2 4" Nothing (align ^. alignmentReadingOrder)
      assertEqual "JoinAlignment 2 5" Nothing (align ^. alignmentRelativeIndent)
      assertEqual "JoinAlignment 2 6" Nothing (align ^. alignmentShrinkToFit)
      assertEqual "JoinAlignment 2 7" Nothing (align ^. alignmentTextRotation)
      assertEqual "JoinAlignment 2 8" Nothing (align ^. alignmentVertical)
      assertEqual "JoinAlignment 2 9" (Just True) (align ^. alignmentWrapText)
   )

testJoinAlignment3 = let
    align = joinAlignment align1 align3
  in
    TestCase (do
      assertEqual "JoinAlignment 3 1" (Just CellHorizontalAlignmentDistributed) (align ^. alignmentHorizontal)
      assertEqual "JoinAlignment 3 2" (Just 2) (align ^. alignmentIndent)
      assertEqual "JoinAlignment 3 3" Nothing (align ^. alignmentJustifyLastLine)
      assertEqual "JoinAlignment 3 4" Nothing (align ^. alignmentReadingOrder)
      assertEqual "JoinAlignment 3 5" Nothing (align ^. alignmentRelativeIndent)
      assertEqual "JoinAlignment 3 6" (Just True) (align ^. alignmentShrinkToFit)
      assertEqual "JoinAlignment 3 7" Nothing (align ^. alignmentTextRotation)
      assertEqual "JoinAlignment 3 8" Nothing (align ^. alignmentVertical)
      assertEqual "JoinAlignment 3 9" (Just True) (align ^. alignmentWrapText)
   )

testJoinBorder = TestList [
  testJoinBorder1, testJoinBorder2
  ]

color1 = Color Nothing (Just "ffff8040") Nothing Nothing

border1 = def & borderOutline ?~ True & borderEnd ?~ (def & borderStyleLine ?~ LineStyleMedium)

border2 = def & borderEnd ?~ (def & borderStyleColor ?~ color1) & borderStart ?~ (def & borderStyleLine ?~ LineStyleThick)

testJoinBorder1 = let
    border = joinBorder border1 border2
  in
    TestCase (do
      assertEqual "JoinBorder 1 1" Nothing (border ^. borderDiagonalDown)
      assertEqual "JoinBorder 1 2" Nothing (border ^. borderDiagonalUp)
      assertEqual "JoinBorder 1 3" (Just True) (border ^. borderOutline)
      assertEqual "JoinBorder 1 4" Nothing (border ^. borderBottom)
      assertEqual "JoinBorder 1 5" Nothing (border ^. borderDiagonal)
      assertBool "JoinBorder 1 6 1" (isJust $ border ^. borderEnd)
      assertEqual "JoinBorder 1 6 2" (Just color1) (border ^. borderEnd . non def . borderStyleColor)
      assertEqual "JoinBorder 1 6 3" Nothing (border ^. (borderEnd . non def . borderStyleLine))
      assertEqual "JoinBorder 1 7" Nothing (border ^. borderHorizontal)
      assertBool "JoinBorder 1 8 1" (isJust $ border ^. borderStart)
      assertEqual "JoinBorder 1 8 2" Nothing (border ^. (borderStart . non def . borderStyleColor))
      assertEqual "JoinBorder 1 8 3" (Just LineStyleThick) (border ^. (borderStart . non def . borderStyleLine))
      assertEqual "JoinBorder 1 9" Nothing (border ^. borderTop)
      assertEqual "JoinBorder 1 10" Nothing (border ^. borderVertical)
      assertEqual "JoinBorder 1 11" Nothing (border ^. borderLeft)
      assertEqual "JoinBorder 1 12" Nothing (border ^. borderRight)
   )

testJoinBorder2 = let
    border = joinBorder border2 border1
  in
    TestCase (do
      assertEqual "JoinBorder 2 1" Nothing (border ^. borderDiagonalDown)
      assertEqual "JoinBorder 2 2" Nothing (border ^. borderDiagonalUp)
      assertEqual "JoinBorder 2 3" (Just True) (border ^. borderOutline)
      assertEqual "JoinBorder 2 4" Nothing (border ^. borderBottom)
      assertEqual "JoinBorder 2 5" Nothing (border ^. borderDiagonal)
      assertBool "JoinBorder 2 6 1" (isJust $ border ^. borderEnd)
      assertEqual "JoinBorder 2 6 2" Nothing (border ^. (borderEnd . non def . borderStyleColor))
      assertEqual "JoinBorder 2 6 3" (Just LineStyleMedium) (border ^. (borderEnd . non def . borderStyleLine))
      assertEqual "JoinBorder 2 7" Nothing (border ^. borderHorizontal)
      assertBool "JoinBorder 2 8 1" (isJust $ border ^. borderStart)
      assertEqual "JoinBorder 2 8 2" Nothing (border ^. (borderStart . non def . borderStyleColor))
      assertEqual "JoinBorder 2 8 3" (Just LineStyleThick) (border ^. (borderStart . non def . borderStyleLine))
      assertEqual "JoinBorder 2 9" Nothing (border ^. borderTop)
      assertEqual "JoinBorder 2 10" Nothing (border ^. borderVertical)
      assertEqual "JoinBorder 2 11" Nothing (border ^. borderLeft)
      assertEqual "JoinBorder 2 12" Nothing (border ^. borderRight)
   )

testJoinFill= TestList [
  testJoinFill1, testJoinFill2
  ]

fill1 = def & fillPattern ?~ (def & fillPatternFgColor ?~ color1 & fillPatternType ?~ PatternTypeSolid)

fill2 = def & fillPattern ?~ (def & fillPatternBgColor ?~ color1 & fillPatternType ?~ PatternTypeLightUp)

testJoinFill1 = let
    fill = joinFill fill1 fill2
  in
    TestCase (do
      assertBool "JoinFill 1 1 1" (isJust $ fill ^. fillPattern)
      assertEqual "JoinFill 1 1 2" Nothing (fill ^. fillPattern . non def . fillPatternFgColor)
      assertEqual "JoinFill 1 1 3" (Just color1) (fill ^. fillPattern . non def . fillPatternBgColor)
      assertEqual "JoinFill 1 1 4" (Just PatternTypeLightUp) (fill ^. fillPattern . non def . fillPatternType)
   )

testJoinFill2 = let
    fill = joinFill fill2 fill1
  in
    TestCase (do
      assertBool "JoinFill 1 1 1" (isJust $ fill ^. fillPattern)
      assertEqual "JoinFill 1 1 2" (Just color1) (fill ^. fillPattern . non def . fillPatternFgColor)
      assertEqual "JoinFill 1 1 3" Nothing (fill ^. fillPattern . non def . fillPatternBgColor)
      assertEqual "JoinFill 1 1 4" (Just PatternTypeSolid) (fill ^. fillPattern . non def . fillPatternType)
   )

testJoinFont= TestList [
  testJoinFont1, testJoinFont2
  ]

font1 = def & fontBold ?~ True & fontFamily ?~ FontFamilySwiss & fontSize ?~ 10

font2 = def & fontItalic ?~ True & fontFamily ?~ FontFamilyModern

testJoinFont1 = let
    font = joinFont font1 font2
  in
    TestCase (do
      assertEqual "JoinFont 1 1" (Just True) (font ^. fontBold)
      assertEqual "JoinFont 1 2" Nothing (font ^. fontCharset)
      assertEqual "JoinFont 1 3" Nothing (font ^. fontColor)
      assertEqual "JoinFont 1 4" Nothing (font ^. fontCondense)
      assertEqual "JoinFont 1 5" Nothing (font ^. fontExtend)
      assertEqual "JoinFont 1 6" (Just FontFamilyModern) (font ^. fontFamily)
      assertEqual "JoinFont 1 7" (Just True) (font ^. fontItalic)
      assertEqual "JoinFont 1 8" Nothing (font ^. fontName)
      assertEqual "JoinFont 1 9" Nothing (font ^. fontOutline)
      assertEqual "JoinFont 1 10" Nothing (font ^. fontScheme)
      assertEqual "JoinFont 1 11" Nothing (font ^. fontShadow)
      assertEqual "JoinFont 1 12" Nothing (font ^. fontStrikeThrough)
      assertEqual "JoinFont 1 13" (Just 10) (font ^. fontSize)
      assertEqual "JoinFont 1 14" Nothing (font ^. fontUnderline)
      assertEqual "JoinFont 1 15" Nothing (font ^. fontVertAlign)
   )

testJoinFont2 = let
    font = joinFont font2 font1
  in
    TestCase (do
      assertEqual "JoinFont 2 1" (Just True) (font ^. fontBold)
      assertEqual "JoinFont 2 2" Nothing (font ^. fontCharset)
      assertEqual "JoinFont 2 3" Nothing (font ^. fontColor)
      assertEqual "JoinFont 2 4" Nothing (font ^. fontCondense)
      assertEqual "JoinFont 2 5" Nothing (font ^. fontExtend)
      assertEqual "JoinFont 2 6" (Just FontFamilySwiss) (font ^. fontFamily)
      assertEqual "JoinFont 2 7" (Just True) (font ^. fontItalic)
      assertEqual "JoinFont 2 8" Nothing (font ^. fontName)
      assertEqual "JoinFont 2 9" Nothing (font ^. fontOutline)
      assertEqual "JoinFont 2 10" Nothing (font ^. fontScheme)
      assertEqual "JoinFont 2 11" Nothing (font ^. fontShadow)
      assertEqual "JoinFont 2 12" Nothing (font ^. fontStrikeThrough)
      assertEqual "JoinFont 2 13" (Just 10) (font ^. fontSize)
      assertEqual "JoinFont 2 14" Nothing (font ^. fontUnderline)
      assertEqual "JoinFont 2 15" Nothing (font ^. fontVertAlign)
   )

format1 = UserNumberFormat "#.0"

format2 = UserNumberFormat "0.00"

width1 = WidthExpand

width2 = WidthFixed 10 

style1 = def & styleAlignment ?~ align1 & styleBorder ?~ border1 & styleFill ?~ fill1 & styleFont ?~ font1 & styleNumberFormat ?~ format1 & styleWidth ?~ width1

style2 = def & styleAlignment ?~ align2 & styleBorder ?~ border2 & styleFill ?~ fill2 & styleFont ?~ font2 & styleNumberFormat ?~ format2 & styleWidth ?~ width2

testStyleMonoid = TestList [
  testStyleMonoid1, testStyleMonoid2
  ]

testStyleMonoid1 = let
    style = style1 <> style2
  in
    TestCase (do
      assertEqual "StyleMonoid 1 1" (Just CellHorizontalAlignmentCenter) (style ^. styleAlignment . non def . alignmentHorizontal)
      assertEqual "StyleMonoid 1 2 1" Nothing (style ^. styleBorder . non def . borderEnd . non def . borderStyleLine)
      assertEqual "StyleMonoid 1 2 2" (Just color1) (style ^. styleBorder . non def . borderEnd . non def . borderStyleColor)
      assertEqual "StyleMonoid 1 3" (Just PatternTypeLightUp) (style ^. styleFill . non def . fillPattern . non def . fillPatternType)
      assertEqual "StyleMonoid 1 4" (Just FontFamilyModern) (style ^. styleFont . non def . fontFamily)
      assertEqual "StyleMonoid 1 5" (Just True) (style ^. styleFont . non def . fontBold)
      assertEqual "StyleMonoid 1 6" (Just format2) (style ^. styleNumberFormat)
      assertEqual "StyleMonoid 1 7" (Just width2) (style ^. styleWidth)
   )

testStyleMonoid2 = let
    style = style2 <> style1
  in
    TestCase (do
      assertEqual "StyleMonoid 2 1" (Just CellHorizontalAlignmentDistributed) (style ^. styleAlignment . non def . alignmentHorizontal)
      assertEqual "StyleMonoid 2 2" (Just LineStyleMedium) (style ^. styleBorder . non def . borderEnd . non def . borderStyleLine)
      assertEqual "StyleMonoid 2 3" (Just PatternTypeSolid) (style ^. styleFill . non def . fillPattern . non def . fillPatternType)
      assertEqual "StyleMonoid 2 4" (Just FontFamilySwiss) (style ^. styleFont . non def . fontFamily)
      assertEqual "StyleMonoid 2 5" (Just True) (style ^. styleFont . non def . fontBold)
      assertEqual "StyleMonoid 2 6" (Just format1) (style ^. styleNumberFormat)
      assertEqual "StyleMonoid 2 7" (Just width1) (style ^. styleWidth)
   )


testStyleMap = TestList [
  testStyleMap1
  ]

testStyleMap1 = TestCase (do
  let worksheet = Worksheet "Test" (rowSlab [def & cellStyle ?~ style1 & cellText ?~ "Text 1", def & cellStyle ?~ style2 & cellText ?~ "Text 2" ])
  let styles = createStyleMap [worksheet]
  assertEqual "StyleMap 1 1" 3 (length $ styleMapCells styles)
  assertEqual "StyleMap 1 2" 3 (length $ styleMapBorders styles)
  assertEqual "StyleMap 1 3" 4 (length $ styleMapFills styles)
  assertEqual "StyleMap 1 4" 3 (length $ styleMapFonts styles)
  assertEqual "StyleMap 1 5" 2 (length $ styleMapFormats styles)
  assertEqual "StyleMap 1 6" 2 (M.size $ styleMapNumFmts styles)
  )

testCellID = TestList [
    TestLabel "CellIDStream" testCellIDStream
  , TestLabel "CellIDLookup" testCellIDLookup
  , TestLabel "CreateCellMap" testCreateCellMap
  ]

testCellIDStream = TestList [
  testCellIDStream1, testCellIDStream2
  ]

testCellIDStream1 = let
   cellid = evalState (nextCellID "test1") 5
 in
  TestCase (assertEqual "CellIDStream 1 1" (CellID "test1" 5) cellid)

testCellIDStream2 = let
 (cellid1, cellid2) = evalState (do
  cid1' <- nextCellID "test2"
  cid2' <- nextCellID "test2"
  return (cid1', cid2')
  ) 1
 in
  TestCase (do
    assertEqual "CellIDStream 2 1" (CellID "test2" 1) cellid1
    assertEqual "CellIDStream 2 1" (CellID "test2" 2) cellid2
    )

cellID1 = CellID "cell" 1

cellID2 = CellID "cell" 5

cellID3 = CellID "cell" 6

cellidmap = M.fromList [(cellID1, ("sheet1", 2, 5)), (cellID2, ("sheet1", 4, 6)), (cellID3, ("sheet2", 7, 1))]

testCellIDLookup = TestList [
  testCellIDLookup1, testCellIDLookup2, testCellIDLookup3, testCellIDLookup4,
  testCellIDLookup5, testCellIDLookup6, testCellIDLookup7, testCellIDLookup8,
  testCellIDLookup9, testCellIDLookup10, testCellIDLookup11, testCellIDLookup12,
  testCellIDLookup13
  ]

testCellIDLookup1 = TestCase (assertEqual "CellIDLookup 1" "E2" (unCellRef $ cellIDLookup cellidmap "sheet1" cellID1))

testCellIDLookup2 = TestCase (assertEqual "CellIDLookup 2" "'sheet1'!E2" (unCellRef $ cellIDLookup cellidmap "sheet4" cellID1))

testCellIDLookup3 = TestCase (assertEqual "CellIDLookup 3" "$E$2" (unCellRef $ cellIDLookup cellidmap "sheet1" (toAbsolute cellID1)))

testCellIDLookup4 = TestCase (assertEqual "CellIDLookup 4" "G3" (unCellRef $ cellIDLookup cellidmap "sheet1" (toOffset 1 2 cellID1)))

testCellIDLookup5 = TestCase (assertEqual "CellIDLookup 5" "$G$4" (unCellRef $ cellIDLookup cellidmap "sheet1" (toAbsolute $ toOffset 2 2 cellID1)))

testCellIDLookup6 = TestCase (assertEqual "CellIDLookup 6" "$G$4" (unCellRef $ cellIDLookup cellidmap "sheet1" (toOffset 2 2 $ toAbsolute cellID1)))

testCellIDLookup7 = TestCase (assertEqual "CellIDLookup 7" "E2:F4" (unCellRef $ cellIDLookup cellidmap "sheet1" (CellRangeID cellID1 cellID2)))

testCellIDLookup8 = TestCase (assertEqual "CellIDLookup 8" "$E$2:$F$4" (unCellRef $ cellIDLookup cellidmap "sheet1" (toAbsolute $ CellRangeID cellID1 cellID2)))

testCellIDLookup9 = TestCase (assertEqual "CellIDLookup 9" "'sheet1'!$E$2:$F$4" (unCellRef $ cellIDLookup cellidmap "" (toAbsolute $ CellRangeID cellID1 cellID2)))

testCellIDLookup10 = TestCase (assertEqual "CellIDLookup 10" "'sheet1'!$E$2:$F$4" (unCellRef $ cellIDLookup cellidmap "sheetx" (toAbsolute $ CellRangeID cellID1 cellID2)))

testCellIDLookup11 = TestCase (assertEqual "CellIDLookup 11" "A2:E7" (unCellRef $ cellIDLookup cellidmap "sheet1" (CellRangeID cellID1 cellID3)))

testCellIDLookup12 = TestCase (assertEqual "CellIDLookup 12" "E2" (unCellRef $ cellIDLookup cellidmap "sheet1" (CellSubrangeID Nothing Nothing Nothing Nothing cellID1)))

testCellIDLookup13 = TestCase (do
    c <- try (do
      c' <- evaluate $ cellIDLookup cellidmap "sheet1" (CellID "error" 99)
      return c'
      ) :: IO (Either ErrorCall CellRef)
    assertBool "CellIDLookup 13" (either (const True) (const False) c)
  )

testCreateCellMap = TestList [
  testCreateCellMap1, testCreateCellMap2
  ]

testCreateCellMap1 = TestCase (let
    slab = rowSlab [ def, def & cellValue ?~ CellText "Hello" & cellID ?~ cellID1 ]
    map = createCellMap id [Worksheet "sheet1" slab]
  in do
    assertEqual "CreateCellMap 1 1" "B1" (unCellRef $ map "sheet1" cellID1)
    assertEqual "CreateCellMap 1 2" "'sheet1'!B1" (unCellRef $ map "sheet2" cellID1)
  )

testCreateCellMap2 = TestCase (let
    slab = columnSlab [ def, def & cellText ?~ "Hello" & cellID ?~ cellID1, def & cellText ?~ "World" & cellID ?~ cellID2 ]
    map = createCellMap id [Worksheet "sheet1" slab]
  in do
    assertEqual "CreateCellMap 1 1" "A2" (unCellRef $ map "sheet1" cellID1)
    assertEqual "CreateCellMap 1 2" "'sheet1'!A2" (unCellRef $ map "sheet2" cellID1)
    assertEqual "CreateCellMap 1 3" "A3" (unCellRef $ map "sheet1" cellID2)
  )

testFormula = TestList [
  TestLabel "FormulaFromFormula" testFormulaFromFormula
  ]

formulaCID1 = CellID "formula" 1

formulaCID2 = CellID "formula" 2

formula1 = FApply "SUM" [ FRef formulaCID1 ]

formula2 = FApply "SUM" [ FRef $ CellRangeID formulaCID1 formulaCID2 ]

formula3 = FApply "+" [ FInt 1, FInt 2 ]

formula4 = FApply "+" [ FApply "*" [FInt 2, FInt 3], FInt 2 ]

formula5 = FApply "*" [ FApply "+" [FInt 2, FInt 3], FInt 2 ]

formula6 = FApply "=" [ FApply "+" [FInt 2, FInt 3], FVar "a" ]

formula7 = FApply "HLOOKUP" [FRef $ CellOffsetID (-1) (-1) formulaCID1, FRef $ CellRangeID formulaCID1 formulaCID2, FBool False ]

formula8 = FDouble 2.5

formula9 = FApply "&" [ FText "One", FText "Two" ]

formulaCellMap = cellIDLookup (M.fromList [(formulaCID1, ("sheet1", 2, 5)), (formulaCID2, ("sheet1", 4, 6))]) "sheet1"

testFormulaFromFormula = TestList [
  testFormulaFromFormula1, testFormulaFromFormula2, testFormulaFromFormula3, testFormulaFromFormula4,
  testFormulaFromFormula5, testFormulaFromFormula6, testFormulaFromFormula7, testFormulaFromFormula8,
  testFormulaFromFormula9
  ]

testFormulaFromFormula1 = TestCase (assertEqual "FormulaFromFormula 1" (simpleCellFormula "SUM(E2)") (formulaFromFormula formulaCellMap formula1))

testFormulaFromFormula2 = TestCase (assertEqual "FormulaFromFormula 2" (simpleCellFormula "SUM(E2:F4)") (formulaFromFormula formulaCellMap formula2))

testFormulaFromFormula3 = TestCase (assertEqual "FormulaFromFormula 3" (simpleCellFormula "1 + 2") (formulaFromFormula formulaCellMap formula3))

testFormulaFromFormula4 = TestCase (assertEqual "FormulaFromFormula 4" (simpleCellFormula "2 * 3 + 2") (formulaFromFormula formulaCellMap formula4))

testFormulaFromFormula5 = TestCase (assertEqual "FormulaFromFormula 5" (simpleCellFormula "(2 + 3) * 2") (formulaFromFormula formulaCellMap formula5))

testFormulaFromFormula6 = TestCase (assertEqual "FormulaFromFormula 6" (simpleCellFormula "2 + 3 = a") (formulaFromFormula formulaCellMap formula6))

testFormulaFromFormula7 = TestCase (assertEqual "FormulaFromFormula 7" (simpleCellFormula "HLOOKUP(D1, E2:F4, FALSE)") (formulaFromFormula formulaCellMap formula7))

testFormulaFromFormula8 = TestCase (assertEqual "FormulaFromFormula 8" (simpleCellFormula "2.5") (formulaFromFormula formulaCellMap formula8))

testFormulaFromFormula9 = TestCase (assertEqual "FormulaFromFormula 8" (simpleCellFormula "\"One\" & \"Two\"") (formulaFromFormula formulaCellMap formula9))

testCell = TestList [
    TestLabel "NullCell" testNullCell
  , TestLabel "CellFromCell" testCellFromCell
  , TestLabel "CellWidth" testCellWidth
  ]

testNullCell = TestList [
  testNullCell1, testNullCell2, testNullCell3, testNullCell4
  ]

testNullCell1 = TestCase (assertEqual "NullCell 1" True (nullCell def))

testNullCell2 = TestCase (assertEqual "NullCell 2" True (nullCell (def & cellStyle ?~ (def & styleNumberFormat ?~ StdNumberFormat NfZero))))

testNullCell3 = TestCase (assertEqual "NullCell 3" False (nullCell (def & cellStyle ?~ (def & styleFill ?~ fill1))))

testNullCell4 = TestCase (assertEqual "NullCell 4" False (nullCell (def & cellValue ?~ CellBool True)))

testCellFromCell = TestList [
  testCellFromCell1, testCellFromCell2, testCellFromCell3, testCellFromCell4,
  testCellFromCell5, testCellFromCell6
  ]

cellxf1 = def & cellXfAlignment ?~ align1

cellxf2 = def & cellXfAlignment ?~ align2

stylemap1 = StyleMap [def cellxf1, cellxf2] (M.fromList [(style1, 1), (style2, 2), (style1 <> style2, 3)]) [] M.empty [] M.empty [] M.empty [] M.empty M.empty

testCellFromCell1 = TestCase (do
  let cell = cellFromCell id stylemap1 formulaCellMap def (def & cellValue ?~ CellDouble 1.1)
  assertEqual "CellFromCell 1 1" Nothing (cell ^. X.cellStyle)
  assertEqual "CellFromCell 1 2" (Just $ CellDouble 1.1) (cell ^. X.cellValue)
  assertEqual "CellFromCell 1 2" Nothing (cell ^. X.cellComment)
  assertEqual "CellFromCell 1 2" Nothing (cell ^. X.cellFormula)
  )

testCellFromCell2 = TestCase (do
  let cell = cellFromCell id stylemap1 formulaCellMap def (def & cellStyle ?~ style1 & cellValue ?~ CellText "Hello")
  assertEqual "CellFromCell 2 1" (Just 1) (cell ^. X.cellStyle)
  assertEqual "CellFromCell 2 2" (Just $ CellText "Hello") (cell ^. X.cellValue)
  assertEqual "CellFromCell 2 2" Nothing (cell ^. X.cellComment)
  assertEqual "CellFromCell 2 2" Nothing (cell ^. X.cellFormula)
  )

testCellFromCell3 = TestCase (do
  let cell = cellFromCell (\t -> t <> "!") stylemap1 formulaCellMap def (def & cellStyle ?~ style2 & cellText ?~ "Text")
  assertEqual "CellFromCell 3 1" (Just 2) (cell ^. X.cellStyle)
  assertEqual "CellFromCell 3 2" (Just $ CellText "Text!") (cell ^. X.cellValue)
  assertEqual "CellFromCell 3 2" Nothing (cell ^. X.cellComment)
  assertEqual "CellFromCell 3 2" Nothing (cell ^. X.cellFormula)
  )

testCellFromCell4 = TestCase (do
  let cell = cellFromCell (\t -> t <> "!") stylemap1 formulaCellMap def (def & cellFormula ?~ formula1)
  assertEqual "CellFromCell 3 1" Nothing (cell ^. X.cellStyle)
  assertEqual "CellFromCell 3 2" Nothing (cell ^. X.cellValue)
  assertEqual "CellFromCell 3 2" Nothing (cell ^. X.cellComment)
  assertEqual "CellFromCell 3 2" (Just $ simpleCellFormula "SUM(E2)") (cell ^. X.cellFormula)
  )

testCellFromCell5 = TestCase (do
  let cell = cellFromCell (\t -> t <> "!") stylemap1 formulaCellMap style1 (def & cellFormula ?~ formula1)
  assertEqual "CellFromCell 3 1" (Just 1) (cell ^. X.cellStyle)
  assertEqual "CellFromCell 3 2" Nothing (cell ^. X.cellValue)
  assertEqual "CellFromCell 3 2" Nothing (cell ^. X.cellComment)
  assertEqual "CellFromCell 3 2" (Just $ simpleCellFormula "SUM(E2)") (cell ^. X.cellFormula)
  )

testCellFromCell6 = TestCase (do
  let cell = cellFromCell (\t -> t <> "!") stylemap1 formulaCellMap style1 (def & cellStyle ?~ style2 & cellFormula ?~ formula1)
  assertEqual "CellFromCell 3 1" (Just 3) (cell ^. X.cellStyle)
  assertEqual "CellFromCell 3 2" Nothing (cell ^. X.cellValue)
  assertEqual "CellFromCell 3 2" Nothing (cell ^. X.cellComment)
  assertEqual "CellFromCell 3 2" (Just $ simpleCellFormula "SUM(E2)") (cell ^. X.cellFormula)
  )

testCellWidth = TestList [
  testCellWidth1, testCellWidth2, testCellWidth3, testCellWidth4,
  testCellWidth5, testCellWidth6, testCellWidth7, testCellWidth8
  ]

testCellWidth1 = TestCase (assertEqual "CellWidth 1" Nothing (cellWidth Nothing Nothing (def & X.cellValue ?~ CellText "A very long piece of text")))

testCellWidth2 = TestCase (assertEqual "CellWidth 2" Nothing (cellWidth (Just WidthNone) Nothing (def & X.cellValue ?~ CellText "A very long piece of text")))

testCellWidth3 = TestCase (assertEqual "CellWidth 3" (Just 15) (cellWidth (Just $ WidthFixed 15) Nothing (def & X.cellValue ?~ CellText "A very long piece of text")))

testCellWidth4 = TestCase (assertEqual "CellWidth 4" (Just 26) (cellWidth (Just $ WidthExpand) Nothing (def & X.cellValue ?~ CellText "A very long piece of text")))

testCellWidth5 = TestCase (assertEqual "CellWidth 5" (Just 20) (cellWidth (Just $ WidthExpandMax 20) Nothing (def & X.cellValue ?~ CellText "A very long piece of text")))

testCellWidth6 = TestCase (assertEqual "CellWidth 6" (Just 13) (cellWidth (Just $ WidthExpandMax 20) Nothing (def & X.cellValue ?~ CellText "012345678912")))

testCellWidth7 = TestCase (assertEqual "CellWidth 7" (Just 9) (cellWidth (Just $ WidthExpand) (Just $ UserNumberFormat "##0.0000") (def & X.cellValue ?~ CellDouble 1.2)))

testCellWidth8 = TestCase (assertEqual "CellWidth 8" Nothing (cellWidth (Just $ WidthExpand) Nothing (def & X.cellValue ?~ CellDouble 1.2)))

testSlab = TestList [
    TestLabel "FMapSlab" testFMapSlab
  , TestLabel "AboveSlab" testAboveSlab
  , TestLabel "LeftSlab" testLeftSlab
  , TestLabel "SlabSize" testSlabSize
  , TestLabel "Collect" testCollect
  , TestLabel "RowSlab" testRowSlab
  , TestLabel "ColumnSlab" testColumnSlab
  ]

slab1 = SEmpty :: Slab (Cell Int)

slab2 = (SCells 1 4 [
    def & cellPos .~ (0, 0) & cellValue ?~ CellText "Hello"
  , def & cellPos .~ (0, 3) & cellValue ?~ CellText "World"
  ]) :: Slab (Cell Int)
  
slab3 = (SCells 2 2 [
    def & cellPos .~ (0, 1) & cellText ?~ 2
  , def & cellPos .~ (1, 0) & cellText ?~ 3
  ]) :: Slab (Cell Int)

testFMapSlab = TestList [
  testFMapSlab1, testFMapSlab2, testFMapSlab3, testFMapSlab4,
  testFMapSlab5, testFMapSlab6, testFMapSlab7, testFMapSlab8
  ]

testFMapSlab1 = TestCase (assertEqual "FMapSlab 1" (0, 0) (slabSize $ id <$> slab1))

testFMapSlab2 = let
    slab = id <$> slab2
    (SCells rsz csz cells) = slab
  in TestCase (do
    assertEqual "FMapSlab 2 1" 2 (length cells)
    assertEqual "FMapSlab 2 2" 1 rsz
    assertEqual "FMapSlab 2 3" 4 csz
    assertEqual "FMapSlab 2 4" (Just $ CellText "Hello") ((cells !! 0) ^. cellValue)
    assertEqual "FMapSlab 4 5" (Just $ CellText "World") ((cells !! 1) ^. cellValue)
    )

testFMapSlab3 = let
    slab = (\c -> c & cellValue ?~ CellText (T.pack $ show $ pos c)) <$> slab2
    (SCells rsz csz cells) = slab
  in TestCase (do
    assertEqual "FMapSlab 3 1" 2 (length cells)
    assertEqual "FMapSlab 3 2" 1 rsz
    assertEqual "FMapSlab 3 3" 4 csz
    assertEqual "FMapSlab 3 4" (Just $ CellText "(0,0)") ((cells !! 0) ^. cellValue)
    assertEqual "FMapSlab 3 5" (Just $ CellText "(0,3)") ((cells !! 1) ^. cellValue)
    )

testFMapSlab4 = let
    slab = (\c -> c & cellValue ?~ CellText (T.pack $ show $ pos c)) <$> (SMargin 4 5 slab2)
    (SMargin ro co (SCells rsz csz cells)) = slab
  in TestCase (do
    assertEqual "FMapSlab 4 1" 2 (length cells)
    assertEqual "FMapSlab 4 2" 1 rsz
    assertEqual "FMapSlab 4 3" 4 csz
    assertEqual "FMapSlab 4 4" 4 ro
    assertEqual "FMapSlab 4 5" 5 co
    assertEqual "FMapSlab 4 6" (Just $ CellText "(0,0)") ((cells !! 0) ^. cellValue)
    assertEqual "FMapSlab 4 7" (Just $ CellText "(0,3)") ((cells !! 1) ^. cellValue)
    )
    
testFMapSlab5 = let
    slab = (\c -> c & cellValue ?~ CellText (T.pack $ show $ pos c)) <$> (SSize 4 5 slab2)
    (SSize rsz' csz' (SCells rsz csz cells)) = slab
  in TestCase (do
    assertEqual "FMapSlab 5 1" 2 (length cells)
    assertEqual "FMapSlab 5 2" 1 rsz
    assertEqual "FMapSlab 5 3" 4 csz
    assertEqual "FMapSlab 5 4" 4 rsz'
    assertEqual "FMapSlab 5 5" 5 csz'
    assertEqual "FMapSlab 5 6" (Just $ CellText "(0,0)") ((cells !! 0) ^. cellValue)
    assertEqual "FMapSlab 5 7" (Just $ CellText "(0,3)") ((cells !! 1) ^. cellValue)
    )
   
testFMapSlab6 = let
    slab = (\c -> c & cellValue ?~ CellText (T.pack $ show $ pos c)) <$> (SRow style1 slab2)
    (SRow style' (SCells rsz csz cells)) = slab
  in TestCase (do
    assertEqual "FMapSlab 6 1" 2 (length cells)
    assertEqual "FMapSlab 6 2" 1 rsz
    assertEqual "FMapSlab 6 3" 4 csz
    assertEqual "FMapSlab 6 4" style1 style'
    assertEqual "FMapSlab 6 5" (Just $ CellText "(0,0)") ((cells !! 0) ^. cellValue)
    assertEqual "FMapSlab 6 6" (Just $ CellText "(0,3)") ((cells !! 1) ^. cellValue)
    )
    
testFMapSlab7 = let
    slab = (\c -> c & cellValue ?~ CellText (T.pack $ show $ pos c)) <$> (SBelow slab2 slab3)
    (SBelow (SCells rsz1 csz1 cells1) (SCells rsz2 csz2 cells2)) = slab
  in TestCase (do
    assertEqual "FMapSlab 5 1" 2 (length cells1)
    assertEqual "FMapSlab 5 2" 1 rsz1
    assertEqual "FMapSlab 5 3" 4 csz1
    assertEqual "FMapSlab 5 5" (Just $ CellText "(0,0)") ((cells1 !! 0) ^. cellValue)
    assertEqual "FMapSlab 5 6" (Just $ CellText "(0,3)") ((cells1 !! 1) ^. cellValue)
    assertEqual "FMapSlab 5 1" 2 (length cells2)
    assertEqual "FMapSlab 5 2" 2 rsz2
    assertEqual "FMapSlab 5 3" 2 csz2
    assertEqual "FMapSlab 5 5" (Just $ CellText "(0,1)") ((cells2 !! 0) ^. cellValue)
    assertEqual "FMapSlab 5 6" (Just $ CellText "(1,0)") ((cells2 !! 1) ^. cellValue)
    )

testFMapSlab8 = let
    slab = (\c -> c & cellValue ?~ CellText (T.pack $ show $ pos c)) <$> (SRight slab2 slab3)
    (SRight (SCells rsz1 csz1 cells1) (SCells rsz2 csz2 cells2)) = slab
  in TestCase (do
    assertEqual "FMapSlab 5 1" 2 (length cells1)
    assertEqual "FMapSlab 5 2" 1 rsz1
    assertEqual "FMapSlab 5 3" 4 csz1
    assertEqual "FMapSlab 5 5" (Just $ CellText "(0,0)") ((cells1 !! 0) ^. cellValue)
    assertEqual "FMapSlab 5 6" (Just $ CellText "(0,3)") ((cells1 !! 1) ^. cellValue)
    assertEqual "FMapSlab 5 1" 2 (length cells2)
    assertEqual "FMapSlab 5 2" 2 rsz2
    assertEqual "FMapSlab 5 3" 2 csz2
    assertEqual "FMapSlab 5 5" (Just $ CellText "(0,1)") ((cells2 !! 0) ^. cellValue)
    assertEqual "FMapSlab 5 6" (Just $ CellText "(1,0)") ((cells2 !! 1) ^. cellValue)
    )

testAboveSlab = TestList [
  testAboveSlab1, testAboveSlab2, testAboveSlab3, testAboveSlab4
  ]

testAboveSlab1 = let
    slab = slab1 >>! slab2
    (SCells rsz csz cells) = slab
  in TestCase (do
    assertEqual "AboveSlab 1 1" 2 (length cells)
    assertEqual "AboveSlab 1 2" 1 rsz
    assertEqual "AboveSlab 1 3" 4 csz
    )
    
testAboveSlab2 = let
    slab = slab2 >>! slab1
    (SCells rsz csz cells) = slab
  in TestCase (do
    assertEqual "AboveSlab 2 1" 2 (length cells)
    assertEqual "AboveSlab 2 2" 1 rsz
    assertEqual "AboveSlab 2 3" 4 csz
    )

testAboveSlab3 = let
    slab = slab2 >>! slab3
    (SBelow (SCells rsz1 csz1 cells1) (SCells rsz2 csz2 cells2)) = slab
  in TestCase (do
    assertEqual "AboveSlab 3 1" 2 (length cells1)
    assertEqual "AboveSlab 3 2" 1 rsz1
    assertEqual "AboveSlab 3 3" 4 csz1
    assertEqual "AboveSlab 3 4" 2 (length cells2)
    assertEqual "AboveSlab 3 5" 2 rsz2
    assertEqual "AboveSlab 3 6" 2 csz2
    )


testAboveSlab4 = let
    slab = slab2 >>! slab3 >>! slab3
    (SBelow (SCells rsz1 csz1 cells1) (SBelow (SCells rsz2 csz2 cells2) (SCells rsz3 csz3 cells3))) = slab
  in TestCase (do
    assertEqual "AboveSlab 4 1" 2 (length cells1)
    assertEqual "AboveSlab 4 2" 1 rsz1
    assertEqual "AboveSlab 4 3" 4 csz1
    assertEqual "AboveSlab 4 4" 2 (length cells2)
    assertEqual "AboveSlab 4 5" 2 rsz2
    assertEqual "AboveSlab 4 6" 2 csz2
    assertEqual "AboveSlab 4 7" 2 (length cells3)
    assertEqual "AboveSlab 4 8" 2 rsz3
    assertEqual "AboveSlab 4 9" 2 csz3
   )


testLeftSlab = TestList [
  testLeftSlab1, testLeftSlab2, testLeftSlab3, testLeftSlab4
  ]

testLeftSlab1 = let
    slab = slab1 >>- slab2
    (SCells rsz csz cells) = slab
  in TestCase (do
    assertEqual "LeftSlab 1 1" 2 (length cells)
    assertEqual "LeftSlab 1 2" 1 rsz
    assertEqual "LeftSlab 1 3" 4 csz
    )
    
testLeftSlab2 = let
    slab = slab2 >>- slab1
    (SCells rsz csz cells) = slab
  in TestCase (do
    assertEqual "LeftSlab 2 1" 2 (length cells)
    assertEqual "LeftSlab 2 2" 1 rsz
    assertEqual "LeftSlab 2 3" 4 csz
    )

testLeftSlab3 = let
    slab = slab2 >>- slab3
    (SRight (SCells rsz1 csz1 cells1) (SCells rsz2 csz2 cells2)) = slab
  in TestCase (do
    assertEqual "LeftSlab 3 1" 2 (length cells1)
    assertEqual "LeftSlab 3 2" 1 rsz1
    assertEqual "LeftSlab 3 3" 4 csz1
    assertEqual "LeftSlab 3 4" 2 (length cells2)
    assertEqual "LeftSlab 3 5" 2 rsz2
    assertEqual "LeftSlab 3 6" 2 csz2
    )


testLeftSlab4 = let
    slab = slab2 >>- slab3 >>- slab3
    (SRight (SCells rsz1 csz1 cells1) (SRight (SCells rsz2 csz2 cells2) (SCells rsz3 csz3 cells3))) = slab
  in TestCase (do
    assertEqual "LeftSlab 4 1" 2 (length cells1)
    assertEqual "LeftSlab 4 2" 1 rsz1
    assertEqual "LeftSlab 4 3" 4 csz1
    assertEqual "LeftSlab 4 4" 2 (length cells2)
    assertEqual "LeftSlab 4 5" 2 rsz2
    assertEqual "LeftSlab 4 6" 2 csz2
    assertEqual "LeftSlab 4 7" 2 (length cells3)
    assertEqual "LeftSlab 4 8" 2 rsz3
    assertEqual "LeftSlab 4 9" 2 csz3
   )

testSlabSize = TestList [
  testSlabSize1, testSlabSize2, testSlabSize3, testSlabSize4,
  testSlabSize5, testSlabSize6, testSlabSize7, testSlabSize8
  ]

testSlabSize1 = TestCase (assertEqual "SlabSize 1" (0, 0) (slabSize slab1))

testSlabSize2 = TestCase (assertEqual "SlabSize 2" (1, 4) (slabSize slab2))

testSlabSize3 = TestCase (assertEqual "SlabSize 3" (2, 2) (slabSize slab3))

testSlabSize4 = TestCase (assertEqual "SlabSize 4" (5, 9) (slabSize $ SMargin 4 5 slab2))

testSlabSize5 = TestCase (assertEqual "SlabSize 5" (4, 5) (slabSize $ SSize 4 5 slab3))

testSlabSize6 = TestCase (assertEqual "SlabSize 6" (2, 2) (slabSize $ SRow style1 slab3))

testSlabSize7 = TestCase (assertEqual "SlabSize 7" (3, 4) (slabSize $ SBelow slab2 slab3))

testSlabSize8 = TestCase (assertEqual "SlabSize 8" (2, 6) (slabSize $ SRight slab2 slab3))

testCollect = TestList [
  testCollect1, testCollect2, testCollect3, testCollect4,
  testCollect5, testCollect6, testCollect7, testCollect8
  ]

collector1 :: Int -> Int -> Int -> Slab (Cell t) -> ([CellValue], Int)
collector1 _row _column context (SCells _rsz _csz cells) = (catMaybes $ map _cellValue cells, context)
collector1 _row _column context _ = ([], context)

testCollect1 = TestCase (assertEqual "Collect 1" [] (collect collector1 1 1 1 slab1))

testCollect2 = TestCase (assertEqual "Collect 2" [CellText "Hello", CellText "World"] (collect collector1 1 1 1 slab2))

testCollect3 = TestCase (assertEqual "Collect 3" [] (collect collector1 1 1 1 slab3))

testCollect4 = TestCase (assertEqual "Collect 4" [CellText "Hello", CellText "World"] (collect collector1 1 1 1 $ SMargin 4 5 slab2))

testCollect5 = TestCase (assertEqual "Collect 5" [] (collect collector1 1 1 1 $ SSize 4 5 slab3))

testCollect6 = TestCase (assertEqual "Collect 6" [] (collect collector1 1 1 1 $ SRow style1 slab3))

testCollect7 = TestCase (assertEqual "Collect 7" [CellText "Hello", CellText "World"] (collect collector1 1 1 1 $ SBelow slab2 slab3))

testCollect8 = TestCase (assertEqual "Collect 8" [CellText "Hello", CellText "World"] (collect collector1 1 1 1 $ SRight slab2 slab3))

testRowSlab = TestList [
  testRowSlab1, testRowSlab2, testRowSlab3, testRowSlab4,
  testRowSlab5
  ]

testRowSlab1 = TestCase (
  let
    slab = rowSlab []
  in
    case slab of
      SEmpty -> assertBool "RowSlab 1" True
      _ -> assertBool "RowSlab 1" False
  )

testRowSlab2 = TestCase (
  let
    slab = rowSlab [def & cellValue ?~ CellText "Hello"]
  in
    case slab of
      (SCells rsz csz cells) -> do
        assertEqual "RowSlab 2 1" 1 (length cells)
        assertEqual "RowSlab 2 2" 1 rsz
        assertEqual "RowSlab 2 3" 1 csz
        assertEqual "RowSlab 2 4" (0, 0) (pos $ cells !! 0)
      _ -> assertBool "RowSlab 2" False
  )

testRowSlab3 = TestCase (
  let
    slab = rowSlab [def, def & cellValue ?~ CellText "Hello"]
  in
    case slab of
      (SCells rsz csz cells) -> do
        assertEqual "RowSlab 3 1" 1 (length cells)
        assertEqual "RowSlab 3 2" 1 rsz
        assertEqual "RowSlab 3 3" 2 csz
        assertEqual "RowSlab 3 4" (0, 1) (pos $ cells !! 0)
      _ -> assertBool "RowSlab 2" False
  )

testRowSlab4 = TestCase (
  let
    slab = rowSlab [def & cellPos .~ (1, 0), def & cellValue ?~ CellText "Hello"]
  in
    case slab of
      (SCells rsz csz cells) -> do
        assertEqual "RowSlab 4 1" 1 (length cells)
        assertEqual "RowSlab 4 2" 2 rsz
        assertEqual "RowSlab 4 3" 2 csz
        assertEqual "RowSlab 4 4" (1, 1) (pos $ cells !! 0)
      _ -> assertBool "RowSlab 2" False
  )

testRowSlab5 = TestCase (
  let
    slab = rowSlab [def & cellValue ?~ CellText "Hello", def, def & cellValue ?~ CellBool True]
  in
    case slab of
      (SCells rsz csz cells) -> do
        assertEqual "RowSlab 5 1" 2 (length cells)
        assertEqual "RowSlab 5 2" 1 rsz
        assertEqual "RowSlab 5 3" 3 csz
        assertEqual "RowSlab 5 4" (0, 0) (pos $ cells !! 0)
        assertEqual "RowSlab 5 4" (0, 2) (pos $ cells !! 1)
      _ -> assertBool "RowSlab 2" False
  )


testColumnSlab = TestList [
  testColumnSlab1, testColumnSlab2, testColumnSlab3, testColumnSlab4,
  testColumnSlab5
  ]

testColumnSlab1 = TestCase (
  let
    slab = columnSlab []
  in
    case slab of
      SEmpty -> assertBool "ColumnSlab 1" True
      _ -> assertBool "ColumnSlab 1" False
  )

testColumnSlab2 = TestCase (
  let
    slab = columnSlab [def & cellValue ?~ CellText "Hello"]
  in
    case slab of
      (SCells rsz csz cells) -> do
        assertEqual "ColumnSlab 2 1" 1 (length cells)
        assertEqual "ColumnSlab 2 2" 1 rsz
        assertEqual "ColumnSlab 2 3" 1 csz
        assertEqual "ColumnSlab 2 4" (0, 0) (pos $ cells !! 0)
      _ -> assertBool "ColumnSlab 2" False
  )

testColumnSlab3 = TestCase (
  let
    slab = columnSlab [def, def & cellValue ?~ CellText "Hello"]
  in
    case slab of
      (SCells rsz csz cells) -> do
        assertEqual "ColumnSlab 3 1" 1 (length cells)
        assertEqual "ColumnSlab 3 2" 2 rsz
        assertEqual "ColumnSlab 3 3" 1 csz
        assertEqual "ColumnSlab 3 4" (1, 0) (pos $ cells !! 0)
      _ -> assertBool "ColumnSlab 2" False
  )

testColumnSlab4 = TestCase (
  let
    slab = columnSlab [def & cellPos .~ (0, 1), def & cellValue ?~ CellText "Hello"]
  in
    case slab of
      (SCells rsz csz cells) -> do
        assertEqual "ColumnSlab 4 1" 1 (length cells)
        assertEqual "ColumnSlab 4 2" 2 rsz
        assertEqual "ColumnSlab 4 3" 2 csz
        assertEqual "ColumnSlab 4 4" (1, 1) (pos $ cells !! 0)
      _ -> assertBool "ColumnSlab 2" False
  )

testColumnSlab5 = TestCase (
  let
    slab = columnSlab [def & cellValue ?~ CellText "Hello", def, def & cellValue ?~ CellBool True]
  in
    case slab of
      (SCells rsz csz cells) -> do
        assertEqual "ColumnSlab 5 1" 2 (length cells)
        assertEqual "ColumnSlab 5 2" 3 rsz
        assertEqual "ColumnSlab 5 3" 1 csz
        assertEqual "ColumnSlab 5 4" (0, 0) (pos $ cells !! 0)
        assertEqual "ColumnSlab 5 4" (2, 0) (pos $ cells !! 1)
      _ -> assertBool "ColumnSlab 2" False
  )

testWorksheet = TestList [
    TestLabel "WorksheetColumnWidth" testWorksheetColumnWidth
  , TestLabel "WorksheetFromCell" testWorksheetFromCell
  , TestLabel "WorksheetFromSlab" testWorksheetFromSlab
  ]

testWorksheetColumnWidth = TestList [
  testWorksheetColumnWidth1, testWorksheetColumnWidth2, testWorksheetColumnWidth3, testWorksheetColumnWidth4
  ]

testWorksheetColumnWidth1 = TestCase (do
  let s1 = worksheetSetColumnWidth def 1 8
  assertEqual "WorksheetColumnWidth 1 1" 1 (length $ s1 ^. wsColumnsProperties)
  let cp = (s1 ^. wsColumnsProperties) !! 0
  assertEqual "WorksheetColumnWidth 1 2" 1 (cpMin cp)
  assertEqual "WorksheetColumnWidth 1 3" 1 (cpMax cp)
  assertEqual "WorksheetColumnWidth 1 4" (Just 8) (cpWidth cp)
  )

testWorksheetColumnWidth2 = TestCase (do
  let s1 = worksheetSetColumnWidth def 1 8
  let s2 = worksheetSetColumnWidth s1 2 20
  assertEqual "WorksheetColumnWidth 2 1" 2 (length $ s2 ^. wsColumnsProperties)
  let cp = (s2 ^. wsColumnsProperties) !! 0
  assertEqual "WorksheetColumnWidth 2 2" 2 (cpMin cp)
  assertEqual "WorksheetColumnWidth 2 3" 2 (cpMax cp)
  assertEqual "WorksheetColumnWidth 2 4" (Just 20) (cpWidth cp)
  )

testWorksheetColumnWidth3 = TestCase (do
  let s1 = worksheetSetColumnWidth def 1 8
  let s2 = worksheetSetColumnWidth s1 1 20
  assertEqual "WorksheetColumnWidth 3 1" 1 (length $ s2 ^. wsColumnsProperties)
  let cp = (s2 ^. wsColumnsProperties) !! 0
  assertEqual "WorksheetColumnWidth 3 2" 1 (cpMin cp)
  assertEqual "WorksheetColumnWidth 3 3" 1 (cpMax cp)
  assertEqual "WorksheetColumnWidth 3" (Just 20) (cpWidth cp)
  )

testWorksheetColumnWidth4 = TestCase (do
  let s1 = worksheetSetColumnWidth def 1 20
  let s2 = worksheetSetColumnWidth s1 1 8
  assertEqual "WorksheetColumnWidth 4 1" 1 (length $ s2 ^. wsColumnsProperties)
  let cp = (s2 ^. wsColumnsProperties) !! 0
  assertEqual "WorksheetColumnWidth 4 2" 1 (cpMin cp)
  assertEqual "WorksheetColumnWidth 4 3" 1 (cpMax cp)
  assertEqual "WorksheetColumnWidth 4 4" (Just 20) (cpWidth cp)
  )


testWorksheetFromCell = TestList [
  testWorksheetFromCell1, testWorksheetFromCell2
  ]

testWorksheetFromCell1 = TestCase (do
  let s1 = worksheetFromCell id stylemap1 formulaCellMap def def 1 1 (def & cellText ?~ "Hello")
  let v1 = s1 ^. atCell (1, 1) . non def . X.cellValue
  assertEqual "WorksheetFromCell 1 1" (Just $ CellText "Hello") v1
  )

testWorksheetFromCell2 = TestCase (do
  let s1 = worksheetFromCell id stylemap1 formulaCellMap def def 1 1 (def & cellStyle ?~ style1 & cellText ?~ "Hello")
  let v1 = s1 ^. atCell (1, 1) . non def . X.cellValue
  let st1 = s1 ^. atCell (1, 1) . non def . X.cellStyle
  assertEqual "WorksheetFromCell 2 1" (Just $ CellText "Hello") v1
  assertEqual "WorksheetFromCell 2 1" (Just 1) st1
  )

testWorksheetFromSlab = TestList [
  testWorksheetFromSlab1, testWorksheetFromSlab2
  ]

testWorksheetFromSlab1 = TestCase (do
  let slab = rowSlab [ def, def & cellText ?~ "Hello", def & cellText ?~ "World" ]
  let s1 = worksheetFromSlab id stylemap1 formulaCellMap def def 1 1 slab
  let v1 = s1 ^. atCell (1, 3) . non def . X.cellValue
  assertEqual "WorksheetFromSlab 1 1" (Just $ CellText "World") v1
  )

testWorksheetFromSlab2 = TestCase (do
  let slab = rowSlab [ def, def & cellStyle ?~ style2 & cellText ?~ "Hello", def & cellText ?~ "World" ]
  let s1 = worksheetFromSlab id stylemap1 formulaCellMap def def 1 1 slab
  let v1 = s1 ^. atCell (1, 2) . non def . X.cellValue
  let st1 = s1 ^. atCell (1, 2) . non def . X.cellStyle
  assertEqual "WorksheetFromSlab 2 1" (Just $ CellText "Hello") v1
  assertEqual "WorksheetFromSlab 2 1" (Just 2) st1
  )

testCreateXlsx = TestList [
  testCreateXlsx1
  ]

testCreateXlsx1 = TestCase (do
  let worksheet = Worksheet "Test" (rowSlab [ def, def & cellText ?~ "Hello", def & cellText ?~ "World" ])
  let xlsx = createXlsx id [worksheet]
  let v1 = xlsx ^. atSheet "Test" . non def . atCell (1, 3) . non def . X.cellValue
  assertEqual "CreateXlsx 1 1" (Just $ CellText "World") v1
  )
