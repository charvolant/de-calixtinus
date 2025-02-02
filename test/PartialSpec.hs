{-# LANGUAGE OverloadedStrings #-}
module PartialSpec(testPartial) where

import Test.HUnit
import Data.Partial
import qualified Data.Set as S

testPartial :: Test
testPartial = TestList [
      TestLabel "Sort" testSort
  ]

set1 = S.fromList [] :: S.Set Integer
set2 = S.fromList [1] :: S.Set Integer
set3 = S.fromList [2] :: S.Set Integer
set4 = S.fromList [1, 2] :: S.Set Integer
set5 = S.fromList [3] :: S.Set Integer
set6 = S.fromList [1, 3] :: S.Set Integer
set7 = S.fromList [2, 3] :: S.Set Integer
set8 = S.fromList [1, 2, 3] :: S.Set Integer

testSort = TestList [
      testSort1, testSort2, testSort3, testSort4, testSort5, testSort6
  ]

testSort1 = let
    sorted = topologicalSort S.isSubsetOf [set1, set2, set3, set4]
  in
    TestCase (do
      assertEqual "Topoligical Sort 1 1" 4 (length sorted)
      assertEqual "Topoligical Sort 1 2" set1 (sorted !! 0)
      assertEqual "Topoligical Sort 1 3" set2 (sorted !! 1)
      assertEqual "Topoligical Sort 1 4" set3 (sorted !! 2)
      assertEqual "Topoligical Sort 1 5" set4 (sorted !! 3)
    )

testSort2 = let
    sorted = topologicalSort S.isSubsetOf [set4, set3, set2, set1]
  in
    TestCase (do
      assertEqual "Topoligical Sort 2 1" 4 (length sorted)
      assertEqual "Topoligical Sort 2 2" set1 (sorted !! 0)
      assertEqual "Topoligical Sort 2 3" set3 (sorted !! 1)
      assertEqual "Topoligical Sort 2 4" set2 (sorted !! 2)
      assertEqual "Topoligical Sort 2 5" set4 (sorted !! 3)
    )

testSort3 = let
    sorted = topologicalSort S.isSubsetOf [set1, set4, set8, set2]
  in
    TestCase (do
      assertEqual "Topoligical Sort 3 1" 4 (length sorted)
      assertEqual "Topoligical Sort 3 2" set1 (sorted !! 0)
      assertEqual "Topoligical Sort 3 3" set2 (sorted !! 1)
      assertEqual "Topoligical Sort 3 4" set4 (sorted !! 2)
      assertEqual "Topoligical Sort 3 5" set8 (sorted !! 3)
    )

testSort4 = let
    sorted = topologicalSort S.isSubsetOf [set1, set4, set8, set2, set5]
  in
    TestCase (do
      assertEqual "Topoligical Sort 4 1" 5 (length sorted)
      assertEqual "Topoligical Sort 4 2" set1 (sorted !! 0)
      assertEqual "Topoligical Sort 4 3" set2 (sorted !! 1)
      assertEqual "Topoligical Sort 4 4" set4 (sorted !! 2)
      assertEqual "Topoligical Sort 4 5" set5 (sorted !! 3)
      assertEqual "Topoligical Sort 4 5" set8 (sorted !! 4)
    )
    
testSort5 = let
    sorted = topologicalSort S.isSubsetOf [set8, set2, set5, set7, set6]
  in
    TestCase (do
      assertEqual "Topoligical Sort 5 1" 5 (length sorted)
      assertEqual "Topoligical Sort 5 2" set2 (sorted !! 0)
      assertEqual "Topoligical Sort 5 3" set5 (sorted !! 1)
      assertEqual "Topoligical Sort 5 4" set7 (sorted !! 2)
      assertEqual "Topoligical Sort 5 5" set6 (sorted !! 3)
      assertEqual "Topoligical Sort 5 5" set8 (sorted !! 4)
    )

    
testSort6 = let
    sorted = topologicalSort S.isSubsetOf [set8, set2, set2, set7, set6]
  in
    TestCase (do
      assertEqual "Topoligical Sort 6 1" 5 (length sorted)
      assertEqual "Topoligical Sort 6 2" set2 (sorted !! 0)
      assertEqual "Topoligical Sort 6 3" set2 (sorted !! 1)
      assertEqual "Topoligical Sort 6 4" set7 (sorted !! 2)
      assertEqual "Topoligical Sort 6 5" set6 (sorted !! 3)
      assertEqual "Topoligical Sort 6 5" set8 (sorted !! 4)
    )
