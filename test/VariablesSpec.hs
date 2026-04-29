{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
-- Some tests are commented out. They should be testing parts of the Haskell expressoin, pattern etc structure
-- but don't seem to compile properly in [e| ... |] code.
-- Kept here in anticipation
module VariablesSpec(testVariables) where

import qualified Data.Set as S
import Language.Haskell.TH
import Language.Haskell.TH.Variables
import Test.HUnit

data TD = TD {
    x :: Int
  , y :: Int
}

testVariables :: Test
testVariables = TestList [
    TestLabel "Expression" testExpression
  , TestLabel "Pattern" testPattern
  , TestLabel "Declaration" testDeclaration
  ]

testExpression = TestList [
  testExpression1, testExpression2, testExpression3, testExpression4,
  testExpression5, testExpression6, testExpression7, testExpression8,
  testExpression9, testExpression10, testExpression11, testExpression12,
  testExpression13, testExpression14, testExpression15, testExpression16,
  testExpression17, testExpression18, testExpression19, testExpression20,
  testExpression21, testExpression22, testExpression23, testExpression24,
  testExpression25, testExpression26, {- testExpression27, -} testExpression28,
  {- testExpression29, testExpression30, testExpression31, testExpression32, -}
  testExpression33
  ]


testExpression1 = TestCase (do
  e <- [e|a|]
  assertEqual "Expression 1: VarE" (S.fromList ["a"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression2 = TestCase (do
  e <- [e|Just|]
  assertEqual "Expression 2: ConE" (S.empty) (S.map nameBase $ unboundVarsExp e)
  )

testExpression3 = TestCase (do
  e <- [e|"Hello"|]
  assertEqual "Expression 3: LitE" (S.empty) (S.map nameBase $ unboundVarsExp e)
  )

testExpression4 = TestCase (do
  e <- [e|maybe a id b|]
  assertEqual "Expression 4: AppE" (S.fromList ["maybe","id","a","b"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression5 = TestCase (do
  e <- [e|a @Int|]
  assertEqual "Expression 5: AppTypeE" (S.fromList ["a"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression6 = TestCase (do
  e <- [e|(+ a)|]
  assertEqual "Expression 6:InFixE" (S.fromList ["a", "+"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression7 = TestCase (do
  e <- [e|a + 2|]
  assertEqual "Expression 7:UInFixE" (S.fromList ["a", "+"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression8= TestCase (do
  e <- [e|(f a)|]
  assertEqual "Expression 8:ParensE" (S.fromList ["a", "f"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression9 = TestCase (do
  e <- [e|\z -> a z|]
  assertEqual "Expression 9:LamE" (S.fromList ["a"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression10 = TestCase (do
  e <- [e|\case { "a" -> m1; "b" -> m2 }|]
  assertEqual "Expression 10:LamCaseE" (S.fromList ["m1", "m2"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression11 = TestCase (do
  e <- [e|\cases { 1 2 -> m1; 3 4 -> m2 }|]
  assertEqual "Expression 11:LamCasesE" (S.fromList ["m1", "m2"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression12 = TestCase (do
  e <- [e|(a, b)|]
  assertEqual "Expression 12:TupE" (S.fromList ["a", "b"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression13 = TestCase (do
  e <- [e|(# a, b #)|]
  assertEqual "Expression 13:UnboxedTupE" (S.fromList ["a", "b"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression14 = TestCase (do
  e <- [e|(# | a | #)|]
  assertEqual "Expression 14:UnboxedSumE" (S.fromList ["a"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression15 = TestCase (do
  e <- [e|if a then b else c|]
  assertEqual "Expression 15:CondE" (S.fromList ["a", "b", "c"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression16 = TestCase (do
  e <- [e|if | a -> b + 1 | not a -> b + 2|]
  assertEqual "Expression 16:CondE" (S.fromList ["not","+","a","b"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression17 = TestCase (do
  e <- [e|let a = b + 1 in a + 2|]
  assertEqual "Expression 17:LetE" (S.fromList ["b", "+"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression18 = TestCase (do
  e <- [e|case a1 of 1 -> b1 + 1; 2 -> a1 + b1; q -> q + 42|]
  assertEqual "Expression 18:CaseE" (S.fromList ["a1", "b1", "+"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression19 = TestCase (do
  e <- [e|do { p <- a; b p }|]
  assertEqual "Expression 19:DoE" (S.fromList ["a", "b"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression20 = TestCase (do
  e <- [e|mdo { p <- a; b p }|]
  assertEqual "Expression 20:MDoE" (S.fromList ["a", "b"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression21 = TestCase (do
  e <- [e|[z + 1 | z <- xs]|]
  assertEqual "Expression 21:CompE" (S.fromList ["xs", "+"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression22 = TestCase (do
  e <- [e|[1, 3 .. a]|]
  assertEqual "Expression 22:ArithSeqE" (S.fromList ["a"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression23 = TestCase (do
  e <- [e|[a, b]|]
  assertEqual "Expression 23:ListE" (S.fromList ["a", "b"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression24 = TestCase (do
  e <- [e|[a :: Int]|]
  assertEqual "Expression 25:SigE" (S.fromList ["a"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression25 = TestCase (do
  e <- [e|TD { x = a, y = b }|]
  assertEqual "Expression 25:RecConE" (S.fromList ["a", "b"]) (S.map nameBase $ unboundVarsExp e)
  )

testExpression26 = TestCase (do
  e <- [e|c { x = a, y = b }|]
  assertEqual "Expression 26:RecUpdE" (S.fromList ["a", "b", "c"]) (S.map nameBase $ unboundVarsExp e)
  )

{-
testExpression27 = TestCase (do
  e <- [e|static (f a)|]
  assertEqual "Expression 27:StaticE" (S.fromList ["a", "f"]) (S.map nameBase $ unboundVarsExp e)
  )
-}

-- Not being detected?
testExpression28 = TestCase (do
  e <- [e|_|]
  assertEqual "Expression 28:UnboundVarE" (S.fromList ["_"]) (S.map nameBase $ unboundVarsExp e)
  )

-- Syntax not supported in current system
{-
testExpression29 = TestCase (do
  e <- [e|#x|]
  assertEqual "Expression 29:LabelE" (S.empty) (S.map nameBase $ unboundVarsExp e)
  )

testExpression30 = TestCase (do
  e <- [e|?x|]
  assertEqual "Expression 30:ImplicitParamVarE" (S.empty) (S.map nameBase $ unboundVarsExp e)
  )

testExpression31 = TestCase (do
  e <- [e|x.y|]
  assertEqual "Expression 31:GetFieldE" ((S.fromList ["x"])) (S.map nameBase $ unboundVarsExp e)
  )

testExpression32 = TestCase (do
  e <- [e||.y.z]
  assertEqual "Expression 32:ProjectionE" (S.empty) (S.map nameBase $ unboundVarsExp e)
  )
-}

testExpression33 = TestCase (do
  e <- [e|"Hello " ++ nameBase p ++ " world, " ++ nameBase langs|]
  assertEqual "Expression 34" (S.fromList ["p", "langs", "++", "nameBase"]) (S.map nameBase $ unboundVarsExp e)
  )

-- Can't Ensure tests in order so strip generated suffix
stripIndex n = takeWhile (/= '_') $ nameBase n 

testPattern = TestList [
    testPattern1
  , testPattern2
  , testPattern3
  , testPattern4
  , testPattern5
  , testPattern6
  -- , testPattern7
  -- , testPattern8
  , testPattern9
  , testPattern10
  , testPattern11
  , testPattern12
  , testPattern13
  , testPattern14
  , testPattern15
  -- , testPattern16
  -- , testPattern17
  ]

testPattern1 = TestCase (do
  p <- [p|"Hello"|]
  assertEqual "Pattern 1:LitP" (S.empty) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern2 = TestCase (do
  p <- [p|v|]
  assertEqual "Pattern 2:VarP" (S.fromList ["v"]) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern3 = TestCase (do
  p <- [p|(v1, v2)|]
  assertEqual "Pattern 3:TupP" (S.fromList ["v1", "v2"]) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern4 = TestCase (do
  p <- [p|(# v1, v2 #)|]
  assertEqual "Pattern 4:UnboxedTupP" (S.fromList ["v1", "v2"]) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern5 = TestCase (do
  p <- [p|(# | p1 | #)|]
  assertEqual "Pattern 5:UnboxedSumP" (S.fromList ["p1"]) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern6 = TestCase (do
  p <- [p|TD v1 v2|]
  assertEqual "Pattern 6:ConP" (S.fromList ["v1", "v2"]) (S.map stripIndex $ unboundVarsPat p)
  )

-- Doesn't compile infix
{-
testPattern7 = TestCase (do
  p <- [p|a + b|]
  assertEqual "Pattern 7:InfixP" (S.fromList ["a", "b"]) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern8 = TestCase (do
  p <- [p|a + b|]
  assertEqual "Pattern 8:UInfixP" (S.fromList ["a", "b"]) (S.map stripIndex $ unboundVarsPat p)
  )
-}

testPattern9 = TestCase (do
  p <- [p|(TD a b)|]
  assertEqual "Pattern 9:ParensP" (S.fromList ["a", "b"]) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern10 = TestCase (do
  p <- [p|~a|]
  assertEqual "Pattern 10:TildeP" (S.fromList ["a"]) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern11 = TestCase (do
  p <- [p|!a|]
  assertEqual "Pattern 11:BangP" (S.fromList ["a"]) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern12 = TestCase (do
  p <- [p|a@(Just a1)|]
  assertEqual "Pattern 12:AsP" (S.fromList ["a", "a1"]) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern13 = TestCase (do
  p <- [p|_|]
  assertEqual "Pattern 13:WildP" (S.empty) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern14 = TestCase (do
  p <- [p|TD { x = a }|]
  assertEqual "Pattern 14:RecP" (S.fromList ["a"]) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern15 = TestCase (do
  p <- [p|[a, b]|]
  assertEqual "Pattern 15:ListP" (S.fromList ["a", "b"]) (S.map stripIndex $ unboundVarsPat p)
  )

-- Doesn't compile
{-
testPattern16 = TestCase (do
  p <- [p|a :: Int|]
  assertEqual "Pattern 16:SigP" (S.fromList ["a"]) (S.map stripIndex $ unboundVarsPat p)
  )

testPattern17 = TestCase (do
  p <- [p|view -> a|]
  assertEqual "Pattern 17:ViewP" (S.fromList ["a"]) (S.map stripIndex $ unboundVarsPat p)
  )
-}

_showPairs (s1, s2) = (S.map show s1, S.map show s2)

stripPairs (s1, s2) = (S.map stripIndex s1, S.map stripIndex s2)

testDeclaration = TestList [
    testDeclaration1
  , testDeclaration2
  , testDeclaration3
 ]


testDeclaration1 = TestCase (do
  [d] <- [d| f a = a |]
  assertEqual "Declaration 1:FunD" (S.fromList ["a"], S.fromList ["a"]) (stripPairs $ unboundVarsDec d)
  )

testDeclaration2 = TestCase (do
  [d] <- [d| f a b = c where c = a + b + d |]
  assertEqual "Declaration 1:FunD" (S.fromList ["a", "b", "c"], S.fromList ["a", "b", "c", "d", "+"]) (stripPairs $ unboundVarsDec d)
  )

testDeclaration3 = TestCase (do
  [d] <- [d| f = c |]
  assertEqual "Declaration 3:ValD" (S.fromList ["f"], S.fromList ["c"]) (stripPairs $ unboundVarsDec d)
  )
