{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module PropositionalSpec(testPropositional) where

import Test.HUnit
import qualified Data.Map as M
import Data.Propositional

testPropositional = TestList [
      TestLabel "Reduce" testReduce
    , TestLabel "Evaluate" testEvaluate
    , TestLabel "Implications" testImplications
  ] :: Test

testReduce = TestList [
       TestLabel "Reduce 1" testReduce1
    ,  TestLabel "Reduce 2" testReduce2
  ]

testReduce1 =
    TestCase (do
      assertEqual "Reduce 1 1" T (reduce $ T :: Formula Int)
      assertEqual "Reduce 1 2" F (reduce $ F :: Formula Int)
      assertEqual "Reduce 1 3" T (reduce $ And [T, T] :: Formula Int)
      assertEqual "Reduce 1 4" F (reduce $ And [T, F] :: Formula Int)
      assertEqual "Reduce 1 5" F (reduce $ And [F, F] :: Formula Int)
      assertEqual "Reduce 1 6" T (reduce $ Or [T, T] :: Formula Int)
      assertEqual "Reduce 1 7" T (reduce $ Or [T, F] :: Formula Int)
      assertEqual "Reduce 1 8" F (reduce $ Or [F, F] :: Formula Int)
      assertEqual "Reduce 1 9" F (reduce $ Not T :: Formula Int)
      assertEqual "Reduce 1 10" T (reduce $ Not F :: Formula Int)
      assertEqual "Reduce 1 11" T (reduce $ Implies T T :: Formula Int)
      assertEqual "Reduce 1 12" F (reduce $ Implies T F :: Formula Int)
      assertEqual "Reduce 1 13" T (reduce $ Implies F T :: Formula Int)
      assertEqual "Reduce 1 14" T (reduce $ Implies F F :: Formula Int)
    )

testReduce2 =
    TestCase (do
      assertEqual "Reduce 2 1" (Variable 1) (reduce $ Variable 1 :: Formula Int)
      assertEqual "Reduce 2 2" (Variable 1) (reduce $ And [T, Variable 1] :: Formula Int)
      assertEqual "Reduce 2 3" F (reduce $ And [F, Variable 1] :: Formula Int)
      assertEqual "Reduce 2 4" (And [Variable 1, Variable 2]) (reduce $ And [T, Variable 1, Variable 2] :: Formula Int)
      assertEqual "Reduce 2 5" (Variable 1) (reduce $ Or [F, Variable 1] :: Formula Int)
      assertEqual "Reduce 2 5" T (reduce $ Or [T, Variable 1] :: Formula Int)
      assertEqual "Reduce 2 6" (Or [Variable 1, Variable 2]) (reduce $ Or [F, Variable 1, Variable 2] :: Formula Int)
      assertEqual "Reduce 2 7" (Not $ Variable 1) (reduce $ Not $ Variable 1 :: Formula Int)
      assertEqual "Reduce 2 8" (Variable 1) (reduce $ Implies T (Variable 1) :: Formula Int)
      assertEqual "Reduce 2 9" T (reduce $ Implies F (Variable 1) :: Formula Int)
      assertEqual "Reduce 2 10" (Variable 1) (reduce $ Implies (Variable 1) T :: Formula Int)
      assertEqual "Reduce 2 11" (Not $ Variable 1) (reduce $ Implies (Variable 1) F :: Formula Int)
      assertEqual "Reduce 2 12" (Implies (Variable 1) (Variable 2)) (reduce $ Implies (Variable 1) (Variable 2) :: Formula Int)
      assertEqual "Reduce 2 13" (Variable 1) (reduce $ Implies (Variable 1) (Or [T, Variable 2]) :: Formula Int)
    )

testEvaluate = TestList [
       TestLabel "Evaluate 1" testEvaluate1
  ]

testEvaluate1 = let
  mape = M.fromList [(1, T), (2, F)]
  eval = \v -> M.lookup v mape
 in
   TestCase (do
     assertEqual "Evaluate 1 1" T (evaluate eval $ Variable 1)
     assertEqual "Evaluate 1 2" F (evaluate eval $ Variable 2)
     assertEqual "Evaluate 1 3" F (evaluate eval $ And [ Variable 1, Variable 2])
     assertEqual "Evaluate 1 4" (Variable 3) (evaluate eval $ And [ Variable 1, Variable 3])
     assertEqual "Evaluate 1 5" F (evaluate eval $ And [ Variable 3, Variable 2])
     assertEqual "Evaluate 1 6" T (evaluate eval $ Or[ Variable 1, Variable 2])
     assertEqual "Evaluate 1 7" T (evaluate eval $ Or [ Variable 1, Variable 3])
     assertEqual "Evaluate 1 8" (Variable 3) (evaluate eval $ Or [ Variable 3, Variable 2])
     assertEqual "Evaluate 1 9" (Not $ Variable 3) (evaluate eval $ Not $ Or [ Variable 3, Variable 2])
     assertEqual "Evaluate 1 10" F (evaluate eval $ Implies (Variable 1) (Variable 2))
     assertEqual "Evaluate 1 11" (Variable 3) (evaluate eval $ Implies (Variable 1) (Variable 3))
     assertEqual "Evaluate 1 12" T (evaluate eval $ Implies (Variable 2) (Variable 3))
     assertEqual "Evaluate 1 13" (Not $ Variable 3) (evaluate eval $ Implies (Variable 3) (Variable 2))
  )
  
clauses1 = [
      Implies (Variable 1) (Variable 2)
    , Implies (Variable 2) (Variable 3)
  ]

  
clauses2 = [
      Implies (And [(Variable 1), (Variable 2)]) (Variable 3)
    , Implies (And [Or [(Variable 1), (Variable 2)], Not (Variable 4)]) (Variable 5)
  ]
  

testImplications = TestList [
       TestLabel "Implications 1" testImplications1
     , TestLabel "Implications 2" testImplications2
     , TestLabel "Implications 3" testImplications3
     , TestLabel "Implications 4" testImplications4
     , TestLabel "Implications 5" testImplications5
  ]
  
testImplications1 = let
     mape = M.fromList [(1, T), (2, T)]
     eval = \v -> M.lookup v mape
    in
      TestCase (do
        assertEqual "Implications 1 1" Nothing ((implications clauses1 eval) 1)
        assertEqual "Implications 1 2" (Just T) ((implications clauses1 eval) 2)
        assertEqual "Implications 1 3" (Just T) ((implications clauses1 eval) 3)
        assertEqual "Implications 1 4" Nothing ((implications clauses1 eval) 4)
     )

  
testImplications2 = let
     mape = M.fromList [(1, F), (2, T)]
     eval = \v -> M.lookup v mape
    in
      TestCase (do
        assertEqual "Implications 2 1" Nothing ((implications clauses1 eval) 1)
        assertEqual "Implications 2 2" Nothing ((implications clauses1 eval) 2)
        assertEqual "Implications 2 3" (Just T) ((implications clauses1 eval) 3)
        assertEqual "Implications 2 4" Nothing ((implications clauses1 eval) 4)
     )

 
testImplications3 = let
     mape = M.fromList [(1, T), (2, T)]
     eval = \v -> M.lookup v mape
    in
      TestCase (do
        assertEqual "Implications 3 1" Nothing ((implications clauses2 eval) 1)
        assertEqual "Implications 3 2" Nothing ((implications clauses2 eval) 2)
        assertEqual "Implications 3 3" (Just T) ((implications clauses2 eval) 3)
        assertEqual "Implications 3 4" Nothing ((implications clauses2 eval) 4)
        assertEqual "Implications 3 5" Nothing ((implications clauses2 eval) 5)
     )


testImplications4 = let
     mape = M.fromList [(1, F), (2, T), (4, T)]
     eval = \v -> M.lookup v mape
    in
      TestCase (do
        assertEqual "Implications 4 1" Nothing ((implications clauses2 eval) 1)
        assertEqual "Implications 4 2" Nothing ((implications clauses2 eval) 2)
        assertEqual "Implications 4 3" Nothing ((implications clauses2 eval) 3)
        assertEqual "Implications 4 4" Nothing ((implications clauses2 eval) 4)
        assertEqual "Implications 4 5" Nothing ((implications clauses2 eval) 5)
     )


testImplications5 = let
     mape = M.fromList [(1, F), (2, T), (4, F)]
     eval = \v -> M.lookup v mape
    in
      TestCase (do
        assertEqual "Implications 5 1" Nothing ((implications clauses2 eval) 1)
        assertEqual "Implications 5 2" Nothing ((implications clauses2 eval) 2)
        assertEqual "Implications 5 3" Nothing ((implications clauses2 eval) 3)
        assertEqual "Implications 5 4" Nothing ((implications clauses2 eval) 4)
        assertEqual "Implications 5 5" (Just T) ((implications clauses2 eval) 5)
     )
