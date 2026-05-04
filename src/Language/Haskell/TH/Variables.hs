{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : TLanguage.Haskell.TH.Variables
Description : Get free variables from template haskell expressions etc
Copyright   : (c) Doug Palmer, 2026
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Collect unbound variables in assorted template Haskell structures.
These can be used, eg, to analyse code and replace unused variables with wildcards in patterns.

Note that, has far as Haskell is concerned, a function name is just a variable that holds a lambda expression. 
So an expression like @a + b@ will produce three variables: @a@, @b@ and @+@.
-}
module Language.Haskell.TH.Variables (
  -- * Primary
    unboundVarsExp
  , unboundVarsPat 
  , unboundVarsDec
  -- * Secondary
  , unboundVarsMatch
  , unboundVarsClause
  , unboundVarsBody
  , unboundVarsGuard
  , unboundVarsStmt
  , unboundVarsFieldExp
  , unboundVarsFieldPat
  , unboundVarsRange
  ) where

import qualified Data.Set as S
import Language.Haskell.TH

unionPair :: (Ord a) => (S.Set a, S.Set a) -> (S.Set a, S.Set a) -> (S.Set a, S.Set a)
unionPair (a1, a2) (b1, b2) = (S.union a1 b1, S.union a2 b2)

unionPairs :: (Ord a) =>  [(S.Set a, S.Set a)] -> (S.Set a, S.Set a)
unionPairs ps = (u1, u2) where
  u1 = S.unions $ map fst ps
  u2 = S.unions $ map snd ps

differencePair :: (Ord a) => (S.Set a, S.Set a)  -> S.Set a
differencePair (v1, v2) = S.difference v2 v1  -- Used minus decs

-- | Get all the visible variables used in an expression
--
--   Variables bound in lambda expressions, etc, are not returned
unboundVarsExp :: Exp -> S.Set Name
unboundVarsExp (VarE name) = S.singleton name
unboundVarsExp (ConE _name) = S.empty
unboundVarsExp (LitE _lit) = S.empty
unboundVarsExp (AppE e1 e2) = S.union (unboundVarsExp e1) (unboundVarsExp e2)
unboundVarsExp (AppTypeE e _t) = unboundVarsExp e
unboundVarsExp (InfixE me1 e2 me3) = S.unions [maybe S.empty unboundVarsExp me1, unboundVarsExp e2, maybe S.empty unboundVarsExp me3]
unboundVarsExp (UInfixE e1 e2 e3) = S.unions [unboundVarsExp e1, unboundVarsExp e2, unboundVarsExp e3]
unboundVarsExp (ParensE e) = unboundVarsExp e
unboundVarsExp (LamE pats e) = S.difference (unboundVarsExp e) (S.unions $ map unboundVarsPat pats)
unboundVarsExp (LamCaseE ms) = differencePair $ unionPairs $ map unboundVarsMatch ms
unboundVarsExp (LamCasesE cs) = differencePair $ unionPairs $ map unboundVarsClause cs
unboundVarsExp (TupE mes) = S.unions $ map (\me -> maybe S.empty unboundVarsExp me) mes
unboundVarsExp (UnboxedTupE mes) = S.unions $ map (\me -> maybe S.empty unboundVarsExp me) mes
unboundVarsExp (UnboxedSumE e _ _) = unboundVarsExp e
unboundVarsExp (CondE e1 e2 e3) = S.unions [unboundVarsExp e1, unboundVarsExp e2, unboundVarsExp e3]
unboundVarsExp (MultiIfE ges) = differencePair $ unionPairs $ map (\(g, e) -> unionPair (S.empty, unboundVarsExp e) (unboundVarsGuard g)) ges
unboundVarsExp (LetE decs e) = differencePair $ unionPairs $ (S.empty, unboundVarsExp e):(map unboundVarsDec decs)
unboundVarsExp (CaseE c ms) = differencePair $ unionPairs $ (S.empty, unboundVarsExp c):(map unboundVarsMatch ms)
unboundVarsExp (DoE _mmn stmts) = differencePair $ unionPairs $ map unboundVarsStmt stmts
unboundVarsExp (MDoE _mmn stmts) = differencePair $ unionPairs $ map unboundVarsStmt stmts
unboundVarsExp (CompE stmts) = differencePair $ unionPairs $ map unboundVarsStmt stmts
unboundVarsExp (ArithSeqE range) = unboundVarsRange range
unboundVarsExp (ListE es) = S.unions $ map unboundVarsExp es
unboundVarsExp (SigE e _t) = unboundVarsExp e
unboundVarsExp (RecConE _name fes) = S.unions $ map unboundVarsFieldExp fes
unboundVarsExp (RecUpdE e fes) = S.unions $ (unboundVarsExp e):(map unboundVarsFieldExp fes)
unboundVarsExp (StaticE e) = unboundVarsExp e
unboundVarsExp (UnboundVarE name) = S.singleton name
unboundVarsExp (LabelE _s) = S.empty
unboundVarsExp (ImplicitParamVarE _s) = S.empty
unboundVarsExp (GetFieldE e _s) = unboundVarsExp e
unboundVarsExp (ProjectionE _ne) = S.empty
unboundVarsExp (TypedBracketE e) = unboundVarsExp e
unboundVarsExp (TypedSpliceE e) = unboundVarsExp e

-- | The variables declared in a pattern
unboundVarsPat :: Pat -> S.Set Name
unboundVarsPat (LitP _l) = S.empty
unboundVarsPat (VarP name) = S.singleton name
unboundVarsPat (TupP ps) = S.unions $ map unboundVarsPat ps
unboundVarsPat (UnboxedTupP ps) = S.unions $ map unboundVarsPat ps
unboundVarsPat (UnboxedSumP p _sal _sar) = unboundVarsPat p
unboundVarsPat (ConP _name _ts ps) = S.unions $ map unboundVarsPat ps
unboundVarsPat (InfixP p1 _name p2) = S.union (unboundVarsPat p1) (unboundVarsPat p2)
unboundVarsPat (UInfixP p1 _name p2) = S.union (unboundVarsPat p1) (unboundVarsPat p2)
unboundVarsPat (ParensP p) = unboundVarsPat p
unboundVarsPat (TildeP p) = unboundVarsPat p
unboundVarsPat (BangP p) = unboundVarsPat p
unboundVarsPat (AsP name p) = S.union (S.singleton name) (unboundVarsPat p)
unboundVarsPat WildP = S.empty
unboundVarsPat (RecP _name fps) = S.unions $ map unboundVarsFieldPat fps
unboundVarsPat (ListP ps) = S.unions $ map unboundVarsPat ps
unboundVarsPat (SigP p _t) = unboundVarsPat p
unboundVarsPat (ViewP _e p) = unboundVarsPat p

-- | The variables declared and the variables used (including those declared) in a declaration
unboundVarsDec :: Dec -> (S.Set Name, S.Set Name)
unboundVarsDec (FunD _name cs) = unionPairs $ map unboundVarsClause cs
unboundVarsDec (ValD p b ds) = unionPairs $ (unboundVarsPat p, S.empty):(unboundVarsBody b):(map unboundVarsDec ds)
unboundVarsDec _ = (S.empty, S.empty)

-- | The variables declared and the variables used (including those declared) in a match
unboundVarsMatch :: Match -> (S.Set Name, S.Set Name)
unboundVarsMatch (Match p b ds) = unionPairs $ (unboundVarsBody b):(unboundVarsPat p, S.empty):(map unboundVarsDec ds)

-- | The variables declared and the variables used (including those declared) in a clause
unboundVarsClause :: Clause -> (S.Set Name, S.Set Name)
unboundVarsClause (Clause ps b ds) = unionPairs $ (unboundVarsBody b):(map (\p -> (unboundVarsPat p, S.empty)) ps) ++ (map unboundVarsDec ds)

-- | The variables declared and the variables used (including those declared) in a body
unboundVarsBody :: Body -> (S.Set Name, S.Set Name)
unboundVarsBody (GuardedB ges) = unionPairs $ map (\(g, e) -> (S.empty, unboundVarsExp e) `unionPair` (unboundVarsGuard g)) ges
unboundVarsBody (NormalB e) = (S.empty, unboundVarsExp e)

-- | The variables declared and the variables used (including those declared) in a guard
unboundVarsGuard :: Guard -> (S.Set Name, S.Set Name)
unboundVarsGuard (NormalG e) = (S.empty, unboundVarsExp e)
unboundVarsGuard (PatG stmts) = unionPairs $ map unboundVarsStmt stmts

-- | The variables declared and the variables used (including those declared) in a statement
unboundVarsStmt:: Stmt -> (S.Set Name, S.Set Name)
unboundVarsStmt (BindS p e) = (unboundVarsPat p, unboundVarsExp e)
unboundVarsStmt (LetS ds) = unionPairs $ map unboundVarsDec ds
unboundVarsStmt (NoBindS e) = (S.empty, unboundVarsExp e)
unboundVarsStmt (ParS sss) = unionPairs $ map (\ss -> unionPairs $ map unboundVarsStmt ss) sss
unboundVarsStmt (RecS ss) = unionPairs $ map unboundVarsStmt ss

-- | The variables declared and the variables used (including those declared) in a field expression
unboundVarsFieldExp :: FieldExp -> S.Set Name
unboundVarsFieldExp (_fn, e) = unboundVarsExp e

-- | The variables declared and the variables used (including those declared) in a field pattern
unboundVarsFieldPat :: FieldPat -> S.Set Name
unboundVarsFieldPat (_fn, e) = unboundVarsPat e

-- | The variables declared and the variables used (including those declared) in a range
unboundVarsRange :: Range -> S.Set Name
unboundVarsRange (FromR e) = unboundVarsExp e
unboundVarsRange (FromThenR e1 e2) = S.union (unboundVarsExp e1) (unboundVarsExp e2)
unboundVarsRange (FromToR e1 e2) = S.union (unboundVarsExp e1) (unboundVarsExp e2)
unboundVarsRange (FromThenToR e1 e2 e3) = S.unions [unboundVarsExp e1, unboundVarsExp e2, unboundVarsExp e3]
