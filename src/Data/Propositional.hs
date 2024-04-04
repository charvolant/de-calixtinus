{-|
Module      : Propositional
Description : Simple propositional logic and formulas
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Handling of propositional formulas, up to simple clause implication
-}
module Data.Propositional (
    Formula(..)
  , evaluate
  , implications
  , reduce
) where

import qualified Data.Map as M

-- Propositional formulas over a set of variables @a@
data (Eq a, Show a) => Formula a =
    T -- ^ True
  | F -- ^ False
  | Variable a -- ^ A variable
  | Or [Formula a] -- ^ Disjunction
  | And [Formula a] -- ^ Conjunction
  | Not (Formula a) -- ^ Negation
  | Implies (Formula a) (Formula a) -- ^ Implication where @Implies p c@ means @p -> c@
  deriving (Show, Eq)
  
-- | Reduce a formula
reduce :: (Eq a, Show a) => Formula a -- ^ The source formula
   -> Formula a -- ^ The reduced formula
reduce (Or fs) = if null fs' then F else if any (== T) fs' then T else if length fs' == 1 then head fs' else Or fs'
  where
    fs' = filter (/= F) $ map reduce fs
reduce (And fs) = if null fs' then T else if any (== F) fs' then F else if length fs' == 1 then head fs' else And fs'
  where
    fs' = filter (/= T) $ map reduce fs
reduce (Not f) = if f' == T then F else if f' == F then T else Not f'
  where
    f' = reduce f
reduce (Implies p c) = case (p', c') of
    (T, c'') -> c''
    (F, _) -> T
    (p'', T) -> p''
    (p'', F) -> reduce (Not p'')
    (p'', c'') -> Implies p'' c''
  where
    p' = reduce p
    c' = reduce c
reduce f = f

evaluate' :: (Eq a, Show a) => (a -> Maybe (Formula a)) -> Formula a -> Formula a
evaluate' mapping vf@(Variable v) = maybe vf id (mapping v)
evaluate' mapping (Or fs) = Or (map (evaluate' mapping) fs)
evaluate' mapping (And fs) = And (map (evaluate' mapping) fs)
evaluate' mapping (Not f) = Not (evaluate' mapping f)
evaluate' mapping (Implies p c) = Implies (evaluate' mapping p) (evaluate' mapping c)
evaluate' _mapping f = f
 
-- | Evaluate a formula.
--   The evaluation can either retyrn a @T@ or @F@ if there is enough information or a reduced formula
evaluate :: (Eq a, Show a) => (a -> Maybe (Formula a)) -> Formula a -> Formula a
evaluate mapping f = reduce $ evaluate' mapping f

implications'':: (Ord a, Show a) => [Formula a] -> (a -> Maybe (Formula a)) -> M.Map a (Formula a) -> M.Map a (Formula a)
implications'' [] _mapping results = results
implications'' ((Implies p (Variable v)):tail) mapping results = implications'' tail mapping (if evaluate mapping p == T then M.insert v T results else results)
implications'' ((Implies p (Not (Variable v))):tail) mapping results = implications'' tail mapping (if evaluate mapping p == T then M.insert v F results else results)
implications'' (f:_) _mapping _results = error ("Unrecognised implication " ++ show f)

implications':: (Ord a, Show a) => [Formula a] -> (a -> Maybe (Formula a)) -> M.Map a (Formula a) -> M.Map a (Formula a)
implications' clauses mapping result = let
    mapping' = \v -> maybe (mapping v) Just (M.lookup v result)
    result' = implications'' clauses mapping' M.empty
  in
    if result == result' then result else implications' clauses mapping result'
    
-- | Process implications in clause form
-- The input is a set of @T@/@F@ mappings for particular variables and a set of formula in clause form.
-- Either @Implies f (Variable a)@ or @Implies f (Not (Variable a))@.
-- The result is a map of variables known to be true or false as consequence.
implications :: (Ord a, Show a) => [Formula a] -- The list of clauses
  -> (a -> Maybe (Formula a)) -- ^ The initial mapping
  -> (a -> Maybe (Formula a)) -- ^ The final mapping (not including anything in the initial mapping unless it is deduced)
implications clauses mapping = let
    result = implications' clauses mapping M.empty
  in
    \v -> M.lookup v result