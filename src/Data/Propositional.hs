{-|
Module      : Propositional
Description : Simple propositional logic and formulas
Copyright   : (c) Doug Palmer, 2024
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Handling of propositional formulas, up to simple program clause implication
-}
module Data.Propositional (
    Formula(..)
  , Substitution
  , clauseConsequentVar
  , evaluate
  , formulaMap
  , implications
  , invert
  , isClause
  , overlay
  , reduce
  , substitutionFromDomain
  , substitutionFromMap
) where

import qualified Data.Map as M
import qualified Data.Set as S

-- Propositional formulas over a set of variables @a@
data (Eq a) => Formula a =
    T -- ^ True
  | F -- ^ False
  | Variable a -- ^ A variable
  | Or [Formula a] -- ^ Disjunction
  | And [Formula a] -- ^ Conjunction
  | Not (Formula a) -- ^ Negation
  | Implies (Formula a) (Formula a) -- ^ Implication where @Implies p c@ means @p -> c@
  deriving (Eq)

instance (Eq a, Show a) => Show (Formula a) where
  show T = "true"
  show F = "false"
  show (Variable v) = show v
  show (Or []) = "empty or"
  show (Or (f:fs)) = "(" <> foldl (\s -> \f' -> s <> " \x2228 " <> show f') (show f) fs <> ")"
  show (And []) = "empty and"
  show (And (f:fs)) = "(" <> foldl (\s -> \f' -> s <> " \x2227 " <> show f') (show f) fs <> ")"
  show (Not f) = "\x00ac " <> show f
  show (Implies p c) = show p <> " \x2192 " <> show c

-- | A substitution is a partial mapping of variables onto other formulae
--   Generally, mappings on to @T@ or @F@ are expected; but other formulas could be used 
type Substitution a = a -> Maybe (Formula a)

-- | Map a formula from one variable type to another
formulaMap :: (Eq a, Eq b) => (a -> b) -- ^ The mappign function
 -> Formula a -- ^ The source formula
 -> Formula b -- ^ The mapped formula
formulaMap _mapper T = T
formulaMap _mapper F = F
formulaMap mapper (Variable a) = Variable (mapper a)
formulaMap mapper (Or fs) = Or (map (formulaMap mapper) fs)
formulaMap mapper (And fs) = And (map (formulaMap mapper) fs)
formulaMap mapper (Not f) = Not (formulaMap mapper f)
formulaMap mapper (Implies p c) = Implies (formulaMap mapper p) (formulaMap mapper c)

-- | Reduce a formula
reduce :: (Eq a) => Formula a -- ^ The source formula
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

evaluate' :: (Eq a) => Substitution a -> Formula a -> Formula a
evaluate' mapping vf@(Variable v) = maybe vf id (mapping v)
evaluate' mapping (Or fs) = Or (map (evaluate' mapping) fs)
evaluate' mapping (And fs) = And (map (evaluate' mapping) fs)
evaluate' mapping (Not f) = Not (evaluate' mapping f)
evaluate' mapping (Implies p c) = Implies (evaluate' mapping p) (evaluate' mapping c)
evaluate' _mapping f = f
 
-- | Evaluate a formula.
--   The evaluation can either retyrn a @T@ or @F@ if there is enough information or a reduced formula
evaluate :: (Eq a) => Substitution a -> Formula a -> Formula a
evaluate mapping f = reduce $ evaluate' mapping f

-- | Create a substrition from a map of values
substitutionFromMap :: (Ord a) =>  M.Map a (Formula a)  -- ^ The map of substitutions
  -> Substitution a -- ^ The resulting substitution
substitutionFromMap smap = \v -> smap M.!? v

-- | Create a base substituion over a domain
--   Anything not explicitly true is asumed to be false
substitutionFromDomain :: (Ord a) => S.Set a -- ^ The domain over which the substituion operates
  -> S.Set a -- ^ The subset of the domain that is true
  -> Substitution a -- ^ The resulting substitution
substitutionFromDomain domain truths = substitutionFromMap $ M.fromSet (\k -> if S.member k truths then T else F) domain

-- | Overlay two substituions.
--   The second substitution overlays values of the first.
overlay :: Substitution a -- ^ The underlying substitution
  -> Substitution a -- ^ The substitution that overlays (takes priority over) the underlying one
  -> Substitution a -- ^ The combined substitution
overlay dflt sub = \v -> maybe (dflt v) Just (sub v)

invert' :: (Eq a) => Maybe (Formula a) -> Maybe (Formula a)
invert' Nothing = Nothing
invert' (Just T) = Just F
invert' (Just F) = Just T
invert' (Just (Not f)) = Just f
invert' (Just f) = Just (Not f)

-- | Invert a substitution
invert :: (Eq a) => Substitution a -- ^ The source substitution
  -> Substitution a -- ^ The inverted substition (T <-> F, f <-> Not f)
invert sub = invert' . sub

implications'':: (Ord a) => [Formula a] -> Substitution a -> M.Map a (Formula a) -> M.Map a (Formula a)
implications'' [] _mapping results = results
implications'' ((Implies p (Variable v)):rest) mapping results = implications'' rest mapping (if evaluate mapping p == T then M.insert v T results else results)
implications'' _ _mapping _results = error "Formula not in clause form"

implications':: (Ord a) => [Formula a] -> Substitution a -> M.Map a (Formula a) -> M.Map a (Formula a)
implications' clauses mapping result = let
    mapping' = \v -> maybe (mapping v) Just (M.lookup v result)
    result' = implications'' clauses mapping' M.empty
  in
    if result == result' then result else implications' clauses mapping result'
    
-- | Process implications in clause form
-- The input is a set of @T@/@F@ mappings for particular variables and a set of formula in program clause form,
-- Basically @Implies f (Variable a)@.
-- The result is a map of variables known to be true or false as consequence.
implications :: (Ord a) => [Formula a] -- The list of clauses
  -> Substitution a -- ^ The initial mapping
  -> Substitution a -- ^ The final mapping (not including anything in the initial mapping unless it is deduced)
implications clauses mapping = let
    result = implications' clauses mapping M.empty
  in
    \v -> M.lookup v result

-- | Is this formula a program clause?
--   A program clause has the form @Implies p (Variable v)@    
isClause :: (Eq a) => Formula a -> Bool
isClause (Implies _p (Variable _v)) = True
isClause _ = False

-- | Get the consequent of an implication
--   If p -> q then the consequent is q otherwise the antecedent is the formula  
clauseConsequentVar :: (Eq a) => Formula a -> a
clauseConsequentVar (Implies _ (Variable v)) = v
clauseConsequentVar _ = error "Formula is not clause"