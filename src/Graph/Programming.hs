{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Programming
Description : Forward-chaining dynamic programming 
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}
module Graph.Programming (
    AcceptFunction
  , Chain(..)
  , ChainGraph
  , ChoiceFunction
  , Cut(..)
  , CutFunction
  , EvaluationFunction
  , NullCut
  , Score(..)

  , extend
  , forwards
  , constructTable
  , nullCutFunction
  , passed
  , paths
  , program
  , step
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Graph.Graph
import Data.Aeson
import Data.Maybe
import Data.List (nub, find)

-- | A measure that can be used to evaluate a programming solution.
--   Scores follow the rules of an ordered monoid, with @mempty@ indicating a zero
--   score and @<>@ composing scores according to whatever rule is specified.
--   The @invalid@ score represents an unreachable path and has the following rules:
--   @invalid@ >= a and @invalid@ @<>@ a = a @<>@ @invalid@ = @invalid@.
class (Eq s, Ord s, Show s, Monoid s) => Score s where
  -- | Construct an invalid score - a score that indicates that the solution cannot be used
  invalid :: s

-- | A measure that can be used to indicate a "cut" in the sequence.
--   Cuts are associated with an additional score but may also have additional
--   effects on overall scores.
class (Score s, Show c) => Cut c s where
  cutScore :: c -> s

-- | A null cut, which always does nothing
data (Score s) => NullCut s = NullCut {
} deriving (Show)

instance (Score s) => Cut (NullCut s) s where
  cutScore _ = mempty

-- | Describe a chain (path with a score and optional cut) in
data (Edge e v, Score s, Cut c s) => Chain v e s c = Chain {
    start :: v
  , finish :: v
  , path :: [e]
  , score :: s
  , cut :: Maybe c
} deriving (Show)

-- | Get all the vertices in a chain (including the start and finish)
passed :: (Edge e v, Cut c s) => Chain v e s c -> S.Set v
passed chain = S.insert (start chain) (S.fromList $ map target (path chain))

instance (FromJSON v, FromJSON e, FromJSON s, FromJSON c, Edge e v, Score s, Cut c s) => FromJSON (Chain v e s c) where
    parseJSON (Object v) = do
      start' <- v .: "start"
      finish' <- v .: "finish"
      path' <- v .: "path"
      score' <- v .: "score"
      cut' <- v .:? "cut"
      return Chain { 
          start = start'
        , finish = finish'
        , path = path'
        , score = score'
        , cut = cut'
      }
    parseJSON v = error ("Unable to parse chain object " ++ show v)

instance (ToJSON v, ToJSON e, ToJSON s, ToJSON c, Edge e v, Score s, Cut c s) => ToJSON (Chain v e s c) where
    toJSON (Chain start' finish' path' score' cut') =
      object [ "start" .= start', "finish" .= finish', "path" .= path', "score" .= score', "cut" .= cut' ]

instance (Edge e v, Score s, Cut c s) => Edge (Chain v e s c) v where
  source = start
  target = finish

-- | A chain graph for further processing
data (Edge e v, Score s, Cut c s) => ChainGraph v e s c = ChainGraph {
  forwards :: M.Map v (M.Map v (Chain v e s c)),
  reverses :: M.Map v (M.Map v (Chain v e s c))
} deriving (Show)

instance (Edge e v, Score s, Cut c s) => Graph (ChainGraph v e s c) (Chain v e s c) v where
  vertex graph vid =  fromJust $ find (\v -> identifier v == vid) (M.keys $ forwards graph)
  incoming graph vx = let out = M.lookup vx (reverses graph) in if isNothing out then [] else M.elems (fromJust out)
  outgoing graph vx = let out = M.lookup vx (forwards graph) in if isNothing out then [] else M.elems (fromJust out)
  edge graph begin end = do
    out <- M.lookup begin (forwards graph)
    M.lookup end out
  mirror (ChainGraph forwards' reverses') = ChainGraph reverses' forwards'
  subgraph graph vs = 
    let
      sub accessor = M.fromList $ map (\v -> (v, M.filterWithKey (\k -> \_c -> S.member k vs) (accessor graph M.! v))) (filter (\k -> S.member k vs) (M.keys $ accessor graph))
      forwards' = sub forwards
      reverses' = sub reverses
    in
        ChainGraph forwards' reverses'
  sources graph vx = let out = M.lookup vx (reverses graph) in if isNothing out then S.empty else M.keysSet (fromJust out)
  targets graph vx = let out = M.lookup vx (forwards graph) in if isNothing out then S.empty else M.keysSet (fromJust out)

-- | Accept a sequence of elements as a possible part
type AcceptFunction e = [e] -> Bool

-- | Evaluate a sequence of elements to give a score
type EvaluationFunction e s c = [e] -> Maybe c -> s

-- | Choose a preferred chain out of two possibilities
type ChoiceFunction v e s c = Chain v e s c -> Chain v e s c -> Chain v e s c

-- | Choose whether a vertex is usable
type SelectFunction v = v -> Bool

-- | Generate a cut chain at this location, if acceptable
--   The generated chain is the input chain, with a re-computed cut
type CutFunction v e s c = Chain v e s c -> Maybe (Chain v e s c)

-- | The null cut function is a cut function that always returns no cut
nullCutFunction :: Chain v e s c -> Maybe (Chain v e s c)
nullCutFunction _ = Nothing

-- | Construct a chain graph from a list of chains
fromChains :: (Edge e v, Cut c s) => EvaluationFunction e s c -> [Chain v e s c] -> ChainGraph v e s c
fromChains evaluate chains  =
  let
     chains' = filter (\c -> start c /= finish c && evaluate (path c) Nothing /= invalid) chains
     starts = nub $ map start chains'
     finishes = nub $ map finish chains'
  in ChainGraph {
    forwards = M.fromList $ map (\s -> (s,
          M.fromList $ map (\c -> (finish c, c)) $ filter (\c -> start c == s) chains')
        ) starts,
    reverses = M.fromList $ map (\s -> (s,
          M.fromList $ map (\c -> (start c, c)) $ filter (\c -> finish c == s) chains')
        ) finishes
  }

-- Extend a stage with a new edge
extend :: (Edge e v, Cut c s) => AcceptFunction e -> EvaluationFunction e s c -> CutFunction v e s c -> Chain v e s c -> e -> [Chain v e s c]
extend accept evaluate cutter chain edg =
  let
    path' = (path chain) ++ [edg]
  in
    if accept path' then
      let
        base' = Chain {
                  start = start chain
                , finish = target edg
                , path = path'
                , score = evaluate path' Nothing
                , cut = Nothing
              }
        cut' = cutter base'
      in
        case cut' of
          Nothing -> [base']
          Just cut'' -> [base', cut'']
    else
      []

-- Compute new paths from a list of chains
paths :: (Graph g e v, Cut c s) => g -> AcceptFunction e -> EvaluationFunction e s c -> CutFunction v e s c -> [Chain v e s c] -> v -> [Chain v e s c]
paths graph accept eval cutter current next =
  let
    incoming' = incoming graph next
    sources' = sources graph next
    chains = filter (\c -> S.member (finish c) sources') current
  in
    concat $ map (\e -> 
        concat $ map (\c -> if source e == finish c then extend accept eval cutter c e else []) chains
      ) incoming'

transpose' ::(Edge e v, Cut c s) => ChoiceFunction v e s c -> [Chain v e s c] -> M.Map (v, v) (Chain v e s c)
transpose' choice chains =
  foldl (\sm -> \s -> let k = (start s, finish s) in M.insertWith choice k s sm) M.empty chains

step :: (Graph g e v, Cut c s) => g -> ChoiceFunction v e s c -> AcceptFunction e -> EvaluationFunction e s c -> CutFunction v e s c -> S.Set v -> [Chain v e s c] -> [Chain v e s c]
step graph choice accept eval cutter reachable' current =
  let
    vertices = available graph reachable' (S.fromList $ map finish current) -- Next available vertices
    verticesList = S.toList vertices
    expanded = concat $ map (paths graph accept eval cutter current) verticesList -- Expand out available vertices
    transposedNC = transpose' choice $ filter (isNothing . cut) expanded
    transposedC = transpose' choice $ filter (isJust . cut) expanded
    contractedNC = filter (\s -> S.member (finish s) vertices) $ M.elems transposedNC
    contractedC = filter (\s -> S.member (finish s) vertices) $ M.elems transposedC
    self = map (\s -> Chain s s [] mempty Nothing) verticesList
  in
    current ++ self ++ contractedNC ++ contractedC

constructTable' :: (Graph g e v, Cut c s) => g -> ChoiceFunction v e s c -> AcceptFunction e -> EvaluationFunction e s c -> CutFunction v e s c -> S.Set v -> [Chain v e s c] -> [Chain v e s c]
constructTable' graph choice accept eval cutter reachable' current =
  let
    update = step graph choice accept eval cutter reachable' current
  in
    if length current == length update then update else constructTable' graph choice accept eval cutter reachable' update

-- | Construct a table of stages from vertices that lead from/to a begin/end point
constructTable :: (Graph g e v, Cut c s) => g -> ChoiceFunction v e s c -> AcceptFunction e -> EvaluationFunction e s c -> CutFunction v e s c -> SelectFunction v -> v -> v -> ChainGraph v e s c
constructTable graph choice accept eval cutter select begin end =
  let
    origin = [Chain begin begin [] mempty Nothing]
    reachable' = reachable graph begin end select
    sg = subgraph graph reachable'
    result = fromChains eval (constructTable' sg choice accept eval cutter reachable' origin)
  in
    -- trace ("Reachable = " ++ (show $ S.map identifier reachable) ++ " subgraph " ++ graphSummary sg reachable) result
    result

findBreak' :: (Graph g e v) => g -> v -> v -> S.Set v -> S.Set v -> v
findBreak' chain begin end visited horizon =
  let 
    missing' = S.toList $ S.filter (null . (outgoing chain)) horizon
    visited' = visited `S.union` horizon
    next' = (S.unions $ S.map (targets chain) horizon) `S.difference` visited'
  in
    if S.member end visited then
      end
    else if not $ null missing' then
      head missing'
    else
      findBreak' chain begin end visited' next'

-- | Find the break in a chain where things can't continue      
findBreak :: (Graph g e v) => g -> v -> v -> v
findBreak chain begin end = findBreak' chain begin end S.empty (S.singleton begin)

-- | Construct a program consisting of a chain of chains that gives the optimal value for traversing the graph from begin to end
program :: (Graph g e v, Cut c1 s1, Cut c2 s2) => g -- ^ The graph to traverse
  -> ChoiceFunction v (Chain v e s2 c2) s1 c1 -- ^ The choice function for programs
  -> AcceptFunction (Chain v e s2 c2)  -- The accept function for programs
  -> EvaluationFunction (Chain v e s2 c2) s1 c1 -- The evaluation function for programs
  -> CutFunction v (Chain v e s2 c2) s1 c1 -- The cut function for programs
  -> ChoiceFunction v e s2 c2 -- ^ The choice function for chains
  -> AcceptFunction e -- ^ The acceptance function for chains
  -> EvaluationFunction e s2 c2 -- ^ The evaluation function for chains
  -> CutFunction v e s2 c2 -- ^ The evaluation function for chains
  -> SelectFunction v  -- ^ The function that determines which vertices can be used
  -> v -- ^ The start vertex
  -> v -- ^ The finish vertex
  -> Either v (Chain v (Chain v e s2 c2) s1 c1) -- ^ A program that splits the traversal into a sequence of chains
program graph programChoice programAccept programEval programCut chainChoice chainAccept chainEval chainCut select begin end =
  let
    table = constructTable graph chainChoice chainAccept chainEval chainCut select begin end
    programs = constructTable table programChoice programAccept programEval programCut select begin end
    route = edge programs begin end
  in
    maybe (Left $ findBreak table begin end) Right route
