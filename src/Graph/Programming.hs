{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Programming
Description : Forward-chaining dynamic programming 
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

Forward-chaining dynamic programming

Choose an optimal path through a graph, dividing the graph into stages.
The programming is forward chaining: all possible routes are built, using the incremental optimisation of
dynamic programming to reduce the search space and then the best route is selected from a table of possibilities.

The optimal paths and stages are expressed as `Chains` of vertices and edges.
-}
module Graph.Programming (
  -- * Solution Evaluation
    Score(..)
  , AcceptFunction
  , ChoiceFunction
  , EvaluationFunction
  , SelectFunction
  -- ** Failure
  , Failure(..)
  , emptyFailure
  , failure
 -- * Chains
  , Chain(..)
  , ChainGraph
  , passed
  , fromChains
  -- * Programming
  , program
  , constructTable
  , extend
  , forwards
  , paths
  , step
) where

import Control.DeepSeq
import qualified Data.Map as M
import qualified Data.Set as S
import Graph.Graph
import Data.Maybe
import Data.List (nub, find)
import Data.Text (Text)

-- | A measure that can be used to evaluate a programming solution.
--
--   Scores follow the rules of an ordered monoid, with @mempty@ indicating a zero
--   score and @<>@ composing scores according to whatever rule is specified.
--   The @invalid@ score represents an unreachable path and has the following properties
--
--   prop> invalid >= a
--   prop> invalid <> a = a <> invalid = invalid.
class (Eq s, Ord s, Show s, Monoid s) => Score s where
  -- | Construct an invalid score - a score that indicates that the solution cannot be used
  invalid :: s
  -- | Is this score invalid?
  --   By default, this just checks against the invalid score
  isInvalid :: s -> Bool
  isInvalid sc = sc == invalid

-- | Describe a chain (path with a score) from a start to an end vertex along a sequence of edges.
--
--   Chains have an evaluated `Score` attached.
--
--   Chains are, themselves, `Edge`s, with the start and finish the source and target, and can be collected together into a `ChainGraph`.
data (Edge e v, Score s) => Chain v e s = Chain {
  start :: v,
  finish :: v,
  path :: [e],
  score :: s
} deriving (Show)

-- | Get all the vertices in a chain (including the start and finish)
passed :: (Edge e v, Score s) => Chain v e s -> S.Set v
passed chain = S.insert (start chain) (S.fromList $ map target (path chain))

instance (Edge e v, Score s, NFData v, NFData e, NFData s) => NFData (Chain v e s) where
  rnf c = start c
    `deepseq` finish c
    `deepseq` path c
    `deepseq` score c
    `deepseq` ()

instance (Edge e v, Score s) => Edge (Chain v e s) v where
  source = start
  target = finish

-- | A chain graph for further processing
--
--   Chain graphs are built with `Chain`s as edges and the start and finish of each chain as vertices.
data Edge e v => ChainGraph v e s = ChainGraph {
  forwards :: M.Map v (M.Map v (Chain v e s)),
  reverses :: M.Map v (M.Map v (Chain v e s))
} deriving (Show)

instance (Edge e v, Score s) => Graph (ChainGraph v e s) (Chain v e s) v where
  vertex graph vid =  fromJust $ find (\v -> identifier v == vid) (M.keys $ forwards graph)
  incoming graph vx = let out = M.lookup vx (reverses graph) in if isNothing out then [] else M.elems (fromJust out)
  outgoing graph vx = let out = M.lookup vx (forwards graph) in if isNothing out then [] else M.elems (fromJust out)
  edge graph begin end = do
    out <- M.lookup begin (forwards graph)
    M.lookup end out
  mirror (ChainGraph forwards' reverses') = ChainGraph reverses' forwards'
  subgraph graph vt _et =
    let
      sub accessor = M.fromList $ map (\v -> (v, M.filterWithKey (\k -> \_c -> vt k) (accessor graph M.! v))) (filter vt (M.keys $ accessor graph))
      forwards' = sub forwards
      reverses' = sub reverses
    in
        ChainGraph forwards' reverses'
  sources graph vx = let out = M.lookup vx (reverses graph) in if isNothing out then S.empty else M.keysSet (fromJust out)
  targets graph vx = let out = M.lookup vx (forwards graph) in if isNothing out then S.empty else M.keysSet (fromJust out)

instance (Edge e v, Score s, NFData v, NFData e, NFData s) => NFData (ChainGraph v e s) where
  rnf cg = forwards cg
    `deepseq` reverses cg
    `deepseq` ()

-- | Accept a sequence of elements as a possible part
--
--   Accept functions need to cut off sequences that are beyond saving, pruning the search space for the programming algorithm
type AcceptFunction e = [e] -> Bool

-- | Evaluate a sequence of elements to give a (possibly modified) sequence and a score
type EvaluationFunction e s = [e] -> ([e], s)

-- | Choose a preferred chain out of two possibilities
--
--   In general, the chain with the lower score will be selected, however that is subject to the whims of the programmer
type ChoiceFunction v e s = Chain v e s -> Chain v e s -> Chain v e s

-- | Choose whether a vertex is usable during programming
type SelectFunction v = v -> Bool

-- | Construct a chain graph from a list of chains
--
--  `invalid` chains are not included
fromChains :: (Edge e v, Score s) => EvaluationFunction e s -- ^ evaluate the chain before including it in the graph (not used)
  -> [Chain v e s] -- ^ The list of chains to build into a graph
  -> ChainGraph v e s -- ^ The resulting chain graph
fromChains _evaluate chains  =
  let
     chains' = filter (\c -> start c /= finish c && (not $ isInvalid $ score c)) chains
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

-- | Construct a chain graph
emptyChainGraph :: (Edge e v) => ChainGraph v e s
emptyChainGraph  = ChainGraph M.empty M.empty

-- | A failure in programming
--
--   Programming failures attempt to report the source of the failure and the underlying context
data (Edge e v, Score s1, Score s2) => Failure v e s1 s2 = Failure Text (Maybe v) [Chain v e s1] (ChainGraph v e s1) (ChainGraph v (Chain v e s1) s2)
 deriving (Show)

instance (Edge e v, Score s1, Score s2, NFData s1, NFData s2, NFData v, NFData e) => NFData (Failure v e s1 s2) where
  rnf (Failure message' location' table1' chain1' chain2') = message'
    `deepseq` location'
    `deepseq` table1'
    `deepseq` chain1'
    `deepseq` chain2'
    `deepseq` ()

-- | Create a failure with the constructed link and program table for debugging
failure :: (Edge e v, Score s1, Score s2) => Text -- ^ The error message
  -> Maybe v -- ^ The vertex where the failure occurs, if found
  -> [Chain v e s1] -- ^ The base chains
  -> (ChainGraph v e s1) -- ^ The link table
  -> (ChainGraph v (Chain v e s1) s2) -- ^ The program table
  -> Failure v e s1 s2 -- ^ The resulting failure
failure msg v tab chains prog = Failure msg v tab chains prog

-- | Create a failure with no debugging information
emptyFailure :: (Edge e v, Score s1, Score s2) => Text -- ^ The error message
  -> Maybe v -- ^ The optional location of the failure
  -> Failure v e s1 s2 -- THe resulting failure
emptyFailure msg v = failure msg v [] emptyChainGraph emptyChainGraph

-- | Extend a stage with a new edge
extend :: (Edge e v, Score s) => AcceptFunction e -- ^ Accept an sequence of edges as usable before evaluation
  -> EvaluationFunction e s -- ^ Evaluate the resulting chain
  -> Chain v e s -- ^ The initial chain
  -> e -- ^ The edge to add to the initial chain
  -> Maybe (Chain v e s) -- ^ Either a new, re-evaluated, chain with the edge added or nothing if the path is not accepted
extend accept evaluate chain edg =
  let
    path' = (path chain) ++ [edg]
  in
    if accept path' then let
        (path'', score') = evaluate path'
      in
        Just (Chain {
          start = start chain,
          finish = target edg,
          path = path'',
          score = score'
        })
    else
      Nothing

-- | Compute new paths from a list of chains
paths :: (Graph g e v, Score s) => g -- ^ The source graph
  -> AcceptFunction e -- ^ Accept an sequence of edges as usable before evaluation
  -> EvaluationFunction e s -- ^ Evaluate the resulting chain
  -> [Chain v e s] -- ^ A pre-exisiting list of chains
  -> v -- ^ The potential target vertex
  -> [Chain v e s] -- ^ A list of new chains that have the target vertex as a finish
paths graph accept eval current next =
  let
    incoming' = incoming graph next
    sources' = sources graph next
    chains = filter (\c -> S.member (finish c) sources') current
  in
    catMaybes $ concat $ map (\e -> 
        map (\c -> if source e == finish c then extend accept eval c e else Nothing) chains
      ) incoming'

transpose' ::(Edge e v, Score s) => ChoiceFunction v e s -> [Chain v e s] -> M.Map (v, v) (Chain v e s)
transpose' choice chains =
  foldl (\sm -> \s -> let k = (start s, finish s) in M.insertWith choice k s sm) M.empty chains

-- | Take a single programming step
--
--   Extend the chains between start and finish vertices one step along the graph and choose the optimal chain between each pair.
step :: (Graph g e v, Score s) => g -> ChoiceFunction v e s -- ^ Choose between two chains
  -> AcceptFunction e -- ^ Accept an sequence of edges as usable before evaluation
  -> EvaluationFunction e s -- ^ Evaluate a candidiate chain
  -> S.Set v -- The set of vertices that are reachable
  -> [Chain v e s] -- ^ The current list of chains
  -> [Chain v e s] -- ^ A new list of chains, extending the chains one step
step graph choice accept eval reachable' current =
  let
    vertices = available graph reachable' (S.fromList $ map finish current) -- Next available vertices
    verticesList = S.toList vertices
    expanded = concat $ map (paths graph accept eval current) verticesList -- Expand out available vertices
    transposed = transpose' choice expanded
    contracted = filter (\s -> S.member (finish s) vertices) $ M.elems transposed
    self = map (\s -> Chain s s [] mempty) verticesList
    result = current ++ self ++ contracted
  in
    -- trace ("Reachable = " ++ (show $ S.map identifier reachable') ++  "\n vertices = " ++ (show $ S.map identifier vertices) ++ "\n contracted = " ++ (show $ S.fromList $ map (\c -> (identifier $ start c, identifier $ finish c)) contracted)) $
    result

constructTable' :: (Graph g e v, Score s) => g -> ChoiceFunction v e s -> AcceptFunction e -> EvaluationFunction e s -> S.Set v -> [Chain v e s] -> [Chain v e s]
constructTable' graph choice accept eval reachable' current =
  let
    update = step graph choice accept eval reachable' current
  in
    if length current == length update then update else constructTable' graph choice accept eval reachable' update

-- | Construct a table of stages from vertices that lead fron\/to a begin\/end point
constructTable :: (Graph g e v, Score s) => g -> ChoiceFunction v e s -> AcceptFunction e -> EvaluationFunction e s -> SelectFunction v -> v -> v -> ([Chain v e s], ChainGraph v e s)
constructTable graph choice accept eval select begin end =
  let
    origin = [Chain begin begin [] mempty]
    reachable' = reachable graph begin end select
    sg = subgraph graph (\v -> S.member v reachable') (const True)
    table = constructTable' sg choice accept eval reachable' origin
    result = fromChains eval table
  in
    -- trace ("Reachable = " ++ (show $ S.map identifier reachable') ++ " subgraph " ++ graphSummary sg reachable') result
    -- trace ("Reachable = " ++ (show $ S.map identifier reachable') ++ "\n table = " ++ (show $ S.fromList $ map (\c -> (identifier $ start c, identifier $ finish c)) table) ++ "\n reached = " ++ (show $ S.map identifier $ M.keysSet $ forwards result)) result
    (table, result)

findBreak' :: (Graph g e v) => g -> v -> v -> S.Set v -> S.Set v -> Maybe v
findBreak' chain begin end visited horizon =
  let 
    missing' = S.toList $ S.filter (null . (outgoing chain)) horizon
    visited' = visited `S.union` horizon
    next' = (S.unions $ S.map (targets chain) horizon) `S.difference` visited'
  in
    if S.member end visited' then
      Nothing
    else case missing' of
      [] -> findBreak' chain begin end visited' next'
      (hm:_) -> Just hm

-- | Find the break in a chain where things can't continue      
findBreak :: (Graph g e v) => g -> v -> v -> Maybe v
findBreak chain begin end = findBreak' chain begin end S.empty (S.singleton begin)


-- | Construct a program consisting of a chain of chains that gives the optimal value for traversing the graph from begin to end
--
--   The reslt is a chain of stages, with each stage a chain of edges between a start and end point.
program :: (Graph g e v, Score s1, Score s2) => g -- ^ The graph to traverse
  -> ChoiceFunction v (Chain v e s2) s1 -- ^ The choice function for programs
  -> AcceptFunction (Chain v e s2)  -- The accept function for programs
  -> EvaluationFunction (Chain v e s2) s1 -- The evaluation function for programs
  -> ChoiceFunction v e s2 -- ^ The choice function for chains
  -> AcceptFunction e -- ^ The acceptance function for chains
  -> EvaluationFunction e s2 -- ^ The evaluation function for chains
  -> SelectFunction v  -- ^ The function that determines which vertices can be used
  -> v -- ^ The start vertex
  -> v -- ^ The finish vertex
  -> Either (Failure v e s2 s1) (Chain v (Chain v e s2) s1) -- ^ A program that splits the traversal into a sequence of chains
program graph programChoice programAccept programEval chainChoice chainAccept chainEval select begin end =
  let
    (cchains, table) = constructTable graph chainChoice chainAccept chainEval select begin end
    (_pchains, programs) = constructTable table programChoice programAccept programEval select begin end
    route = edge programs begin end
  in
    if isJust route then
      Right (fromJust route)
    else let
        break' = findBreak table begin end
      in
        if isJust break' then
          Left $ failure "No path at " break' cchains table programs
        else let
            break'' = findBreak programs begin end
          in
            if isJust break'' then
              Left $ failure "No chain at " break'' cchains table programs
            else
              Left $ failure "Unable to find break point" Nothing cchains table programs

