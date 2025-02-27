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
  , EvaluationFunction
  , Failure(..)
  , Score(..)

  , emptyFailure
  , extend
  , failure
  , forwards
  , fromChains
  , constructTable
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
import Data.Text (Text)

-- | A measure that can be used to evaluate a programming solution.
--   Scores follow the rules of an ordered monoid, with @mempty@ indicating a zero
--   score and @<>@ composing scores according to whatever rule is specified.
--   The @invalid@ score represents an unreachable path and has the following rules:
--   @invalid@ >= a and @invalid@ @<>@ a = a @<>@ @invalid@ = @invalid@.
class (Eq s, Ord s, Show s, Monoid s) => Score s where
  -- | Construct an invalid score - a score that indicates that the solution cannot be used
  invalid :: s

-- | Describe a chain (path with a score) in
data (Edge e v, Score s) => Chain v e s = Chain {
  start :: v,
  finish :: v,
  path :: [e],
  score :: s
} deriving (Show)

-- | Get all the vertices in a chain (including the start and finish)
passed :: (Edge e v, Score s) => Chain v e s -> S.Set v
passed chain = S.insert (start chain) (S.fromList $ map target (path chain))

instance (FromJSON v, FromJSON e, FromJSON s, Edge e v, Score s) => FromJSON (Chain v e s) where
    parseJSON (Object v) = do
      start' <- v .: "start"
      finish' <- v .: "finish"
      path' <- v .: "path"
      score' <- v .: "score"
      return Chain { start = start', finish = finish', path = path', score = score' }
    parseJSON v = error ("Unable to parse chain object " ++ show v)

instance (ToJSON v, ToJSON e, ToJSON s, Edge e v, Score s) => ToJSON (Chain v e s) where
    toJSON (Chain start' finish' path' score') =
      object [ "start" .= start', "finish" .= finish', "path" .= path', "score" .= score' ]

instance (Edge e v, Score s) => Edge (Chain v e s) v where
  source = start
  target = finish

-- | A chain graph for further processing
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

-- | Evaluate a sequence of elements to give a possibly modified sequence and a score
type EvaluationFunction e s = [e] -> ([e], s)

-- | Choose a preferred chain out of two possibilities
type ChoiceFunction v e s = Chain v e s -> Chain v e s -> Chain v e s

-- | Choose whether a vertex is usable
type SelectFunction v = v -> Bool

-- | Construct a chain graph from a list of chains
fromChains :: (Edge e v, Score s) => EvaluationFunction e s -> [Chain v e s] -> ChainGraph v e s
fromChains evaluate chains  =
  let
     chains' = filter (\c -> start c /= finish c && snd (evaluate (path c)) /= invalid) chains
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

-- | Construct a chain graph from a list of chains
emptyChainGraph :: (Edge e v, Score s) => ChainGraph v e s
emptyChainGraph  = ChainGraph M.empty M.empty

data (Edge e v, Score s1, Score s2) => Failure v e s1 s2 = Failure Text (Maybe v) (ChainGraph v e s1) (ChainGraph v (Chain v e s1) s2)
 deriving (Show)

-- | Create a failure with the constructed link and program table for debugging
failure :: (Edge e v, Score s1, Score s2) => Text -- ^ The error message
  -> Maybe v -- ^ The vertex where the failure occurs, if found
  -> (ChainGraph v e s1) -- ^ The link table
  -> (ChainGraph v (Chain v e s1) s2) -- ^ The program table
  -> Failure v e s1 s2 -- ^ The resulting failure
failure msg v tab prog = Failure msg v tab prog

-- | Create a failure with no debugging information
emptyFailure :: (Edge e v, Score s1, Score s2) => Text -- ^ The error message
  -> Maybe v -- ^ The optional location of the failure
  -> Failure v e s1 s2 -- THe resulting failure
emptyFailure msg v = failure msg v emptyChainGraph emptyChainGraph

-- Extend a stage with a new edge
extend :: (Edge e v, Score s) => AcceptFunction e -> EvaluationFunction e s -> Chain v e s -> e -> Maybe (Chain v e s)
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

-- Compute new paths from a list of chains
paths :: (Graph g e v, Score s) => g -> AcceptFunction e -> EvaluationFunction e s -> [Chain v e s] -> v -> [Chain v e s]
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

step :: (Graph g e v, Score s) => g -> ChoiceFunction v e s -> AcceptFunction e -> EvaluationFunction e s -> S.Set v -> [Chain v e s] -> [Chain v e s]
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
    -- trace ("Vertices = " ++ (show $ S.map identifier vertices) ++ "\n contracted = " ++ (show $ S.fromList $ map (\c -> (identifier $ start c, identifier $ finish c)) contracted)) result
    result

constructTable' :: (Graph g e v, Score s) => g -> ChoiceFunction v e s -> AcceptFunction e -> EvaluationFunction e s -> S.Set v -> [Chain v e s] -> [Chain v e s]
constructTable' graph choice accept eval reachable' current =
  let
    update = step graph choice accept eval reachable' current
  in
    if length current == length update then update else constructTable' graph choice accept eval reachable' update

-- | Construct a table of stages from vertices that lead from/to a begin/end point
constructTable :: (Graph g e v, Score s) => g -> ChoiceFunction v e s -> AcceptFunction e -> EvaluationFunction e s -> SelectFunction v -> v -> v -> ChainGraph v e s
constructTable graph choice accept eval select begin end =
  let
    origin = [Chain begin begin [] mempty]
    reachable' = reachable graph begin end select
    sg = subgraph graph reachable'
    table = constructTable' sg choice accept eval reachable' origin
    result = fromChains eval table
  in
    -- trace ("Reachable = " ++ (show $ S.map identifier reachable') ++ " subgraph " ++ graphSummary sg reachable') result
    -- trace ("Reachable = " ++ (show $ S.map identifier reachable') ++ "\n table = " ++ (show $ S.fromList $ map (\c -> (identifier $ start c, identifier $ finish c)) table) ++ "\n reached = " ++ (show $ S.map identifier $ M.keysSet $ forwards result)) result
    result

findBreak' :: (Graph g e v) => g -> v -> v -> S.Set v -> S.Set v -> Maybe v
findBreak' chain begin end visited horizon =
  let 
    missing' = S.toList $ S.filter (null . (outgoing chain)) horizon
    visited' = visited `S.union` horizon
    next' = (S.unions $ S.map (targets chain) horizon) `S.difference` visited'
  in
    if S.member end visited' then
      Nothing
    else if not $ null missing' then
      Just (head missing')
    else
      findBreak' chain begin end visited' next'

-- | Find the break in a chain where things can't continue      
findBreak :: (Graph g e v) => g -> v -> v -> Maybe v
findBreak chain begin end = findBreak' chain begin end S.empty (S.singleton begin)


-- | Construct a program consisting of a chain of chains that gives the optimal value for traversing the graph from begin to end
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
    table = constructTable graph chainChoice chainAccept chainEval select begin end
    programs = constructTable table programChoice programAccept programEval select begin end
    route = edge programs begin end
  in
    if isJust route then
      Right (fromJust route)
    else let
        break' = findBreak table begin end
      in
        if isJust break' then
          Left $ failure "No path at " break' table programs
        else let
            break'' = findBreak programs begin end
          in
            if isJust break'' then
              Left $ failure "No chain at " break'' table programs
            else
              Left $ failure "Unable to find break point" Nothing table programs

