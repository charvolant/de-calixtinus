{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_HADDOCK prune #-}
{-|
Module      : Graph
Description : Abstract graph model
Copyright   : (c) Doug Palmer, 2023
License     : MIT
Maintainer  : doug@charvolant.org
Stability   : experimental
Portability : POSIX

An abstract graph model that can be fed into other algorithms
-}

module Graph.Graph(
  -- * Graph components
    Vertex(..)
  , Edge(..)
  -- * A Complete Graph
  , Graph(..)
  , available
  , graphSummary
  , predecessors
  , reachable
  , successors
) where

import qualified Data.List as L
import qualified Data.Set as S

-- | Something that can act as a vertex node on a graph
--
--   Vertices can have additional attributes.
--   The vertex class simply identifies something as suitable to be a vertex
class (Eq v, Ord v, Show v) => Vertex v where
  -- | Get the vertex identifier
  identifier :: v -> String

-- | An edge between two vertices
--
--   Edges can have additional attributes.
--   The edge class simply identifies something as suitable to be an edge
class (Vertex v, Show e) => Edge e v | e -> v where
  -- | Get the edge source (from)
  source :: e -> v
  -- | Get the edge target (to)
  target :: e -> v

-- | A graph implementation
--
--   Graphs allow something to identify the edges running between vertices
class Edge e v => Graph g e v | g -> e, g -> v where
  -- | Find a vertex based on identifier
  vertex :: g -- ^ The graph
    -> String -- ^ The vertex identifier
    -> v -- ^ The corresponding vertex

  -- | Find the edges that lead to a particular vertex
  incoming :: g  -- ^ The graph
    -> v -- ^ The desination vertex
    -> [e] -- ^ The incoming edges

  -- | Find the edges that leave from a particular vertex
  outgoing :: g  -- ^ The graph
    -> v -- ^ The source vertex
    -> [e] -- ^ The outgoing edges

  -- | Get the single edge, if it exists, between two vertices
  --   If multiple edges exist, a consistent single edge is returned
  edge :: g  -- ^ The graph
    -> v -- ^ The source vertex
    -> v -- ^ The target vertex
    -> Maybe e -- ^ An edge running between the two vertices, if one exists

  -- | Construct a subgraph containing only selected vertices and edges
  subgraph :: g  -- ^ The original graph
    -> (v -> Bool) -- ^ A vertex selection fuction. Only vertices that return true will be in the subgraph
    -> (e -> Bool) -- ^ An edge selection function. Only edges that go between selected vertices and which are themselves selected will be in the subgraph
    -> g -- ^ The resulting subgraph

  -- | Mirror (reverse) the graph so that all edges are reversed
  --
  --   Edges and vertices may need to have attributes changed to reflect the reversal
  mirror :: g -> g

  -- | Get the immediately preceding vertices in a graph
  sources :: g -- ^ The graph
    -> v -- ^ The vertex that is the target
    -> S.Set v -- ^ All vertices that have an edge leading to the target
  sources g v = S.fromList $ map source (incoming g v)

  -- | Get the imemdiately following vertices in a graph
  targets :: g -- ^ The graph
    -> v -- ^ The vertex that is the source
    -> S.Set v -- ^ All vertices that have an edge leading from the source
  targets g v = S.fromList $ map target (outgoing g v)

-- | Transitive closure of either `sources` or `targets` for a graph
transitive :: Graph g e v => g -> (g -> v -> S.Set v) -> S.Set v -> S.Set v -> S.Set v
transitive graph direction new seen
  | S.null new = seen
  | otherwise = let 
    steps = map (direction graph) (S.toList new)
    next = foldl S.union S.empty steps 
    new' = next `S.difference` seen
  in
    transitive graph direction new' (S.union seen new')

-- | Compute all successor vertices reachable from a start vertex    
successors :: Graph g _e v => g -- ^ The graph to traverse
  -> v -- ^ The start vertex
  -> S.Set v -- ^ All vertices reachable from the start vertex, only including the start vertex if it is self-reachable
successors graph start = transitive graph targets (S.singleton start) S.empty

-- | Compute all predecessor vertices that eventually come to a terminal vertex
predecessors :: Graph g _e v => g -- ^ The graph to traverse
  -> v -- ^ The final vertex
  -> S.Set v -- ^ All vertices that reach the final vertex, only including the start vertex if it is self-reachable
predecessors graph final = transitive graph sources (S.singleton final) S.empty

-- | Compute all the vertices that have become available
--
--   Available vertices are those which have not yet been visited and which have all the source edges that
--   are reachable in the visited set.
--   This function can be used for a breadth-first traversal of a graph, ensuring that information about preceeding
--   vertices are available before computing something about anotgher vertex.
available :: (Graph g _e v) => g -- ^ The graph to traverse
  -> S.Set v -- ^ The reachable vertices
  -> S.Set v -- ^ The visited vertices
  -> S.Set v -- ^ The vertices that can now be computed
available graph reach visited =
  let
    active = foldl (\a -> \v -> a `S.union` (targets graph v)) S.empty visited
    next = active `S.difference` visited
    trigger v = ((sources graph v) `S.intersection` reach) `S.isSubsetOf` visited
    triggered = (S.filter trigger next) `S.intersection` reach
  in
    -- trace ("Available from " ++ (show $ S.map identifier visited) ++ " active=" ++ (show $ S.map identifier active) ++ " triggered=" ++ (show $ S.map identifier triggered)) triggered
    triggered

-- | Generate a summary of the graph as a string
graphSummary :: (Graph g e v) => g -- ^ The graph to summarise
  -> S.Set v -- ^ The set of vertices to summarise
  -> String -- ^ The graph summary
graphSummary graph vertices = 
  "[" 
  ++ (L.intercalate ", " 
    (map (\v -> 
      (identifier v) 
      ++ "<-" 
      ++ (L.intercalate "," (map (identifier . source) (incoming graph v)))
      ++ "->" 
      ++ (L.intercalate "," (map (identifier . target) (outgoing graph v)))
    ) (S.toList vertices)
    )
  )
  ++ "]"
  
-- | Find the vertices that are reachable between two vertices, given a select function
reachable :: (Graph g e v) => g -- ^ The graph
  -> v -- ^ The beginning vertex on the graph
  -> v -- ^ The ending vertex on the graph
  -> (v -> Bool) -- ^ The selection function that determines whether to include a vertex
  -> S.Set v -- ^ The reachable vertices
reachable graph begin end select = let
    succs = (successors graph begin) `S.union` (S.singleton begin)
    preds = (predecessors graph end) `S.union` (S.singleton end)
  in
    S.filter select (succs `S.intersection` preds)
    