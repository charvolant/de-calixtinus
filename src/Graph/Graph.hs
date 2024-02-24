{-# LANGUAGE FunctionalDependencies #-}
module Graph.Graph(
  Vertex(..),
  Edge(..),
  Graph(..),
  
  successors,
  predecessors,
  available,
  graphSummary
) where

import qualified Data.List as L
import qualified Data.Set as S
-- import Debug.Trace

-- | Something that can act as a vertex node on a graph
class (Eq v, Ord v, Show v) => Vertex v where
  -- | Get the vertex identifier
  identifier :: v -> String

-- | An edge between two vertices
class (Vertex v, Show e) => Edge e v | e -> v where
  -- | Get the edge source (from)
  source :: e -> v
  -- | Get the edge target (to)
  target :: e -> v

-- | A graph implementation
class Edge e v => Graph g e v | g -> e, g -> v where
  -- | Find a vertex based on identifier
  vertex :: g -> String -> v
   -- | Find the edges that lead to a particular vertex
  incoming :: g -> v -> [e]
  -- | Find the edges that leave from a particular vertex
  outgoing :: g -> v -> [e]
  -- | Get the single edge, if it exists, between two vertices
  --   If multiple edges exist, a consistent single edge is returned
  edge :: g -> v -> v -> Maybe e
  -- | Construct a subgraph containing only the given vertices
  subgraph :: g -> S.Set v -> g
  -- | Get the preceding vertices in a graph
  sources :: g -> v -> S.Set v
  sources g v = S.fromList $ map source (incoming g v)
  -- | Get the following vertices in a graph
  targets :: g -> v -> S.Set v
  targets g v = S.fromList $ map target (outgoing g v)

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
--   Available vertices are those which have not yet been visited and which have all the source edges that
--   are reachable in the visited set
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
     -- trace ("Available from " ++ (show $ S.map identifier visited) ++ " triggered=" ++ (show $ S.map identifier triggered)) triggered
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