{-# LANGUAGE MultiParamTypeClasses #-}
module GraphSpec(testGraph) where

import Test.HUnit
import Graph.Graph
import qualified Data.Set as S
import Data.List (find)

newtype TestVertex = Vertex Int deriving (Eq, Ord, Show)

instance Vertex TestVertex where
  identifier (Vertex v) = show v

data TestEdge = Edge Int Int deriving (Show)

instance Edge TestEdge TestVertex where
  source (Edge a _b) = Vertex a
  target (Edge _a b) = Vertex b
  
newtype TestGraph = TestGraph [TestEdge]

instance Graph TestGraph TestEdge TestVertex where
  vertex _g a = Vertex (read a)
  incoming (TestGraph edges) a = filter (\e -> a == target e) edges
  outgoing (TestGraph edges) a = filter (\e -> a == source e) edges
  edge (TestGraph edges) a b = find (\e -> source e == a && target e == b) edges
  subgraph (TestGraph edges) vs = TestGraph (filter (\e -> S.member (source e) vs && S.member (target e) vs) edges)
  
graph1 = TestGraph [
  Edge 1 2,
  Edge 2 3,
  Edge 3 4,
  Edge 3 5,
  Edge 4 6,
  Edge 5 6,
  Edge 6 1
  ]

graph2 = TestGraph [
  Edge 1 2,
  Edge 2 3,
  Edge 2 4,
  Edge 3 5,
  Edge 4 6,
  Edge 5 6
  ]

testGraph = TestList [
  TestLabel "Basics" testBasics,
  TestLabel "Sucessors" testSuccessors,
  TestLabel "Predecessors" testPredecessors,
  TestLabel "Available" testAvailable
  ] :: Test
  
testBasics = TestList [testBasics1, testBasics2, testBasics3, testBasics4]

testBasics1 = TestCase (assertEqual "Basics 1" [Vertex 6] (S.toList $ sources graph1 (Vertex 1)))

testBasics2 = TestCase (assertEqual "Basics 2" [Vertex 4, Vertex 5] (S.toList $ sources graph1 (Vertex 6)))

testBasics3 = TestCase (assertEqual "Basics 3" [Vertex 2] (S.toList $ targets graph1 (Vertex 1)))

testBasics4 = TestCase (assertEqual "Basics 4" [Vertex 4, Vertex 5] (S.toList $ targets graph1 (Vertex 3)))

testSuccessors = TestList [testSuccessors1, testSuccessors2, testSuccessors3, testSuccessors4, testSuccessors5]

testSuccessors1 = TestCase (assertEqual "Successors 1" [Vertex 6] (S.toList $ successors graph2 (Vertex 5)))

testSuccessors2 = TestCase (assertEqual "Successors 2" [Vertex 5, Vertex 6] (S.toList $ successors graph2 (Vertex 3)))

testSuccessors3 = TestCase (assertEqual "Successors 3" [Vertex 3, Vertex 4, Vertex 5, Vertex 6] (S.toList $ successors graph2 (Vertex 2)))

testSuccessors4 = TestCase (assertEqual "Successors 4" [Vertex 2, Vertex 3, Vertex 4, Vertex 5, Vertex 6] (S.toList $ successors graph2 (Vertex 1)))

testSuccessors5 = TestCase (assertEqual "Successors 5" [Vertex 1, Vertex 2, Vertex 3, Vertex 4, Vertex 5, Vertex 6] (S.toList $ successors graph1 (Vertex 4)))

testPredecessors = TestList [testPredecessors1, testPredecessors2, testPredecessors3, testPredecessors4, testPredecessors5, testPredecessors6]

testPredecessors1 = TestCase (assertEqual "Predecessors 1" [Vertex 1] (S.toList $ predecessors graph2 (Vertex 2)))

testPredecessors2 = TestCase (assertEqual "Predecessors 2" [Vertex 1, Vertex 2] (S.toList $ predecessors graph2 (Vertex 3)))

testPredecessors3 = TestCase (assertEqual "Predecessors 3" [Vertex 1, Vertex 2] (S.toList $ predecessors graph2 (Vertex 4)))

testPredecessors4 = TestCase (assertEqual "Predecessors 4" [Vertex 1, Vertex 2, Vertex 3] (S.toList $ predecessors graph2 (Vertex 5)))

testPredecessors5 = TestCase (assertEqual "Predecessors 5" [Vertex 1, Vertex 2, Vertex 3, Vertex 4, Vertex 5] (S.toList $ predecessors graph2 (Vertex 6)))

testPredecessors6 = TestCase (assertEqual "Predecessors 5" [Vertex 1, Vertex 2, Vertex 3, Vertex 4, Vertex 5, Vertex 6] (S.toList $ predecessors graph1 (Vertex 2)))

testAvailable = TestList [testAvailable1, testAvailable2, testAvailable3, testAvailable4, testAvailable5]

testAvailable1 = TestCase (assertEqual "Available 1" [Vertex 2] (S.toList $ available graph1 (successors graph1 (Vertex 1)) (S.fromList [Vertex 1])))

testAvailable2 = TestCase (assertEqual "Available 2" [Vertex 4, Vertex 5] (S.toList $ available graph1 (successors graph1 (Vertex 1)) (S.fromList [Vertex 2, Vertex 3])))

testAvailable3 = TestCase (assertEqual "Available 3" [] (S.toList $ available graph1 (S.fromList [Vertex 2, Vertex 3, Vertex 4, Vertex 5]) (S.fromList [Vertex 2, Vertex 3, Vertex 4, Vertex 5])))

testAvailable4 = TestCase (assertEqual "Available 4" [Vertex 6] (S.toList $ available graph1 (S.fromList [Vertex 2, Vertex 3, Vertex 4, Vertex 5, Vertex 6]) (S.fromList [Vertex 2, Vertex 3, Vertex 4, Vertex 5])))

testAvailable5 = TestCase (assertEqual "Available 5" [Vertex 6] (S.toList $ available graph1 (S.fromList [Vertex 2, Vertex 3, Vertex 5, Vertex 6]) (S.fromList [Vertex 2, Vertex 3, Vertex 5])))
