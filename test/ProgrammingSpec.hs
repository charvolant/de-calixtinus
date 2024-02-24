{-# LANGUAGE MultiParamTypeClasses #-}
module ProgrammingSpec(testProgramming) where

import Test.HUnit(Test(..), assertEqual, assertBool)
import Graph.Graph
import Graph.Programming
import Data.Either
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

instance Score Int where
  invalid = 10000

instance Monoid Int where
  mempty = 0
    
instance Semigroup Int where
  a <> b = min invalid (a + b)
  
data TestVertex = Vertex Int deriving (Eq, Ord, Show)

instance Vertex TestVertex where
  identifier (Vertex v) = show v
  
data TestEdge = Edge Int Int deriving Show

instance Edge TestEdge TestVertex where
  source (Edge a _b) = Vertex a
  target (Edge _a b) = Vertex b
  
testValue (Edge _a b) = b
  
newtype TestGraph = TestGraph [TestEdge]

instance Graph TestGraph TestEdge TestVertex where
  vertex _g a = Vertex (read a)
  incoming (TestGraph edges) a = filter (\e -> a == target e) edges
  outgoing (TestGraph edges) a = filter (\e -> a == source e) edges
  edge (TestGraph edges) a b = L.find (\e -> source e == a && target e == b) edges
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
  
chains1 = [
  Chain (Vertex 1) (Vertex 2) [Edge 1 2] 1,
  Chain (Vertex 2) (Vertex 3) [Edge 2 3] 1,
  Chain (Vertex 1) (Vertex 3) [Edge 1 2, Edge 2 3] 2
  ]

accept1 :: Edge e _v => AcceptFunction e
accept1 aseq = length aseq < 3

accept2 :: Edge e _v => AcceptFunction e
accept2 _seq = True

evaluate1 :: EvaluationFunction TestEdge Int
evaluate1 eseq = sum $ map testValue eseq

pevaluate1 :: EvaluationFunction (Chain TestVertex TestEdge Int) Int
pevaluate1 eseq = sum $ map score eseq

choice1 :: (Edge e v) => ChoiceFunction v e Int
choice1 s1 s2 = if score s1 < score s2 then s1 else s2

select1 _v = True

testProgramming = TestList [
  TestLabel "Extend" testExtend,
  TestLabel "Paths" testPaths,
  TestLabel "Step" testStep,
  TestLabel "ConstructTable" testConstructTable,
  TestLabel "ChainGraph" testChainGraph,
  TestLabel "Program" testProgram
  ]
  
testExtend = TestList [testExtend1, testExtend2]

testExtend1 =
  let
    extended = extend accept1 evaluate1 (chains1 !! 1) (Edge 3 4)
  in
    TestCase (do
      assertBool "Extend 1 1" (isJust extended)
      assertEqual "Extend 1 2" (Vertex 2) (start $ fromJust extended)
      assertEqual "Extend 1 3" (Vertex 4) (finish $ fromJust extended)
      assertEqual "Extend 1 4" 2 (length $ path $ fromJust extended)
      assertEqual "Extend 1 5" 7 (score $ fromJust extended)
    )


testExtend2 =
  let
    extended = extend accept1 evaluate1 (chains1 !! 2) (Edge 3 4)
  in
    TestCase (assertBool "Extend 2 1" (isNothing extended))


testPaths = TestList [testPaths1, testPaths2]

testPaths1 =
  let
    paths' = paths graph1 accept1 evaluate1 chains1 (Vertex 4)
  in
    TestCase (do
      assertEqual "Paths 1 1" 1 (length paths')
      assertEqual "Paths 1 2" (Vertex 2) (start (paths' !! 0))
      assertEqual "Paths 1 3" (Vertex 4) (finish (paths' !! 0))
      assertEqual "Paths 1 4" 2 (length $ path $ paths' !! 0)
      assertEqual "Paths 1 5" 7 (score $ paths' !! 0)
    )


testPaths2 =
  let
    paths' = paths graph1 accept2 evaluate1 chains1 (Vertex 4)
  in
    TestCase (do
       assertEqual "Paths 2 1" 2 (length paths')
       assertEqual "Paths 2 2" (Vertex 1) (start (paths' !! 1))
       assertEqual "Paths 2 3" (Vertex 4) (finish (paths' !! 1))
       assertEqual "Paths 2 4" 3 (length $ path $ paths' !! 1)
       assertEqual "Paths 2 5" 9 (score $ paths' !! 1)
   )

testStep = TestList [testStep1, testStep2, testStep3, testStep4, testStep5]

testStep1 = 
  let
    reach = successors graph1 (Vertex 1)
    current = [
        (Chain (Vertex 1) (Vertex 1) [] 0)
      ]
    chains = step graph1 choice1 accept1 evaluate1 reach current
  in
    TestCase (do
      assertEqual "Step 1 1" 3 (length chains)
      assertEqual "Step 1 2" (Vertex 2) (start (chains !! 1))
      assertEqual "Step 1 3" (Vertex 2) (finish (chains !! 1))
      assertEqual "Step 1 4" 0 (length $ path $ chains !! 1)
      assertEqual "Step 1 5" 0 (score $ chains !! 1)
      assertEqual "Step 1 6" (Vertex 1) (start (chains !! 2))
      assertEqual "Step 1 7" (Vertex 2) (finish (chains !! 2))
      assertEqual "Step 1 8" 1 (length $ path $ chains !! 2)
      assertEqual "Step 1 9" 2 (score $ chains !! 2)
   )

testStep2 =
  let
    vertex1 = Vertex 1
    vertex2 = Vertex 2
    reach = successors graph1 vertex1
    current = [
        (Chain vertex1 vertex1 [] 0),
        (Chain vertex2 vertex2 [] 0),
        (Chain vertex1 vertex2 [Edge 1 2] 0)
      ]
    chains = step graph1 choice1 accept1 evaluate1 reach current
  in
    TestCase (do
      assertEqual "Step 2 1" 6 (length chains)
      assertEqual "Step 2 2" vertex1 (start (chains !! 4))
      assertEqual "Step 2 3" (Vertex 3) (finish (chains !! 4))
      assertEqual "Step 2 4" 2 (length $ path $ chains !! 4)
      assertEqual "Step 2 5" 5 (score $ chains !! 4)
   )

testStep3 =
  let
    vertex1 = Vertex 1
    vertex2 = Vertex 2
    vertex3 = Vertex 3
    vertex5 = Vertex 5
    reach = successors graph1 vertex1
    current = [
        (Chain vertex1 vertex1 [] 0),
        (Chain vertex2 vertex2 [] 0),
        (Chain vertex3 vertex3 [] 0),
        (Chain vertex1 vertex2 [Edge 1 2] 2),
        (Chain vertex1 vertex3 [Edge 1 2, Edge 2 3] 5),
        (Chain vertex2 vertex3 [Edge 2 3] 3)
      ]
    chains = step graph1 choice1 accept1 evaluate1 reach current
  in
    TestCase (do
      assertEqual "Step 3 1" 12 (length chains)
      assertEqual "Step 3 2" vertex2 (start (chains !! 9))
      assertEqual "Step 3 3" vertex5 (finish (chains !! 9))
      assertEqual "Step 3 4" 2 (length $ path $ chains !! 9)
      assertEqual "Step 3 5" 8 (score $ chains !! 9)
      assertEqual "Step 3 6" vertex3 (start (chains !! 11))
      assertEqual "Step 3 7" vertex5 (finish (chains !! 11))
      assertEqual "Step 3 8" 1 (length $ path $ chains !! 11)
      assertEqual "Step 3 9" 5 (score $ chains !! 11)
    )

testStep4 =
  let
    vertex1 = Vertex 1
    vertex2 = Vertex 2
    vertex3 = Vertex 3
    vertex4 = Vertex 4
    vertex5 = Vertex 5
    reach = successors graph1 vertex1
    current = [
        (Chain vertex1 vertex1 [] 0),
        (Chain vertex2 vertex2 [] 0),
        (Chain vertex3 vertex3 [] 0),
        (Chain vertex4 vertex4 [] 0),
        (Chain vertex1 vertex2 [Edge 1 2] 2),
        (Chain vertex1 vertex3 [Edge 1 2, Edge 2 3] 5),
        (Chain vertex2 vertex3 [Edge 2 3] 3),
        (Chain vertex2 vertex4 [Edge 2 3, Edge 3 4] 7)
      ]
    chains = step graph1 choice1 accept1 evaluate1 reach current
  in
    TestCase (do
      assertEqual "Step 4 1" 11 (length chains)
      assertEqual "Step 4 2" vertex3 (start (chains !! 10))
      assertEqual "Step 4 3" vertex5 (finish (chains !! 10))
    )

testStep5 =
  let
    vertex1 = Vertex 1
    vertex2 = Vertex 2
    vertex3 = Vertex 3
    vertex4 = Vertex 4
    vertex5 = Vertex 5
    vertex6 = Vertex 6
    reach = successors graph1 vertex1
    current = [
        (Chain vertex1 vertex1 [] 0),
        (Chain vertex1 vertex1 [] 0),
        (Chain vertex2 vertex2 [] 0),
        (Chain vertex3 vertex3 [] 0),
        (Chain vertex4 vertex4 [] 0),
        (Chain vertex5 vertex5 [] 0),
        (Chain vertex1 vertex2 [Edge 1 2] 2),
        (Chain vertex1 vertex3 [Edge 1 2, Edge 2 3] 5),
        (Chain vertex2 vertex3 [Edge 2 3] 3),
        (Chain vertex2 vertex4 [Edge 3 4] 4),
        (Chain vertex2 vertex5 [Edge 3 5] 5),
        (Chain vertex1 vertex4 [Edge 2 3, Edge 3 4] 7),
        (Chain vertex1 vertex5 [Edge 2 3, Edge 3 5] 8)
      ]
    chains = step graph1 choice1 accept1 evaluate1 reach current
  in
    TestCase (do
      assertEqual "Step 5 1" 17 (length chains)
      assertEqual "Step 5 2" vertex5 (start (chains !! 16))
      assertEqual "Step 5 3" vertex6 (finish (chains !! 16))
      assertEqual "Step 5 4" 1 (length $ path $ chains !! 16)
      assertEqual "Step 5 5" 6 (score $ chains !! 16)
    )

chainGraphSize :: Edge e v => ChainGraph v e s -> Int
chainGraphSize chains = M.foldl (\s -> \v -> s + length v) 0 (forwards chains)

testConstructTable = TestList [testConstructTable1, testConstructTable2, testConstructTable3]

testConstructTable1 =
  let
    chains = constructTable graph1 choice1 accept1 evaluate1 select1 (Vertex 1) (Vertex 6)
  in
    TestCase (do
      assertEqual "ConstructTable 1 1" 10 (chainGraphSize chains)
      let chain1 = fromJust $ edge chains (Vertex 1) (Vertex 3)
      assertEqual "ConstructTable 1 2" (Vertex 1) (start chain1)
      assertEqual "ConstructTable 1 3" (Vertex 3) (finish chain1)
      assertEqual "ConstructTable 1 4" 2 (length $ path chain1)
      assertEqual "ConstructTable 1 5" 5 (score chain1)
      let chain2 = fromJust $ edge chains (Vertex 3) (Vertex 6)
      assertEqual "ConstructTable 1 6" (Vertex 3) (start chain2)
      assertEqual "ConstructTable 1 7" (Vertex 6) (finish chain2)
      assertEqual "ConstructTable 1 8" 2 (length $ path chain2)
      assertEqual "ConstructTable 1 9" 10 (score chain2)
      assertBool "ConstructTable 1 10" (isNothing $ edge chains (Vertex 1) (Vertex 6))
   )

testConstructTable2 =
  let
    chains = constructTable graph1 choice1 accept2 evaluate1 select1 (Vertex 1) (Vertex 6)
  in
    TestCase (do
      assertEqual "ConstructTable 2 1" 14 (chainGraphSize chains)
      let chain1 = fromJust $ edge chains (Vertex 1) (Vertex 3)
      assertEqual "ConstructTable 2 2" (Vertex 1) (start chain1)
      assertEqual "ConstructTable 2 3" (Vertex 3) (finish chain1)
      assertEqual "ConstructTable 2 4" 2 (length $ path chain1)
      assertEqual "ConstructTable 2 5" 5 (score chain1)
      let chain2 = fromJust $ edge chains (Vertex 3) (Vertex 6)
      assertEqual "ConstructTable 2 6" (Vertex 3) (start chain2)
      assertEqual "ConstructTable 2 7" (Vertex 6) (finish chain2)
      assertEqual "ConstructTable 2 8" 2 (length $ path chain2)
      assertEqual "ConstructTable 2 9" 10 (score chain2)
      let chain3 = fromJust $ edge chains (Vertex 1) (Vertex 6)
      assertEqual "ConstructTable 1 10" (Vertex 1) (start chain3)
      assertEqual "ConstructTable 1 11" (Vertex 6) (finish chain3)
      assertEqual "ConstructTable 1 12" 4 (length $ path chain3)
      assertEqual "ConstructTable 1 13" 15 (score chain3)
   )


testConstructTable3 =
  let
    chains = constructTable graph2 choice1 accept2 evaluate1 select1 (Vertex 1) (Vertex 4)
  in
    TestCase (do
      assertEqual "ConstructTable 2 1" 3 (chainGraphSize chains)
      let chain1 = fromJust $ edge chains (Vertex 1) (Vertex 4)
      assertEqual "ConstructTable 2 2" (Vertex 1) (start chain1)
      assertEqual "ConstructTable 2 3" (Vertex 4) (finish chain1)
      assertEqual "ConstructTable 2 4" 2 (length $ path chain1)
      assertEqual "ConstructTable 2 5" 6 (score chain1)
   )

testChainGraph = TestList [testChainGraph1, testChainGraph2, testChainGraph3, testChainGraph4]

testChainGraph1 =
  let
    chains = constructTable graph1 choice1 accept2 evaluate1 select1 (Vertex 1) (Vertex 6)
  in
    TestCase (do
      assertEqual "ChainGraph 1 1" (Vertex 5) (vertex chains "5")
      assertEqual "ChainGraph 1 1" (Vertex 1) (vertex chains "1")
    )

testChainGraph2 =
  let
    chains = constructTable graph1 choice1 accept1 evaluate1 select1 (Vertex 1) (Vertex 6)
  in
    TestCase (do
      let incoming1 = incoming chains (Vertex 4)
      assertEqual "ChainGraph 2 1" 2 (length incoming1)
      assertEqual "ChainGraph 2 2" (Vertex 2) (start $ incoming1 !! 0)
      assertEqual "ChainGraph 2 3" (Vertex 4) (finish $ incoming1 !! 0)
      assertEqual "ChainGraph 2 4" 2 (length $ path $ incoming1 !! 0)
      assertEqual "ChainGraph 2 5" 7 (score $ incoming1 !! 0)
    )


testChainGraph3 =
  let
    chains = constructTable graph1 choice1 accept1 evaluate1 select1 (Vertex 1) (Vertex 6)
  in
    TestCase (do
      let outgoing1 = outgoing chains (Vertex 1)
      assertEqual "ChainGraph 3 1" 2 (length outgoing1)
      assertEqual "ChainGraph 3 2" (Vertex 1) (start $ outgoing1 !! 1)
      assertEqual "ChainGraph 3 3" (Vertex 3) (finish $ outgoing1 !! 1)
      assertEqual "ChainGraph 3 4" 2 (length $ path $ outgoing1 !! 1)
      assertEqual "ChainGraph 3 5" 5 (score $ outgoing1 !! 1)
    )

testChainGraph4 =
  let
    chains = constructTable graph1 choice1 accept1 evaluate1 select1 (Vertex 1) (Vertex 6)
  in
    TestCase (do
      let sources1 = sources chains (Vertex 4)
      assertEqual "ChainGraph 4 1" (S.fromList [Vertex 2, Vertex 3]) sources1
    )


testProgram = TestList [testProgram1, testProgram2, testProgram3]

testProgram1 =
  let
    optimal = program graph2 choice1 accept2 pevaluate1 choice1 accept1 evaluate1 select1 (Vertex 1) (Vertex 3)
  in
    TestCase (do
      assertBool "Program 1 1" (isRight optimal)
      let accepted = fromRight (error "Bad route") optimal
      assertEqual "Program 1 2" (Vertex 1) (start accepted)
      assertEqual "Program 1 3" (Vertex 3) (finish accepted)
      assertEqual "Program 1 4" 1 (length $ path accepted)
      assertEqual "Program 1 5" 5 (score accepted)
   )
   
testProgram2 =
  let
    optimal = program graph2 choice1 accept2 pevaluate1 choice1 accept1 evaluate1 select1 (Vertex 1) (Vertex 4)
  in
    TestCase (do
      assertBool "Program 2 1" (isRight optimal)
      let accepted = fromRight (error "Bad route") optimal
      assertEqual "Program 2 2" (Vertex 1) (start accepted)
      assertEqual "Program 2 3" (Vertex 4) (finish accepted)
      assertEqual "Program 2 4" 1 (length $ path accepted)
      assertEqual "Program 2 5" 6 (score accepted)
   )

testProgram3 =
  let
    optimal = program graph2 choice1 accept2 pevaluate1 choice1 accept1 evaluate1 select1 (Vertex 1) (Vertex 6)
  in
    TestCase (do
      assertBool "Program 3 1" (isRight optimal)
      let accepted = fromRight (error "Bad route") optimal
      assertEqual "Program 3 2" (Vertex 1) (start accepted)
      assertEqual "Program 3 3" (Vertex 6) (finish accepted)
      assertEqual "Program 3 4" 2 (length $ path accepted)
      assertEqual "Program 3 5" 12 (score accepted)
   )

