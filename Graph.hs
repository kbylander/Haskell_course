{- Functional Programming 1
   A Module for Graphs
   ===================
 -}

-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINE \/\/\/
module Graph(Graph,empty,addVertex,addEdge,vertices,neighbors) where
-- /\/\/\ DO NOT MODIFY THE PRECEDING LINE /\/\/\

{- Graphs model the connections between objects in a network. They have
   applications in computer science and many other areas. For instance, the
   topology of the Internet, the interactions between proteins in molecular
   biology, and the interactions between users on a social networking site can
   all be modeled by graphs.
   Formally, a /graph/ G = (V,E) consists of a set V of /vertices/ and a set
   E ⊆ V × V of /edges/ between these vertices. We will only consider /simple/
   graphs in this assignment. A simple graph is an undirected graph without
   multiple edges or loops. In other words, for all vertices u and v, an edge
   between u and v is the same as an edge between v and u, there is at most one
   edge between u and v, and there is no edge between u and u. See
   [ https://en.wikipedia.org/wiki/Graph_%28discrete_mathematics%29 ] for
   further explanations.
   This file defines a module `Graph' for simple graphs. This module exports a
   polymorphic type `Graph a' of simple graphs with vertices of type `a', and
   the following operations on them:
   1. To obtain an empty graph:
        empty :: Graph a
      The empty graph does not contain any vertices or edges.
   2. To add a vertex to a graph:
        addVertex :: Eq a => Graph a -> a -> Graph a
   3. To add an edge to a graph:
        addEdge :: Eq a => Graph a -> (a,a) -> Graph a
   4. To obtain a list of all vertices in a graph:
        vertices :: Eq a => Graph a -> [a]
   5. To obtain a list of all neighbors of a given vertex:
        neighbors :: Eq a => Graph a -> a -> [a]
      Vertex u is a /neighbor/ of vertex v if there is an edge between u and v.
   Your task is to implement the type `Graph a' and the above operations.
   You will need to decide what the exact preconditions for some of the
   functions should be. The preconditions for `addVertex' and `addEdge' shall
   not disallow adding a vertex (edge, respectively) that is already present in
   the graph.
   The lists returned by `vertices' and `neighbors' shall not contain any
   duplicates. Think about how to achieve this. Remember to state any invariant
   that your implementation assumes on the type `Graph a' as part of the type's
   representation comment -- see the Coding Convention for details!
   THE `Graph' MODULE SHALL NOT EXPORT ANY OTHER OPERATIONS. THE TYPE `Graph a'
   SHALL BE IMPLEMENTED BY A DATA TYPE. THE CONSTRUCTORS(S) OF THIS DATA TYPE
   SHALL NOT BE EXPORTED.
   This means that the implementation of your graph type will not be visible
   outside of the `Graph' module. Your graphs will appear as an abstract data
   type that can ONLY be accessed through the operations provided.
   You may use any suitable data structure that has been presented in class, or
   invent one yourself to implement graphs. Haskell's `Data.Graph' library might
   provide some inspiration.
 -}

{- Graph implementation with polymorphic node types.
Data is represented in empty, i.e. no data is inserted yet, or Graph [a] [(a,a)]
where [a] is a list of vertices in the graph and [(a,a)] is a list of tuples representing edges between two vertices.
  INVARIANT: Graph [a] [(a1,a2)] where a1 /= a2 and a1 & a2 must be a node in [a]
 -}
data Graph a = Graph [a] [(a,a)]
  deriving (Show)  -- do not modify this line

{- empty
Creates a graph without vertices or edges.
RETURNS: An empty graph
EXAMPLES: empty = graph [] []
 -}
empty :: Graph a  -- do not modify this line
empty = Graph [] []

{- addVertex (Graph a) n
Adds a vertex to the graph.
RETURNS: The graph with the added vertex n.
EXAMPLES: addVertex empty 2 = Graph [2] []
          addVertex (Graph [1,3] [(1,3)]) 2 = Graph [2,1,3] [(1,3)]
-}
addVertex :: Eq a => Graph a -> a -> Graph a
addVertex (Graph v e) n  = Graph (n:v) e

{- addEdge (Graph a) (n,m)
Adds an edge in the graph.
RETURNS: The graph with added edge between vertices n and m.
EXAMPLES: addEdge empty (1,2) = Graph [] [(1,2)]
          addEdge (Graph [1,2] []) (1,2) = Graph [1,2] [(1,2)]
 -}
addEdge :: Eq a => Graph a -> (a,a) -> Graph a
addEdge (Graph v e) (n,m)
  | n == m = Graph v e
  | not (elem n v) || not (elem m v) = Graph v e
  | otherwise = Graph v ((n,m):e)

{- vertices (Graph a)
Returns a list of all vertices in the graph without duplicates.
RETURNS: List with all vertices in the graph.
EXAMPLES: vertices empty = []
          vertices (Graph [1,2,1] []) = [1,2]
 -}

 --VARIANTS: Length of the graph's list of vertices.
vertices :: Eq a => Graph a -> [a]
vertices (Graph [] e) = []
vertices (Graph (v:vs) e)
  | elem v vs = vertices (Graph vs e)
  | elem v vs == False = (v:vertices (Graph vs e))

{- neigbors (Graph a) n
Returns a list of all edges in the graph without duplicates from vertex n.
RETURNS: List of all vertices with an edge to n.
EXAMPLES: neighbors empty 1 = []
          neighbors (Graph [1,2,3] [(1,2),(1,3)]) 1 = [2,3]
 -}

--VARIANTS: Length of the graph's list of tuples representing edges.
neighbors :: Eq a => Graph a -> a -> [a]
neighbors (Graph v []) n = []
neighbors (Graph v ((e1,e2):es)) n
  | elem (e1,e2) es || elem (e2,e1) es = neighbors (Graph v es) n
  | n == e1 = (e2:neighbors (Graph v es) n)
  | n == e2 = (e1:neighbors (Graph v es) n)
  | otherwise = neighbors (Graph v es) n
