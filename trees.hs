{- Functional Programming 1
   Assignment 3
 -}
-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Assignment3(Fruit(..),
            sumPrice,
            BSTree(Void,BSNode),
            subTree,
            Tree(..),
            count,labels,height,
            append,elem,last,reverse,filter) where

import Prelude hiding (elem,last,reverse,filter)
-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- Exercise 2: Binary Search Trees
   ===============================
   The nodes of a binary search tree are ordered from left to right according to
   their key value. Consider the following datatype declaration of binary search
   trees:
 -}

{- Binary search trees with Integer labels
   Void represents an empty tree. BSNode l x r represents a tree with
   left subtree l, root label x, and right subtree r.
   INVARIANT: in every tree of the form BSNode l x r, all labels in l
     are < x, and all labels in r are > x.
 -}
data BSTree = Void | BSNode BSTree Integer BSTree  -- do not modify this line
  deriving (Eq,Show)  -- do not modify this line

{- Define a function
     subTree :: Integer -> Integer -> BSTree -> BSTree
   such that `subTree a b t' yields a binary search tree containing the keys in
   `t' that are greater than or equal to `a' but smaller than `b'.
   Example: Assume that `t' is defined as follows:
     t = BSNode (BSNode (BSNode Void 0 (BSNode Void 2 Void))
                        3
                        (BSNode Void 5 Void))
                6
                (BSNode Void
                        7
                        (BSNode Void 8 (BSNode Void 9 Void)))
   We now have
     subTree 5 8 t == BSNode (BSNode Void 5 Void)
                             6
                             (BSNode Void 7 Void)
   (The tree produced by your solution might be shaped differently, but it
   should also contain the keys 5, 6 and 7, and no other keys.) We also have
     subTree 10 20 t == Void
   This question can (and should) be solved without introducing intermediate
   data structures (such as lists).
 -}

{- subTree a b t
Generates a subtree of t between given interval.
PRE: a <= b, t is a binary tree.
RETURNS: Binary subtree of nodes x in t between interval a <= x < b.
EXAMPLE: subTree 5 8 BSNode (BSNode (BSNode Void 0 (BSNode Void 2 Void)) 3 (BSNode Void 5 Void)) 6 (BSNode Void 7 (BSNode Void 8 (BSNode Void 9 Void)))
        = (BSNode Void 5 Void) 6 (BSNode Void 7 Void)
 -}
subTree :: Integer -> Integer -> BSTree -> BSTree

-- VARIANT: size of t
subTree a b Void = Void
subTree a b (BSNode l node r)
  | b <= a = Void
  | node >= a && node < b = (BSNode (subTree a b l) node (subTree a b r))
  | node < a = subTree a b r
  | node >= b = subTree a b l

{- Exercise 3: Trees
   =================
   1. Define a datatype `Tree a' of (finitely branching) labeled trees. Each
      node (`Node') carries a label (of polymorphic type `a') and may have an
      arbitrary (non-negative) number of children. Different nodes may have
      different numbers of children. Each tree has at least one node (so there
      should be no `Void' case in your datatype definition).
      See [ https://en.wikipedia.org/wiki/Tree_%28data_structure%29 ] for
      further definitions and some hints.
 -}

{- Rose tree / Multiway tree with polymorphic labels
   Node a [] represents a tree with root value a. Values inside the list, [], represents the children of Node a.
   INVARIANT: Nodes in the form of Node a [children], each tree has at least one node.
 -}
data Tree a = Node a [Tree a]
  deriving (Eq,Show)  -- do not modify this line

{- 2. Define functions to
      (a) compute the number of nodes in such a tree:
            count :: Tree a -> Integer
      (b) compute the list of all node labels in such a tree:
            labels :: Tree a -> [a]
      (c) compute the height of such a tree:
            height :: Tree a -> Integer
          The height of a leaf (that is, a node with no children) should be 0.
      Hint: a possible solution is to use helper functions and mutual recursion.
      Another solution uses higher-order functions such as `map'.
 -}

{- count Tree a
Count the number of nodes in a tree.
RETURNS: Sum of number of nodes in the tree as an Integer
EXAMPLES: count Node 'a' [Node 'w' [Node 'u' [Node 'g' []]], Node 'c' []]
              = 5
 -}
count :: Tree a -> Integer

-- VARIANT: Size of tree
count (Node a [] ) = 1
count (Node a subtree) = 1 + sum(map count subtree)

{- labels Tree a
Takes a tree and collects all elements in a lists
RETURNS: A list of all node values in the input tree
EXAMPLES: labels Node 'a' [Node 'w' [Node 'u' [Node 'g' []]], Node 'c' []] = "awugc"
 -}
labels :: Tree a -> [a]

-- VARIANT: Size of tree
labels (Node a []) = [a]
labels (Node a subtree) = a:(foldr (++) [] ( map (labels) subtree))

{- height Tree a
Computes the height of a multiway tree
RETURNS: The height of the input Tree a as an integer
EXAMPLES: height Node 'a' [Node 'w' [Node 'u' [Node 'g' []]], Node 'c' []] = 3
 -}
height :: Tree a -> Integer

-- VARIANT: Size of tree
height (Node a []) = 0
height (Node a subtree) = 1 + maximum(map height subtree)


{- Exercise 4: Higher-Order Functions
   ==================================
   You have already seen recursive definitions for the functions `(++)', `elem',
   `last', `reverse' and `filter' in class. Now your task is to give
   non-recursive definitions for these functions, using the higher-order
   functions `foldl' or `foldr'. Define
   1. append :: [a] -> [a] -> [a]  (apart from its name, this should behave like (++))
   2. elem :: Eq a => a -> [a] -> Bool
   3. last :: [a] -> a
   4. reverse :: [a] -> [a]
   5. filter :: (a -> Bool) -> [a] -> [a]
   In all cases, the definition using foldl or foldr should be quite simple
   (i.e., a single line of code).
 -}

{- append a b
Concatenates two lists with a before b
PRE: Both input lists has to contain elements of same datatype
RETURNS: A concatenated list [a,b]
EXAMPLE: append [1,2,3] [4,5,6] = [1,2,3,4,5,6]
 -}
append :: [a] -> [a] -> [a]
append a b = foldr (:) b a

{- elem key l
Checks if the list l includes the key
RETURNS: True if key is in list l, otherwise False
EXAMPLES: elem 1 [1,2,3,4] = True
          elem 0 [1,2,3,4] = False
 -}
elem :: Eq a => a -> [a] -> Bool
elem key l = foldl (\ls element -> if element==key then True else ls) False l

{- last a
Takes a list and outputs the last element in the input list
RETURNS: The last element in list a
EXAMPLES: last [1,2,3] = 3
 -}
last :: [a] -> a
last xs = foldl (\_ x -> x) undefined xs

{- reverse l
Reverses a list
RETURNS: The reversed list l
EXAMPLES: reverse [1,2,3] = [3,2,1]
 -}
reverse :: [a] -> [a]
reverse l = foldl (\xs x -> x:xs) [] l

{- filter cond l
Filters a list based on an condition
PRE: Condition, cond, has to return a bool
RETURNS: A list with elements from input list, l, that satisfies the condition, cond.
EXAMPLES: filter (<3) [1,2,3,4] = [4]
          filter even [1,2,3,4] = [2,4]
 -}
filter :: (a -> Bool) -> [a] -> [a]
filter cond l = foldr (\element l -> if (cond element) then (element : l) else l) [] l
