{- My first slightly complex data type - a Tree.
-}

data Tree a = NIL | Tree a (Tree a) (Tree a) deriving (Show)


tree1 = Tree 1 NIL NIL
tree2 = Tree 1 (Tree 2 NIL NIL) NIL
tree3 = Tree 1 (Tree 2 NIL NIL) (Tree 3 NIL NIL)
tree4 = Tree 4 tree2 tree3

-- One thing that becomes apparent from the above is that haskell is not good at encapsulating types
-- Ultimately I want this Tree type to have a data invariant rule that all nodes to the left are less than and all to the right are greater than.
-- This type of data invariant is difficult to enforce in haskell. You are relying on people using the data type sticking to the official API functions
-- to create and access it. See http://stackoverflow.com/questions/4810538/specifying-invariants-on-value-constructors

-- So Let's create the API functions that we need for this tree type
-- First the easy function - an ability to create a new Tree with a single element in it
singleton:: a->Tree a
singleton x = Tree x NIL NIL 

tree5 = singleton 5
tree6 = singleton "Hello"

-- The interesting thing about this Tree data type is that the classic imperative method for creating it which is ingrained in my mind is to create
-- it as a mutable data structure. Only being able to create immutable datastructures makes things far more interesting
-- See following for an immutable version in python:
-- https://github.com/mkersh/Python/blob/master/experiment/tree.py
-- This python test also has an immutable version similar to the one below (but not as neat and concise)

insert::(Ord a)=>a->Tree a->Tree a
insert x NIL = singleton x
insert x (Tree y leftTree rightTree)
    | x <= y = Tree y (insert x leftTree) rightTree
    | otherwise = Tree y leftTree (insert x rightTree)

-- It was not that difficult afterall and IMO looks nicer than mutable versions
-- Both immutable implementations like this one and mutable ones (like my python one) have same asymptotic performance characteristics
-- For insertion these are:
-- Worst Case: O(n)
-- Best Case: O(log n)
--
-- The immutable version needs to create more new nodes though and hence will be slower in reality.

tree7 = insert 10 NIL
tree8 = insert 5 tree7
tree9 = insert 20 tree8

-- Playing with foldr (remember the accumulator pattern) to create a relatively large tree
-- This tree is essentially just a Sorted List though because it is 100 nodes deep
-- It ends up 100 nodes deep because we are inserting items that are already sorted and this results in the Worst Case
-- performance from this simplistic Tree DS.
--
-- A more sophisticated version of this Tree would be a balanced version of it. This would ensure that the nodes in the tree are relatively balanced.
-- By doing this we can ensure that insertion (and search)  are O(log n) operations in terms of asymptotic performance. 
tree10 = foldr (\x tr->insert x tr) (singleton 0) [1..100]

tree11 = foldl (\tr x->insert x tr) (singleton 0) [1..100]
