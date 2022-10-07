-- Just in case you forgot...
{-# OPTIONS_GHC -W #-}

module Warmup where

type Pos = (Int, Int)

data Direction = North | South | East | West
  deriving (Eq, Show, Read, Ord)

move :: Direction -> Pos -> Pos
move North (x, y) = (x, y + 1)
move West (x, y) = (x - 1, y)
-- complete the definition
move East (x, y) = (x + 1, y)
move South (x, y) = (x, y - 1)

moves :: [Direction] -> Pos -> Pos
-- replace with actual definition of moves, and likewise for the
-- other 'undefined' functions
moves [] (x, y) = (x, y)
moves (h : list_direction) (x, y) =
  moves list_direction (move h (x, y))

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add n1 n2 = int2nat (nat2int n1 + nat2int n2)

mult :: Nat -> Nat -> Nat
mult n1 n2 = int2nat (nat2int n1 * nat2int n2)

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ nat) = 1 + nat2int nat

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat integer = Succ (int2nat (integer - 1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert integer Leaf = Node integer Leaf Leaf
insert integer (Node value left right)
  | integer == value = Node value left right
  | integer > value = Node value left (insert integer right)
  | otherwise = Node value (insert integer left) right

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Eq, Show, Read, Ord)

pinsert :: a -> PTree a -> PTree a
pinsert = undefined
-- pinsert element PLeaf = PNode element PLeaf PLeaf
-- pinsert element (PNode value left right)
--   | element == value = PNode value left right
--   | element > value = PNode value left (pinsert element right)
--   | otherwise = PNode value (pinsert element left) right
