---------------------------------------------
------------- Touring Haskell ---------------
---------------------------------------------

type Pos = (Int, Int)

data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x, y) = (x, y + 1)
move West (x, y) = (x -1, y)
-- #1
move East (x, y) = (x + 1, y)
move South (x, y) = (x, y -1)

-- #2
moves :: [Direction] -> Pos -> Pos
moves [] (x, y) = (x, y)
moves (h : list_direction) (x, y) =
  moves list_direction (move h (x, y))

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

-- #3
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ nat) = 1 + nat2int nat

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat integer = Succ (int2nat (integer -1))

-- #4
data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert integer Leaf = Node integer Leaf Leaf

insert integer (Node value left right)
    | integer == value = Node value left right
    | integer > value = Node value left (insert integer right)
    | otherwise = Node value (insert integer left) right

-- #4
-- data PTree a = PNode a (PTree a) (PTree a) | PLeaf 
--   deriving (Eq, Show, Read, Ord)
--
-- pinsert :: a -> PTree a -> PTree a
-- pinsert element PLeaf = PNode element PLeaf PLeaf
-- pinsert element (PNode value left right)
--     | element == value = PNode value left right
--     | element > value = PNode value left (pinsert element right)
--     | otherwise = PNode value (pinsert element left) right


