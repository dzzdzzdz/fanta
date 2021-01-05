{-# OPTIONS_GHC -Wall #-}

--import Prelude hiding ()

-- 1.

data Nat = Zero | Succ Nat
           deriving Show

mult :: Nat -> Nat -> Nat
mult Zero     _ = Zero
mult (Succ m) n = add n (mult m n)

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

-- 2.
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
         (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                          EQ -> True
                          LT -> occurs x l
                          GT -> occurs x r

-- 3.
data Tree' a = Leaf' a | Node' (Tree' a) (Tree' a)

balanced :: Tree' a -> Bool
balanced (Leaf' _)   = True
balanced (Node' l r) = balanced l && balanced r && abs(numLeaves l - numLeaves r) <= 1

numLeaves :: Tree' a -> Int
numLeaves (Leaf' _)   = 1
numLeaves (Node' l r) = numLeaves l + numLeaves r

-- 4.
balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs  = Node' (balance ys) (balance zs)
              where (ys, zs) = halveList xs

halveList :: [a] -> ([a],[a])
halveList xs = splitAt (length xs `div` 2) xs

-- 5.
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x)   = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)
