{-# OPTIONS_GHC -Wall #-}

import Data.Char

-- 1.
squareSum :: Integer
squareSum = sum [x*x | x <- [1..100]]

-- 2.
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]

-- 3.
square :: Int -> [(Int,Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4.
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

-- 5.
pyths :: Int -> [(Int,Int,Int)]
pyths n = [(a, b, c) | a <- [1..n],
                       b <- [1..n],
                       c <- [1..n],
                       a^(2::Int) + b^(2::Int) == c^(2::Int)]

-- 6.
perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum (factors x) == 2*x]
             where factors y = [z | z <- [1..y], y `mod` z == 0]

perfects' :: Int -> [Int]
perfects' n = [x | x <- [1..n], perfect x]
             where perfect y = sum (factors y) == 2*y
                   factors z = [ft | ft <- [1..z], z `mod` ft == 0]

-- 7.
-- [(x,y) | x <- [1,2], y <- [3,4]]
listComprehension :: [(Int,Int)]
listComprehension = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- 8.
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..])

-- 9.
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- 10.
let2int :: Char -> Int
let2int c = ord c - ord 'a'

let2int' :: Char -> Int
let2int' c = ord c - ord 'A'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

int2let' :: Int -> Char
int2let' n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2let' ((let2int' c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
