{-# OPTIONS_GHC -Wall #-}

import Prelude hiding ((^), and, concat, replicate, (!!), elem, sum, take, last)

-- 1.
fac' :: Int -> Int
fac' n | n <= 0    = 1
       | otherwise = n * fac' (n - 1)

-- fac'' :: Int -> Int
-- fac'' 0         = 1
-- fac'' n | n > 0 = n * fac''(n - 1)

-- 2.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown(n - 1)

-- 3.
(^) :: (Num a, Integral b) => a -> b -> a
_ ^ 0 = 1
m ^ n = m * m^(n-1)

-- 4.
euclid :: Int -> Int -> Int
euclid a b | a < b = euclid a (b-a)
           | a > b = euclid (a-b) b
           | otherwise = a

-- 5.

-- 6.
-- a.
and :: [Bool] -> Bool
and []     = True
and (x:xs) | x == False = False
           | otherwise  = and xs

-- b.
concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

-- c.
replicate :: Int -> a -> [a]
replicate n x | n <= 0    = []
              | otherwise = x : replicate (n-1) x

-- d.
(!!) :: [a] -> Int -> a
x !! 0 = head x
x !! n = (tail x) !! (n-1)

-- e.
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem m xs = if head xs == m then True
            else elem m (tail xs)

-- elem' e (x:xs)
--   | e == x    = True
--   | otherwise = elem' e xs

-- 7.
merge :: Ord a => [a] -> [a] -> [a]
merge xs     []      = xs
merge []      ys     = ys
merge (x:xs) (y:ys)
  | x < y     = x : merge xs     (y:ys)
  | otherwise = y : merge (x:xs) ys

-- 8.
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
           where (left, right) = halve xs

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([x], [])
halve xs = ((take n xs), (drop n xs))
           where n = length xs `div` 2

-- 9.
sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

take :: Int -> [a] -> [a]
take _ []     = []
take n (x:xs)
  | n <= 0    = []
  | otherwise = x : take (n-1) xs

last :: [a] -> a
last [] = error "Empty list"
last [x] = x
last (_:xs) = last xs
