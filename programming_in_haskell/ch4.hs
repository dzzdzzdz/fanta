{-# OPTIONS_GHC -Wall #-}

import Prelude hiding ((||))
-- 1.
halve :: [a] -> ([a],[a])
halve xs = (take halfLength xs, drop halfLength xs)
           where halfLength = length xs `div` 2

-- 2.
third :: [a] -> a
third xs = head $ tail $ tail xs

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_ : _ : x : _) = x

-- 3.
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs 

safetail' :: [a] -> [a]
safetail' xs | null xs   = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' []     = []
safetail'' (_:xs) = xs

-- 4.
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _         = True

-- 5.
func :: Bool -> Bool -> Bool
func a b = if a then
             if b then True else False
           else False

-- 6.
func' :: Bool -> Bool -> Bool
func' a b = if a then b else False

-- 7.
mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> (x*y*z)))

-- 8.
luhnDouble :: Int -> Int
luhnDouble n = if 2*n > 9 then 2*n-9 else 2*n

luhnDouble' :: Int -> Int
luhnDouble' x
  | n < 10 = n
  | otherwise = n-9
  where n = x*2

luhnDouble'' :: Int -> Int
luhnDouble'' x = y - (if y > 9 then 9 else 0)
                 where y = 2*x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0
