{-# OPTIONS_GHC -Wall #-}

import Prelude hiding (all, any, takeWhile, dropWhile, map, filter, curry, uncurry, iterate)
import Data.Char

type Bit = Int

-- 1.
lc :: (a -> b) -> (a -> Bool) -> [a] -> [b]
lc f p = map f . filter p

-- 2.
-- a.
-- book error
-- all :: (a -> Bool) -> [Bool] -> Bool
-- all p = and . map p

-- -- b.
-- book error
-- any :: (a -> Bool) -> [Bool] -> Bool
-- any p = or . map p

-- c.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile p (x:xs)
  | p x       = x : takeWhile p xs
  | otherwise = []

-- d.
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile p (x:xs)
  | p x       = dropWhile p xs
  | otherwise = x:xs

-- 3.
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x:xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x xs -> if p x then x:xs else xs) []

-- 4.
dec2int :: [Int] -> Int
dec2int = foldl (\x xs -> 10*x + xs) 0

-- 5.
curry :: ((a,b) -> c) -> a -> b -> c
curry f = \x y -> f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f = \(x,y) -> f x y

-- 6.
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
  | p x       = []
  | otherwise = h x : unfold p h t (t x)

int2bin :: Bit -> [Bit]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold null (take 8) (drop 8)

map' :: (a -> b) -> [a] -> [b]
map' f = unfold null (f . head) tail

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (\_ -> False) id f

-- 7.
transmit :: String -> String
transmit = decode . channel . encode

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

channel :: [Bit] -> [Bit]
channel = id

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)
--
calculateParity :: [Bit] -> Bit
calculateParity bits = sum bits `mod` 2

addParityBit :: [Bit] -> [Bit]
addParityBit bits = bits ++ [calculateParity bits]

encode' :: String -> [Bit]
encode' = concat . map (addParityBit . make8 . int2bin . ord)

decode' :: [Bit] -> String
decode' = map (chr . bin2int . checkParity) . chop9

checkParity :: [Bit] -> [Bit]
checkParity [] = []
checkParity (x:xs)
  | x == calculateParity xs = xs
  | otherwise               = error "Parity check failed!"
  

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold null (take 9) (drop 9)

-- 8.
faultyChannel :: [Bit] -> [Bit]
faultyChannel bits = tail bits

faultyTransmit :: String -> String
faultyTransmit = decode' . faultyChannel . encode'

-- 9.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ []       = []
altMap f _ [x]      = [f x]
altMap f g (x:y:ys) = f x : g y : altMap f g ys

-- 10.
luhn :: [Int] -> Bool
luhn = mod10NoRemainder . sum . (altMap luhnDouble id)

mod10NoRemainder :: Integral a => a -> Bool
mod10NoRemainder x = (mod x 10) == 0

luhnDouble :: Int -> Int
luhnDouble n = if 2*n > 9 then 2*n-9 else 2*n
