module MyModule where
import Euterpea
import HSoM

-- 5.1
twice :: (a -> a) -> a -> a
twice f = f . f

-- 5.2
power :: (a -> a) -> Int -> a -> a
power f 0 = id
power f n = power f (n-1) . f

-- 5.3
fix :: (t -> t) -> t
fix f = f (fix f)

remainder :: Integer -> Integer -> Integer
remainder a b = if a < b then a
                else remainder (a-b) b

remainder' :: Integer -> Integer -> Integer
remainder' a b = fix (\rem a b -> if (a < b) then a else rem (a-b) b) a b

-- 5.4
apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch,AbsPitch)]
apPairs ap1 ap2 = [(x,y) | x <- ap1,
                           y <- ap2,
                           abs(x-y) > 2 && abs(x-y) < 8]

pairsToMusic :: [(AbsPitch,AbsPitch)] -> Music Pitch
pairsToMusic = let f (ap1,ap2) = let d = if ap1 `mod` 2 == 0 then en else qn
                                 in note d (pitch ap1) :=: note d (pitch ap2)
               in line . map (f)

apPairs' :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
apPairs' aps1 aps2 = [(ap1, ap2) | ap1 <- aps1,
                                  ap2 <- aps2,
                                  abs(ap1 - ap2) > 2,
                                  abs(ap1 - ap2) > 8]

pairsToMusic' :: [(AbsPitch, AbsPitch)] -> Music Pitch
pairsToMusic' = line . map pairToNotes
  where pairToNotes (ap1, ap2) = let d1 = if ap1 `mod` 2 == 0 then en else sn
                                     d2 = if ap2 `mod` 2 == 0 then en else sn
                                 in note d1 (pitch ap1) :=: note d2 (pitch ap2) 
test4 = pairsToMusic $ apPairs [304..312] [300,302..310]

-- 5.5
hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (-3) p)

hList :: Dur -> [Pitch] -> Music Pitch
hList d = line . map (hNote d)

{-
hList d = line . map (hNote d)
hList   = (.) line (map (hNote d))
hList   = (.) line . (map . hNote)
-}

hList' :: Dur -> [Pitch] -> Music Pitch
hList' = (.) line . (map . hNote)

-- 5.6
addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = let f n = n d
              in line (map f ns)

addDur' :: Dur -> [Dur -> Music a] -> Music a
addDur' d ns = line [n d | n <- ns]

addDur'' :: Dur -> [Dur -> Music a] -> Music a
addDur'' d = line . map (\n -> n d)

-- 5.7
-- map7  = map (\x -> (x+1)/2) xs
-- map7' = map ((/2) . (+1))

-- 5.8
-- map8   = f (map g xs)
-- map8'  = f g = map (f . g)
-- map7'' = map (/2) . map (+1)

-- 5.9

-- 5.10
-- f1 (f2 (*) [1,2,3,4]) 5 ==> [5,10,15,20]
