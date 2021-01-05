module MyModule where
import Euterpea hiding (Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian)
import HSoM

wts :: Pitch -> [Music Pitch]
wts p = let f ap = note qn (pitch (absPitch p + ap))
        in map f [0,2,4,6,8]

wts' :: Pitch -> [Music Pitch]
wts' p = map f $ take 5 [0,2..]
  where f ap = note qn (pitch (absPitch p + ap))

-- 3.1
f1 :: Int -> [Pitch] -> [Pitch]
f1 _ []   = []
f1 ap ps = map (trans ap) ps

f2 :: [Dur] -> [Music a]
f2 [] = []
f2 ds = map (\d -> Prim (Rest d)) ds

f3 :: [Music Pitch] -> [Music Pitch]
f3 [] = []
f3 ns = map f ns
  where f (Prim (Note d p)) = Prim (Note (d/2) p) :+: Prim (Rest (d/2))

--

hList :: Dur -> [Pitch] -> Music Pitch
hList d []     = rest 0
hList d (p:ps) = hNote d p :+: hList d ps

hList' :: Dur -> [Pitch] -> Music Pitch
hList' d ps = let f p = hNote d p
              in line (map f ps)

hList'' d ps = line (map f ps)
  where f p = hNote d p

hNote :: Dur -> Pitch -> Music Pitch
hNote d p = note d p :=: note d (trans (-3) p)

-- 3.2
{-
:t flip
flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

flip f is a partially applied function that, when given 2 args x and y, yields f y x

flip (flip f) == flip (f y x) ==> f x y

flip (flip f) == f
f      = [1,2,3]
flip f = [3,2,1] = g
flip g = [1,2,3] = f
-}
test = (flip (flip efff) 3 3) == (6,12)
  where efff a b = (a*2, b*4)

-- 3.3
xs = [1,2,3] :: [Integer]
ys = map (+) xs
-- ys :: (Integer -> Integer) -> [Integer] -> [Integer -> Integer]
-- ys :: [Integer -> Integer]

-- 3.4
simple :: Num a => a -> a -> a -> a
simple x y z = x * (y + z)

applyEach :: [t -> b] -> t -> [b]
applyEach fs v = map (\f -> f v) fs

applyEach' :: [(a -> b)] -> a -> [b]
applyEach' fs v = let f5 v f = f v
                  in map (f5 v) fs

applyEach'' :: [(a -> b)] -> a -> [b]
applyEach'' fs v = map (f5 v) fs
  where f5 v f = f v

applyEach''' :: [(a -> b)] -> a -> [b]
applyEach''' fs v = map ($ v) fs
       
test' = (applyEach''' [simple 2 2, (+3)] 5) == ([14,8])

-- 3.5
applyAll :: [a -> a] -> a -> a
applyAll fs v = foldr (\f acc -> f acc) v fs

applyAll' :: [a -> a] -> a -> a
applyAll' [] v     = v
applyAll' (f:fs) v = f (applyAll fs v)

test'' = (applyAll [simple 2 2, (+3)] 5) == (20)

-- 3.6
{-
appendr, appendl :: [[a]] -> [a]
appendr = foldr (flip (++)) []
appendl = foldl (flip (++)) []

for ref: https://github.com/tkuriyama/hsom/blob/master/exercises/chapter3/ex3_6.hs
-}

-- 3.7
length' :: [a] -> Int
length' = foldl (\acc _ -> acc+1) 0

length'' :: [a] -> Int
length'' = foldr (\_ acc -> acc+1) 0

-- 3.8
doubleEach :: Num a => [a] -> [a]
doubleEach = map (*2)

pairAndOne :: Num a => [a] -> [(a,a)]
pairAndOne = map (\n -> (n, n+1))

addEachPair :: Num a => [(a,a)] -> [a]
addEachPair = map (\(a,b) -> a + b)

addPairsPointwise :: Num a => [(a,a)] -> (a,a)
addPairsPointwise = foldr (\(x,y) (a,b) -> (x+a,y+b)) (0,0)

test8a = doubleEach [1,2,3] == [2,4,6]
test8b = pairAndOne [1,2,3] == [(1,2),(2,3),(3,4)]
test8c = addEachPair [(1,2),(2,3),(5,6)] == [3,5,11]
test8d = addPairsPointwise [(1,2),(3,4),(5,6)] == (9,12)

-- 3.9
fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse ds ns = if length ds /= length ns
               then error "fuse: lengths must match"
               else map (\(d,n) -> n d) comb where comb = zip ds ns
                              

test9 = fuse [qn,hn,sn] [c 4, d 4, e 4] == [c 4 qn, d 4 hn, e 4 sn]

-- 3.10
maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch [] = error "maxAbsPitch: cannot be applied to empty list"
maxAbsPitch ps = maximum ps

minAbsPitch :: [AbsPitch] -> AbsPitch
minAbsPitch [] = error "minAbsPitch: cannot be applied to empty list"
minAbsPitch ps = minimum ps

-- 3.11
chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2
  | absPitch p1 < absPitch p2 = note qn p1 :+: chrom (trans 1 p1) p2
  | absPitch p1 > absPitch p2 = note qn p1 :+: chrom (trans (-1) p1) p2
  | otherwise                 = note qn p1

-- 3.12
mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p []     = note qn p
mkScale p (i:is) = note qn p :+: mkScale (trans i p) is

-- 3.13
data MajorMode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
  deriving (Eq,Ord,Show)

genScale :: Pitch -> MajorMode -> Music Pitch
genScale p mode = mkScale p (getModeIntervals mode)
  where intervals                   = cycle [2,2,1,2,2,2,1]
        getIntervals n              = take 7 $ drop n $ intervals
        getModeIntervals Ionian     = getIntervals 0
        getModeIntervals Dorian     = getIntervals 1
        getModeIntervals Phrygian   = getIntervals 2
        getModeIntervals Lydian     = getIntervals 3
        getModeIntervals Mixolydian = getIntervals 4
        getModeIntervals Aeolian    = getIntervals 5
        getModeIntervals Locrian    = getIntervals 6
        
-- 3.14
fj :: Music Pitch
fj = let
       first  = line [c 4 qn, d 4 qn, e 4 qn, c 4 qn]
       second = line [e 4 qn, f 4 qn, g 4 hn]
       third  = line [g 4 en, a 4 en, g 4 en, f 4 en, e 4 qn, c 4 qn]
       fourth = line [c 4 qn, g 3 qn, c 4 hn]
       melody = first :+: first :+: second :+: second :+: third :+: third :+: fourth :+: fourth
     in
       melody :=:
       (instrument Violin ((rest (2*wn)) :+: melody)) :=:
       (instrument Tuba   (rest (4*wn)  :+: melody))

-- 3.15
encrypt :: String -> String
encrypt = map (\s -> toEnum ((fromEnum s) + 1))

decrypt :: String -> String
decrypt = map (\s -> toEnum ((fromEnum s) - 1))

test15 = "Hello world!" == (decrypt . encrypt $ "Hello world!")
