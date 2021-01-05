module MyModule where
import Euterpea
import HSoM

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor

midC :: Music Pitch
midC = note qn (C, 4)

-- 2.1
twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = let two  = note d (trans 2 p) :=: note d (trans 5 p)     :=: note d (trans 9 p)
                     five = note d (trans 7 p) :=: note d (trans 11 p)    :=: note d (trans 14 p)
                     one  = note (2*d) p       :=: note (2*d) (trans 4 p) :=: note (2*d) (trans 7 p)
                 in two :+: five :+: one    

twoFiveOne' :: Pitch -> Dur -> Music Pitch
twoFiveOne' p d = let n    = note d p
                      two  = triad (transpose 2 n) False
                      five = triad (transpose 7 n) True
                      one  = triad (note (d*2) p)  True
                  in two :+: five :+: one    

triad :: Music Pitch -> Bool -> Music Pitch
triad n isMajor = n :=: n' :=: n''
  where n'  = transpose (if isMajor then 4 else 3) n
        n'' = transpose 7 n


-- 2.2
data BluesPitchClass = Ro | MT | Fo | Fi | MS
  deriving (Eq,Show,Ord)

type BluesPitch = (BluesPitchClass, Octave)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro,o)
mt o d = note d (MT,o)
fo o d = note d (Fo,o)
fi o d = note d (Fi,o)
ms o d = note d (MS,o)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Rest d))        = Prim (Rest d)
fromBlues (Prim (Note d (Ro,o))) = Prim (Note d (C,o))
fromBlues (Prim (Note d (MT,o))) = Prim (Note d (Ef,o))
fromBlues (Prim (Note d (Fo,o))) = Prim (Note d (F,o))
fromBlues (Prim (Note d (Fi,o))) = Prim (Note d (G,o))
fromBlues (Prim (Note d (MS,o))) = Prim (Note d (Bf,o))
fromBlues (m1 :+: m2)            = (fromBlues m1) :+: (fromBlues m2)
fromBlues (m1 :=: m2)            = (fromBlues m1) :=: (fromBlues m2)
fromBlues (Modify c m)           = Modify c (fromBlues m)

m1 :: Music BluesPitch
m1 = ro 4 qn :+: fi 4 qn :+: ms 4 qn :+: fo 4 qn :+: ms 4 qn
-- play $ fromBlues m1

melody4 = fromBlues( mt 6 qn :+: rest en :+: times 2 ( mt 6 sn ) :+:
                     mt 5 en :+: rest sn :+: mt 6 en :+: rest sn :+:
                     times 2 (mt 6 qn ) :+: mt 6 en )
-- play melody4

-- 2.3
test = all (\ap -> absPitch (pitch ap) == ap) [0..1000]

test' = all (\p -> pitch (absPitch p) == p) $
        [(pc, oct) | pc  <- [C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B],
                     oct <- [1..8]]

-- 2.4
test'' = all (\(i,j,p) -> trans i (trans j p) == trans (i+j) p) $
         [(i, j, (C,4)) | i <- [0..100], j <- [0..100]]

-- 2.5
transM :: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Rest d))   = Prim (Rest d)
transM ap (Prim (Note d p)) = Prim (Note d (trans ap p))
transM ap (m1 :+: m2)       = (transM ap m1) :+: (transM ap m2)
transM ap (m1 :=: m2)       = (transM ap m1) :=: (transM ap m2)
transM ap (Modify c m)      = Modify c (transM ap m)

-- play $ transM 12 $ fromBlues m1
