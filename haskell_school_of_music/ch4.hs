module MyModule where
import Euterpea
import HSoM

twinkle =
  c 4 qn :+: c 4 qn :+: g 4 qn :+: g 4 qn :+: a 4 qn :+: a 4 qn :+: g 4 hn :+:
  f 4 qn :+: f 4 qn :+: e 4 qn :+: e 4 qn :+: d 4 qn :+: d 4 qn :+: c 4 hn :+:
  g 4 qn :+: g 4 qn :+: f 4 qn :+: f 4 qn :+: e 4 qn :+: e 4 qn :+: d 4 hn :+:
  g 4 qn :+: g 4 qn :+: f 4 qn :+: f 4 qn :+: e 4 qn :+: e 4 qn :+: d 4 hn :+:
  c 4 qn :+: c 4 qn :+: g 4 qn :+: g 4 qn :+: a 4 qn :+: a 4 qn :+: g 4 hn :+:
  f 4 qn :+: f 4 qn :+: e 4 qn :+: e 4 qn :+: d 4 qn :+: d 4 qn :+: c 4 hn

pcToQN :: PitchClass -> Music Pitch
pcToQN pc = note qn (pc,4)

twinkle' =
  line (map pcToQN [C,C,G,G,A,A]) :+: g 4 hn :+:
  line (map pcToQN [F,F,E,E,D,D]) :+: c 4 hn :+:
  line (map pcToQN [G,G,F,F,E,E]) :+: d 4 hn :+:
  line (map pcToQN [G,G,F,F,E,E]) :+: d 4 hn :+:
  line (map pcToQN [C,C,G,G,A,A]) :+: g 4 hn :+:
  line (map pcToQN [F,F,E,E,D,D]) :+: c 4 hn

twinkle'' =
  let m1 = line (map pcToQN [C,C,G,G,A,A]) :+: g 4 hn
      m2 = line (map pcToQN [F,F,E,E,D,D]) :+: c 4 hn
      m3 = line (map pcToQN [G,G,F,F,E,E]) :+: d 4 hn
  in line [m1,m2,m3,m3,m1,m2]

times' :: Int -> Music a -> Music a
times' 0 m = rest 0
times' n m = m :+: times' (n-1) m

--

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = line (map f ns) where f n = n d

graceNote :: Int -> Music Pitch -> Music Pitch
graceNote n (Prim (Note d p)) = note (d/8) (trans n p) :+: note (7*d/8) p
graceNote n _               = error "graceNote: can only add a grace note to a note"

bassLine = times' 3 b1 :+: times' 2 b2 :+: times' 4 b3 :+: times' 5 b1
  where b1 = addDur dqn [b 3, fs 4, g  4, fs 4]
        b2 = addDur dqn [b 3, es 4, fs 4, es 4]
        b3 = addDur dqn [as 3,fs 4, g  4, fs 4]

mainVoice = times 3 v1 :+: v2
  where v1  = v1a :+: graceNote (-1) (d 5 qn) :+: v1b -- bars 1-2
        v1a = addDur en [a  5, e 5, d 5, fs 5, cs 5, b 4, e 5, b 4]
        v1b = addDur en [cs 5, b 4]
        v2  = v2a :+: v2b :+: v2c :+: v2d :+: v2e :+: v2f :+: v2g
        v2a = line [cs 5 (dhn+dhn), d 5 dhn, f 5 hn, gs 5 qn, fs 5 (hn+en), g 5 en] -- B 7-11
        v2b = addDur en [fs 5, e  5, cs 5, as 4] :+: a 4 dqn :+:
              addDur en [as 4, cs 5, fs 5, e 5, fs 5] -- B 12-13
        v2c = line [g  5 en, as 5 en, cs 6 (hn+en), d 6 en, cs 6 en] :+:
              e 5 en :+: enr :+:
              line [as 5 en, a 5 en, g 5 en, d 5 qn, c 5 en, cs 5 en] -- B 14-16
        v2d = addDur en [fs 5, cs 5, e 5, cs 5, a 4, as 4, d 5, e 5, fs 5] -- B 17-18.5
        v2e = line [graceNote 2 (e  5 qn), d 5 en, graceNote 2 (d 5 qn), cs 5 en,
                    graceNote 1 (cs 5 en), b 4 (en+hn), cs 5 en, b 4 en] -- 18.5-20
        v2f = line [fs 5 en, a  5 en, b 5 (hn+qn), a 5 en, fs 5 en, e 5 qn,
                    d  5 en, fs 5 en, e 5 hn, d 5 hn, fs 5 qn] -- B 21-23
        v2g = tempo (3/2) (line [cs 5 en, d 5 en, cs 5 en]) :+: b 4 (hn+dhn*3) -- B 24-28

childSong6 :: Music Pitch
childSong6 = instrument RhodesPiano (tempo t (bassLine :=: mainVoice))
  where t = (dhn/qn) * (69/120)

--

prefixes :: [a] -> [[a]]
prefixes []     = []
prefixes (x:xs) = let f pf = x:pf
                  in [x]:map f (prefixes xs)

prefix :: [Music a] -> Music a
prefix mel = m :+: transpose 5 m :+: m
  where
    m1 = line (concat (prefixes mel))
    m2 = transpose 12 (line (concat (prefixes (reverse mel))))
    m  = instrument Flute m1 :=: instrument VoiceOohs m2

mel1 = [c 5 en, e 5 sn, g 5 en, b 5 sn, a 5 en, f 5 sn, d 5 en, b 4 sn, c 5 en]
mel2 = [c 5 sn, e 5 sn, g 5 sn, b 5 sn, a 5 sn, f 5 sn, d 5 sn, b 4 sn, c 5 sn]
