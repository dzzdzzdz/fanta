type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n src dst tmp
  | n <= 0 = []
  | n == 1 = [(src, dst)]
  | otherwise = hanoi (n-1) src tmp dst ++ hanoi 1 src dst tmp ++ hanoi (n-1) tmp dst src
