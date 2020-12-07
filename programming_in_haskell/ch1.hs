double :: Num a => a -> a
double n = 2 * n

sum [n] = n

product' :: Num a => [a] -> a
product' []     = 1
product' (n:ns) = n * product' (ns)
