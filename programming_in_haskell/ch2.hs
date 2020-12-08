n = a `div` length xs
    where
      a = 10
      xs = [1,2,3,4,5]

last' :: [x] -> x
last' xs = head (reverse xs)

init' :: [x] -> [x]
init' [] = []
init' xs = take (length xs-1) xs

init'' :: [x] -> [x]
init'' [] = []
init'' xs = reverse $ tail $ reverse xs
