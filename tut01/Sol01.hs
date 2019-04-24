import Prelude hiding (rem)

-- 1a

fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n - 1)

fac' :: Int -> Int
fac' n = product [1 .. n]

-- 1b

sumFacs :: Int -> Int -> Int
sumFacs m n
  = sum [fac i | i <- [m .. n]]

-- 2

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib i = fib (i - 2) + fib (i - 1)

fibFast :: Int -> Int -> Int -> Int
fibFast 0 x _ = x
fibFast 1 _ y = y
fibFast i x y = fibFast (i - 1) y (x + y)

-- 3a

prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

-- 3b

rev, rev' :: [a] -> [a] -- comment
rev [] = []
rev (foo:bar) = rev bar ++ [foo]

rev'
  = foldr (\x ys -> ys ++ [x]) []

-- 3c

rem, rem' :: Eq a => a -> [a] -> [a]
rem _ [] = []
rem x (y : ys)
  | x == y    = rem x ys
  | otherwise = y : rem x ys

rem' x = filter (/= x)

-- 3e

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
