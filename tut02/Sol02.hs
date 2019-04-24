import Prelude hiding (words, unwords)

-- 1a

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = pack' [x] xs

pack' :: Eq a => [a] -> [a] -> [[a]]
pack' [] _ = undefined
pack' ys [] = [ys]
pack' (y:ys) (x:xs)
  | y == x    = pack' (x:y:ys) xs
  | otherwise = (y:ys) : pack' [x] xs

-- 1b

encode :: Eq a => [a] -> [(Int, a)]
encode xs
  = let ys = pack xs
        f [] = undefined
        f (z:zs) = (length (z:zs), z)
    in map f ys

-- 1c

decode :: [(Int, a)] -> [a]
decode
  = concatMap (\(i, x) -> take i $ repeat x)

-- 1d

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate (x:xs) i
  | i > 0     = rotate (xs ++ [x]) (i - 1)
  | i < 0     = rotate (x:xs) (i + length (x:xs))
  | otherwise = (x:xs)

rotate' :: [a] -> Int -> [a]
rotate' [] _ = []
rotate' (x:xs) i
  | i > 0     = drop i xs ++ take i xs
  | i < 0     = rotate' (x:xs) (i + length (x:xs))
  | otherwise = (x:xs)

-- 2a

unwords :: [String] -> String
unwords [] = []
unwords [x] = x
unwords (x:xs) = x ++ " " ++ unwords xs

-- 2b

words :: String -> [String]
words [] = []
words xs = words' [] xs
  where words' ys [] = [ys]
        words' ys (' ':zs) = ys : words' [] zs
        words' ys (z:zs) = words' (ys ++ [z]) zs

-- 3

data Tree = Node String [Tree]

-- 3a

tree :: Tree
tree = Node "Wurzel"
       [
         Node "l" [],
         Node "m"
         [
           Node "lu" [],
           Node "ru" []           
         ],
         Node "r" []
       ]

-- 3b

level :: Int -> Tree -> [String]
level 0 (Node x _)  = [x]
level i (Node _ ts) = concatMap (level (i-1)) ts
