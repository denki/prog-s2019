-- 2

data Tree = Node Int [Tree]

-- 2a

countLeaves :: Tree -> Int
countLeaves (Node _ [])
  = 1
countLeaves (Node _ ts)
  = sum (map countLeaves ts)

-- 2b

isEven :: Tree -> Bool
isEven (Node _ ts)
  = even (length ts) && and (map isEven ts)

-- 3

f :: [Int] -> Int
f = foldr (+) 0 . map (\x -> x * x) . filter even
