-- 1

data Tree = Node Int Tree Tree | Nil deriving Show

-- Kinds und Typen der definierten Konstruktoren
-- Tree :: *                            (Kind des Typkonstruktors)
-- Node :: Int -> Tree -> Tree -> Tree  (Typ des Wertkonstruktors)
-- Nil  :: Tree                         (Typ des Wertkonstruktors)

-- 1a

insert :: Tree -> [Int] -> Tree
insert = foldl insert'

insert' :: Tree -> Int -> Tree
insert' Nil j
  = Node j Nil Nil
insert' u@(Node i s t) j
  | j < i = Node i (insert' s j) t
  | j > i = Node i s (insert' t j)
  | otherwise = u

-- 1b

equal :: Tree -> Tree -> Bool
equal Nil Nil = True
equal (Node x1 s1 t1) (Node x2 s2 t2)
  = (x1 == x2) && equal s1 s2 && equal t1 t2
equal _ _ = False
