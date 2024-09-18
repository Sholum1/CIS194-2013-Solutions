module Homework4 where

-- Exercise 1
fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2' :: Integer -> Integer
fun2' =
  sum
    . filter even
    . takeWhile (/= 1)
    . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Exercise 2
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf
  where
    insertTree :: a -> Tree a -> Tree a
    insertTree x Leaf = Node 0 Leaf x Leaf
    insertTree x1 (Node h t1 x2 t2)
      | h1 < h2 = Node h t1n x2 t2
      | h1 > h2 = Node h t1 x2 t2n
      | h1n < h2n = Node h t1n x2 t2
      | otherwise = Node (h2n + 1) t1 x2 t2n
      where
        height :: Tree a -> Integer
        height (Node n _ _ _) = n
        height Leaf = 0
        h1 = height t1
        h2 = height t2
        t1n = insertTree x1 t1
        h1n = height t1n
        t2n = insertTree x1 t2
        h2n = height t2n

-- Exercise 3
xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x g a -> g (f a x)) id xs base

-- Exercise 4
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram =
  map (\i -> 2 * i + 1)
    . filter
      ( \n ->
          n
            `notElem` filter
              (<= n)
              ( map
                  (\(x, y) -> if x <= y then x + y + 2 * x * y else 2 * n)
                  (cartProd (enumFromTo 1 n) (enumFromTo 1 (n - 1)))
              )
      )
    . enumFromTo 1
