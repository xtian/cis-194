module Homework4 where

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map ((-) 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate f
  where f n | even n = n `div` 2
            | otherwise = 3 * n + 1


data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    height Leaf = 0
    height (Node _ l _ r) = 1 + max (height l) (height r)

    insert x Leaf = Node 0 Leaf x Leaf
    insert x (Node _ l@Leaf y r) = Node 1 (insert x l) y r
    insert x (Node _ l y r@Leaf) = Node 1 l y (insert x r)
    insert x (Node _ l@(Node lDepth _ _ _) y r@(Node rDepth _ _ _)) =
      if lDepth < rDepth
        then Node (rDepth + 1) (insert x l) y r
        else
          let r' = insert x r
          in  Node (height r') l y r'

xor :: [Bool] -> Bool
xor = foldl fn False
  where
    fn acc x = if x == True
      then not acc
      else acc

xor' :: [Bool] -> Bool
xor' = odd . length . filter (== True)
