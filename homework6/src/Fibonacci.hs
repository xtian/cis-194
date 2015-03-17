module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = (fib $ x - 1) + (fib $ x - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate fn (0, 1)
  where fn (x, y) = (y, x + y)

data Stream a = Element a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Element x s) = x : streamToList s
