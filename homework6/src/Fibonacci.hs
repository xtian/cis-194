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

streamRepeat :: a -> Stream a
streamRepeat x = Element x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap fn (Element x s) = Element (fn x) (streamMap fn s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed fn x = Element x (streamFromSeed fn (fn x))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Element x s1) s2 = Element x (interleaveStreams s2 s1)

ruler :: Stream Integer
ruler = fn 0
  where fn n = interleaveStreams (streamRepeat n) (fn $ n + 1)
