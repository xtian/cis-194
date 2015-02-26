module Homework4 where

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map ((-) 2) . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate f
  where f n | even n = n `div` 2
            | otherwise = 3 * n + 1
