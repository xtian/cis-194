module Homework1 where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0    = []
  | otherwise = map digitToInteger $ show x

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse $ toDigits x

digitToInteger :: Char -> Integer
digitToInteger x = toInteger $ digitToInt x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ map fn indexes
  where
    indexes = [0..(length xs - 1)]
    xs' = reverse xs
    fn i
      | i `mod` 2 > 0 = (xs' !! i) * 2
      | otherwise     = (xs' !! i)

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 $ concat $ map toDigits xs
