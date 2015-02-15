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

validate :: Integer -> Bool
validate cardNumber =
  let cardSum = sumDigits $ doubleEveryOther $ toDigits cardNumber
  in  cardSum `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n src dest store =
  (hanoi (n-1) src store dest) ++ [(src, dest)] ++ (hanoi (n-1) store dest src)
