module Homework1 where

import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits i
  | i <= 0    = []
  | otherwise = map digitToInteger $ show i

toDigitsRev :: Integer -> [Integer]
toDigitsRev i = reverse $ toDigits i

digitToInteger :: Char -> Integer
digitToInteger i = toInteger $ digitToInt i
