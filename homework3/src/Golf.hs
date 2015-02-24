module Golf where

import Data.List
  ( elemIndices
  , group
  , sort
  , tails
  , transpose
  )

-- Maps over each successively smaller tail of the input list and drops every
-- n elements from each tail, where n is the difference in length between the
-- input list and the tail. Drops the last element of the returned list to avoid
-- including an empty list.
skips :: [a] -> [[a]]
skips [] = []
skips xs = take l $ map f $ tails xs
  where
    l = length xs
    f x = d (l - length x) x

    d _ [] = []
    d n (y:ys) = y : (d n $ drop n ys)

-- Creates a list out of the middle element of each successively smaller tail of
-- the input list where the middle element is greater than the preceding and
-- succeding integers.
localMaxima :: [Integer] -> [Integer]
localMaxima xs = [y | (x:y:z:_) <- tails xs, y > x, y > z]

-- Maps over each number from 0 to 9 and returns a string of stars equal to the
-- number of times that integer appears in the input array. Each string of stars
-- is padded with spaces in order to make them equal in length. The maximum
-- length of each string is found by grouping like numbers of the input array
-- and finding the length of the longest resulting group. The `transpose`
-- function then collates the strings by index to convert them into rows
-- and columns.
histogram :: [Integer] -> String
histogram xs = unlines $ transpose $ map f [0..9]
  where
    f n = reverse $ show n ++ "=" ++ take m (s n ++ repeat ' ')
    s n = take (length $ elemIndices n xs) $ repeat '*'
    m = maximum $ map length $ group $ sort xs
