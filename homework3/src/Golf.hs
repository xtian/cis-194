module Golf where

import Data.List (tails)

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
