{-# LANGUAGE TupleSections #-}

-- https://adventofcode.com/2021/day/6
module AOC21_06 where

import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))

---------------
-- Problem 1 --
---------------
p1 :: [String] -> IO ()
p1 input =
  parse input & group
    & simulate 80
    & count
    & print

parse :: [String] -> [(Int, Int)]
parse [x] = (read ("[" <> x <> "]") :: [Int]) & fmap (,1)
parse _ = error "bad input"

group :: [(Int, Int)] -> [(Int, Int)]
group [] = []
group fs@((d, n) : fs_) =
  (d, filter (\(x, _) -> x == d) fs & count) :
  group (filter (\(x, _) -> x /= d) fs_)

-- group xs@(x:xs_) = (x, length $ filter (== x) xs)
--     : group (filter (/= x) xs_)

simulate :: Int -> [(Int, Int)] -> [(Int, Int)]
simulate 0 fish = fish
simulate n fish = simulate (n -1) (next fish & group)

next :: [(Int, Int)] -> [(Int, Int)]
next [] = []
next ((0, n) : fs) = (6, n) : (8, n) : next fs
next ((d, n) : fs) = (d -1, n) : next fs

count :: [(Int, Int)] -> Int
count = foldl' (\a (_, n) -> a + n) 0

---------------
-- Problem 2 --
---------------
p2 :: [String] -> IO ()
p2 input =
  parse input
    & group
    & simulate 256
    & count
    & print
