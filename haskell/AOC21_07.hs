{-# LANGUAGE TupleSections #-}

-- https://adventofcode.com/2021/day/7
module AOC21_07 where

import Data.Foldable (Foldable (foldl'))
import Data.Function (on, (&))
import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)

---------------
-- Problem 1 --
---------------
p1 :: [String] -> IO ()
p1 input =
  p1' [min' .. max'] input'
    & minimumBy (comparing snd)
    & snd
    & print
  where
    input' = parse input
    min' = fst (head input')
    max' = fst (last input')
    p1' [] _ = []
    p1' (x : xs) ys = (x, cost x ys) : p1' xs ys

parse :: [String] -> [(Int, Int)]
parse [x] = (read ("[" <> x <> "]") :: [Int]) & fmap (,1) & group & sortBy (compare `on` fst)
  where
    group [] = []
    group cs@((h, n) : cs_) =
      (h, filter (\(x, _) -> x == h) cs & count) :
      group (filter (\(x, _) -> x /= h) cs_)
    count = foldl' (\a (_, n) -> a + n) 0
parse _ = error "bad input"

cost :: Int -> [(Int, Int)] -> Int
cost p = foldl' (\a (x, n) -> abs (x - p) * n + a) 0

middle :: [a] -> [a]
middle l@(_ : _ : _ : _) = middle $ tail $ init l
middle l = l

---------------
-- Problem 2 --
---------------
p2 :: [String] -> IO ()
p2 input =
  p2' [min' .. max'] input'
    & minimumBy (comparing snd)
    & snd
    & print
  where
    input' = parse input
    min' = fst (head input')
    max' = fst (last input')
    p2' [] _ = []
    p2' (x : xs) ys = (x, cost' x ys) : p2' xs ys

cost' :: Int -> [(Int, Int)] -> Int
cost' p = foldl' (\a (x, n) -> sum [1 .. (abs (x - p))] * n + a) 0
