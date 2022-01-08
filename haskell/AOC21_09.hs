-- https://adventofcode.com/2021/day/9
module AOC21_09 where

import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import Data.List (transpose)

---------------
-- Problem 1 --
---------------
p1 :: [String] -> IO ()
p1 input =
  lows xs
    & zipWith (zipWith (\n b -> if b then n + 1 else 0)) xs
    & concat
    & sum
    & print
  where
    xs = parse input

parse :: [String] -> [[Int]]
parse = fmap (fmap digitToInt)

lows :: [[Int]] -> [[Bool]]
lows xs = zipWith (zipWith (&&)) (fmap low xs) (transpose $ fmap low xs')
  where
    xs' = transpose xs

low :: [Int] -> [Bool]
low [x1, x2, x3] = [x1 < x2, x2 < x1 && x2 < x3, x3 < x2]
low (x1 : x2 : x3 : xs) = (x1 < x2) : (x2 < x1 && x2 < x3) : tail (low (x2 : x3 : xs))
low _ = error "bad input"

---------------
-- Problem 2 --
---------------
p2 :: [String] -> IO ()
p2 = print
