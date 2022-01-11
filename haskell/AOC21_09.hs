-- https://adventofcode.com/2021/day/9
module AOC21_09 where

import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import Data.List (sort, transpose)

---------------
-- Problem 1 --
---------------
p1 :: [String] -> IO ()
p1 input =
  let xs = parse input
   in lows xs
        & zipWith (zipWith (\n b -> if b then n + 1 else 0)) xs
        & concat
        & sum
        & print

parse :: [String] -> [[Int]]
parse = fmap (fmap digitToInt)

lows :: [[Int]] -> [[Bool]]
lows xs =
  let xs' = transpose xs
   in zipWith (zipWith (&&)) (fmap low xs) (transpose $ fmap low xs')

low :: [Int] -> [Bool]
low [x1, x2, x3] = [x1 < x2, x2 < x1 && x2 < x3, x3 < x2]
low (x1 : x2 : x3 : xs) = (x1 < x2) : (x2 < x1 && x2 < x3) : tail (low (x2 : x3 : xs))
low _ = error "bad input"

---------------
-- Problem 2 --
---------------
p2 :: [String] -> IO ()
p2 input =
  let xs = parse input
   in basins xs
        & sort
        & reverse
        & take 3
        & foldl' (*) 1
        & print

basins :: [[Int]] -> [Int]
basins xs =
  concatMap
    (filter (/= 0) . fmap snd)
    ( fmap (fmap codify) xs
        & evalRows
        & evalCols
        & evalRows
        & evalCols
        & evalRows
        & evalCols
    )

codify :: Int -> (Int, Int)
codify 9 = (9, 0)
codify x = (x, 1)

evalRows :: [[(Int, Int)]] -> [[(Int, Int)]]
evalRows = fmap (eval' . eval)

evalCols :: [[(Int, Int)]] -> [[(Int, Int)]]
evalCols xs = transpose $ evalRows (transpose xs)

eval :: [(Int, Int)] -> [(Int, Int)]
eval [x] = [x]
eval ((h, c) : (h', c') : xs) =
  if h' < h
    then (h, 0) : eval ((h', c + c') : xs)
    else (h, c) : eval ((h', c') : xs)
eval _ = error "bad input"

eval' :: [(Int, Int)] -> [(Int, Int)]
eval' [x] = [x]
eval' ((h, c) : xs) =
  let xs' = eval' xs
      (xh, xc) = head xs'
   in if h < xh
        then (h, c + xc) : (xh, 0) : tail xs'
        else (h, c) : xs'
eval' _ = error "bad input"
