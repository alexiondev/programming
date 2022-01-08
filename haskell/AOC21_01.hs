-- https://adventofcode.com/2021/day/1
module AOC21_01 where

import Data.Function ((&))

---------------
-- Problem 1 --
---------------

p1 :: [String] -> IO ()
p1 input =
  print $
    p1' (fmap read input :: [Int])

p1' :: [Int] -> Int
p1' xs =
  zip xs (tail xs)
    & filter (uncurry (<))
    & length

---------------
-- Problem 2 --
---------------

p2 :: [String] -> IO ()
p2 input =
  print $
    p2' (fmap read input :: [Int])

p2' :: [Int] -> Int
p2' xs =
  sum <$> window 3 xs
    & p1'

window :: Int -> [Int] -> [[Int]]
window n [] = []
window n xs =
  take n xs :
  window n (tail xs)
    & filter (\x -> length x == n)
