-- https://adventofcode.com/2021/day/3
module AOC21_03 where

import Data.Char (digitToInt)
import Data.Function ((&))
import Data.List (foldl', transpose)

type BinaryNumber = [Int]

---------------
-- Problem 1 --
---------------
p1 :: [String] -> IO ()
p1 input =
  fmap (fmap digitToInt) input & transpose
    & fmap ((\(a, b) -> if a > b then 1 else 0) . count)
    & withCompliment
    & fmap toDecimal
    & product
    & print

count :: BinaryNumber -> (Int, Int)
count xs = (sum xs, length xs - sum xs)

withCompliment :: BinaryNumber -> [BinaryNumber]
withCompliment xs = [xs, fmap (1 -) xs]

toDecimal :: BinaryNumber -> Int
toDecimal = foldl' (\acc x -> acc * 2 + x) 0

---------------
-- Problem 2 --
---------------
p2 :: [String] -> IO ()
p2 input =
  fmap (fmap digitToInt) input
    & p2' 0 . (\x -> (x, x))
    & uncurry (*)
    & print

p2' :: Int -> ([BinaryNumber], [BinaryNumber]) -> (Int, Int)
p2' _ ([x], [y]) = (toDecimal x, toDecimal y)
p2' i (xs, [y]) = p2' (i + 1) (filter (select (bit o2 i (transpose xs)) i) xs, [y])
p2' i ([x], ys) = p2' (i + 1) ([x], filter (select (bit co2 i (transpose ys)) i) ys)
p2' i (xs, ys) = p2' (i + 1) (filter (select (bit o2 i (transpose xs)) i) xs, filter (select (bit co2 i (transpose ys)) i) ys)

o2 :: (Int, Int) -> Int
o2 (ones, zero)
  | ones > zero = 1
  | zero > ones = 0
  | ones == zero = 1
  | otherwise = error "Bad input"

co2 :: (Int, Int) -> Int
co2 (ones, zero)
  | ones < zero = 1
  | zero < ones = 0
  | ones == zero = 0
  | otherwise = error "bad input"

bit :: ((Int, Int) -> Int) -> Int -> [BinaryNumber] -> Int
bit f 0 (x : _) = count x & f
bit f i (_ : xs) = bit f (i -1) xs
bit _ _ [] = error "bad input"

select :: Int -> Int -> BinaryNumber -> Bool
select want 0 (x : _) = x == want
select want i (_ : xs) = select want (i -1) xs
select _ _ [] = error "bad input"
