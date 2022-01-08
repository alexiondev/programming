-- https://adventofcode.com/2021/day/8
module AOC21_08 where

import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import Data.List
import Data.Maybe (fromJust)

---------------
-- Problem 1 --
---------------
p1 :: [String] -> IO ()
p1 input =
  fmap (fmap length . (drop 11 . words)) input
    & foldl' (++) []
    & filter (\x -> x == 2 || x == 4 || x == 3 || x == 7)
    & length
    & print

---------------
-- Problem 2 --
---------------
p2 :: [String] -> IO ()
p2 input =
  parse input
    & fmap p2'
    & sum
    & print

p2' :: ([String], [String]) -> Int
p2' (input, output) =
  fmap (fromJust . flip elemIndex (digits input)) output
    & foldl' (\n i -> n * 10 + i) 0

parse :: [String] -> [([String], [String])]
parse = fmap ((\x -> (x, drop 10 x)) . fmap sort . filter (/= "|") . words)

digits :: [String] -> [String]
digits xs = ls
  where
    ls = sort <$> [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9]

    six = f 6 xs
    five = f 5 xs

    p0 = head $ (six `has` p1) \\ [p9]
    p1 = head $ f 2 xs
    p2 = head $ five \\ [p3, p5]
    p3 = head $ five `has` p1
    p4 = head $ f 4 xs
    p5 = p6 \\ (p6 \\ p9)
    p6 = head $ six \\ [p0, p9]
    p7 = head $ f 3 xs
    p8 = head $ f 7 xs
    p9 = head $ six `has` p4

f :: Int -> [String] -> [String]
f n = filter ((== n) . length)

has :: [String] -> String -> [String]
has xs s = filter (\x -> foldl' (\b c -> b && c `elem` x) True s) xs
