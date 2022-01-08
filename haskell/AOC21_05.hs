-- https://adventofcode.com/2021/day/5
module AOC21_05 where

import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M

----------------
-- Data Types --
----------------

type Point = (Int, Int)

type Line = (Point, Point)

parse :: [String] -> Line
parse [p1, _, p2] = (coords p1, coords p2)
  where
    coords p = read ("(" <> p <> ")") :: Point
parse _ = error "bad input"

---------------
-- Problem 1 --
---------------
p1 :: [String] -> IO ()
p1 input =
  fmap (parse . words) input
    & filter (not . diagonal)
    & compute
    & print

diagonal :: Line -> Bool
diagonal ((x1, y1), (x2, y2)) = x1 /= x2 && y1 /= y2

compute :: [Line] -> Int
compute lines =
  fmap points lines
    & foldl' add M.empty
    & M.elems
    & filter (> 1)
    & length

points :: Line -> [Point]
points ((x1, y1), (x2, y2)) = zip [x1, x1 + dx .. x2] [y1, y1 + dy .. y2]
  where
    dx = d x1 x2
    dy = d y1 y2
    d a b
      | a == b = 0
      | a < b = 1
      | a > b = -1
      | otherwise = error "¯\\_(ツ)_/¯"

add :: Map Point Int -> [Point] -> Map Point Int
add m [] = m
add m (p : ps) = add (M.alter f p m) ps
  where
    f Nothing = Just 1
    f (Just n) = Just (n + 1)

---------------
-- Problem 2 --
---------------
p2 :: [String] -> IO ()
p2 input =
  fmap (parse . words) input
    & compute
    & print
