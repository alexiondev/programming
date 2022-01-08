-- https://adventofcode.com/2021/day/4
module AOC21_04 where

import Control.Concurrent (yield)
import Data.Function ((&))
import Data.List (find, foldl', transpose)

type Board = [[Maybe Int]]

---------------
-- Problem 1 --
---------------
p1 :: [String] -> IO ()
p1 input =
  parseInput input
    & p1' Nothing
    & print

parseInput :: [String] -> ([Int], [Board])
parseInput (x : xs) = (read ("[" <> x <> "]"), parseBoard xs')
  where
    xs' = (fmap (fmap read . words) xs :: [[Int]]) & fmap (fmap Just)
parseInput _ = error "bad input"

parseBoard :: [[Maybe Int]] -> [Board]
parseBoard [] = []
parseBoard xs = take 5 xs : parseBoard (drop 5 xs)

p1' :: Maybe Int -> ([Int], [Board]) -> Int
p1' (Just n) (_, bs) = score (find hasWon bs) * n
p1' Nothing (q, bs) = p1' won (tail q, bs')
  where
    n = head q
    bs' = fmap (markBoard n) bs
    won = if any hasWon bs' then Just n else Nothing

mark :: Int -> Maybe Int -> Maybe Int
mark x Nothing = Nothing
mark x (Just y)
  | x == y = Nothing
  | otherwise = Just y

markBoard :: Int -> Board -> Board
markBoard x = fmap (fmap (mark x))

hasWon :: Board -> Bool
hasWon b = won b || won (transpose b)
  where
    won = any (all (== Nothing))

score :: Maybe Board -> Int
score Nothing = 0
score (Just b) = foldl' (foldl' (+)) 0 b'
  where
    b' = fmap (fmap x) b
    x (Just n) = n
    x Nothing = 0

---------------
-- Problem 2 --
---------------
p2 :: [String] -> IO ()
p2 input =
  parseInput input
    & p2' Nothing
    & print

p2' :: Maybe Int -> ([Int], [Board]) -> Int
p2' (Just n) (_, [b]) = score (Just b) * n
p2' (Just _) (q, bs) = p2' Nothing (q, filter (not . hasWon) bs)
p2' Nothing (q, bs) = p2' won (tail q, bs')
  where
    n = head q
    bs' = fmap (markBoard n) bs
    won = if any hasWon bs' then Just n else Nothing
