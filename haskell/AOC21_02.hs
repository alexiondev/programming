-- https://adventofcode.com/2021/day/2
module AOC21_02 where

import Data.Foldable (Foldable (foldl'))
import Data.Function ((&))

-------------
-- Utility --
-------------
data Step
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

parse :: [String] -> [Step]
parse = fmap (parseStep . words)

parseStep :: [String] -> Step
parseStep xs = case xs of
  ["forward", n] -> Forward (read n)
  ["down", n] -> Down (read n)
  ["up", n] -> Up (read n)
  _ -> error "Bad input"

---------------
-- Problem 1 --
---------------
p1 :: [String] -> IO ()
p1 input =
  parse input
    & p1'
    & print

p1' :: [Step] -> Int
p1' xs =
  foldl' move (0, 0) xs
    & uncurry (*)

move :: (Int, Int) -> Step -> (Int, Int)
move (h, d) (Forward n) = (h + n, d)
move (h, d) (Down n) = (h, d + n)
move (h, d) (Up n) = (h, d - n)

---------------
-- Problem 2 --
---------------
p2 :: [String] -> IO ()
p2 input =
  parse input
    & p2'
    & print

p2' :: [Step] -> Int
p2' xs =
  foldl' move2 (0, 0, 0) xs
    & (\(h, d, _) -> h * d)

move2 :: (Int, Int, Int) -> Step -> (Int, Int, Int)
move2 (h, d, a) (Forward n) = (h + n, d + a * n, a)
move2 (h, d, a) (Down n) = (h, d, a + n)
move2 (h, d, a) (Up n) = (h, d, a - n)
