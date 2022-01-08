module Main where

import qualified AOC21_01
import qualified AOC21_02
import qualified AOC21_03
import qualified AOC21_04
import qualified AOC21_05
import qualified AOC21_06
import qualified AOC21_07
import qualified AOC21_08
import qualified AOC21_09
import Data.Functor ((<&>))
import System.Environment (getArgs)
import Text.Printf (printf)

readInput :: FilePath -> IO [String]
readInput file =
  readFile file
    <&> lines
    <&> filter (/= "")

getInput :: [String] -> IO [String]
getInput [src, day, _] = readInput (printf "_input/%s/%02d" src (read day :: Int))
getInput _ = error "Problem does not have any input?"

problem :: [String] -> [String] -> IO ()
problem ["aoc21", "1", "1"] = AOC21_01.p1
problem ["aoc21", "1", "2"] = AOC21_01.p2
problem ["aoc21", "2", "1"] = AOC21_02.p1
problem ["aoc21", "2", "2"] = AOC21_02.p2
problem ["aoc21", "3", "1"] = AOC21_03.p1
problem ["aoc21", "3", "2"] = AOC21_03.p2
problem ["aoc21", "4", "1"] = AOC21_04.p1
problem ["aoc21", "4", "2"] = AOC21_04.p2
problem ["aoc21", "5", "1"] = AOC21_05.p1
problem ["aoc21", "5", "2"] = AOC21_05.p2
problem ["aoc21", "6", "1"] = AOC21_06.p1
problem ["aoc21", "6", "2"] = AOC21_06.p2
problem ["aoc21", "7", "1"] = AOC21_07.p1
problem ["aoc21", "7", "2"] = AOC21_07.p2
problem ["aoc21", "8", "1"] = AOC21_08.p1
problem ["aoc21", "8", "2"] = AOC21_08.p2
problem ["aoc21", "9", "1"] = AOC21_09.p1
problem ["aoc21", "9", "2"] = AOC21_09.p2
problem _ = error "Problem not found!"

main :: IO ()
main = do
  args <- getArgs
  input <- getInput args
  problem args input
