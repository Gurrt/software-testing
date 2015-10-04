module Lab5 where

import Data.List
import System.Random
import Lecture5

-- Exercise 1

-- Exercise 2

-- Exercise 3
-- Starts 19:00 (+15')
-- Start at 21:15 (+15')
-- Start at 20:00

-- A sudoku problem is minimal if
-- 1) it admits a unique solution
-- 2) every other sudoku substracting one value accept more than one solution.
-- We can split this problem into checking both conditions independetly.

bfsSearch :: (node -> [node]) -> (node -> Bool) -> [node] -> [node]
bfsSearch _ _ [] = []
bfsSearch children goal (x:xs)
  | goal x    = x : bfsSearch children goal xs
  | otherwise = bfsSearch children goal (xs ++ children x)

solveBfsNs :: [Node] -> [Node]
solveBfsNs = bfsSearch succNode solved

countSolutions :: Grid -> Int
countSolutions gr =  length (solveBfsNs (initNode gr))

admitsOneSolution :: Grid -> Bool
admitsOneSolution gr = countSolutions gr == 1

-- Exercise 4

-- Exercise 5
