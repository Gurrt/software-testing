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

-- The first thing we have to do is to generate all possible solutions. For that
-- instead of using a DFS search, we use a BFS search in order to get all possible
-- solutions for the given tree. We will use this search for all the properties
-- in this exercise.
bfsSearch :: (node -> [node]) -> (node -> Bool) -> [node] -> [node]
bfsSearch _ _ [] = []
bfsSearch children goal (x:xs)
  | goal x    = x : bfsSearch children goal xs
  | otherwise = bfsSearch children goal (xs ++ children x)

solveBfsNs :: [Node] -> [Node]
solveBfsNs = bfsSearch succNode solved

-- Property 1: It admits only one solution.
-- The following function count the amount of solutions that a grid has in total,
-- using a BFS search. The problem of checking this property is that in case it
-- has many solutions it will take long time to calculate. Trying to optimise
-- the solution, we will apply take 2 to the search so this way the lazy evaluation
-- will only calculate 2 of the solutions and therefore, it will end up faster.
-- The existence of two solutions is enough to invalidate the property.
countSolutions :: Grid -> Int
countSolutions gr =  length (take 2 (solveBfsNs (initNode gr)))

admitsOneSolution :: Grid -> Bool
admitsOneSolution gr = countSolutions gr == 1

-- Property 2:
replaceInCol :: Int -> [Value] -> [Value]
replaceInCol 0 row = row
replaceInCol n row = fst splitCol ++ ((0::Value):tail (snd splitCol))
    where
        splitCol = splitAt (n-1) row

replaceInGrid :: (Int,Int) -> Grid -> Grid
replaceInGrid (0,_) gr = gr
replaceInGrid (x,y) gr = fst splitRow ++ (replaceInCol y (head (snd splitRow)):tail (snd splitRow))
    where
        splitRow = splitAt (x-1) gr


-- Exercise 4

-- Exercise 5
