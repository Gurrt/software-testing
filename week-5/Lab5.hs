module Lab5 where

import Data.List
import System.Random
import qualified Lecture5 as L
import qualified Exercise1 as E1
import qualified Exercise2 as E2

-- Exercise 1
-- Time spent: 3 hours

--Grid definitions are the same in all files but for the sake of compatibility E1 is used.
nrcGrid :: E1.Grid
nrcGrid = [[0,0,0,3,0,0,0,0,0],
           [0,0,0,7,0,0,3,0,0],
           [2,0,0,0,0,0,0,0,8],
           [0,0,6,0,0,5,0,0,0],
           [0,9,1,6,0,0,0,0,0],
           [3,0,0,0,7,1,2,0,0],
           [0,0,0,0,0,0,0,3,1],
           [0,8,0,0,4,0,0,0,0],
           [0,0,2,0,0,0,0,0,0]]

showNrcGrid:: IO()
showNrcGrid = E1.showGrid nrcGrid

-- "E1.showGrid nrcGrid" produces the output below:
-- +---------+----------+----------+
-- |         | 3         |         |
-- |   +-----|---+   +---|-----+   |
-- |   |     | 7 |   |   | 3   |   |
-- | 2 |     |   |   |   |     | 8 |
-- +---------+----------+----------+
-- |   |   6 |   |   | 5 |     |   |
-- |   +-----|---+   +---|-----+   |
-- |     9 1 | 6         |         |
-- |   +-----|---+   +---|-----+   |
-- | 3 |     |   | 7 | 1 | 2   |   |
-- +---------+----------+----------+
-- |   |     |   |   |   |   3 | 1 |
-- |   | 8   |   | 4 |   |     |   |
-- |   +-----|---+   +---|-----+   |
-- |       2 |           |         |
-- +---------+----------+----------+
solveNrcGrid:: IO [()]
solveNrcGrid = E1.solveAndShow nrcGrid

-- "E1.solveAndShow nrcGrid" produces the output below:
-- +---------+----------+----------+
-- | 4   7 8 | 3   9   2 | 6 1   5 |
-- |   +-----|---+   +---|-----+   |
-- | 6 | 1 9 | 7 | 5 | 8 | 3 2 | 4 |
-- | 2 | 3 5 | 4 | 1 | 6 | 9 7 | 8 |
-- +---------+----------+----------+
-- | 7 | 2 6 | 8 | 3 | 5 | 1 4 | 9 |
-- |   +-----|---+   +---|-----+   |
-- | 8   9 1 | 6   2   4 | 7 5   3 |
-- |   +-----|---+   +---|-----+   |
-- | 3 | 5 4 | 9 | 7 | 1 | 2 8 | 6 |
-- +---------+----------+----------+
-- | 5 | 6 7 | 2 | 8 | 9 | 4 3 | 1 |
-- | 9 | 8 3 | 1 | 4 | 7 | 5 6 | 2 |
-- |   +-----|---+   +---|-----+   |
-- | 1   4 2 | 5   6   3 | 8 9   7 |
-- +---------+----------+----------+

-- Exercise 2
-- Time spent: 2 hours
--
-- In the chosen solutions the solution of exercise 1 bares striking resemblance to the solution of exercise 2.
-- However the solution in exercise 2 allows for the removal of several methods making the processing of constraints
-- uniform.
-- Injectives still have to be applied to seperate block-definitions as in exercise 1 as the definition of a multitide
-- of blocks doesn't allow for extensibility.
--
-- Conclusion: In the chosen solutions adding more constraints isn't easier or harder. It is however much clearer what
-- certain functions do, due to increased readability after proposed refactor.
-- It should be possible to improve the code even further. However a choice was made to go with the solutions that solved
-- the nrcGrid sudoku and improve upon the code if time would allow us to do so.

showGridE2:: IO()
showGridE2 = E2.showGrid nrcGrid

solveGridE2:: IO [()]
solveGridE2 = E2.solveAndShow nrcGrid

-- Exercise 3

-- Exercise 4

-- Exercise 5
-- By adding the constraints in the freeAtPos in ex1 the automatic generator automatically takes the new NRC constraint into account
-- Time taken programming : 0 hours (More than ex2)
-- Time taken to figure this out : Longer than I'll admit.

generateRandomNRCGrid :: IO ()
generateRandomNRCGrid = E1.main
