module Lab5 where

import Data.List
import System.Random
import qualified Lecture5 as L
import qualified Exercise1 as E1
import qualified Exercise2 as E2

-- Exercise 1
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

showGridE2:: IO()
showGridE2 = E2.showGrid nrcGrid

solveGridE2:: IO [()]
solveGridE2 = E2.solveAndShow nrcGrid

-- Exercise 3

-- Exercise 4

-- Exercise 5
