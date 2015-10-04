module Lab5 where

import Data.List
import System.CPUTime
import System.Random
import Control.Exception
import Text.Printf
import Control.Exception
import System.CPUTime
import Text.Printf
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

showGridE1:: IO()
showGridE1 = E1.showGrid nrcGrid

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
solveGridE1:: IO [()]
solveGridE1 = E1.solveAndShow nrcGrid

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
-- Note: Even though, it isn't explicitly mentioned that we should, we also included the NRC constraint in this code.

showGridE2:: IO()
showGridE2 = E2.showGrid nrcGrid

solveGridE2:: IO [()]
solveGridE2 = E2.solveAndShow nrcGrid

-- This function compares the two functions once, hardly a statistic proof of difference but more stable in execution time than
-- the compareRefactoredToNormal implementation.
compareE1VsE2:: IO ()
compareE1VsE2 = do
                    start1 <- getCPUTime
                    solveGridE1
                    end1   <- getCPUTime
                    let diff1 = (fromIntegral (end1 - start1)) / (10^9)
                    printf "Computation time: %0.3f millisec\n" (diff1 :: Double)
                    start2 <- getCPUTime
                    solveGridE2
                    end2   <- getCPUTime
                    let diff2 = (fromIntegral (end2 - start2)) / (10^9)
                    printf "Computation time: %0.3f millisec\n" (diff2 :: Double)
                    printf "Computation time difference: %0.3f millisec\n" (diff1 - diff2 :: Double)

-- Unfortunately haskell does some funky caching resulting resulting in different results. However results do not differ
-- accross several manual runs.
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
-- Computation time: 31.250 millisec
-- +---------+-----------+---------+
-- | 4   7 8 | 3   9   2 | 6 1   5 |
-- |   +-----|---+   +---|-----+   |
-- | 6 | 1 9 | 7 | 5 | 8 | 3 2 | 4 |
-- | 2 | 3 5 | 4 | 1 | 6 | 9 7 | 8 |
-- +---------+-----------+---------+
-- | 7 | 2 6 | 8 | 3 | 5 | 1 4 | 9 |
-- |   +-----|---+   +---|-----+   |
-- | 8   9 1 | 6   2   4 | 7 5   3 |
-- |   +-----|---+   +---|-----+   |
-- | 3 | 5 4 | 9 | 7 | 1 | 2 8 | 6 |
-- +---------+-----------+---------+
-- | 5 | 6 7 | 2 | 8 | 9 | 4 3 | 1 |
-- | 9 | 8 3 | 1 | 4 | 7 | 5 6 | 2 |
-- |   +-----|---+   +---|-----+   |
-- | 1   4 2 | 5   6   3 | 8 9   7 |
-- +---------+-----------+---------+
-- Computation time: 46.875 millisec
-- Computation time difference: -15.625 millisec

-- Uses seq to force evaluation of solveEmpty event hough we're not doing anything with it, which is what we want in the case of testing efficiency.
-- The freeAtPos function is also used in generating sudoku answers so speed differences should show up here too, however
-- the random algorithm used to solve the empty sudoku has wildly varying execution times. This means that the results from this test are absolutely worthless,
-- because they vary so much that we cannot say anything meaningful about the difference in execution time. Due to the whole IO monad I could not get a 100 time
-- solving of the same Sudoku to work, so this is the best we could come up with with regards to 'random' testing.
compareRefactoredToNormal = do
    start <- getCPUTime
    1 `seq` solveManyEmpty 100
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "100 Normal Generations Took: %0.3f sec\n" (diff :: Double)
    start2 <- getCPUTime   
    1 `seq` solveManyEmptyRefac 100
    end2 <- getCPUTime
    let diff2 = (fromIntegral (end2- start2)) / (10^12)
    printf "100 Refactored Generations Took: %0.3f sec\n" (diff2 :: Double)
    
-- getRandomInt is purely here so we can return something
solveManyEmpty :: Int -> IO Int
solveManyEmpty 0 = L.getRandomInt 0
solveManyEmpty x = do 
    y <- 1 `seq` solveEmpty     
    solveManyEmpty (x-1)

solveManyEmptyRefac :: Int -> IO Int
solveManyEmptyRefac 0 = L.getRandomInt 0
solveManyEmptyRefac x = do 
    1 `seq` solveEmptyRefac     
    solveManyEmptyRefac (x-1)
    
solveEmpty :: IO [E1.Node]
solveEmpty = E1.rsolveNs [E1.emptyN]

solveEmptyRefac :: IO [E2.Node]
solveEmptyRefac = E2.rsolveNs [E2.emptyN]

-- Exercise 3

-- Exercise 4

-- Exercise 5
-- By adding the constraints in the freeAtPos in ex1 the automatic generator automatically takes the new NRC constraint into account
-- Time taken programming : 0 hours (More than ex2)
-- Time taken to figure this out : Longer than I'll admit.

generateRandomNRCGrid :: IO ()
generateRandomNRCGrid = E1.main
