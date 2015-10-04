module Lab5 where

import Data.List
import System.Random
import Lecture5

-- Exercise 1

-- Exercise 2

-- Exercise 3
-- Time spent: 5 hours. Monads are really time consuming.

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
-- the solution, we will apply take 2 on the search so the lazy evaluation
-- will only calculate 2 of the solutions and therefore, it will end up faster.
-- The existence of two solutions is enough to invalidate the property.
countSolutions :: Grid -> Int
countSolutions gr =  length (take 2 (solveBfsNs (initNode gr)))

admitsOneSolution :: Grid -> Bool
admitsOneSolution gr = countSolutions gr == 1

-- Property 2: every other sudoku substracting one value accept more than one solution
-- To check this property, we need iterate over all the values of a sudoku and change
-- to 0 one by one. In other words, we generate up to 81 different sudokus from
-- a given one and check if all of them have more than one solution. In case any of
-- the generated sudokus have more than one solution, this property is false.

-- The first thing we need to be able to do is to change a value in the grid.
-- A Grid is just a double list of values ([[Value]]) and we are changing
-- the value to 0, which is always a correct value. The easiest way to approach
-- this problem is to create a function that change a value to 0 given the
-- coordinates.
-- Partially based in this solution http://stackoverflow.com/a/15530742, we create
-- the following functions:

-- Given a list and an index, changes the value in the index to 0.
replaceInCol :: Int -> [Value] -> [Value]
replaceInCol 0 row = row
replaceInCol n row = fst splitCol ++ ((0::Value):tail (snd splitCol))
    where
        splitCol = splitAt (n-1) row

-- Given a Grid and a pair of coordinates, applies the replaceInCol function to
-- the selected row.
replaceInGrid :: (Int,Int) -> Grid -> Grid
replaceInGrid (0,_) gr = gr
replaceInGrid (x,y) gr = fst splitRow ++ (replaceInCol y (head (snd splitRow)):tail (snd splitRow))
    where
        splitRow = splitAt (x-1) gr

-- Apply replaceInGrid to every position in the given grid, generating up to 81
-- different sudokus. In order to reduce the amount of sudokus generate, we
-- introduce an optimisation of skipping the substitutions that are the same
-- as the original.
substitutions :: Grid -> [Grid]
substitutions gr = [replaceInGrid (x,y) gr | x <- [1..9], y <- [1..9], replaceInGrid (x,y) gr /= gr]

-- Finally, we only need to check that all the substitutions have more than one
-- solution. We use the negation of the function admitsOneSolution to take
-- advantage of the optimisations we did. We also use the combination of all and
-- the negation because the lazy evalution will stop as soon as it finds a substitution
-- with 2 solutions.
subsHaveMoreThanOneSolution :: Grid -> Bool
subsHaveMoreThanOneSolution gr = all (not.admitsOneSolution) (substitutions gr)

-- Finally, we combine both properties in one function. The properties are ordered
-- based on the computational cost of execution so if it is not minimal, it fails
-- as soon as possible. Still, it is a rather expensive property to check because
-- of all the cases that need to be checked.
isMinimal :: Grid -> Bool
isMinimal gr = admitsOneSolution gr && subsHaveMoreThanOneSolution gr

-- Once defined the property, we will use it to check if the sudokus generated
-- by the sudoku generator fulfill it. To do this, we are not going to use
-- QuickCheck for the simple reason that QuickCheck would generate random
-- sudokus and what we want is to test the generator, not the property.

-- The first thing is to obtain a monadic version of the property. This four
-- simple lines took much time to discover due to the obscure nature of the monads
-- (seriosly, it is really hard to find a good documentation about them).
-- Basically, it just return isMinimal with an output of type IO Bool instead of Bool
mIsMinimal :: Grid -> IO Bool
mIsMinimal gr = if isMinimal gr
                then return True
                else return False

-- Once we have a monadic version of the property, we rewrite the generator function
-- (Lecture5.main) adding the return of the test. This will generate a minimal sudoku
-- and test it. We define two versions of the function, one showing the sudokus
-- generated and other without. The purpose of the second function is to use it
-- in a test generator like in week-3. The unitary checks can be executed with:
--  testShowIsMinimal
-- or
--  testIsMinimal
testShowIsMinimal :: IO Bool
testShowIsMinimal = do [r] <- rsolveNs [emptyN]
                       showNode r
                       s  <- genProblem r
                       showNode s
                       mIsMinimal (sud2grid(fst s))

testIsMinimal :: IO Bool
testIsMinimal = do [r] <- rsolveNs [emptyN]
                   s  <- genProblem r
                   mIsMinimal (sud2grid(fst s))

-- Executes testIsMinimal n times. At the end, it will show the amount of
-- valid tests executed. Due to the computational time that requires to generate
-- the sudokus and to test them, be careful with the n chosen. In order to execute
-- this function:
--   testEx3 n
testEx3 :: Int -> IO ()
testEx3 n = executeTest n n
    where
        executeTest x 0 = print (show x ++ " Valid")
        executeTest x y = do
                            print ("Executing test number " ++ show y)
                            t <- testIsMinimal
                            if t
                                then executeTest x (y-1)
                                else error ("Test failed on " ++ show y)

-- Exercise 4

-- Exercise 5
