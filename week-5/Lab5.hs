module Lab5 where

import Data.List
import Data.Char
import Data.Maybe
import System.Random
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
                    let diff1 = fromIntegral (end1 - start1) / (10^9)
                    printf "Computation time: %0.3f millisec\n" (diff1 :: Double)
                    start2 <- getCPUTime
                    solveGridE2
                    end2   <- getCPUTime
                    let diff2 = fromIntegral (end2 - start2) / (10^9)
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
    let diff = fromIntegral (end - start) / (10^12)
    printf "100 Normal Generations Took: %0.3f sec\n" (diff :: Double)
    start2 <- getCPUTime
    1 `seq` solveManyEmptyRefac 100
    end2 <- getCPUTime
    let diff2 = fromIntegral (end2- start2) / (10^12)
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

solveBfsNs :: [L.Node] -> [L.Node]
solveBfsNs = bfsSearch L.succNode L.solved

-- Property 1: It admits only one solution.
-- The following function count the amount of solutions that a grid has in total,
-- using a BFS search. The problem of checking this property is that in case it
-- has many solutions it will take long time to calculate. Trying to optimise
-- the solution, we will apply take 2 on the search so the lazy evaluation
-- will only calculate 2 of the solutions and therefore, it will end up faster.
-- The existence of two solutions is enough to invalidate the property.
countSolutions :: L.Grid -> Int
countSolutions gr =  length (take 2 (solveBfsNs (L.initNode gr)))

admitsOneSolution :: L.Grid -> Bool
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
replaceInCol :: Int -> [L.Value] -> [L.Value]
replaceInCol 0 row = row
replaceInCol n row = fst splitCol ++ ((0::L.Value):tail (snd splitCol))
    where
        splitCol = splitAt (n-1) row

-- Given a Grid and a pair of coordinates, applies the replaceInCol function to
-- the selected row.
replaceInGrid :: (Int,Int) -> L.Grid -> L.Grid
replaceInGrid (0,_) gr = gr
replaceInGrid (x,y) gr = fst splitRow ++ (replaceInCol y (head (snd splitRow)):tail (snd splitRow))
    where
        splitRow = splitAt (x-1) gr

-- Apply replaceInGrid to every position in the given grid, generating up to 81
-- different sudokus. In order to reduce the amount of sudokus generate, we
-- introduce an optimisation of skipping the substitutions that are the same
-- as the original.
substitutions :: L.Grid -> [L.Grid]
substitutions gr = [replaceInGrid (x,y) gr | x <- [1..9], y <- [1..9], replaceInGrid (x,y) gr /= gr]

-- Finally, we only need to check that all the substitutions have more than one
-- solution. We use the negation of the function admitsOneSolution to take
-- advantage of the optimisations we did. We also use the combination of all and
-- the negation because the lazy evalution will stop as soon as it finds a substitution
-- with 2 solutions.
subsHaveMoreThanOneSolution :: L.Grid -> Bool
subsHaveMoreThanOneSolution gr = all (not.admitsOneSolution) (substitutions gr)

-- Finally, we combine both properties in one function. The properties are ordered
-- based on the computational cost of execution so if it is not minimal, it fails
-- as soon as possible. Still, it is a rather expensive property to check because
-- of all the cases that need to be checked.
isMinimal :: L.Grid -> Bool
isMinimal gr = admitsOneSolution gr && subsHaveMoreThanOneSolution gr

-- Once defined the property, we will use it to check if the sudokus generated
-- by the sudoku generator fulfill it. To do this, we are not going to use
-- QuickCheck for the simple reason that QuickCheck would generate random
-- sudokus and what we want is to test the generator, not the property.

-- The first thing is to obtain a monadic version of the property. This four
-- simple lines took much time to discover due to the obscure nature of the monads
-- (seriosly, it is really hard to find a good documentation about them).
-- Basically, it just return isMinimal with an output of type IO Bool instead of Bool
mIsMinimal :: L.Grid -> IO Bool
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
testShowIsMinimal = do [r] <- L.rsolveNs [L.emptyN]
                       L.showNode r
                       s  <- L.genProblem r
                       L.showNode s
                       mIsMinimal (L.sud2grid(fst s))

testIsMinimal :: IO Bool
testIsMinimal = do [r] <- L.rsolveNs [L.emptyN]
                   s  <- L.genProblem r
                   mIsMinimal (L.sud2grid(fst s))

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
-- Time: aprox 7 hours (struggling with Haskell).

-- test: exercise4 "eI"
-- +-------+-------+-------+
-- |x      |      x|      x|
-- |   A   |   B   |   C   |
-- |       |       |       |
-- +-------+-------+-------+
-- |       |       |       |
-- |   D   |   E   |   F   |
-- |x      |      x|      x|
-- +-------+-------+-------+
-- |       |       |       |
-- |  G    |   H   |   I   |
-- |x      |     x |      x|
-- +-------+-------+-------+

delBlockN :: String ->L.Node-> L.Node
delBlockN nameB node = (s, L.constraints s)
  where s = delBlockS nameB (fst node)

delBlockS :: String -> L.Sudoku -> L.Sudoku
delBlockS [] s = s
delBlockS nameB s = foldl delBlock s (take (length nameB) (findBlockCoordinates nameB))

delBlock :: L.Sudoku -> (L.Row,L.Column) ->  L.Sudoku
delBlock s (r,c) (x,y) = foldl L.eraseS s (subBlockPosition s (r,c)) (x,y)

subBlockPosition :: L.Sudoku -> E2.Position -> [E2.Position]
subBlockPosition _ (r, c) = [(r',c') | r' <- L.bl r, c' <- L.bl c]

findBlockCoordinates:: String -> [(L.Row, L.Column)]
findBlockCoordinates = map (\ x -> findBlk (toUpper (head [x])))

findBlk :: Char -> (L.Row, L.Column)
findBlk c = fromJust (lookup c [('A',(1,1)), ('B',(1,6)), ('C', (1,9)),('D', (6,1)),('E', (6,6)),('F', (6,9)),('G', (9,1)), ('H', (9,6)),('I',(9,9))])

minimalize :: L.Node -> [(L.Row, L.Column)] -> L.Node
minimalize n [] = n
minimalize n ((r,c):rcs) | L.uniqueSol n' = minimalize n' rcs
                         | otherwise    = minimalize n  rcs
  where n' = L.eraseN n (r,c)

filledPositions :: L.Sudoku -> [(L.Row, L.Column)]
filledPositions s = [ (r,c) | r <- L.positions,
                              c <- L.positions, s (r,c) /= 0 ]

genProblem :: L.Node -> IO L.Node
genProblem n = do ys <- L.randomize xs
                  return (minimalize n ys)
   where xs = filledPositions (fst n)

-- nameB given letter to choose a block nodes
exercise4 :: String -> IO ()
exercise4 nameB = do
                    [r] <- L.rsolveNs [L.emptyN]
                    L.showNode r
                    let sBlock = delBlockN nameB r
                    s  <- genProblem sBlock
                    L.showNode s

-- Exercise 5
-- By adding the constraints in the freeAtPos in ex1 the automatic generator automatically takes the new NRC constraint into account
-- Time taken programming : 0 hours (More than ex1)
-- Time taken to figure this out : Longer than I'll admit.

generateRandomNRCGrid :: IO ()
generateRandomNRCGrid = E1.main
