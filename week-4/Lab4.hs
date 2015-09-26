module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Exercise 1

-- Question 1
-- I was wondering how I should read notion 5 in Theorem 4.38.
-- What does the "["insert-some-set-operations-here"]" mean compared to "("insert-someset-operations-here")" ?
-- Or is this a printing mistake?

-- Question 2
-- I used the term "notion" to address a part of a theorem in the question above. I was wondering if this is correct.
-- The beginning of the chapter briefly address the terms, however it seems not to address this specific question.

-- Question 3
-- In Haskell what is the difference between the usage of "case" and equation guarding?
-- To me the two seem oddly similar.

-- Concerning the logic in chapter 4 I have no specific questions.

-- Time spent: 3 hours.

-- Exercise 2

-- Using the Random class from the Haskell library does not apply to our type Set a.
-- The Random class specifically needs a type of * while Set is of type *->*. This causes clashes in the Random class.
-- I try to write a Wrapper as suggest on some internet fora, however I did not succeed.

exercise2:: Int -> IO()
exercise2 n = do
    seed  <- newStdGen
    let rs = randomlist n seed
    let s = list2set nub(rs) -- Removes the duplicates before converting the list to a set
    print (show s)

randomlist :: Int -> StdGen -> [Int]
randomlist n gen = take n (randoms gen)

-- Exercise 3

-- Exercise 4

-- Exercise 5

-- Exercise 6

-- Exercise 7

-- Exercise 8