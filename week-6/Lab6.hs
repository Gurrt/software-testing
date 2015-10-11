module Lab6 where

import qualified Lecture6 as L

-- Exercise 1

-- Exercise 2

-- Exercise 3

-- Exercise 4

-- Exercise 5
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) | 
        k <- [2..], 
        L.isPrime (6*k+1), 
        L.isPrime (12*k+1), 
        L.isPrime (18*k+1) ]
-- Exercise 6

-- Exercise 7
