module Lab6 where

import qualified Lecture6 as L

-- Exercise 1

exM :: Integer -> Integer -> Integer -> Integer
exM b 1 m = rem (b*1) m
exM b e m = rem (b* (exM b (e-1) m) ) m

-- Exercise 2

-- Exercise 3

-- Exercise 4

-- Exercise 5

-- Exercise 6

-- Exercise 7