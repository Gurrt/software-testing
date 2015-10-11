module Lab6 where

import System.CPUTime
import Text.Printf
import System.Random
import Control.Monad

import qualified Lecture6 as L

-- Exercise 1

exM :: Integer -> Integer -> Integer -> Integer
exM _ 0 _ = 1
exM x y m | even y = L.multM e e m
          | otherwise = L.multM x (L.multM e e m) m
  where e = exM x (y `div` 2) m


-- Exercise 2

-- Exercise 3

-- Exercise 4 Fermat

-- Exercise 5


-- Exercise 6 Miller-Rabin

testMR :: Int -> [Integer] -> IO ()
testMR k (p:ps) = do
  r <- L.primeMR k p
  when r $ print(show p ++ ", Miller-Rabin: " ++ show r)
  testMR k ps

-- Test: testMR 1 carmichael, will take forever
-- Test: testMR 1 100264053529 (true)
-- Alternative: primeMR 1 294409 (False)

-- Mersenne

mersennePrimes :: Integer -> IO ()
mersennePrimes p = do
  print(show p)
  let p1 = (2^p - 1) in
    do
      r <- L.primeMR 5 p1
      when r $ mersennePrimes p1

--Test : mersennePrimes 2
--Test : mersennePrimes 5
--Test : mersennePrimes m3
-- "31"
-- "2147483647" (= m8: 2^31-1)


-- Exercise 7
