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
-- Test: testMR 1 (take 1000 carmichael) 118901521 Miller-Rabin:True, and probably many more but my processor fails.
-- Conclusion: testER uses an iterator int k, and list of Carmichael numbers to test. Our test isn't consistent 
-- enough to write a solid conclusion, but they are hard to find and this fact make presume that using carmichael 
-- numbers the MR test is more difficult to fool.


-- Exercise 6 Mersenne

mersnPrimes :: Integer -> IO ()
mersnPrimes p = do
  print(show p)
  let p1 = (2^p - 1) in
    do
      r <- L.primeMR 5 p1
      when r $ mersnPrimes p1

--Test : mersnPrimes 5
--Test : mersnPrimes m3 "2147483647" (= m8: 2^31-1)
--Conclusion: mersnPrimes takes a p (prime number) and check in primeMR searching for similarity.  
--According with https://en.wikipedia.org/wiki/Mersenne_prime not all the numbers has pass the check are genuine Mersenne primes.




-- Exercise 7
