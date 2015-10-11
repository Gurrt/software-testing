module Lab6 where

import System.CPUTime
import System.Random
import Control.Monad

import qualified Lecture6 as L

-- Exercise 1

-- Catch some edge cases and then use the exMsq to calculate squared modulo
exM :: Integer -> Integer -> Integer -> Integer
exM b e m
    | e < 0 = 0
    | e == 0 = 1 `mod` m
    | otherwise = exMsq b e m 1

-- Squaring
-- Usage: exMsq b e m 1
exMsq :: Integer -> Integer -> Integer -> Integer -> Integer
exMsq _ 0 _ r = r
exMsq b e m r
  | odd e = exMsq b (e-1) m (mod (r*b) m)
  | otherwise = exMsq (mod (b*b) m) (div e 2) m r

-- Memory-efficient method
exMmem :: Integer -> Integer -> Integer -> Integer
exMmem b 1 m = mod b m
exMmem b e m = mod (b* exMmem b (e-1) m) m

-- Exercise 2
-- Usage: randomFaster minRange maxRange
testEx2 :: Integer -> Integer -> IO()
testEx2 = randomFaster

randomFaster:: Integer -> Integer -> IO()
randomFaster x y = do
         g <- newStdGen
         let (b, newGen) = randomR (x,y) g
         let (e, newGen') = randomR (x,y) newGen
         let (m, _) = randomR (x,y) newGen'
         faster b e m


faster:: Integer -> Integer -> Integer -> IO ()
faster b e m = do
                    print ("Comparison between the Ex1 method and lecture's method"::String)
                    print ("Results are expressed in pico seconds."::String)
                    x <- getDiffMsq b e m
                    y <- getDiffDefault b e m
                    r <- compareDiff x y
                    if r then print ("--> Squaring method is faster"::String)
                      else print ("--> Lecture's method is faster"::String)

-- Difference is in pico seconds
compareDiff:: Integer -> Integer -> IO Bool
compareDiff x y = return (x < y)

getDiffMsq:: Integer -> Integer -> Integer -> IO Integer
getDiffMsq b e m = do
            start <- getCPUTime
            print ("Square method result: " ++ show (exMsq b e m 1))
            end   <- getCPUTime
            let diff = fromIntegral (end - start)
            print ("- Execution time: " ++ show diff)
            return diff

getDiffDefault:: Integer -> Integer -> Integer -> IO Integer
getDiffDefault b e m = do
            start <- getCPUTime
            print ("Lecture method result: " ++ show (L.expM b e m))
            end   <- getCPUTime
            let diff = fromIntegral (end - start)
            print ("- Execution time: " ++ show diff)
            return diff

-- Exercise 3

composites :: [Integer]
composites = 4 : filter (not . L.isPrime) [5..]

-- Exercise 4

-- Lowest found values for foolFermat k
--   (k = 1) lowest found: 4
--   (k = 2) lowest found: 4
--   (k = 3) lowest found: 15
--   (k = 4) lowest found: 4
--
-- If you increase k the probability of fooling the test becomes smaller due to
-- a larger number of random samples, However for low numbers the unique
-- possible samples are quite small, for example for 4 there are only 3 samples.
-- Namely : 1^3 mod 4, 2^3 mod 4 and 3^3 mod 4. One of which returns 1, which
-- means that with k = 4 the chance of fooling the test is 1/3 ^ 4 == 1.23%.

testEx4 :: Int -> IO()
testEx4 k = do
              f <- foolFermat k
              print ("(k = " ++ show k ++ ") lowest found: " ++ show f)

foolFermat :: Int -> IO Integer
foolFermat k  = lowestFermatFooler k composites

lowestFermatFooler :: Int -> [Integer] -> IO Integer
lowestFermatFooler _ [] = return 0
lowestFermatFooler k (x:xs) = do
    result <- L.prime_tests_F k x
    if result then return x else lowestFermatFooler k xs

-- Exercise 5

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
       k <- [2..],
       L.isPrime (6*k+1),
       L.isPrime (12*k+1),
       L.isPrime (18*k+1) ]

testFermat :: Int -> [Integer] -> [IO Bool]
testFermat k = map (L.prime_tests_F k)

testFermatWithCarmichael :: Int -> [IO Bool]
testFermatWithCarmichael n = testFermat 4 (take n carmichael)

-- Exercise 6

-- Miller-Rabin
testMR :: Int -> [Integer] -> IO ()
testMR k (p:ps) = do
                      r <- L.primeMR k p
                      when r $ print(show p ++ ", Miller-Rabin: " ++ show r)
                      testMR k ps

-- Test: testMR 1 carmichael, will take forever
-- Test: testMR 1 (take 1000 carmichael) 118901521 Miller-Rabin:True, and
-- probably many more but my processor fails.
-- Conclusion: testER uses an iterator int k, and list of Carmichael numbers to
-- test. Our test isn't consistent enough to write a solid conclusion, but they
-- are hard to find and this fact make presume that using carmichael numbers the
-- MR test is more difficult to fool.

-- Mersenne
mersnPrimes :: Integer -> IO ()
mersnPrimes p = do
                  print(show p)
                  let p1 = (2^p - 1) in
                    do
                      r <- L.primeMR 5 p1
                      when r $ mersnPrimes p1

--Test : mersnPrimes 5
--Test : mersnPrimes m3 "2147483647" (= m8: 2^31-1)
--Conclusion: mersnPrimes takes a p (prime number) and check in primeMR
-- searching for similarity. According with
-- https://en.wikipedia.org/wiki/Mersenne_prime not all the numbers has pass the
-- check are genuine Mersenne primes.

-- Exercise 7
