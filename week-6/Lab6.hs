module Lab6 where

import System.CPUTime
import Text.Printf
import System.Random
import Test.QuickCheck

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
exMsq b 0 m r = r
exMsq b e m r 
  | odd e = exMsq b (e-1) m (mod (r*b) m)
  | otherwise = exMsq (mod (b*b) m) (div e 2) m r

-- Memory-efficient method
exMmem :: Integer -> Integer -> Integer -> Integer
exMmem b 1 m = mod (b*1) m
exMmem b e m = mod (b* (exMmem b (e-1) m) ) m

-- Exercise 2
-- Usage: randomFaster minRange maxRange
randomFaster:: Integer -> Integer -> IO()
randomFaster x y = do
         g <- newStdGen
         let (b, newGen) = randomR (x,y) g
         let (e, newGen') = randomR (x,y) newGen
         let (m, newgen) = randomR (x,y) newGen'
         faster b e m


faster:: Integer -> Integer -> Integer -> IO ()
faster b e m = do
                    print "Comparison between the Ex1 method and lecture's method"
                    x <- getDiffMsq b e m
                    y <- getDiffDefault b e m
                    r <- compareDiff x y
                    if r then print "--> Squaring method is faster"
                      else print "--> Lecture's method is faster"

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
{-
    Lowest found values for foolFermat k
    (k = 1) lowest found: 4
    (k = 2) lowest found: 4
    (k = 3) lowest found: 15
    (k = 4) lowest found: 4
    
    If you increase k the probability of fooling the test becomes smaller due to a larger number of random samples,
    However for low numbers the unique possible samples are quite small, for example for 4 there are only 3 samples.
    Namely : 1^3 mod 4, 2^3 mod 4 and 3^3 mod 4. One of which returns 1, which means that with k = 4 the chance of fooling
    the test is 1/3 ^ 4 == 1.23%.
-}

foolFermat :: Int -> IO Integer
foolFermat k  = lowestFermatFooler k composites

lowestFermatFooler :: Int -> [Integer] -> IO Integer
lowestFermatFooler k [] = return 0
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
testFermat k [] = []
testFermat k (x:xs) = L.prime_tests_F k x : testFermat k xs
       
testFermatWithCarmichael :: Int -> [IO Bool]
testFermatWithCarmichael n = do
     testFermat 4 (take n carmichael)

-- Exercise 6

-- Exercise 7
