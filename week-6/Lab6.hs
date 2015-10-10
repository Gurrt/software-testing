module Lab6 where

import System.CPUTime
import Text.Printf
import Test.QuickCheck

import qualified Lecture6 as L

-- Exercise 1

-- Catch some edge cases and then use the exMsq to calculate squared modulo
exM :: Integer -> Integer -> Integer -> Integer
exM b e m 
    | e < 0 = 0
    | e == 0 = 1 `mod` m
    | otherwise = exMsq b e m 1
    
-- Memory-efficient method
exMmem :: Integer -> Integer -> Integer -> Integer
exMmem b 1 m = mod (b*1) m
exMmem b e m = mod (b* (exMmem b (e-1) m) ) m

-- Squaring
-- Usage: exMsq b e m 1
exMsq :: Integer -> Integer -> Integer -> Integer -> Integer
exMsq b 0 m r = r
exMsq b e m r | odd e = exMsq b (e-1) m (mod (r*b) m)
exMsq b e m r = exMsq (mod (b*b) m) (div e 2) m r

-- Exercise 2
-- Does not work yet. Initial setup.
{-
test:: IO()
test = do
        let x = getDiffMsq 4 10 497
        let y = getDiffDefault 4 10 497
        x y >>= compareDiff

compareDiff:: Double -> Double -> Bool
compareDiff x y = x < y

getDiffMsq:: Integer -> Integer -> Integer -> IO Double
getDiffMsq b e m = do
            start <- getCPUTime
            let result = exMsq b e m 1
            end   <- getCPUTime
            let diff = fromIntegral (end - start) / (10^9)
            return (diff :: Double)

getDiffDefault:: Integer -> Integer -> Integer -> IO Double
getDiffDefault b e m = do
            start <- getCPUTime
            let result = L.expM b e m
            end   <- getCPUTime
            let diff = fromIntegral (end - start) / (10^9)
            return (diff :: Double)
-}
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
    This means that due to the high amount of false positives 4 is probably the easiest number to fool the fermat test with, regardless of the amount of samples.
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