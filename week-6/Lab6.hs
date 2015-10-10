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
			
-- Exercise 3

-- Exercise 4

-- Exercise 5

-- Exercise 6

-- Exercise 7