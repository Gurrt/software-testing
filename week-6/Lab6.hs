module Lab6 where

import System.CPUTime
import Text.Printf
import System.Random

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
-- Usage: randomFaster minRange maxRange
-- Output: Bool, True = squaring is faster, False = squaring is slower.

randomFaster:: Integer -> Integer -> IO()
randomFaster x y = do
         g <- newStdGen
         let (b, newGen) = randomR (x,y) g
         let (e, newGen') = randomR (x,y) newGen
         let (m, newgen) = randomR (x,y) newGen'
         faster b e m


faster:: Integer -> Integer -> Integer -> IO ()
faster b e m = do
                    x <- getDiffMsq b e m
                    y <- getDiffDefault b e m
                    compareDiff x y

-- Difference is in pico seconds
getDiffMsq:: Integer -> Integer -> Integer -> IO Integer
getDiffMsq b e m = do
            start <- getCPUTime
            print (exMsq b e m 1)
            end   <- getCPUTime
            let diff = fromIntegral (end - start)
            print diff
            return diff

getDiffDefault:: Integer -> Integer -> Integer -> IO Integer
getDiffDefault b e m = do
            start <- getCPUTime
            print (L.expM b e m)
            end   <- getCPUTime
            let diff = fromIntegral (end - start)
            print diff
            return diff

compareDiff:: Integer -> Integer -> IO ()
compareDiff x y = print (x < y)

-- Exercise 3

-- Exercise 4

-- Exercise 5

-- Exercise 6

-- Exercise 7