module Lab6 where

import qualified Lecture6 as L

-- Exercise 1

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

-- Exercise 3

-- Exercise 4

-- Exercise 5

-- Exercise 6

-- Exercise 7