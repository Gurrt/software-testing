module Lab6 where

import qualified Lecture6 as L

-- Exercise 1

-- Memory-efficient method
exMmem :: Integer -> Integer -> Integer -> Integer
exMmem b 1 m = rem (b*1) m
exMmem b e m = rem (b* (exMmem b (e-1) m) ) m

-- Squaring (Does not work yet)
-- exMsq :: Integer -> Integer -> Integer -> Integer
-- exMsq b e m | odd e = (exMsq b e-1 m) * (rem b m)
--            | otherwise = exMsq (b*b) e-1 m

-- Exercise 2

-- Exercise 3

-- Exercise 4

-- Exercise 5

-- Exercise 6

-- Exercise 7