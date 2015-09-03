{-# OPTIONS_GHC -Wall #-}
module HW01 where

--Carla

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit 0 = 0
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit 0 = 0
dropLastDigit n = (n - (lastDigit n)) `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n | n < 0 = []
toRevDigits n = lastDigit n:(toRevDigits (dropLastDigit n))

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:[]) = [x,y*2]
doubleEveryOther (x:y:xs) = x:y*2:doubleEveryOther xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [x] = x
sumDigits (x:xs) = lastDigit x + (sumDigits [dropLastDigit x]) + sumDigits xs


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = sumDigits (doubleEveryOther (toRevDigits x)) `mod` 10 == 0 
