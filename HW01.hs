{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
-- Note : casting (last (show x)) to array to make String from char. -- Ger
lastDigit :: Integer -> Integer
lastDigit x = read [(last (show x))]

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = read (replaceEmptyStringByZero (init (show x)))

replaceEmptyStringByZero :: String -> String
replaceEmptyStringByZero x = if (length x) > 0 then x
                                               else "0"

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x = if x > 0 then lastDigit x : toRevDigits (dropLastDigit x)
                         else []

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = undefined

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = undefined


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = undefined
