module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
-- Note : casting (last (show x)) to array to make String from char. -- Ger
lastDigit :: Integer -> Integer
lastDigit x = read [last (show x)]

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = read (replaceEmptyStringByZero (init (show x)))

-- null "" => True
replaceEmptyStringByZero :: String -> String
replaceEmptyStringByZero x = if not (null x) then x else "0"

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x
    | x > 0     = lastDigit x : toRevDigits (dropLastDigit x)
    | otherwise = []

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs = head xs : doubleNow (tail xs)

doubleNow :: [Integer] -> [Integer]
doubleNow [] = []
doubleNow xs = 2 * head xs : doubleEveryOther (tail xs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = sumLeft (fixLeftmostDigit xs)

-- Splits the starting digit so it can be used to correctly calculate sum
-- if starting value of that digit >= 10
fixLeftmostDigit :: [Integer] -> [Integer]
fixLeftmostDigit xs
    | head xs > 9 = head xs - 9 : tail xs
    | otherwise   =  xs

-- Sums every digit seperately (substracting 9 is the same as adding the digits seperately for values under 20)
sumLeft :: [Integer] -> Integer
sumLeft xs
    | length xs == 1 = head xs
    | head (tail xs) > 9 = sumLeft (head xs + (head (tail xs) - 9)  : tail (tail xs))
    | otherwise =  sumLeft (head xs + head (tail xs) : tail (tail xs))

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = lastDigit x == (10 - lastDigit (sumDigits (tail (doubleEveryOther (toRevDigits x)))))
