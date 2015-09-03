-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

testReplaceEmptyStringByZero :: (String, String) -> Bool
testReplaceEmptyStringByZero (original, expected) = replaceEmptyStringByZero original == expected

ex1Tests :: [Test]
ex1Tests = [Test "lastDigit" testLastDigit
                [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)],
            Test "dropLastDigit" testDropLastDigit
                [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)],
            Test "replaceEmptyStringByZero" testReplaceEmptyStringByZero
                [("", "0"), ("not empty string", "not empty string")]]

-- Exercise 2 -----------------------------------------
testToRevDigits :: (Integer, [Integer]) -> Bool
testToRevDigits (n, expected) = toRevDigits n == expected

ex2Tests :: [Test]
ex2Tests = [Test "toRevDigits" testToRevDigits
                [(1, [1]), (123, [3,2,1]), (121, [1,2,1])]]

-- Exercise 3 -----------------------------------------
testDoubleEveryOther :: ([Integer], [Integer]) -> Bool
testDoubleEveryOther (n, expected) = doubleEveryOther n == expected

testDoubleNow :: ([Integer], [Integer]) -> Bool
testDoubleNow (n, expected) = doubleNow n == expected

ex3Tests :: [Test]
ex3Tests = [Test "doubleEveryOther" testDoubleEveryOther
                [([1],[1]), ([1,2], [1,4]), ([1,2,3],[1,4,3]), ([1,2,3,4],[1,4,3,8])],
            Test "doubleNow"testDoubleNow
                [([1],[2]), ([1,2], [2,2]), ([1,2,3],[2,2,6]), ([1,2,3,4],[2,2,6,4])]]

-- Exercise 4 -----------------------------------------
testSumDigits :: ([Integer], Integer) -> Bool
testSumDigits (n, expected) = sumDigits n == expected

testFixLeftmostDigit :: ([Integer], [Integer]) -> Bool
testFixLeftmostDigit (n, expected) = fixLeftmostDigit n == expected

testSumLeft :: ([Integer], Integer) -> Bool
testSumLeft (n, expected) = sumLeft n == expected

ex4Tests :: [Test]
ex4Tests = [Test "sumDigits" testSumDigits
                [([1], 1), ([9], 9), ([1,0], 1), ([1,2], 3), ([5,5], 10), ([6,7], 13)],
            Test "fixLeftmostDigit" testFixLeftmostDigit
                [([12, 3], [3,3]), ([3,3], [3,3])],
            Test "sumLeft" testSumLeft
                [([1], 1), ([1,2,10], 4), ([1,15,10], 8), ([5,15,13,7], 22)]]


-- Exercise 5 -----------------------------------------
testLuhn :: (Integer, Bool) -> Bool
testLuhn (n, expected) = luhn n == expected

-- Examples from http://www.getcreditcardnumbers.com/
ex5Tests :: [Test]
ex5Tests = [Test "luhn Visa" testLuhn
                [(4539985049046869, True), (4916122681759665, True), (4916122681759656, False)],
            Test "luhn Mastercard" testLuhn
                [(5213370088961163, True), (5402016037107892, True), (5322327476083934, False)],
            Test "luhn Discover" testLuhn
                [(6011823388153016, True), (6011120647004092, True), (6011779126514155, False)],
            Test "luhn AmericanExpress" testLuhn
                [(376061407959574, True), (346930887567189, True), (349439227744484, False)]]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  ]
