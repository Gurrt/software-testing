module Lab6 where

import System.CPUTime
import System.Random
import Control.Monad

import Lecture6

-- Exercise 1
-- Catches some edge cases and then use the exMsq to calculate squared modulo.
exM :: Integer -> Integer -> Integer -> Integer
exM b e m
    | e < 0 = 0
    | e == 0 = 1 `mod` m
    | otherwise = exMsq b e m 1

-- Squaring
-- Usage: exMsq b e m 1
exMsq :: Integer -> Integer -> Integer -> Integer -> Integer
exMsq _ 0 _ r = r
exMsq b e m r
  | odd e = exMsq b (e-1) m (mod (r*b) m)
  | otherwise = exMsq (mod (b*b) m) (div e 2) m r

-- Memory-efficient method
exMmem :: Integer -> Integer -> Integer -> Integer
exMmem b 1 m = mod b m
exMmem b e m = mod (b* exMmem b (e-1) m) m


-- Exercise 2
-- Usage: testEx2 minRange maxRange
testEx2 :: Integer -> Integer -> IO()
testEx2 = randomFaster

-- Generates a random test which compares both methods performance.
randomFaster:: Integer -> Integer -> IO()
randomFaster x y = do
         g <- newStdGen
         let (b, newGen) = randomR (x,y) g
         let (e, newGen') = randomR (x,y) newGen
         let (m, _) = randomR (x,y) newGen'
         faster b e m

-- Given a base, exponent and modulus, it compares the performance of both methods.
-- Time is in pico seconds.
faster:: Integer -> Integer -> Integer -> IO ()
faster b e m = do
                    print ("Comparison between the Ex1 method and lecture's method"::String)
                    print ("Results are expressed in pico seconds."::String)
                    x <- getDiffMsq b e m
                    y <- getDiffDefault b e m
                    r <- compareDiff x y
                    if r then print ("--> Squaring method is faster"::String)
                      else print ("--> Lecture's method is faster"::String)

compareDiff:: Integer -> Integer -> IO Bool
compareDiff x y = return (x < y)

-- Calculates the execution time using the square method from the exercise 1.
getDiffMsq:: Integer -> Integer -> Integer -> IO Integer
getDiffMsq b e m = do
            start <- getCPUTime
            print ("Square method result: " ++ show (exMsq b e m 1))
            end   <- getCPUTime
            let diff = fromIntegral (end - start)
            print ("- Execution time: " ++ show diff)
            return diff

-- Calculates the execution time using the default method from the lectures.
getDiffDefault:: Integer -> Integer -> Integer -> IO Integer
getDiffDefault b e m = do
            start <- getCPUTime
            print ("Lecture method result: " ++ show (expM b e m))
            end   <- getCPUTime
            let diff = fromIntegral (end - start)
            print ("- Execution time: " ++ show diff)
            return diff


-- Exercise 3
-- The approach taken is fairly easy. Starting by the first known not prime number
-- we generate a list filtering in not being prime using the function isPrime
-- from the lectures. The function takes long time to compute if a number is
-- prime, but in case it isn't it gives a result really fast.
composites :: [Integer]
composites = 4 : filter (not . isPrime) [5..]


-- Exercise 4
-- Usage: testEx4 k

-- Lowest found values for textEx k
--   (k = 1) lowest found: 4
--   (k = 2) lowest found: 4
--   (k = 3) lowest found: 15
--   (k = 4) lowest found: 4
--
-- If you increase k the probability of fooling the test becomes smaller due to
-- a larger number of random samples, However for low numbers the unique
-- possible samples are quite small, for example for 4 there are only 3 samples.
-- Namely : 1^3 mod 4, 2^3 mod 4 and 3^3 mod 4. One of which returns 1, which
-- means that with k = 4 the chance of fooling the test is 1/3 ^ 4 == 1.23%.

testEx4 :: Int -> IO()
testEx4 k = do
              f <- foolFermat k
              print ("(k = " ++ show k ++ ") lowest found: " ++ show f)

foolFermat :: Int -> IO Integer
foolFermat k  = lowestFermatFooler k composites

lowestFermatFooler :: Int -> [Integer] -> IO Integer
lowestFermatFooler _ [] = return 0
lowestFermatFooler k (x:xs) = do
    result <- prime_tests_F k x
    if result then return x else lowestFermatFooler k xs


-- Exercise 5
-- Usage: testEx5 k

-- We are going to test Fermant's primalty test using Carmichael's numbers.
-- The first thing we do is to define a function to generate Carmichael's numbers.
-- In this case, it is given in the description of the exercise.
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
        k <- [2..],
        isPrime (6*k+1),
        isPrime (12*k+1),
        isPrime (18*k+1) ]

-- Test the first k Carmichael numbers using the Fermat's primalty test. We take
-- the function prime_test_F from the lectures as our implementation of Fermat's
-- primalty test.
testFermatC :: Int -> IO [Bool]
testFermatC k = mapM prime_test_F (take k carmichael)

-- We also define a test using the Sieve of Erastothenes to check if the output
-- of the Fermat's test is true.
testIsPrime :: Int -> [Bool]
testIsPrime k = map isPrime (take k carmichael)

-- Finally, we output the solution. Carmichael numbers are quite big so the
-- primalty check takes long time to compute.
testEx5 :: Int -> IO()
testEx5 k = do
              let num = take k carmichael
              print ("Test for the first " ++ show k ++ " Carmichael's numbers.")
              print ("Displays the result as a triple following the structure:"::String)
              print ("- (Carmichael number, (Fermat's test, Sieve of Erastothenes))"::String)
              ferm <- testFermatC k
              let eras = testIsPrime k
              let comb = zip num (zip ferm eras)
              print comb

-- The output of the test deservers to be discussed. The Carmichael's numbers is
-- derivated from Fermat's primalty test. Fermat's primalty test states that if
-- p is prime, then a random number a not divisible by p fulfills
-- a^(p-1) = 1 mod p. Carmichael numbers are composite numbers (as we can see in
-- the function used to generate them) that pass Fermat's test but they are not
-- prime. Indeed, those numbers p are not prime but coprimes with the chosen
-- base a. This turns Fermat's test into a necessary condition but not sufficient:
-- If a number is prime, it fulfills Fermat's theorem but if a number fulfills
-- Fermat's theorem, it may not be prime as Carmichael numbers demostrate.

-- The current formula used to calculate this numbers was proved by J. Chernick
-- in 1939 and it produces Carmichael numbers as far as his 3 components are
-- prime numbers. As the number is a Carmichael number, it should pass Fermat's
-- test but fail on Erastothenes sieve.

-- Exercise 6
-- Miller-Rabin
testMR :: Int -> [Integer] -> IO ()
testMR k (p:ps) = do
                      r <- primeMR k p
                      when r $ print(show p ++ ", Miller-Rabin: " ++ show r)
                      testMR k ps

-- Test: testMR 1 carmichael, will take forever
-- Test: testMR 1 (take 1000 carmichael) 118901521 Miller-Rabin:True, and
-- probably many more but my processor fails.
-- Conclusion: testER uses an iterator int k, and list of Carmichael numbers to
-- test. Our test isn't consistent enough to write a solid conclusion, but they
-- are hard to find and this fact make presume that using carmichael numbers the
-- MR test is more difficult to fool.

-- Mersenne
mersnPrimes :: Integer -> IO ()
mersnPrimes p = do
                  print(show p)
                  let p1 = (2^p - 1) in
                    do
                      r <- primeMR 5 p1
                      when r $ mersnPrimes p1

--Test : mersnPrimes 5
--Test : mersnPrimes m3 "2147483647" (= m8: 2^31-1)
--Conclusion: mersnPrimes takes a p (prime number) and check in primeMR
-- searching for similarity. According with
-- https://en.wikipedia.org/wiki/Mersenne_prime not all the numbers has pass the
-- check are genuine Mersenne primes.

-- Exercise 7
