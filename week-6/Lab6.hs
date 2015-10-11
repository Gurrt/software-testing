module Lab6 where

import qualified Lecture6 as L

-- Exercise 1

-- Exercise 2

-- Exercise 3

-- Exercise 4

-- Exercise 5

-- In order to
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
        k <- [2..],
        L.isPrime (6*k+1),
        L.isPrime (12*k+1),
        L.isPrime (18*k+1) ]

-- Test the first k Carmichael numbers using the Fermat's primalty test.
testFermatC :: Int -> IO [Bool]
testFermatC k = mapM L.prime_test_F (take k carmichael)

printTest :: IO Bool -> IO()
printTest b = do
                b' <- b
                if b' then print ("True"::String)
                    else print ("False"::String)

testEx5 :: Int -> [IO()]
testEx5 k = do
              arr <- testFermatC k
              print arr

-- Exercise 6

-- Exercise 7
