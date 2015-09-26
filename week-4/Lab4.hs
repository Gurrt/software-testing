module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Exercise 1
{- Questions : 
    [Ger] -> Chapter 4.2 & Example 4.5 the Russel paradoxes. The logic appears to make no sense at all.
        Perhaps a practical example would help to make sense of what the authors are trying to tell us,
        Because pseudo-haskell code only confuses me more. The rest of the chapter was pretty clear.
-}    
-- Exercise 2

-- First determine the length of the set, anywhere between 0 and 30
-- Defines
min_set_length = 0 :: Int
max_set_length = 30 :: Int

-- Exercise 3
-- unionSet is already defined in SetOrd.hs so we will only implement intersection and difference

intersectionSet :: (Ord a) => Set a -> Set a -> Set a 
intersectionSet (Set []) set2  =  (Set [])
intersectionSet set1 (Set []) = (Set [])
intersectionSet (Set (x:xs)) set2
    | inSet x set2 = insertSet x (intersectionSet (Set xs) set2)
    | otherwise = intersectionSet (Set xs) set2
    
differenceSet :: (Ord a) => Set a -> Set a -> Set a
differenceSet set1 (Set []) = set1
differenceSet (Set []) set2 = set2
differenceSet (Set (x:xs)) set2
    | not $ inSet x set2 = insertSet x (differenceSet (Set xs) set2)
    | otherwise = differenceSet (Set xs) (deleteSet x set2)

-- Exercise 4

-- Exercise 5

-- Exercise 6

-- Exercise 7

-- Exercise 8
