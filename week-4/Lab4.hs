module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

-- Exercise 1
{- Questions : 
    [Ger] (Time spent reading: 2 hours) -> Chapter 4.2 & Example 4.5 the Russel paradoxes. The logic appears to make no sense at all.
        Perhaps a practical example would help to make sense of what the authors are trying to tell us,
        Because pseudo-haskell code only confuses me more. The rest of the chapter was pretty clear.
-}    
-- Exercise 2

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
{-
	Questions :
	[Ger] (Time spent reading: 2 hours) -> The entire chapter was very mathsy, making it long and unintersting to read. Furthermore the chapter
		introduces a lot of concepts which are fragmented throughout, perhaps a summary of concepts introduced in
		the chapter would be in order.
-}

-- Exercise 5
-- Time spent : 30 minutes

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos x = nub $ generateSymClosures x

generateSymClosures :: Ord a => Rel a -> Rel a
generateSymClosures [] = []
generateSymClosures ((x,y):xs) = [(x,y)] ++ [(y,x)] ++ generateSymClosures xs 

-- Exercise 6
-- Time spent : 35 minutes

infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos [(x,y)] = [(x,y)]
trClos ((x,y):xs) = [(x,y)] ++ generateTransClosures [(x,y)] xs ++ trClos xs

generateTransClosures :: Ord a => Rel a -> Rel a -> Rel a
generateTransClosures x y
    | (x @@ y) /= [] = (x @@ y) ++ generateTransClosures (x @@ y) y
    | otherwise = []
-- Exercise 7

-- Exercise 8
