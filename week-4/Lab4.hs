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

  [Robert] (Time spent reading: 3 hours) ->
   Question 1
   I was wondering how I should read notion 5 in Theorem 4.38.
   What does the "["insert-some-set-operations-here"]" mean compared to "("insert-someset-operations-here")" ?
   Or is this a printing mistake?

   Question 2
   I used the term "notion" to address a part of a theorem in the question above. I was wondering if this is correct.
   The beginning of the chapter briefly address the terms, however it seems not to address this specific question.

   Question 3
   In Haskell what is the difference between the usage of "case" and equation guarding?
   To me the two seem oddly similar.

   Concerning the logic in chapter 4 I have no specific questions.
-}    
-- Exercise 2

-- Using the Random class from the Haskell library does not apply to our type Set a.
-- The Random class specifically needs a type of * while Set is of type *->*. This causes clashes in the Random class.
-- I try to write a Wrapper as suggest on some internet fora, however I did not succeed.

exercise2:: Int -> IO()
exercise2 n = do
    seed  <- newStdGen
    let rs = randomlist n seed
    let s = list2set nub(rs) -- Removes the duplicates before converting the list to a set
    print (show s)

randomlist :: Int -> StdGen -> [Int]
randomlist n gen = take n (randoms gen)

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
-- Time spent : 1 hour

infixr 5 @@
 
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Wrapper function since recursive nature of trClos' does not prevent duplicates
trClos :: Ord a => Rel a -> Rel a
trClos x = nub $ trClos' x

trClos' :: Ord a => Rel a -> Rel a
trClos' [(x,y)] = [(x,y)]
trClos' ((x,y):xs) = [(x,y)] ++ generateTransClosures [(x,y)] xs [] ++ trClos' xs

-- 1st argument : Tuple to generate closures for
-- 2nd argument : Relations to generate closures against
-- 3th argument : List of already checked closures
generateTransClosures :: Ord a => Rel a -> Rel a -> Rel a -> Rel a
generateTransClosures x y z
    | ((removeAlreadyChecked x z) @@ y) /= [] = ((removeAlreadyChecked x z) @@ y) ++ 
       generateTransClosures ((removeAlreadyChecked x z) @@ y) y ((removeAlreadyChecked x z) ++ z)
    | otherwise = []

-- Makes sure we do not get stuck in an infinite loop
removeAlreadyChecked :: Ord a => Rel a -> Rel a -> Rel a
removeAlreadyChecked [] z = []
removeAlreadyChecked ((x,y):xs) z
    | elem (x,y) z = removeAlreadyChecked xs z
    | otherwise = (x,y) : removeAlreadyChecked xs z 

-- Exercise 7

-- Exercise 8
{-
 As example we can use the basic relation from exercise 5 and 6.
 GHCI Code :
 *Lab4> symClos $ trClos [(1,2),(2,3),(3,4)]
 [(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)]

 *Lab4> trClos $ symClos [(1,2),(2,3),(3,4)]
 [(1,2),(1,1),(1,3),(1,4),(2,1),(2,3),(2,2),(2,4),(3,2),(3,4),(3,3),(4,3)]
 
 As you can see these aren't the same, this is because if you first apply the symmetric closure and then the transitive closure,
 you will always get a link from a node to itself, as you can always get back to the starting point in two hops. Whereas, if you
 first apply the transitive closure and then the symmetric one. You will get no relations from a node to itself if they weren't there
 before.
-}
