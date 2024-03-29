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

   [Alberto] (Time spent reading: several days in small chunks, mostly during commuting) ->
   As Ger, my I didn't understand the example 4.5 about the Russel Paradox. It starts with
   an acceptable assumption (most of the sets don't contain themselves) which makes sense.
   For instance, a set of integeres only contains integers, not sets of integers. But after,
   when supposing the opposite, I get lost in the following statements, specially in the one
   that affirms that that is not possible to prove.

   After, the section 4.2 is confusing as well. I have some background about the halt problem
   but the whole approach (specially with the funny function) is confusing and as I wasn't even
   able to understand what the author wants to prove, I decided to just read the section quickly. 
-}  


-- Exercise 2

-- Using the Random class from the Haskell library does not apply to our type Set a.
-- The Random class specifically needs a type of * while Set is of type *->*. This causes clashes in the Random class.
-- I try to write a Wrapper as suggest on some internet fora, however I did not succeed.

min_set_length :: Int
min_set_length = 0

max_set_length :: Int
max_set_length = 30

min_random_value :: Int
min_random_value = -1000

max_random_value :: Int
max_random_value = 1000

exercise2:: IO()
exercise2 = do
    seed  <- newStdGen
    let (int1, seed2) = randomInt seed
    let rs = randomlist int1 seed2
    let s = list2set rs
    print $ (show s)

randomInt :: StdGen -> (Int, StdGen)
randomInt gen = randomR (min_set_length, max_set_length) gen

randomlist :: Int -> StdGen -> [Int]
randomlist n gen = take n (randomRs (min_random_value, max_random_value) gen)

-- Should be able to generate random list of any type
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
   arbitrary = fmap list2set arbitrary

-- Use this to test the above function: verboseCheck prop_setIntEqItself
prop_setIntEqItself :: (Set Int) -> Bool
prop_setIntEqItself a = a == a


-- Exercise 3

-- unionSet is already defined in SetOrd.hs so we will only implement intersection and difference
intersectionSet :: (Ord a) => Set a -> Set a -> Set a 
intersectionSet (Set []) _  =  (Set [])
intersectionSet _ (Set []) = (Set [])
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

    [Alberto] (Time spent reading: several days in small chunks, mostly during commuting) ->
    In several of the examples (specially at around page 175 because I had to check them often
    in order to do the other exercises), the the topic covered are relations and although the
    definition of the functions refer to Rel, the implementation use Set. Is this a mistake?

-}


-- Exercise 5
-- Time spent : 30 minutes

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos x = sort $ nub $ generateSymClosures x

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
trClos x = sort $ nub $ trClos' x

trClos' :: Ord a => Rel a -> Rel a
trClos' [] = []
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
removeAlreadyChecked [] _ = []
removeAlreadyChecked ((x,y):xs) z
    | elem (x,y) z = removeAlreadyChecked xs z
    | otherwise = (x,y) : removeAlreadyChecked xs z 


-- Exercise 7
-- Time spent: 6 hours

-- The first step is to define the properties that those relations hold.
-- Starting with the symmetric relation, based on the following definition:
-- (definition from Wikipedia: https://en.wikipedia.org/wiki/Symmetric_closure)
--
--  S = R u {(x,y):(y,x) € R)
-- 
-- We can state that:
--  1. S size must be smaller or equal than twice R.
--  2. For each element of S must exist other element in S that is symmetric.
--  3. All the elements must be ordered.

-- First property: S must be smaller or equal than twice R.
-- In case R does not contains any symmetric relation, size S == 2 * size R.
-- In case R only contains symmetric relations, size S == size R
symSizeProp :: Rel a -> Rel a -> Bool
symSizeProp r s = length s <= 2 * length r

-- Second property: Foreach element in R must be one symmetric in S and itself.
symSymProp :: Ord a => Rel a -> Rel a -> Bool
symSymProp [] [] = True    -- In case R == S
symSymProp [] _  = True    -- Normal base case.
symSymProp _  [] = False   -- In case the size R > size S
symSymProp ((x,y):rs) s = elem (x,y) s && elem (y,x) s && (symSymProp rs s)

-- Third property: Elements must be ordered.
symOrdProp :: Ord a => Rel a -> Bool
symOrdProp [] = True
symOrdProp [(_,_)] = True
symOrdProp ((x,y):(x',y'):ss) 
  | x < x'  = True && symOrdProp ((x',y'):ss)
  | x == x' && y <= y' = True && symOrdProp ((x',y'):ss)
  | otherwise = False


-- This one is specially tricky. Uncountable hours have been spent just
-- generating proper relation. The first issue to address is to generate a pair
-- of arbitrary elements. The only way to address this issue is generating two
-- elements independently and then using and if-then-else reordering them. After
-- generating a pair, we create a list based on this pair. 
--
-- As the only constraint for the Relations is to be Ord a, QuickCheck by
-- default will use empty pairs ((),()) (this can be checked running
-- `verboseCheck testSymClos` after commenting the following lines). Therefore,
-- I added the type Num to the definition to force QuickCheck to use meaningful
-- data.
instance (Num a, Ord a, Arbitrary a) => Arbitrary (Rel a) where
  arbitrary = listOf $ do
    x <- arbitrary
    y <- arbitrary
    if x < y then return (x,y)
      else return (y,x)
        
-- Test function for the symmetric closures. In order to supress the warning,
-- Integer can be used insted of a. In order to define the most general test
-- possible, the anonymous type has been preserved.
--
-- Run using:
--   quickCheck testSymClos
testSymClos :: Ord a => Rel a -> Bool
testSymClos r = let s = symClos r in symOrdProp s && symSizeProp r s && symSymProp r s

-- For the transitive relation, we apply the same idea
-- 1. R is contained in R+
-- 2. R+ is ordered.
-- 3. R+ is transitive.
-- 4. R+ is the minimal union of R^i

-- First property: To check that the elements of R are contained in R+ is almost
-- trivial.
trContainsProp :: Ord a => Rel a -> Rel a -> Bool
trContainsProp r rp = all (\x -> x `elem` rp) r

-- Second property: This is not a real property of the transitive closure, but
-- the description says that the relation is ordered. Therefore, we will reuse
-- the same implementation used with the symmetric closure.
trOrdProp :: Ord a => Rel a -> Bool
trOrdProp = symOrdProp 

-- Third property: In order to test this property we will just follow the
-- guidelines in the pages 175 and 177 of the book to check the transitivity.
trTransProp :: Ord a => Rel a -> Bool
trTransProp [] = True
trTransProp rp = and [trans pair rp | pair <- rp] where 
  trans (x,y) rp' = 
    and [(x,v) `elem` rp' | (u,v) <- rp', u == y ]

-- To check the last property is not viable due to the combinatorial explosion
-- that can be generated when trying to check for every element of R+ in which
-- R^i it is placed (in relations big enough it can grow really big with a high 
-- i value, in addition to be difficult to decide when to stop checking new 
-- iterations). It would be a hard test on the software because it will
-- secure not only the property but also if it is complete. As we learnt during 
-- week 2 testing the dearrangements, it is not viable to do it and other
-- properties (like property number 3) should be tested instead.

-- Test function for the transitive closures. In order to supress the warning,
-- Integer can be used insted of a. In order to define the most general test
-- possible, the anonymous type has been preserved.
--
-- Run using:
--   quickCheck testTrClos
testTrClos :: Ord a => Rel a -> Bool
testTrClos r = let rp = trClos r in trContainsProp r rp && trOrdProp rp && trTransProp rp


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
