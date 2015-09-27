module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
import Lecture3
import Data.Set (Set)
import qualified Data.Set as Set

--exercise 1
--[Carla]
--reading time : undefined.
--After reading the chapter4, I tryed to inicialize a set fromList and add to it random elements.
--It wasnt possible on a simple way with simple commands, It is because some kind of Haskell limitation?


--exercise 2

intRandom :: Int -> IO Int
intRandom n = randomRIO(0, n)

insertS :: Int -> Set a
insertS (x:xs) = insert (intRandom x) ++ xs 


--exercise 3

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


--exercise 4
--[Carla]
--reading time : undefined.
--From definition 5.3 I could understand de concept from the title itself and ilustration, but explanation
--is hokum.
--From definition 'equivalence class' should we understand the concept of class as the given by POO?



--exercise 5

type Rel a = [(a, a)]

symClos:: Ord a => Rel a -> Rel a
symClos r = [ (x,y) | (y,x) <- r ]
--sysClos r = nub $ closures r

closures :: Ord a => Rel a -> Rel a
closures [] = []
closures ((x,y):xs) = [(x,y)] ++ [(y,x)] ++ closures xs


--exercise 6

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos [] = []
trClos [x] = [x]
--trClos x = convertR (sort x)

--convertR :: Ord a => Rel a -> Rel a
--convertR (x:xs) = [x] ++ (trans [x] [xs]) ++ ( trClos xs)

--trans :: Rel a -> Rel a
--trans x [y] = [(x1, y2) | (x1, x2) <- x, (y1, y2) <- trClos (y), x2 == y1]



-- exercise 7
-- Start at 20:21

-- The first step is to define the properties that those relations hold.
-- Starting with the symmetric relation, based on the following definition:
-- (definition extracted from Wikipedia)
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


testSymClos :: Ord a => Rel a -> Bool
testSymClos r = let s = symClos r in symOrdProp s && symSizeProp r s && symSymProp r s



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

