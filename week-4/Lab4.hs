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

--exercise 2

intRandom :: Int -> IO Int
intRandom n = randomRIO(0, n)

insertS :: Int -> Set a
insertS (x:xs) = insert (intRandom x) ++ xs 


--exercise 3
--exercise 4
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

--exercise 7
--exercise 8

