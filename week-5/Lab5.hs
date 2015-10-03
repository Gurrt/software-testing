module Lab5 where

import Data.List
import System.Random
import qualified Lecture5 as L
import qualified Exercise1 as E1
import qualified Exercise2 as E2

-- Exercise 1

nrcGrid :: L.Grid
nrcGrid = [[0,0,0,3,0,0,0,0,0],
           [0,0,0,7,0,0,3,0,0],
           [2,0,0,0,0,0,0,0,8],
           [0,0,6,0,0,5,0,0,0],
           [0,9,1,6,0,0,0,0,0],
           [3,0,0,0,7,1,2,0,0],
           [0,0,0,0,0,0,0,3,1],
           [0,8,0,0,4,0,0,0,0],
           [0,0,2,0,0,0,0,0,0]]

showNrcGrid:: IO()
showNrcGrid = E1.showGrid nrcGrid

-- Exercise 2

-- Exercise 3

-- Exercise 4

-- Exercise 5
