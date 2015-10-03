module Lab5 where

import Data.List
import System.Random
import Lecture5

-- Exercise 1

nrcGrid :: Grid
nrcGrid = [[0,0,0,3,0,0,0,0,0],
           [0,0,0,7,0,0,3,0,0],
           [2,0,0,0,0,0,0,0,8],
           [0,0,6,0,0,5,0,0,0],
           [0,9,1,6,0,0,0,0,0],
           [3,0,0,0,7,1,2,0,0],
           [0,0,0,0,0,0,0,3,1],
           [0,8,0,0,4,0,0,0,0],
           [0,0,2,0,0,0,0,0,0]]

nrcGridLine :: String
nrcGridLine = "+---------+----------+----------+"
nrcSubGridLine:: String
nrcSubGridLine = "|   +-----|---+   +---|-----+   |"

showNrcGrid:: Grid -> IO()
showNrcGrid [as,bs,cs,ds,es,fs,gs,hs,is] =
    do putStrLn (nrcGridLine)
       showNormalRow as;
       putStrLn (nrcSubGridLine)
       showNrcSubGridRow bs; showNrcSubGridRow cs
       putStrLn (nrcGridLine)
       showNrcSubGridRow ds;
       putStrLn (nrcSubGridLine)
       showNormalRow es;
       putStrLn (nrcSubGridLine)
       showNrcSubGridRow fs;
       putStrLn (nrcGridLine)
       showNrcSubGridRow gs; showNormalRow hs
       putStrLn (nrcSubGridLine)
       showNormalRow is;
       putStrLn (nrcGridLine)

showNrcSubGridRow:: [Value] -> IO()
showNrcSubGridRow vs = showNrcRow '|' vs

showNormalRow:: [Value] -> IO()
showNormalRow vs = showNrcRow ' ' vs

showNrcRow :: Char -> [Value] -> IO()
showNrcRow ch [a1,a2,a3,a4,a5,a6,a7,a8,a9] =
   do  putChar '|'         ; putChar ' '
       putStr (showVal a1) ; putChar ' '
       putChar ch          ; putChar ' '
       putStr (showVal a2) ; putChar ' '
       putStr (showVal a3) ; putChar ' '
       putChar '|'         ; putChar ' '
       putStr (showVal a4) ; putChar ' '
       putChar ch          ; putChar ' '
       putStr (showVal a5) ; putChar ' '
       putChar ch          ; putChar ' '
       putStr (showVal a6) ; putChar ' '
       putChar '|'         ; putChar ' '
       putStr (showVal a7) ; putChar ' '
       putStr (showVal a8) ; putChar ' '
       putChar ch          ; putChar ' '
       putStr (showVal a9) ; putChar ' '
       putChar '|'         ; putChar '\n'

-- Exercise 2

-- Exercise 3

-- Exercise 4

-- Exercise 5
