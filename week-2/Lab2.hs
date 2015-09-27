import Data.List
import Data.Char
import Data.Maybe
import Control.Monad

--exercise 1

--Not a triangle (Geen driehoek) if the three numbers cannot occur as the lengths of the sides of triangle,
--Equilateral (Gelijkzijdig) if the three numbers are the lengths of the sides of an equilateral triangle,
--Rectangular (Rechthoekig) if the three numbers are the lengths of the sides of a rectangular triangle,
--Isosceles (Gelijkbenig) if the three numbers are the lengths of the sides of an isosceles (but not equilateral) triangle,
--Other (Anders) if the three numbers are the lengths of the sides of a triangle that is not equilateral, not rectangular, and not isosceles.


data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- Sides must be given in ascending order: triangle 1 2 3
isTriangle :: Integer -> Integer -> Integer -> Shape
isTriangle a b c
    | a > 0 && b > 0 && c > 0 && a + b < c = NoTriangle
    | a == b && b == c && a == c = Equilateral
    | a == b && b /= c = Isosceles
    | a^2 + b^2 == c^2 = Rectangular
    | otherwise = Other


--exercise 2

iban :: String -> Bool
iban s = ((read (stringCleaned s)  `mod` 97) == 1) || (specialMod97 (stringCleaned s))

specialMod97 :: String -> Bool 
specialMod97 x = (read (concat[show (read (take 9 (concat [show ((read (take 9 x)) `mod` 97) , drop 9 x])) `mod` 97), drop 16 x]) `mod` 97)== 1

stringCleaned :: String -> String
stringCleaned x = replaceLetters (shiftFour (removeWhite x))

removeWhite :: String -> String
removeWhite x = filter (/=' ') x

shiftFour :: String -> String
shiftFour x = concat [drop 4 x, take 4 x]

replaceLetters :: String -> String
replaceLetters [] = []
replaceLetters (x:xs) = findLetter [toUpper x] ++ replaceLetters xs

findLetter:: String -> String
findLetter a = if (isDigit (a !!0) ) then a
else 
 fromJust (lookup a [("A","10"),("B","11"),("C","12"),("D","13"),("E","14"),("F","15"),("G","16"),("H","17"),("I","18"),("J","19"),("K","20"),("L","21"),("M","22"),("N","23"),("O","24"),("P","25"),("Q","26"),("R","27"),("S","28"),("T","29"),("U","30"),("V","31"),("W","32"),("X","33"),("W","34"),("Z","35")] ) 


