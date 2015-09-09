--Not a triangle (Geen driehoek) if the three numbers cannot occur as the lengths of the sides of triangle,
--Equilateral (Gelijkzijdig) if the three numbers are the lengths of the sides of an equilateral triangle,
--Rectangular (Rechthoekig) if the three numbers are the lengths of the sides of a rectangular triangle,
--Isosceles (Gelijkbenig) if the three numbers are the lengths of the sides of an isosceles (but not equilateral) triangle,
--Other (Anders) if the three numbers are the lengths of the sides of a triangle that is not equilateral, not rectangular, and not isosceles.

 
import Data.List
--import System.Random

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- Sides must be given in ascending order: triangle 1 2 3
isTriangle :: Integer -> Integer -> Integer -> Shape
isTriangle a b c
    | a > 0 && b > 0 && c > 0 && a + b < c = NoTriangle
    | a == b && b == c && a == c = Equilateral
    | a == b && b /= c = Isosceles
    | a^2 + b^2 == c^2 = Rectangular
    | otherwise = Other