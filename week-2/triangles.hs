module Triangles where
import Data.List

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

          -- Write a program (in Haskell) that takes a triple of integer values as arguments and gives as output one of the following statements:

           --Not a triangle (Geen driehoek) if the three numbers cannot occur as the lengths of the sides of triangle,

           --Equilateral (Gelijkzijdig) if the three numbers are the lengths of the sides of an equilateral triangle,

           --Rectangular (Rechthoekig) if the three numbers are the lengths of the sides of a rectangular triangle,

           --Isosceles (Gelijkbenig) if the three numbers are the lengths of the sides of an isosceles (but not equilateral) triangle,

           --Other (Anders) if the three numbers are the lengths of the sides of a triangle that is not equilateral, not rectangular, and not isosceles.

triangle :: Integer -> Integer -> Integer -> Shape
triangle x y z | checkForZeros x y z  = Other
               | isRectangular x y z = Rectangular
               | isIsocles x y z = Isosceles
               | isEquilateral x y z = Equilateral
               | isNoTriangle x y z = NoTriangle
               | otherwise = Other

isRectangular :: Integer -> Integer -> Integer -> Bool
isRectangular x y z = (x^2 + y^2) == z^2

isEquilateral :: Integer -> Integer -> Integer -> Bool
isEquilateral x y z = (x == y) && (y == z) && (x == z)

isIsocles :: Integer -> Integer -> Integer -> Bool
isIsocles x y z = (x == y) && (y /= z)

isNoTriangle ::Integer -> Integer -> Integer -> Bool
isNoTriangle x y z = (x + y) < z

checkForZeros:: Integer -> Integer -> Integer -> Bool
checkForZeros x y z = (x <= 0) || (y <= 0) ||( z <= 0)