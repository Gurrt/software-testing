import Data.Char
-- Data.Char is imported for isAlphaNum check in IBAN assignment

-- Triangle Assignment
-- Time taken : 40 minutes
data Shape = NoTriangle | Equilateral 
            | Isosceles  | Rectangular | Other deriving (Eq,Show)

isTriangle :: Integer -> Integer -> Integer -> Bool
isTriangle a b c = a > 0 && b > 0 && c > 0 && (a + b > c) && (a + c > b) && (b + c > a) 

isRectangularTriangle :: Integer -> Integer -> Integer -> Bool
isRectangularTriangle a b c = a*a + b*b == c*c || a*a + c*c == b*b || b*b + c*c == a*a

isEquilateralTriangle :: Integer -> Integer -> Integer -> Bool
isEquilateralTriangle a b c = a == b && b == c

-- we define Isosceles as at LEAST two sides of the same size.
-- The order of the pattern matches in checkTriangle classes
-- three matching sides as equilateral anyway
isIsoscelesTriangle :: Integer -> Integer -> Integer -> Bool
isIsoscelesTriangle a b c = a == b || a == c || b == c

checkTriangle :: Integer -> Integer -> Integer -> Shape
checkTriangle a b c
    | not (isTriangle a b c) = NoTriangle
    | isRectangularTriangle a b c = Rectangular
    | isEquilateralTriangle a b c = Equilateral
    | isIsoscelesTriangle a b c = Isosceles
    | otherwise = Other

-- Permutation Assignment
-- Time taken : 30 minutes
isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = (length xs == length ys) && all (\x -> elem x ys) xs

-- Derangement Assignment
-- Time taken: 1 Hour
isDerangement :: [Integer] -> [Integer] -> Bool
isDerangement (x:xs) (y:ys)
    | not (isPermutation (x:xs) (y:ys)) = False
    | otherwise = (recDerangementCheck (x:xs) (y:ys))

recDerangementCheck :: [Integer] -> [Integer] -> Bool
recDerangementCheck [] [] = True
recDerangementCheck (x:xs) (y:ys)
    | (x == y) = False
    | otherwise = recDerangementCheck xs ys

deran :: Integer -> [[Integer]]
deran x
    | x <= 1 = [[]]
    | otherwise = [ perm | perm <- perms [0..x-1], isDerangement perm [0..x-1]]

-- Copied from assignment 1
perms :: [a] ->[[a]]
perms [] = [[]]
perms (x:xs) = concat (map (insrt x) (perms xs)) where
insrt x [] = [[x]]
insrt x (y:ys) = (x:y:ys) : map (y:) (insrt x ys)

-- IBAN Assignment
-- Time Taken:  50 minutes

iban :: String -> Bool
iban xs = calculateModulo97 (convertLetters (shiftFour (removeNonAlphaNum xs))) == 1

removeNonAlphaNum :: String -> String
removeNonAlphaNum [] = []
removeNonAlphaNum (x:xs)
    | isAlphaNum x =  x : removeNonAlphaNum xs 
    | otherwise = removeNonAlphaNum xs

shiftFour :: String -> String
shiftFour (a:b:c:d:xs) = xs ++ [a:b:c:d]
shiftFour _ = []

convertLetters :: String -> String
convertLetters [] = []
convertLetters (x:xs)
    |  x == 'a' || x == 'A' = concat ["10", (convertLetters xs)]
    |  x == 'b' || x == 'B' = concat ["11", (convertLetters xs)]
    |  x == 'c' || x == 'C' = concat ["12", (convertLetters xs)]
    |  x == 'd' || x == 'D' = concat ["13", (convertLetters xs)]
    |  x == 'e' || x == 'E' = concat ["14", (convertLetters xs)]
    |  x == 'f'  || x == 'F' = concat ["15", (convertLetters xs)]
    |  x == 'g' || x == 'G' = concat ["16", (convertLetters xs)]
    |  x == 'h' || x == 'H' = concat ["17", (convertLetters xs)]
    |  x == 'i'  || x == 'I' = concat ["18", (convertLetters xs)]
    |  x == 'j'  || x == 'J' = concat ["19", (convertLetters xs)]
    |  x == 'k' || x == 'K' = concat ["20", (convertLetters xs)]
    |  x == 'l'  || x == 'L' = concat ["21", (convertLetters xs)]
    |  x == 'm' || x == 'M' = concat ["22", (convertLetters xs)]
    |  x == 'n' || x == 'N' = concat ["23", (convertLetters xs)]
    |  x == 'o' || x == 'O' = concat ["24", (convertLetters xs)]
    |  x == 'p' || x == 'P' = concat ["25", (convertLetters xs)]
    |  x == 'q' || x == 'Q' = concat ["26", (convertLetters xs)]
    |  x == 'r' || x == 'R' = concat ["27", (convertLetters xs)]
    |  x == 's' || x == 'S' = concat ["28", (convertLetters xs)]
    |  x == 't' || x == 'T' = concat ["29", (convertLetters xs)]
    |  x == 'u' || x == 'U' = concat ["30", (convertLetters xs)]
    |  x == 'v' || x == 'V' = concat ["31", (convertLetters xs)]
    |  x == 'w' || x == 'W' = concat ["32", (convertLetters xs)]
    |  x == 'x' || x == 'X' = concat ["33", (convertLetters xs)]
    |  x == 'y' || x == 'Y' = concat ["34", (convertLetters xs)]
    |  x == 'z' || x == 'Z' = concat ["35", (convertLetters xs)]
    | otherwise = x : convertLetters xs

calculateModulo97 :: String -> Integer
calculateModulo97 xs
    | length xs > 9 = calculateModulo97 (modulo97FirstNine xs)
    | otherwise = (read xs) `mod` 97
    
modulo97FirstNine :: String -> String
modulo97FirstNine [] = []
modulo97FirstNine xs = concat [show ((read (take 9 xs)) `mod` 97) , drop 9 xs]

