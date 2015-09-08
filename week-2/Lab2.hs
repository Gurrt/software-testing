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
