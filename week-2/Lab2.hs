module Lab2 where
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

    triangle :: Integer -> Integer -> Integer -> Shape
    triangle a b c
        | not (isTriangle a b c) = NoTriangle
        | isRectangularTriangle a b c = Rectangular
        | isEquilateralTriangle a b c = Equilateral
        | isIsoscelesTriangle a b c = Isosceles
        | otherwise = Other

    -- Permutation Assignment
    -- Time taken : 30 minutes
    isPermutation :: Eq a => [a] -> [a] -> Bool
    isPermutation xs ys = (length xs == length ys) && all (`elem` ys) xs

    -- Derangement Assignment
    -- Time taken: 1 Hour
    isDerangement :: [Integer] -> [Integer] -> Bool
    isDerangement xs ys
        | not (isPermutation xs ys) = False
        | otherwise = recDerangementCheck xs ys

    recDerangementCheck :: [Integer] -> [Integer] -> Bool
    recDerangementCheck [] (_:_) = False
    recDerangementCheck (_:_) [] = False
    recDerangementCheck [] [] = True
    recDerangementCheck (x:xs) (y:ys)
        | x == y = False
        | otherwise = recDerangementCheck xs ys

    deran :: Integer -> [[Integer]]
    deran x
        | x <= 1 = [[]]
        | otherwise = [ perm | perm <- perms [0..x-1], isDerangement perm [0..x-1]]

    -- Copied from assignment 1
    perms :: [a] ->[[a]]
    perms [] = [[]]
    perms (x:xs) = concatMap (insrt x) (perms xs) where
        insrt a [] = [[a]]
        insrt a (b:bs) = (a:b:bs) : map (b:) (insrt a bs)

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
    shiftFour (a:b:c:d:xs) = xs ++ [a] ++ [b] ++ [c] ++ [d]
    shiftFour _ = []

    convertLetters :: String -> String
    convertLetters [] = []
    convertLetters (x:xs)
        |  x == 'a' || x == 'A'  = "10" ++ convertLetters xs
        |  x == 'b' || x == 'B'  = "11" ++ convertLetters xs
        |  x == 'c' || x == 'C'  = "12" ++ convertLetters xs
        |  x == 'd' || x == 'D'  = "13" ++ convertLetters xs
        |  x == 'e' || x == 'E'  = "14" ++ convertLetters xs
        |  x == 'f' || x == 'F' = "15" ++ convertLetters xs
        |  x == 'g' || x == 'G'  = "16" ++ convertLetters xs
        |  x == 'h' || x == 'H'  = "17" ++ convertLetters xs
        |  x == 'i' || x == 'I' = "18" ++ convertLetters xs
        |  x == 'j' || x == 'J' = "19" ++ convertLetters xs
        |  x == 'k' || x == 'K'  = "20" ++ convertLetters xs
        |  x == 'l' || x == 'L' = "21" ++ convertLetters xs
        |  x == 'm' || x == 'M'  = "22" ++ convertLetters xs
        |  x == 'n' || x == 'N'  = "23" ++ convertLetters xs
        |  x == 'o' || x == 'O'  = "24" ++ convertLetters xs
        |  x == 'p' || x == 'P'  = "25" ++ convertLetters xs
        |  x == 'q' || x == 'Q'  = "26" ++ convertLetters xs
        |  x == 'r' || x == 'R'  = "27" ++ convertLetters xs
        |  x == 's' || x == 'S'  = "28" ++ convertLetters xs
        |  x == 't' || x == 'T'  = "29" ++ convertLetters xs
        |  x == 'u' || x == 'U'  = "30" ++ convertLetters xs
        |  x == 'v' || x == 'V'  = "31" ++ convertLetters xs
        |  x == 'w' || x == 'W'  = "32" ++ convertLetters xs
        |  x == 'x' || x == 'X'  = "33" ++ convertLetters xs
        |  x == 'y' || x == 'Y'  = "34" ++ convertLetters xs
        |  x == 'z' || x == 'Z'  = "35" ++ convertLetters xs
        | otherwise = x : convertLetters xs

    calculateModulo97 :: String -> Integer
    calculateModulo97 xs
        | length xs > 9 = calculateModulo97 (modulo97FirstNine xs)
        | otherwise = read xs `mod` 97

    modulo97FirstNine :: String -> String
    modulo97FirstNine [] = []
    modulo97FirstNine xs = show (read (take 9 xs) `mod` 97) ++ drop 9 xs
