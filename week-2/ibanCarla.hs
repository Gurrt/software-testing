import Data.List
import Data.Char
import Data.Maybe
import Control.Monad

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
--replaceLetters (x:xs) | x = findLetter x xs

findLetter:: String -> String
findLetter a = fromJust (lookup a [("A","10"),("B","11"),("C","12"),("D","13"),("E","14"),("F","15"),("G","16"),("H","17"),("I","18"),("J","19"),("K","20"),("L","21"),("M","22"),("N","23"),("O","24"),("P","25"),("Q","26"),("R","27"),("S","28"),("T","29"),("U","30"),("V","31"),("W","32"),("X","33"),("W","34"),("Z","35")] ) 








