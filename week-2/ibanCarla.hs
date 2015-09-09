-- must be improve
import Data.List
import Data.Char

iban :: String -> Bool
iban s = ((read (stringCleaned s)  `mod` 97) == 1) || (specialMod97 (stringCleaned s))

specialMod97 :: String -> Bool 
specialMod97 xs = (read (concat[show (read (take 9 (concat [show ((read (take 9 xs)) `mod` 97) , drop 9 xs])) `mod` 97), drop 16 xs]) `mod` 97)== 1

stringCleaned :: String -> String
stringCleaned x = x



