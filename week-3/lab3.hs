module Lab3 where
    
import Data.List
import System.Random
import Lecture3

--exercise 1

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)

entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)



--exercise 2


-- Generating random formulas

getRandomInt :: Int -> IO Int 
getRandomInt n = getStdRandom (randomR (0,n))

getRandomF :: IO Form 
getRandomF = do d <- getRandomInt 5 
                getRandomForm d

getRandomForm :: Int -> IO Form 
getRandomForm 0 = do m <- getRandomInt 20 
                     return (Prop (m+1))

getRandomForm d = do 
 n <- getRandomInt 5 
 case n of 
  0 -> do m <- getRandomInt 20 
          return (Prop (m+1)) 
  1 -> do f <- getRandomForm (d-1) 
          return (Neg f) 
  2 -> do m <- getRandomInt 5 
          fs <- getRandomForms (d-1) m 
          return (Cnj fs) 
  3 -> do m <- getRandomInt 5 
          fs <- getRandomForms (d-1) m 
          return (Dsj fs) 
  4 -> do f <- getRandomForm (d-1) 
          g <- getRandomForm (d-1) 
          return (Impl f g) 
  5 -> do f <- getRandomForm (d-1) 
          g <- getRandomForm (d-1) 
          return (Equiv f g)


getRandomFs :: Int -> IO [Form] 
getRandomFs n = do d <- getRandomInt 3 
                   getRandomForms d n


getRandomForms :: Int -> Int -> IO [Form] 
getRandomForms _ 0 = return [] 
getRandomForms d n = do
        f <- getRandomForm d 
        fs <- getRandomForms d (n-1) 
        return (f:fs)

test :: Int -> (Form -> Bool) -> [Form] -> IO () 
test n _ [] = print (show n ++ " Valid") 
test n p (f:fs) = 
  if p f 
  then do print ("pass on: " ++ show f) 
          test n p fs
  else error ("fail on: " ++ show f)

testForms :: Int -> (Form -> Bool) -> IO () 
testForms n p = do
        fs <- getRandomFs n
        test n p fs

-- Testing parse function

testParserFunc = testForms 50 
	(\ f -> let [g] = parse (show f) in f == g)



