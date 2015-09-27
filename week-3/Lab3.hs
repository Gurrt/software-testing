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



--Exercise 2

-- Our goal is to test this function
-- Precondition, string consists of digits,'(',')','*','+','-','==>','<=>'
-- parse :: String -> [Form]
-- parse s = [ f | (f,_) <- parseForm (lexer s) ]
-- Postcondition, list of Forms corresponding to the used strings

-- We took a well know function as starting point in order to generate random
-- formulas that generates an integer of n numbers, then it generates a random
-- formula with limit 5 for this case. In case of formulas with zero levels, it
-- generates an arbitrary number of indices. We make an arrangement for formulas
-- until 5 levels in order to fulfil some syntactic forms and then generate
-- random formulas with variable levels. Then we create the test function with
-- 50 random generated formulas showing the ones that pass the parsing test and
-- the ones that do not.

-- To execute the test, run
-- testParser


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



-- exercise 3

-- precondition: input forms are in cnf
dist' :: Form -> Form -> Form
dist' p (Cnj fs) = Cnj (map (\ x -> dist' p x) fs)
dist' (Cnj fs) q = Cnj (map (\ x -> dist' x q) fs)
dist' (Dsj ps) (Dsj qs) = Dsj (ps ++ qs)  {- These three lines prevent unnecessary Dsj nesting -}
dist' (Dsj ps) q = Dsj (ps ++ [q])
dist' p (Dsj qs) = Dsj (p : qs)
dist' p q = Dsj [p, q]

-- precondition: all input forms are in cnf
dist :: [Form] -> Form
dist [] = error "empty list"
dist [f] = f
dist (f : fs) = dist' f (dist fs)

-- precondition: input is in nnf
cnf :: Form -> Form
cnf (Prop p) = Prop p
cnf (Neg p) = Neg p
cnf (Cnj fs) = Cnj (map cnf fs)
cnf (Dsj fs) = dist (map cnf fs)

-- Function that can transform any formula to CNF
formToCNF :: Form -> Form
formToCNF = cnf . nnf . arrowfree

