module Lab3 where
import System.Random
import Lecture3

-- Exercise 1
-- Time: 2 Hours

-- Makes a List of truth values for Form f
truths :: Form -> [Bool]
truths f = map (\v -> evl v f) (allVals f)

-- A contradiction is where all truth values are False
-- Definition is very straightforward, just checked definition with a few short formulas.
contradiction :: Form -> Bool
contradiction f = all (\x -> not x) (truths f)

-- A Tautology is where all truth values are True
-- Definition is straightforward, just checked definition with a few short formulas.
tautology :: Form -> Bool
tautology f = all (\x-> x) (truths f)

-- Entailment is where if F is true, G is true too.
-- Checked definition with a couple tailor made formulas.
-- Couldn't find information on where entailment can still be achieved with differing number
-- of variables, so we assumed that it cannot.
entail :: Form -> Form -> Bool
entail f g = satisfiable f &&
                 length (truths f) == length (truths g) &&
                 isEntailment (zip (truths f) (truths g))

isEntailment :: [(Bool,Bool)] -> Bool
isEntailment [] = True
isEntailment ((x,y):xs)
     | x && not y = False
     | otherwise = isEntailment xs

-- Equivalence is where the truth table of F equals the truth table of G
-- Definition is very straightforward and relies heavily on the evl and allVals functions from the lecture, which we assume to be correct
-- so we just checked definition with a few short formulas.
equiv :: Form -> Form -> Bool
equiv f g = truths f == truths g


--Exercise 2

--Our goal is to test this function
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
                 return (Cnj (Prop m:fs))
         3 -> do m <- getRandomInt 5
                 fs <- getRandomForms (d-1) m
                 return (Dsj (Prop m:fs))
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
test n pr (f:fs) = if pr f then do print ("pass on: " ++ show f)
                                   test n pr fs
                   else error ("fail on: " ++ show f)

testForms :: Int -> (Form -> Bool) -> IO ()
testForms n pr = do
    fs <- getRandomFs n
    test n pr fs

testParser :: IO()
testParser = testForms 50  (\ f -> let [g] = parse (show f) in f == g)


-- Exercise 3
-- Function to transform a form into a CNF
cnf :: Form -> Form
cnf = distribute.nnf.arrowfree

-- Parse a given string, convert the given Forms to CNF, Show the forms
stringToCNF:: String -> String
stringToCNF  = showLst.convertToCNF.parse

-- Make a Form in the Form list arrowfree, move negations inward, and distribute Disjunctions
convertToCNF:: [Form] -> [Form]
convertToCNF [] = []
convertToCNF (f:fs) = (distribute.nnf.arrowfree) f : convertToCNF fs

-- Negations and prop's can be returned immedately, Conjunction only need the forms within distributed
-- Perform distrubition specifically for Disjunctions
distribute:: Form -> Form
distribute (Prop x) = Prop x
distribute (Neg (Prop x)) = Neg (Prop x)
distribute (Cnj fs) = Cnj (map distribute fs)
distribute (Dsj fs) = distributeDsjFormList fs
-- This case will never happen because it is a precondition of the function to not contain "arrow" functions.
distribute _ = undefined

-- Perform disjucntion distribution on all Forms in a Disjuntion
distributeDsjFormList:: [Form] -> Form
distributeDsjFormList [] = error "Disjunction on one element?"
distributeDsjFormList [f] = f
distributeDsjFormList (f : fs) = distributeDsj' f (distributeDsjFormList fs)

-- If either the first or the second argument is a Conjuction apply distribution according to Distributive laws of Disjunctions
-- In any other case distribute the 2 forms and add them to a disjunction.
distributeDsj':: Form -> Form -> Form
distributeDsj' f1 (Cnj(f2:f3)) = Cnj [Dsj[f1', f2'] ,Dsj(f1':f3')] where
                                 f1' = distribute f1
                                 f2' = distribute f2
                                 f3' = map distribute f3
distributeDsj' (Cnj(f2:f3)) f1 = Cnj [Dsj[f1', f2'] ,Dsj(f1':f3')] where
                                 f1' = distribute f1
                                 f2' = distribute f2
                                 f3' = map distribute f3
distributeDsj' f1 f2 = Dsj [f1', f2'] where
                       f1' = distribute f1
                       f2' = distribute f2

-- Exercise 4
-- Time spent: 2 hours.

-- The purpose of this exercise is to test the correctness of the previous exercise.
-- The exercise defines a function to transform a valid boolean formula into a CNF.
-- CNF are formulas that hold the following properties:
-- (Source: https://en.wikipedia.org/wiki/Conjunctive_normal_form)
-- * They only allow operations AND, OR and negations.
-- * The negation can be only applied on the literals.
-- * The formula is expressed conjuntion of clauses which are literals or disjunctions.
--
-- Therefore, the output of the function must hold these properties and be equivalent
-- to the original formula to be a correct output.

-- Tests if it only contains AND, OR and negations.
isArrowFree :: Form -> Bool
isArrowFree (Prop _)    = True
isArrowFree (Neg f)     = isArrowFree f
isArrowFree (Cnj fs)    = all isArrowFree fs
isArrowFree (Dsj fs)    = all isArrowFree fs
isArrowFree (Impl _ _)  = False
isArrowFree (Equiv _ _) = False

-- Tests if the only literals are negated.
noNegatedFunctions :: Form -> Bool
noNegatedFunctions (Prop _) = True
noNegatedFunctions (Neg (Prop _))  = True
noNegatedFunctions (Neg (Cnj _)) = False
noNegatedFunctions (Neg (Dsj _)) = False
noNegatedFunctions (Neg (Impl _ _ )) = False
noNegatedFunctions (Neg (Equiv _ _)) = False
noNegatedFunctions (Neg (Neg f)) = noNegatedFunctions f
noNegatedFunctions (Cnj fs) = all noNegatedFunctions fs
noNegatedFunctions (Dsj fs) = all noNegatedFunctions fs
noNegatedFunctions (Impl f g) = noNegatedFunctions f && noNegatedFunctions g
noNegatedFunctions (Equiv f g) = noNegatedFunctions f && noNegatedFunctions g

-- A CNF is a conjunction of cluases where a clause is a literal or a disjunction of literals.
-- This functions test if the given formula is a clause.
isClause :: Form -> Bool
isClause (Prop _) = True
isClause (Neg f) = isClause f
isClause (Dsj fs) = all isClause fs
isClause _ = False

-- Test if the given formula is a conjunction of clauses.
isConjOfClauses :: Form -> Bool
isConjOfClauses (Cnj fs) = all isClause fs
isConjOfClauses f = isClause f

-- Finally, we define a function that combines the three previous properties.
-- By design, all the functions cover all possible cases, even if they are not expected
-- (for instance, an implication while testing if it is a clause). The design idea
-- was to be able to combine all the properties is just one. This approach is slower
-- because there are redundant tests but it is easier to combine all the properties.
isCNF :: Form -> Bool
isCNF f = isArrowFree f && noNegatedFunctions f && isConjOfClauses f

-- In order to test the formulas, we are using the functions defined in the exercise 2
-- This could be done using QuickCheck but due to time restrictions, it has not been
-- possible.

-- First we want tovalidate the properties running them against the examples in the
-- reference.
cnf1 :: Form
cnf1 = Cnj [Neg(Prop 1), Dsj[Prop 2, Prop 3]]

cnf2 :: Form
cnf2 = Cnj [Dsj[Prop 1, Prop 2], Dsj[Neg(Prop 2), Prop 3, Prop 4], Dsj[Prop 4, Neg(Prop 5)]]

cnf3 :: Form
cnf3 = Cnj[Prop 1, Prop 2]

cnf4 :: Form
cnf4 = Dsj [Prop 1, Prop 2]

notcnf1 :: Form
notcnf1 = Neg(Dsj[Prop 1, Prop 2])

notcnf2 :: Form
notcnf2 = Dsj [Prop 1, Cnj [Prop 2, Prop 3]]

notcnf3 :: Form
notcnf3 = Cnj [Prop 1, Dsj[Prop 2, Cnj [Prop 3, Prop 4]]]

-- Test the valid cases.
testValidCNF :: IO()
testValidCNF = test 4 isCNF [cnf1, cnf2, cnf3, cnf4]

-- Test the invalid cases.
testInvalidCNF :: IO()
testInvalidCNF = test 3 (not.isCNF) [notcnf1, notcnf2, notcnf3]

-- Finally we define the test function.
-- It should return true if the output is a CNF and the input and the output
-- are equivalent. They are equivalent if the equivalent operation <=> is a tautology.
isCNFandEquiv :: Form -> Bool
isCNFandEquiv f = let f' = cnf f in isCNF f' && tautology (Equiv f f')

testCNF :: IO()
testCNF = testForms 50 isCNFandEquiv
