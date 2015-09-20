module Lab3 where
import Data.List
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
--  so we just checked definition with a few short formulas.
equiv :: Form -> Form -> Bool
equiv f g = truths f == truths g

--Exercise 2
--Our goal is to test this function

-- Precondition, string consists of digits,'(',')','*','+','-','==>','<=>'
--parse :: String -> [Form]
--parse s = [ f | (f,_) <- parseForm (lexer s) ]
-- Postcondition, list of Forms corresponding to the used strings

--See Tests.hs

--Exercise 3
cnf :: Form -> Form
cnf = distribute.nnf.arrowfree

--Parse a given string, convert the given Forms to CNF, Show the forms
stringToCNF:: String -> String
stringToCNF  = showLst.convertToCNF.parse

--Make a Form in the Form list arrowfree, move negations inward, and distribute Disjunctions
convertToCNF:: [Form] -> [Form]
convertToCNF [] = []
convertToCNF (f:fs) = (distribute.nnf.arrowfree) f : convertToCNF fs

--Negations and prop's can be returned immedately, Conjunction only need the forms within distributed
-- Perform distrubition specifically for Disjunctions
distribute:: Form -> Form
distribute (Prop x) = Prop x
distribute (Neg (Prop x)) = Neg (Prop x)
distribute (Cnj fs) = Cnj (map distribute fs)
distribute (Dsj fs) = distributeDsjFormList fs
distribute _ = undefined

--Perform disjucntion distribution on all Forms in a Disjuntion
distributeDsjFormList:: [Form] -> Form
distributeDsjFormList [] = error "Disjunction on one element?"
distributeDsjFormList [f] = f
distributeDsjFormList (f : fs) = distributeDsj' f (distributeDsjFormList fs)

--If either the first or the second argument is a Conjuction apply distribution according to Distributive laws of Disjunctions
--In any other case distribute the 2 forms and add them to a disjunction.
distributeDsj':: Form -> Form -> Form
distributeDsj' f1 (Cnj(f2:f3)) = Cnj [Dsj[f1', f2'] ,Dsj(f1':f3')] where
                         f1' = distribute f1
                         f2' = distribute f2
                         f3' = map (\f -> distribute f ) f3
distributeDsj' (Cnj(f2:f3)) f1 = Cnj [Dsj[f1', f2'] ,Dsj(f1':f3')] where
                                 f1' = distribute f1
                                 f2' = distribute f2
                                 f3' = map (\f -> distribute f ) f3
distributeDsj' f1 f2 = Dsj [f1', f2'] where
                       f1' = distribute f1
                       f2' = distribute f2
