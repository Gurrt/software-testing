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

