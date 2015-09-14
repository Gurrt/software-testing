module Lab3 where
import Data.List
import System.Random
import Lecture3

truths :: Form -> [Bool]
truths f = map (\v -> evl v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = all (\x -> not x) (truths f)

tautology :: Form -> Bool
tautology f = all (\x-> x) (truths f)

entail :: Form -> Form -> Bool
entail f g = satisfiable f &&
                 length (truths f) == length (truths g) &&
                 isEntailment (zip (truths f) (truths g))
                 
isEntailment :: [(Bool,Bool)] -> Bool
isEntailment [] = True
isEntailment ((x,y):xs)
     | x && not y = False
     | otherwise = isEntailment xs

equiv :: Form -> Form -> Bool
equiv f g = truths f == truths g