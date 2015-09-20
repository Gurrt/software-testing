module Lab3 where

import Data.List
import System.Random
import Lecture3

--Exercise 3

stringToCNF:: String -> String
stringToCNF s = (showLst.convertToCNF.parse) s

convertToCNF:: [Form] -> [Form]
convertToCNF [] = []
convertToCNF (f:fs) = (distribute.nnf.arrowfree) f : convertToCNF fs

distribute:: Form -> Form
distribute (Prop x) = Prop x
distribute (Neg (Prop x)) = Neg (Prop x)
distribute (Cnj fs) = Cnj (map distribute fs)
distribute (Dsj fs) = distributeDsjFormList (fs)

distributeDsjFormList:: [Form] -> Form
distributeDsjFormList [] = error "Disjunction on one element?"
distributeDsjFormList [f] = f
distributeDsjFormList (f : fs) = distributeDsj' f (distributeDsjFormList fs)

distributeDsj':: Form -> Form -> Form
distributeDsj' f1 (Cnj(f2:f3)) = Cnj [Dsj[f1', f2'] ,Dsj([f1']++f3')] where
                         f1' = distribute f1
                         f2' = distribute f2
                         f3' = map (\f -> distribute f ) f3
distributeDsj' (Cnj(f2:f3)) f1 = Cnj [Dsj[f1', f2'] ,Dsj([f1']++f3')] where
                                 f1' = distribute f1
                                 f2' = distribute f2
                                 f3' = map (\f -> distribute f ) f3
distributeDsj' f1 f2 = Dsj [f1', f2'] where
                       f1' = distribute f1
                       f2' = distribute f2

