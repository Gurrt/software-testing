module Lab3 where

import Data.List
import System.Random
import Lecture3

stringToCNF:: String -> String
stringToCNF s = (showLst.convertToCNF.parse) s

convertToCNF:: [Form] -> [Form]
convertToCNF [] = []
convertToCNF (f:fs) = (nnf.arrowfree) f : convertToCNF fs

--orsInward:: Form -> Form
--orsInward (Prop x) = Prop x
--orsInward (Neg (Prop x)) = Neg (Prop x)
--orsInward (Cnj f1:f2:fs) = Dsj[Cnj[f1', f2'], Cnj[f1', f3']]
--where f1' = map orsInward f1
--f2' = map orsInward f2
--f3' = map orsInward f3
