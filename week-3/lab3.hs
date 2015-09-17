module Lab3 where
    
import Data.List
import System.Random
import Lecture3

--exercise 1

contradiction :: Form -> Bool
contradiction f = not (satisfiable f)


tautology :: Form -> Bool
tautology f = all (\ v -> eval v f) (allVals f)


-- | logical entailment 
entails :: Form -> Form -> Bool
entails f g = tautology (Impl f g)


-- | logical equivalence
equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

--exercise 2

-- Exercise 3
-- Time spent: 2.5 hours (1 impl, 1.5 test)
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
-- VVZ: You miss another function that would 'flatten' nested conjunctions and disjunctions. The formulae on the slides used associativity and hence assumed the flattener of x & (y & z) to x & y & z in the head of the reader, but in the implementation your rewritings could make quite a mess of the structure of conjunction/disjunction lists, not to mention that the input is 'any formula', so it can be already messed up.
-- VVZ: formToCNF (Cnj [Cnj [p,q,p], q])
-- VVZ: I expect to see (p AND q AND p AND q), not ((p AND q AND p) AND q)


