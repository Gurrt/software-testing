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


-- test cases
-- always false
contradictions = [(Cnj [p, (Neg p)]),                           -- p AND ~p
                  (Equiv p (Neg p)),                            -- p <-> ~p
                  (Neg (Dsj [p, (Neg p)])),                     -- ~(p OR ~p)
                  (Impl (Dsj [p, (Neg p)]) (Cnj [p, (Neg p)]))] -- (p OR ~p) -> (p AND ~p)
-- always true
tautologies    = [(Dsj [p, (Neg p)]),                           -- p OR ~p
                  (Equiv p p),                                  -- p <-> p
                  (Impl (Cnj [p, q]) q)]                        -- (p AND q) -> q
-- entailment
entailments    = [(p, p),                                       -- p |= p
                 ((Cnj [p, q]), (Dsj [p, q])),                  -- p AND q |= p OR q
                 ((Cnj [p, q]), p)]                             -- p AND q |= p
-- same valuations
equivalences   = [((Cnj [p, q]), (Neg(Dsj [Neg(p), Neg(q)])))]  -- (p AND q) <=> ~(~p OR ~q)

-- combined test cases
allUnaryTests = [(contradictions, contradiction, "Contradictions"),
                  (tautologies, tautology, "Tautologies")]
allBinaryTests = [(entailments, entails, "Entailments"),
                   (equivalences, equiv, "Equivalences")]

-- single assertion
testUnary :: [Form] -> (Form -> Bool) -> Bool
testUnary tests fn = and (map (\ item -> fn item) tests)

-- compare assertion
testBinary :: [(Form, Form)] -> (Form -> Form -> Bool) -> Bool
testBinary tests fn = and (map (\ (item1, item2) -> fn item1 item2) tests)

-- run test suite for unary functions
runUnaryTests :: [([Char],Bool)]
runUnaryTests = map (\ (testCase, fn, name) -> (name, (testUnary testCase fn))) allUnaryTests

-- run test suite for comparing formulas
runBinaryTests :: [([Char],Bool)]
runBinaryTests = map (\ (testCase, fn, name) -> (name, (testBinary testCase fn))) allBinaryTests

-- run whole test suite
runLogicTests :: [([Char],Bool)]
runLogicTests = runUnaryTests ++ runBinaryTests


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


