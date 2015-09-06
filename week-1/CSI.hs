{-# OPTIONS_GHC -Wall #-}
module CSI where

data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)

boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- give you True only when boy x accuses boy y of stealing
says :: Boy -> Boy -> Bool
-- Matthew: Carl didn’t do it, and neither did I.
-- Redundant says Matthew Matthew = False
-- Redundant says Matthew Carl = False
-- Bit confused about the semantics of the word "or" here
-- As peter says it was Matthew "or" Jack I would assume it is exclusive but I'm not sure
says Peter Matthew = True
-- Jack Matthew and Peter are both lying.
says Jack x = (not $ says Peter x )&&  (not $ says Matthew x)
-- Arnold Matthew or Peter is speaking the truth, but not both.
says Arnold x = says Matthew x /= says Peter x
-- Carl What Arnold says is not true.
says Carl x = not $ says Arnold x
says _ _ = False

accusers :: Boy -> [Boy]
accusers accused = [accuser | accuser <- boys, says accuser accused]

guilty :: [Boy]
guilty = undefined

honest :: [Boy]
honest = undefined

totalAccusations :: [Boy] -> [Boy]
totalAccusations [] = []
totalAccusations (x:xs) = accusers x ++ totalAccusations xs

accusationsBoysGive :: [(Boy,Int)]
accusationsBoysGive = [(boy, countAccusationsByBoy boy)| boy <- boys]

countAccusationsByBoy :: Boy -> Int
countAccusationsByBoy boy = count boy (totalAccusations boys)

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)