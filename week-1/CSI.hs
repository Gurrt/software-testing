{-# OPTIONS_GHC -Wall #-}
module CSI where

data Boy = Matthew | Peter | Jack | Arnold | Carl
            deriving (Eq,Show)
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- says accuser accused = True
says :: Boy -> Boy -> Bool

-- None of the kids has claimed guilty, so we assume that all of them claim inocence.
says Matthew Matthew = False
says Peter Peter = False
says Jack Jack = False
says Arnold Arnold = False
says Carl Carl = False

-- Matthew: Carl didn’t do it, and neither did I.
-- says Matthew Matthew = False
says Matthew Carl = False
says Matthew _ = True

-- Peter: It was Matthew or it was Jack.
says Peter Matthew = True
says Peter Jack = True
says Peter _ = False

-- Jack: Matthew and Peter are both lying.
says Jack y = not(says Matthew y) && not(says Peter y)

-- Arnold: Matthew or Peter is speaking the truth, but not both.
says Arnold y =  (says Matthew y && not(says Peter y)) || (not(says Matthew y) && says Peter y)

-- Carl: What Arnold says is not true.
says Carl y = not(says Arnold y)

-- The accusers of a boy is each boy that says he is guilty.
accusers :: Boy -> [Boy]
accusers accused = [accuser | accuser <- boys, says accuser accused]

-- The one that all the honest kids point. Only 3 will say the truth, therefore,
-- the one accused by 3 is the right one.
guilty :: [Boy]
guilty = [boy | boy <- boys, length (accusers boy) == 3]

-- The lier is didn't claim guilty therefore is a lier. The kid that didn't accused him too.
honest :: [Boy]
honest = accusers (head guilty)
