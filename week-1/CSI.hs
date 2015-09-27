{-# OPTIONS_GHC -Wall #-}
module CSI where

data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
			
boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: Boy -> Boy -> Bool
says Carl Arnold = False 
says Jack Matthew = False
says Jack Peter = False
says x y = undefined

getLiars :: [Boy] -> Bool
getLiars 

expression :: Boy -> Bool


accusers :: Boy -> [Boy]

guilty :: [Boy]

honest :: [Boy]