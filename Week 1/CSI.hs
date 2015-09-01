{-# OPTIONS_GHC -Wall #-}
module HW01 where

data Boy = Matthew | Peter | Jack | Arnold | Carl 
            deriving (Eq,Show)
			
boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: Boy -> Boy -> Bool

accusers :: Boy -> [Boy]

guilty :: [Boy]

honest :: [Boy]