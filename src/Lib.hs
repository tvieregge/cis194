module Lib
    ( hanoi
    ) where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _    = []
hanoi n p1 p2 p3 = hanoi (n-1) p1 p3 p2 ++ [(p1, p3)] ++ hanoi (n-1) p2 p1 p3
