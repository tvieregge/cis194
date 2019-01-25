module Lib
    ( hanoi,
      parseMessage
    ) where

-- import           Data
import           Log

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _    = []
hanoi n p1 p2 p3 = hanoi (n-1) p1 p3 p2 ++ [(p1, p3)] ++ hanoi (n-1) p2 p1 p3

parseMessage :: String -> LogMessage
parseMessage iStr
    | (length tokens) < 3 = Unknown iStr
    | "I" == first = LogMessage Info 1 "Info"
    | "W" == first = LogMessage Warning 1 "Warn"
    | "E" == first = LogMessage (Error 1) 1 "Err"
    | otherwise = Unknown iStr
    where tokens = words iStr
          first  = head tokens

-- getTS :: String -> Int
-- getTS iStr = break (' ' ==) iStr
