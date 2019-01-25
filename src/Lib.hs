module Lib
    ( hanoi,
      parseMessage,
      parse
    ) where

-- import           Data
import           Log

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _    = []
hanoi n p1 p2 p3 = hanoi (n-1) p1 p3 p2 ++ [(p1, p3)] ++ hanoi (n-1) p2 p1 p3

parseMessage :: String -> LogMessage
parseMessage line = case words line of
                         ("I":ts:errMsg) -> LogMessage Info (read ts) (unwords errMsg)
                         ("W":ts:errMsg) -> LogMessage Warning (read ts) (unwords errMsg)
                         ("E":errNo:ts:errMsg) -> LogMessage (Error (read errNo))
                                                             (read ts)
                                                             (unwords errMsg)
                         _ -> Unknown line

parse :: String -> [LogMessage]
parse file = map parseMessage $ lines file
