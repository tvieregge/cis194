module Lib
    ( hanoi,
      parseMessage,
      parse,
      insert
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
                         ("I":ts:logMsg) -> LogMessage Info (read ts) (unwords logMsg)
                         ("W":ts:logMsg) -> LogMessage Warning (read ts) (unwords logMsg)
                         ("E":errNo:ts:logMsg) -> LogMessage (Error (read errNo))
                                                             (read ts)
                                                             (unwords logMsg)
                         _ -> Unknown line

parse :: String -> [LogMessage]
parse file = map parseMessage $ lines file

insert :: LogMessage -> MessageTree -> MessageTree
-- insert (Unknown str) msgTree                  = msgTree
insert logEntry Leaf             = Node Leaf logEntry Leaf
insert il@(LogMessage _ insertTs _) (Node left nl@(LogMessage _ nodeTs _) right)
    | insertTs == nodeTs = Node Leaf (Unknown "Tim: parse error, timestamps are equal.") Leaf
    | insertTs >= nodeTs = Node left nl (insert il right)
    | insertTs <= nodeTs = Node (insert il left) nl right
