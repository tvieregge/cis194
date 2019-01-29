module Lib
    ( hanoi
    , parseMessage
    , parse
    , insert
    , inOrder
    , skips
    , localMaxima
    , localMaxima'
    ) where

import Log

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n p1 p2 p3 =
    hanoi (n - 1) p1 p3 p2 ++ [(p1, p3)] ++ hanoi (n - 1) p2 p1 p3

parseMessage :: String -> LogMessage
parseMessage line =
    case words line of
        ("I":ts:logMsg) -> LogMessage Info (read ts) (unwords logMsg)
        ("W":ts:logMsg) -> LogMessage Warning (read ts) (unwords logMsg)
        ("E":errNo:ts:logMsg) ->
            LogMessage (Error (read errNo)) (read ts) (unwords logMsg)
        _ -> Unknown line

parse :: String -> [LogMessage]
parse file = map parseMessage $ lines file

insert :: LogMessage -> MessageTree -> MessageTree
-- insert (Unknown str) msgTree                  = msgTree
insert logEntry Leaf = Node Leaf logEntry Leaf
insert il@(LogMessage _ insertTs _) (Node left nl@(LogMessage _ nodeTs _) right)
    | insertTs == nodeTs =
        Node Leaf (Unknown "Tim: parse error, timestamps are equal.") Leaf
    | insertTs >= nodeTs = Node left nl (insert il right)
    | insertTs <= nodeTs = Node (insert il left) nl right

inOrder :: MessageTree -> [LogMessage]
inOrder mTree = aux mTree []
  where
    aux :: MessageTree -> [LogMessage] -> [LogMessage]
    aux Leaf = id
    aux (Node left logMsg right) = (aux left) . (logMsg :) . aux right

skips :: [a] -> [[a]]
skips l = map (\x -> every x l) [1 .. (length l)]

every :: Int -> [a] -> [a]
every n l =
    case drop (n - 1) l of
        [] -> []
        (x:xs) -> x : (every n xs)

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
    | y > x && y > z = y : localMaxima (y : z : zs)
    | otherwise = localMaxima (y : z : zs)
localMaxima _ = []

localMaxima' :: [Integer] -> [Integer]
localMaxima' (x: r@(y:z:_)) = (if  y > x && y > z then [y] else []) ++ localMaxima r

