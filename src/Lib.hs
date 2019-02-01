module Lib
    ( hanoi
    , parseMessage
    , parse
    , insert
    , inOrder
    , skips
    , localMaxima
    , localMaxima'
    , fun1
    , fun1'
    , fun2
    , fun2'
    , foldTree
    , Tree(TNode, TLeaf)
    , eval
    , evalStr
    , Expr(lit, add, mul)
    , fibs2
    ) where

import ExprT
import Log
import Parser

data Stream a =
    Cons a
         (Stream a)

instance Show a => Show (Stream a) where
    show a = concat . map (++ " , ") . map show . take 20 $ streamToList a

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Bool where
    lit n =
        if n <= 0
            then False
            else True
    add = (||)
    mul = (&&)

data Tree a
    = TLeaf
    | TNode Integer
            (Tree a)
            a
            (Tree a)
    deriving (Show, Eq)

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
localMaxima' (x:r@(y:z:_)) =
    (if y > x && y > z
         then [y]
         else []) ++
    localMaxima r

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 1) . iterate collatz
  where
    collatz n
        | even n = n `div` 2
        | otherwise = 3 * n + 1

foldTree :: [a] -> Tree a
foldTree xs = foldr treeInsert TLeaf xs

treeInsert :: a -> Tree a -> Tree a
treeInsert x TLeaf = TNode 0 TLeaf x TLeaf
treeInsert x (TNode h left val right)
    | height left == height right = TNode (h + 1) (treeInsert x left) val right
    | height left > height right = TNode h left val (treeInsert x right)
    | height right > height left = TNode h (treeInsert x left) val right

height :: Tree a -> Integer
height TLeaf = -1
height (TNode h _ _ _) = h

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add left right) = eval left + eval right
eval (Mul left right) = eval left * eval right

evalStr :: String -> Maybe Integer
evalStr s =
    case parseExp Lit Add Mul s of
        Nothing -> Nothing
        Just x -> Just $ eval x

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fib2 :: Integer -> Integer
fib2 n = go 0 1 n
  where
    go prev _ 0 = prev
    go prev prev2 n = go (prev + prev2) prev (n - 1)

fibs2 :: [Integer]
fibs2 = 0 : 1 : next fibs2
  where
    next (a:rest@(b:_)) = a + b : next rest

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a : streamToList b

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) $ streamMap f y

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons (f s) $ streamFromSeed f (f s)

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a1 a2) b =
    Cons a1 $ interleaveStreams b a2

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) $ streamMap (+ 1) ruler
