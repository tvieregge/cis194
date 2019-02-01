module JoinList where

import Sized

data JoinList m a
    = Empty
    | Single m
             a
    | Append m
             (JoinList m a)
             (JoinList m a)
    deriving (Eq, Show)

testjl =
    Append
        (Size 4)
        (Append
             (Size 3)
             (Single (Size 1) " y ")
             (Append (Size 2) (Single (Size 1) " e ") (Single (Size 1) " a ")))
        (Single (Size 1) " h ")

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) ja jb = Append ((tag ja) `mappend` (tag jb)) ja jb

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append m ja jb)
    | i < leftChildSize = indexJ i ja
    | otherwise = indexJ (i - leftChildSize) jb
  where
    leftChildSize = getSize . size $ tag ja

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i j
    | i <= 0 = j
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
dropJ i (Append m jl jr)
    | i == nodeSize = Empty
    | i <= leftSize = (dropJ i jl) +++ jr
    | otherwise = dropJ (i - leftSize) jr
  where
    leftSize = getSize . size $ tag jl
    nodeSize = getSize $ size m

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i _
    | i <= 0 = Empty
takeJ i s@(Single _ _) = s
takeJ _ Empty = Empty
takeJ i j@(Append m jl jr)
    | i == nodeSize = j
    | i <= leftSize = (takeJ i jl)
    | otherwise = jl +++ takeJ (i - leftSize) jr
  where
    leftSize = getSize . size $ tag jl
    nodeSize = getSize $ size m
