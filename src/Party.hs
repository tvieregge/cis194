{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = ef}) (GL es glf) = GL (e : es) (ef + glf)

instance Semigroup GuestList where
    (<>) (GL es1 fun1) (GL es2 fun2) = GL (es1 <> es2) (fun1 + fun2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL es1 f1) gl2@(GL es2 f2)
    | f1 > f2 = gl1
    | otherwise = gl2

data Tree a = Node
    { rootLabel :: a -- label value
    , subForest :: [Tree a] -- zero or more child trees
    }

treeFold :: (Semigroup b) => (a -> b) -> Tree a -> b
treeFold f (Node a []) = f a
treeFold f (Node a ts) = (f a) <> (foldr1 (<>) (map (treeFold f) ts))

pTestCompany :: Tree Employee
pTestCompany =
    Node
        (Emp "Stan" 9)
        [ Node
              (Emp "Bob" 2)
              [ Node
                    (Emp "Joe" 5)
                    [Node (Emp "John" 1) [], Node (Emp "Sue" 5) []]
              , Node (Emp "Fred" 3) []
              ]
        , Node (Emp "Sarah" 17) [Node (Emp "Sam" 4) []]
        ]

