{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Tree
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = ef}) (GL es glf) = GL (e : es) (ef + glf)

instance Semigroup GuestList where
    (<>) (GL es1 fun1) (GL es2 fun2) = GL (es1 <> es2) (fun1 + fun2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a ts) = f a $ map (treeFold f) ts

-- Employee : Boss of current subtree
-- [(gl,gl)] : List of the results for each subtree
-- (gl,gl) : overall best
-- first in pair is best with sub boss second is best without
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (bestWithBoss, bestWithoutBoss)
  where
    bestWithBoss = glCons boss $ foldr (<>) (mempty :: GuestList) $ map snd gls
    bestWithoutBoss = foldr (<>) mempty $ map (uncurry max) gls

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry max res
  where
    res = treeFold nextLevel t

format :: GuestList -> String
format (GL emps fun) = funLine ++ (mconcat $ sort names)
    where funLine = "Total fun: " ++ show fun ++ "\n"
          names = foldr (\x acc -> formatEmp x : acc) [] emps
          formatEmp e = (show $ empName e) ++ "\n"

-- main :: IO()
-- main = readFile "test/company.txt" >>= readFn
--     where readFn s = putStrLn . format . maxFun $ read s
