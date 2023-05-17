{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Party where

import Data.Tree
import Employee

-- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e : es) (fun + empFun e)

instance Semigroup GuestList where
  (<>) (GL es1 fun1) (GL es2 fun2) = GL (es1 ++ es2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

-- Whats the point if we don't uncurry (The exercise asks for uncurried)
-- It might be even nicer to use a record instead of a tuple. In that case we
-- should be able to implement an instance of monoid  for that record what
-- would provide us the desired `treeFold` via `foldMap`
moreFun :: (GuestList, GuestList) -> GuestList
moreFun = uncurry max

-- Exercise 2
treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold z f (Node {rootLabel, subForest}) =
  f rootLabel (map (treeFold z f) subForest)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b gls = (glCons b withoutSubordinates, withOrWithoutSubordinates)
  where
    withoutSubordinates = mconcat $ map snd gls
    withOrWithoutSubordinates = mconcat $ map moreFun gls

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = moreFun . treeFold (mempty, mempty) nextLevel

-- Exercise 5
describe :: GuestList -> String
describe (GL es fun) = unlines (("Total fun: " ++ show fun) : map empName es)

main :: IO ()
main = do
  readFile "company.txt" >>= putStr . describe . maxFun . read
