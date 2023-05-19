{-# OPTIONS_GHC -Wall #-}

module Calc where

import Control.Applicative
import Data.Map.Strict qualified as M
import ExprT qualified as E
import Parser
import StackVM qualified as S

-- Exercise 1
eval :: E.ExprT -> Integer
eval (E.Lit x) = x
eval (E.Mul x y) = eval x * eval y
eval (E.Add x y) = eval x + eval y

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp E.Lit E.Add E.Mul

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr E.ExprT where
  lit = E.Lit
  add = E.Add
  mul = E.Mul

-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

new :: Integer -> Mod7
new = Mod7 . (`mod` 7)

instance Expr Mod7 where
  lit = new
  add (Mod7 x) (Mod7 y) = new $ x + y
  mul (Mod7 x) (Mod7 y) = new $ x * y

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

-- Exercise 5
instance Expr S.Program where
  lit x = [S.PushI x]
  add x y = x ++ y ++ [S.Add]
  mul x y = x ++ y ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul

class HasVars a where
  var :: String -> a

data VarExprT
  = Lit Integer
  | Add VarExprT VarExprT
  | Mul VarExprT VarExprT
  | Var String
  deriving (Eq, Show)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n _ = Just n
  add = (liftA2 . liftA2) (+)
  mul = (liftA2 . liftA2) (*)

withVars ::
  [(String, Integer)] ->
  (M.Map String Integer -> Maybe Integer) ->
  Maybe Integer
withVars vs ex = ex $ M.fromList vs
