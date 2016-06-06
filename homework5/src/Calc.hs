{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Control.Applicative ((<$>), liftA2)
import qualified Data.Map as M

import qualified ExprT as ET
import Parser
import StackVM

-- Exercise 1
eval :: ET.ExprT -> Integer
eval (ET.Lit n)     = n
eval (ET.Add e1 e2) = eval e1 + eval e2
eval (ET.Mul e1 e2) = eval e1 * eval e2

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr s = eval <$> parseExp ET.Lit ET.Add ET.Mul s

-- Exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ET.ExprT where
  lit = ET.Lit
  add = ET.Add
  mul = ET.Mul

reify :: ET.ExprT -> ET.ExprT
reify = id

-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n = n > 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

-- Exercise 5

binopExpr op p1 p2 =
  case (stackVM p1, stackVM p2) of
    (Right (IVal n1), Right (IVal n2)) -> [PushI n1, PushI n2, op]
    _                                  -> []

instance Expr Program where
  lit n = [PushI n]
  add = binopExpr Add
  mul = binopExpr Mul

compile :: String -> Maybe Program
compile = parseExp lit add mul

-- Exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = \m -> M.lookup s m


binOp op f1 f2 = \m -> let n1 = f1 m
                           n2 = f2 m
                       in liftA2 op n1 n2

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n = \m -> Just n
  add = binOp (+)
  mul = binOp (*)

withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
