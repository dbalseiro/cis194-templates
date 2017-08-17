{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 05
--
----------------------------------------------------------------------

module Calc where

import ExprT
import Parser
import StackVM
import qualified Data.Map as M

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> eval (ExprT.Mul (ExprT.Add (ExprT.Lit 2) (ExprT.Lit 3)) (ExprT.Lit 4)) == 20
-- True

eval :: Maybe ExprT -> Maybe Integer
eval Nothing = Nothing
eval (Just e) = Just (eval' e)

eval' :: ExprT -> Integer
eval' (Lit x) = x
eval' (Add e1 e2) = (eval' e1) + (eval' e2)
eval' (Mul e1 e2) = (eval' e1) * (eval' e2)


----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

evalStr :: String -> Maybe Integer
evalStr = eval . parseExp Lit Add Mul

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> reify $ mul (add (lit 2) (lit 3)) (lit 4)
-- Mul (Add (Lit 2) (Lit 3)) (Lit 4)

reify :: ExprT -> ExprT
reify = id

class Expr expression where
    lit :: Integer -> expression
    mul :: expression -> expression -> expression
    add :: expression -> expression -> expression

instance Expr ExprT where
    lit i = Lit i
    mul e1 e2 = Mul e1 e2
    add e1 e2 = Add e1 e2

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr Bool where
    lit = (>0)
    mul = (&&)
    add = (||)

instance Expr MinMax where
    lit i = MinMax i
    mul = min
    add = max

instance Expr Mod7 where
    lit i = Mod7 (i `mod` 7)
    mul (Mod7 i) (Mod7 j) = lit (i * j)
    add (Mod7 i) (Mod7 j) = lit (i + j)

----------------------------------------------------------------------
-- Exercise 5 (do this OR exercise 6)
----------------------------------------------------------------------

data Token = Const Integer
           | Plus Token Token
           | Star Token Token
  deriving Show

class Expr expression where
    lit :: Integer -> expression
    mul :: expression -> expression -> expression
    add :: expression -> expression -> expression

-- Exercise 5
instance Expr S.Program where
    lit i = [PushI i]
    add e1 e2 = e1 ++ e2 ++ [Add]
    mul e1 e2 = e1 ++ e2 ++ [Mul]

compile :: String -> Maybe Program
compile = peval . parseExp Const Plus Star

peval :: Maybe Token -> Maybe Program
peval Nothing = Nothing
peval (Just e) = Just (peval' e)

peval' :: Token -> Program
peval' (Const i) = lit i :: Program
peval' (Plus e1 e2) = add (peval' e1) (peval' e2) :: Program
peval' (Star e1 e2) = mul (peval' e1) (peval' e2) :: Program

----------------------------------------------------------------------
-- Exercise 6 (do this OR exercise 5)
----------------------------------------------------------------------

-- |
--
-- >>> :t add (lit 3) (var "x")
-- add (lit 3) (var "x") :: (Expr a, HasVars a) => a
-- >>> withVars [("x", 6)] $ add (lit 3) (var "x")
-- Just 9
-- >>> withVars [("x", 6)] $ add (lit 3) (var "y")
-- Nothing
-- >>> withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
-- Just 54

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
