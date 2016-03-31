{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import qualified StackVM  as S
import qualified Data.Map as M

--Ex 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

--Ex 2
evalStr :: String -> Maybe Integer
evalStr x = let j_eT = parseExp Lit Add Mul x --just value
            in if j_eT == Nothing then Nothing else (\(Just a) -> Just $ eval a) j_eT  
--Ex 3
class Expr a where
  lit :: Integer -> a 
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id

--Ex4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

--Integer
instance Expr Integer where
  lit x = x
  add x y = x+y
  mul x y = x*y

--Bool
instance Expr Bool where
  lit x = if x > 0 then True else False
  add x y = x || y
  mul x y = x && y

--MinMax
instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = lit $ max x y
  mul (MinMax x) (MinMax y) = lit $ min x y

--Mod7
instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = lit $ mod (x+y) 7
  mul (Mod7 x) (Mod7 y) = lit $ mod (x*y) 7

testExp :: Expr a => String -> Maybe a
testExp = parseExp lit add mul --"(3 * -4) + 5"

testInteger x = testExp x :: Maybe Integer
testBool x = testExp x :: Maybe Bool
testMM x = testExp x :: Maybe MinMax 
testSat x = testExp x :: Maybe Mod7

--Ex 5
instance Expr S.Program where
  lit x = [S.PushI x]
  add x y = x ++ y ++ [S.Add] 
  mul x y = x ++ y ++ [S.Mul] 

 
compile :: String -> Maybe S.Program
compile x = parseExp lit add mul x

--Ex 6
class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String

  deriving (Show, Eq)

instance HasVars VarExprT where
   var x = VVar x

instance Expr VarExprT where
   lit x = VLit x
   add x y = VAdd x y
   mul x y = VMul x y

instance HasVars (M.Map String Integer -> Maybe Integer) where
   var = M.lookup 

--took the help at #haskell-beginners @ webIRC to finish
instance Expr (M.Map String Integer -> Maybe Integer) where
   lit x m = Just x 
   add f g m = case f m of
                 Nothing -> Nothing
                 Just x -> case g m of
                             Nothing -> Nothing
                             Just y -> Just (x+y)
   mul f g m = case f m of
                 Nothing -> Nothing
                 Just x -> case g m of
                             Nothing -> Nothing
                             Just y -> Just (x*y)
withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


