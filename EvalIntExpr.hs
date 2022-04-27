{-# LANGUAGE FlexibleContexts #-}

module EvalIntExpr (evalExpr) where

import AbsYAPL
import Data.Map as Map
import EnvYAPL
import Control.Monad.Reader

mulOp :: MulOp -> Value -> Value -> Value
mulOp (Times _) (IntVal x) (IntVal y) = IntVal (x * y)
mulOp (Div _) (IntVal x) (IntVal y) = IntVal (div x y)
mulOp (Mod _) (IntVal x) (IntVal y) = IntVal (mod x y)

addOp :: AddOp -> Value -> Value -> Value
addOp (Plus _) (IntVal x) (IntVal y) = IntVal (x + y)
addOp (Minus _) (IntVal x) (IntVal y) = IntVal (x - y)
addOp (Plus _) (StringVal x) (StringVal y) = StringVal (x ++ y)

relOp :: RelOp -> Value -> Value -> Value
relOp (LTH _) x y = BoolVal (x < y)
relOp (LE _) x y = BoolVal (x <= y)
relOp (GTH _) x y = BoolVal (x > y)
relOp (GE _) x y = BoolVal (x >= y)
relOp (EQU _) x y = BoolVal (x == y)
relOp (NE _) x y = BoolVal (x /= y)

evalExprM :: Expr -> VEnv -> Value
-- evalExpM (EInt n) = \_ -> n
-- evalExpM (EInt n) = const n
-- evalExpM :: MonadReader (Map Var Int) m => Exp -> m Int
evalExprM (ELitInt _ n) = return $ IntVal n
evalExprM (ELitTrue _) = return $ BoolVal True
evalExprM (ELitFalse _) = return $ BoolVal False
--evalExprM (EApp _ ident list)
evalExprM (EString _ str) = return $ StringVal str
-- evalExprM (EList _ x) = return x

{-evalExprM (Neg _ x) = do
  x' <- evalExprM x
  return -1 * x'

evalExprM (Not _ x) = do
  x' <- evalExprM x
  return not x'-}

evalExprM (EMul _ l op r) = do
  let p' = mulOp op
  l' <- evalExprM l
  r' <- evalExprM r
  return (p' l' r')

evalExprM (EAdd _ l op r) = do
  let p' = addOp op
  l' <- evalExprM l
  r' <- evalExprM r
  return (p' l' r')

evalExprM (ERel _ l op r) = do
  let p' = relOp op
  l' <- evalExprM l
  r' <- evalExprM r
  return (p' l' r')

evalExpM (EVar _ v) = do
  s <- ask
  let x = Map.lookup v s
  case x of
	Nothing -> error $ "Varible not declared!"
	Just y -> return y

evalExpr :: Expr -> VEnv -> Value
evalExpr e v = evalExprM e v