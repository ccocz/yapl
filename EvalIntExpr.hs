module EvalIntExpr (evalExpr) where

import AbsYAPL
import Data.Map as Map

type Var = String
type VEnv = Map.Map Var Integer -- need more types

mulOp :: MulOp -> Integer -> Integer -> Integer
mulOp (Times _) = (*)
mulOp (Div _) = (div)
mulOp (Mod _) = (mod)

addOp :: AddOp -> Integer -> Integer -> Integer
addOp (Plus _) = (+)
addOp (Minus _) = (-)

relOp :: RelOp -> Integer -> Integer -> Bool
relOp (LTH _) = (<)
relOp (LE _) = (<=)
relOp (GTH _) = (>)
relOp (GE _) = (>=)
relOp (EQU _) = (==)
relOp (NE _) = (/=)

evalExprM :: Expr -> VEnv -> Integer
-- evalExpM (EInt n) = \_ -> n
-- evalExpM (EInt n) = const n
-- evalExpM :: MonadReader (Map Var Int) m => Exp -> m Int
evalExprM (ELitInt _ n) = return n
--evalExprM (ELitTrue _) = return True
--evalExprM (ELitFalse _) = return False
--evalExprM (EApp _ ident list)
--evalExprM (EString _ str) = return str
-- evalExprM (EList _ x) = return x

--evalExprM (Neg _ x) = do
 -- x' <- evalExprM x
  --return -1 * x'

--evalExprM (Not _ x) = do
  --x' <- evalExprM x
  --return not x'

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

{-evalExprM (ERel _ l op r) = do
  let p' = relOp op
  l' <- evalExprM l
  r' <- evalExprM r
  return (p' l' r')-}

{-
evalExprM (EOp p l r) = do
  let p' = op p
  l' <- evalExprM l
  r' <- evalExprM r
  return (p' l' r')
evalExprM (EVar v) = do
  s <- ask
  let x = Map.lookup v s
  case x of
	Nothing -> error $ "Niezdefiniowana zmienna: " ++ v
	Just y -> return y
evalExprM (ELet v l r) = do
  l' <- evalExprM l
  r' <- local (Map.insert v l') $ evalExprM r
  return r'
-}

evalExpr :: Expr -> Integer
evalExpr e = evalExprM e Map.empty