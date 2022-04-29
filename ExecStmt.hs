module ExecStmt(execStmt) where

import Control.Monad.State
import Control.Monad.Reader
import AbsYAPL
--import EvalIntExpr
import qualified Data.Map as Map
import EnvYAPL
import Debug.Trace

execStmtM :: Stmt -> State VEnv ()
execStmtM (Empty _) = return ()
execStmtM w@(While _ e s) = do
   en <- get
   if (evalExpr e en) == (BoolVal True)
      then
         execStmtM s >>
         execStmtM w
      else
         execStmtM (Empty undefined) --todo

execStmtM (Cond _ e s) = do
  en <- get
  if (evalExpr e en) == (BoolVal True)
    then execStmtM s
    else execStmtM (Empty undefined) --todo
  return ()

execStmtM (CondElse _ e s1 s2) = do
  en <- get
  if (evalExpr e en) == (BoolVal True)
    then execStmtM s1
    else execStmtM s2
  return ()

-- fixme local variables within blocks
execStmtM (BStmt _ b) = do
  go b
  return () where
    go (Block _ []) = return ()
    go (Block _ (x : xs)) = do
      execStmtM x
      go (Block undefined xs) --fixme
      return ()

execStmtM (SExp _ e) = do
  en <- get
  -- fixme what to do with this value? >>
  let val = evalExpr e en
  return ()

execStmtM (Ass _ (Ident s) e) = do
  en <- get
  modify (Map.insert s $ evalExpr e en)
  return ()

execStmtM (FnDefArg _ (Ident s) l b) = do
  en <- get
  -- todo: what if it exists
  modify (Map.insert s $ Closure l b)
  return ()

--execStmtM (SExp _ e) = return (evalExpr e)

execStmt :: Stmt -> VEnv -> VEnv
execStmt s e = execState (execStmtM s) e

-- eval

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

evalExprM (EVar p (Ident v)) = do
  s <- ask
  let x = Map.lookup v s
  case x of
    Nothing -> error $ "not declared" ++ show p
    Just y -> return y

evalExprM (EApp _ (Ident f) values) = do
  s <- ask
  let x = Map.lookup f s
  case x of
    Nothing -> error $ "not declared function"
    Just (Closure args b) -> return local (Map.union $ newMap) $ execStmt (BStmt undefined b) where
      keys = map (\ (Ar _ (Ident s)) -> s) args
      vals = map (\e -> evalExprM e) values
      newMap = Map.fromList $ zip keys vals

{-evalExpM (ELet v l r) = do
  l' <- evalExpM l
  r' <- local (Map.insert v l') $ evalExpM r
  return r'-}

--apply (Closure l b) en = evalExprM b

evalExpr :: Expr -> VEnv -> Value
evalExpr e v = evalExprM e v