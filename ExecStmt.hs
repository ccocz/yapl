module ExecStmt(execStmt) where

import Control.Monad.State
import Control.Monad.Reader
import AbsYAPL
--import EvalIntExpr
import qualified Data.Map as Map
import EnvYAPL
import Debug.Trace

execStmt :: Stmt -> RT Env
execStmt (Empty _) = do
  en <- get
  return en
execStmt w@(While _ e s) = do
   en <- get
   if (evalExpr e en) == (BoolVal True)
      then
         execStmt s >>
         execStmt w
      else
         execStmt (Empty undefined) --todo

execStmt (Cond _ e s) = do
  en <- get
  if (evalExpr e en) == (BoolVal True) then
    execStmt s
  else
    execStmt (Empty undefined) --todo

execStmt (CondElse _ e s1 s2) = do
  en <- get
  if (evalExpr e en) == (BoolVal True) then
    execStmt s1
  else
    execStmt s2

-- fixme local variables within blocks
execStmt (BStmt _ (Block _ (x : xs))) = do
  en <- execStmt x
  traceM("block env: " ++ show en)
  if ((retVal en) == NoneVal) then
    do
    traceM("x: " ++ show x)
    (execStmt (BStmt undefined (Block undefined xs)))-- fixme
  else do
    traceM("faak:" ++ show en)
    return en

execStmt (BStmt _ (Block _ [])) = do
  en <- get
  --traceM(show en)
  return en

execStmt (SExp _ e) = do
  en <- get
  -- fixme what to do with this value? >>
  let val = evalExpr e en
  traceM("valexp: " ++ show val)
  return en

execStmt (Ass _ (Ident s) e) = do
  en <- get
  modify (\v -> (Env (Map.insert s (evalExpr e en) (vEnv v)) (retVal v)))
  return en

execStmt (FnDefArg _ (Ident s) l b) = do
  en <- get
  -- todo: what if it exists
  modify (\e -> (Env (Map.insert s (Closure l b) (vEnv e)) (retVal e)))
  return en

execStmt (Ret _ e) = do
  en <- get
  let val = evalExpr e en
  traceM("val: " ++ show val)
  return (Env (vEnv en) val)

{-execStmt (Print _ e) = do
  en <- get
  let ev = evalExpr e en
  case ev of
    (IntVal i) ->
      putStrLn $ show e
  return en-}

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

evalExpr :: Expr -> Env -> RT Value
-- evalExpM (EInt n) = \_ -> n
-- evalExpM (EInt n) = const n
-- evalExpM :: MonadReader (Map Var Int) m => Exp -> m Int
evalExpr (ELitInt _ n) = return $ IntVal n
evalExpr (ELitTrue _) = return $ BoolVal True
evalExpr (ELitFalse _) = return $ BoolVal False
--evalExpr (EApp _ ident list)
evalExpr (EString _ str) = return $ StringVal str
-- evalExpr (EList _ x) = return x

{-evalExpr (Neg _ x) = do
  x' <- evalExpr x
  return -1 * x'

evalExpr (Not _ x) = do
  x' <- evalExpr x
  return not x'-}

evalExpr (EMul _ l op r) = do
  let p' = mulOp op
  l' <- evalExpr l
  r' <- evalExpr r
  return (p' l' r')

evalExpr (EAdd _ l op r) = do
  let p' = addOp op
  l' <- evalExpr l
  r' <- evalExpr r
  return (p' l' r')

evalExpr (ERel _ l op r) = do
  let p' = relOp op
  l' <- evalExpr l
  r' <- evalExpr r
  return (p' l' r')

evalExpr (EVar p (Ident v)) = do
  s <- ask
  let x = Map.lookup v (vEnv s)
  case x of
    Nothing -> error $ "not declared" ++ show p
    Just y -> return y

evalExpr (EApp _ (Ident f) values) = do
  s <- ask
  let x = Map.lookup f (vEnv s)
  case x of
    Nothing -> error $ "not declared function"
    Just (Closure args b) -> do
      let keys = map (\ (Ar _ (Ident st)) -> st) args
      vals <- mapM evalExpr values
      let newMap = Map.fromList $ zip keys vals
      --traceM("map before: " ++ show (Map.union newMap (vEnv s)))
      let ig = local (\e -> e) $ execStmt (BStmt undefined b)
      return (NoneVal)