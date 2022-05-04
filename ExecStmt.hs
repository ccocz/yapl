{-# LANGUAGE CPP #-}

module ExecStmt(execStmt) where

import Control.Monad.State
import Control.Monad.Reader
import AbsYAPL
import qualified Data.Map as Map
import EnvYAPL
import Debug.Trace

execStmt :: Stmt -> RT Env

execStmt (Empty _) = do
  en <- ask
  return en

execStmt w@(While _ e s) = do
   --en <- get
   (BoolVal b) <- evalExpr e
   if b
      then
         execStmt s >>
         execStmt w
      else
         execStmt (Empty undefined) --todo

execStmt (Cond _ e s) = do
  (BoolVal b) <- evalExpr e
  if b
    then execStmt s
    else execStmt (Empty undefined) --todo

execStmt (CondElse _ e s1 s2) = do
  (BoolVal b) <- evalExpr e
  if b
    then execStmt s1
    else execStmt s2

-- fixme local variables within blocks
execStmt (BStmt _ (Block _ (x : xs))) = do
  em <- get
#ifdef DEBUG
  traceM("block env: " ++ show em ++ " " ++ show x)
#endif
  en <- execStmt x
  if ((retVal en) == NoneVal) then
    local (\_ -> en) (execStmt (BStmt undefined (Block undefined xs)))-- fixme
  else do
    --traceM("faak:" ++ show en)
    return en

execStmt (BStmt _ (Block _ [])) = do
  en <- ask
  --traceM(show en)
  return en

execStmt (SExp _ e) = do
  en <- ask
  -- fixme what to do with this value? >>
  val <- evalExpr e
  --traceM("valexp: " ++ show val)
  return en

execStmt (Ass _ (Ident s) e) = do
  en <- ask
  mem <- get
  ex <- evalExpr e
  let x = Map.lookup s (vEnv en)
  case x of
    Nothing -> do
      let newLoc = (Map.size mem)
      modify (Map.insert newLoc ex)
      return (Env (Map.insert s newLoc (vEnv en)) (retVal en))
    Just y -> do
      modify (Map.insert y ex)
      return en

execStmt (FnDefArg _ (Ident s) l b) = do
  en <- ask
  mem <- get
  -- todo: what if it exists
  let newLoc = (Map.size mem)
  modify (Map.insert newLoc (Closure l b))
  return (Env (Map.insert s newLoc (vEnv en)) (retVal en))
  --return (Env (Map.insert s (Closure l b) (vEnv e)) (retVal e))

execStmt (Ret _ e) = do
  en <- ask
  val <- evalExpr e
#ifdef DEBUG
  traceM("val: " ++ show val)
#endif
  return (Env (vEnv en) val)

execStmt (Print _ e) = do
  en <- ask
  val <- evalExpr e
  (liftIO $ putStrLn $ show val)
#ifdef DEBUG
  traceM("val: " ++ show val)
#endif
  return en

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

evalExpr :: Expr -> RT Value
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
    Nothing -> error $ "not declared variable "
      ++ show v
      ++ " on line "
      ++ show p
    Just y -> do
      val <- gets (Map.! y)
      return val

-- why not have only reader monad?

evalExpr (EApp _ (Ident f) values) = do
  s <- ask
  mem <- get
  let x = Map.lookup f (vEnv s)
  case x of
    Nothing -> error $ "not declared function"
    Just y -> do
      f <- gets(Map.! y)
      case f of
        (Closure args b) -> do
          let keys = map (\ (Ar _ (Ident st)) -> st) args
          let size = Map.size mem
          let locs = [size..(size + (length keys) - 1)]
          vals <- mapM evalExpr values
          let newMap = Map.fromList $ zip keys locs
          let newState = Map.fromList $ zip locs vals
          modify (Map.union newState)
#ifdef DEBUG
          traceM("args map " ++ show newMap)
#endif
          ig <- local (\e -> Env (Map.union newMap (vEnv e)) (retVal e)) $ execStmt (BStmt undefined b)
          return (retVal ig)
        _ -> error $ "function and variable names collide"