{-# LANGUAGE CPP #-}

module ExecYAPL(interpret) where

import Control.Monad.State
import Control.Monad.Reader
import AbsYAPL
import qualified Data.Map as Map
import EnvYAPL
import Debug.Trace

data LogOp = LogOpAnd | LogOpOr
type Pos = (Int, Int)

logOp :: LogOp -> Pos -> Value -> Value -> Value
logOp LogOpAnd _ (BoolVal l) (BoolVal r) = BoolVal (l && r)
logOp LogOpOr _ (BoolVal l) (BoolVal r) = BoolVal (l || r)
logOp _ p _ _ = error $ "logical operation on non-boolean on line " ++ show p

execStmt :: Stmt -> RT Env

execStmt (Empty _) = do
  en <- ask
  return en

execStmt w@(While ps@(Just p) e s) = do
   e' <- evalExpr e
   en <- ask
   let b = case e' of
        (BoolVal b') -> b'
        _ -> error $ "while condition non-boolean on line " ++ show p
   if b
      then
        case (retVal en) of
          BreakVal -> do
            return (Env (vEnv en) NoneVal)
          ContVal -> do
            nxt <- local (\_ -> Env (vEnv en) NoneVal) (execStmt s)
            local (\_ -> nxt) (execStmt w)
          NoneVal -> do
            nxt <- execStmt s
            local (\_ -> nxt) (execStmt w)
          _ -> return en
   else
      execStmt (Empty ps)

execStmt (Cond p e s) = do
  (BoolVal b) <- evalExpr e
  if b
    then execStmt s
    else execStmt (Empty p)

execStmt (CondElse _ e s1 s2) = do
  (BoolVal b) <- evalExpr e
  if b
    then execStmt s1
    else execStmt s2

execStmt (BStmt p (Block q (x : xs))) = do
#ifdef DEBUG
  traceM("block env: " ++ show em ++ " " ++ show x)
#endif
  en <- execStmt x
  if ((retVal en) == NoneVal) then
    local (\_ -> en) (execStmt (BStmt p (Block q xs)))
  else do
    return en

execStmt (BStmt _ (Block _ [])) = do
  en <- ask
  return en

execStmt (SExp _ e) = do
  en <- ask
  val <- evalExpr e
  return en

execStmt (Ass _ (Ident s) e) = do
  en <- ask
  ex <- evalExpr e
  mem <- get
  let x = Map.lookup s (vEnv en)
  case x of
    Nothing -> do
      let newLoc = (Map.size mem)
      modify (Map.insert newLoc ex)
      return (Env (Map.insert s newLoc (vEnv en)) (retVal en))
    Just y -> do
      modify (Map.insert y ex)
      return en

execStmt (LocAss _ (Ident s) e) = do
  en <- ask
  ex <- evalExpr e
  mem <- get
  let newLoc = (Map.size mem)
  modify (Map.insert newLoc ex)
  return (Env (Map.insert s newLoc (vEnv en)) (retVal en))

execStmt (FnDefArg _ (Ident s) l b) = do
  en <- ask
  mem <- get
  let newLoc = (Map.size mem)
  let newEnv = Env (Map.insert s newLoc (vEnv en)) (retVal en)
  modify (Map.insert newLoc (Closure l b newEnv))
  return newEnv

execStmt (Ret _ e) = do
  en <- ask
  val <- evalExpr e
#ifdef DEBUG
  traceM("val: " ++ show val)
#endif
  return (Env (vEnv en) val)

execStmt (VRet _) = do
  en <- ask
#ifdef DEBUG
  traceM("val: " ++ show val)
#endif
  return (Env (vEnv en) VoidVal)

execStmt (Break _) = do
  en <- ask
  return (Env (vEnv en) BreakVal)

execStmt (Continue _) = do
  en <- ask
  return (Env (vEnv en) ContVal)

execStmt (Print _ e) = do
  en <- ask
  val <- evalExpr e
  (liftIO $ putStrLn $ show val)
#ifdef DEBUG
  traceM("val: " ++ show val)
#endif
  return en

execStmt (ConstFor (Just p) (Ident s) e1 e2 stmt) = do
  begin <- evalExpr e1
  final <- evalExpr e2
  let (start, end) = case (begin, final) of
        (IntVal n, IntVal m) -> (n, m)
        _ -> error $ "invalid for init/end expression on line " ++ show p
  mem <- get
  let newLoc = (Map.size mem)
  modify (Map.insert newLoc begin)
  mdf <- local (\e -> Env (Map.insert s newLoc (vEnv e)) (retVal e)) (execConstFor newLoc end stmt)
  return mdf

execStmt (Incr s@(Just p) i) = do
  val <- evalExpr (EVar s i)
  case val of
    (IntVal n) -> execStmt (Ass s i (ELitInt s (n + 1)))
    _ -> error $ "incremental on non int value on line " ++ show p

execStmt (Decr s@(Just p) i) = do
  val <- evalExpr (EVar s i)
  case val of
    (IntVal n) -> execStmt (Ass s i (ELitInt s (n - 1)))
    _ -> error $ "decremental on non int value on line" ++ show p

execConstFor :: Loc -> Integer -> Stmt -> RT Env
execConstFor loc end stmt = do
  en <- ask
  (IntVal now) <- gets(Map.! loc)
  if (now <= end) then do
    case (retVal en) of
      BreakVal -> do
        return (Env (vEnv en) NoneVal)
      ContVal -> do
        nxt <- local (\_ -> Env (vEnv en) NoneVal) (execStmt stmt)
        modify (Map.insert loc (IntVal (now + 1)))
        local (\_ -> nxt) (execConstFor loc end stmt)
      NoneVal -> do
        nxt <- execStmt stmt
        modify (Map.insert loc (IntVal (now + 1)))
        local (\_ -> nxt) (execConstFor loc end stmt)
      _ -> return en
  else return en

assignAll :: [Item] -> RT Env
assignAll [] = do
  en <- ask
  return en
assignAll ((Init p s e) : xs) = do
  en <- execStmt (Ass p s e)
  ret <- local (\_ -> en) (assignAll xs)
  return ret

mulOp :: MulOp -> Pos -> Value -> Value -> Value
mulOp (Times _) _ (IntVal x) (IntVal y) = IntVal (x * y)
mulOp (Div _) _ (IntVal x) (IntVal y) = IntVal (div x y)
mulOp (Mod _) _ (IntVal x) (IntVal y) = IntVal (mod x y)
mulOp _ p _ _ = error $ "*,/,% operator on inconsistent types on line " ++ show p

addOp :: AddOp -> Pos -> Value -> Value -> Value
addOp (Plus _) _ (IntVal x) (IntVal y) = IntVal (x + y)
addOp (Minus _) _ (IntVal x) (IntVal y) = IntVal (x - y)
addOp (Plus _) _ (StringVal x) (StringVal y) = StringVal (x ++ y)
addOp _ p _ _ = error $ "-,+ operator on inconsistent types on line " ++ show p

relOp :: RelOp -> Value -> Value -> Value
relOp (LTH _) x y = BoolVal (x < y)
relOp (LE _) x y = BoolVal (x <= y)
relOp (GTH _) x y = BoolVal (x > y)
relOp (GE _) x y = BoolVal (x >= y)
relOp (EQU _) x y = BoolVal (x == y)
relOp (NE _) x y = BoolVal (x /= y)

evalExpr :: Expr -> RT Value
evalExpr (ELitInt _ n) = return $ IntVal n
evalExpr (ELitTrue _) = return $ BoolVal True
evalExpr (ELitFalse _) = return $ BoolVal False
evalExpr (EString _ str) = return $ StringVal str
-- evalExpr (EList _ x) = return x

evalExpr (Neg (Just p) x) = do
  x' <- evalExpr x
  case x' of
    (IntVal val) -> return $ IntVal $ -val
    _ -> error $ "Minus sign on non-int type on line " ++ show p

evalExpr (Not (Just p) x) = do
  x' <- evalExpr x
  case x' of
    (BoolVal b) -> return $ BoolVal $ not b
    _ -> error $ "Logical not operator applied to non-boolean on line " ++ show p

evalExpr (EMul (Just p) l op r) = do
  let p' = mulOp op
  l' <- evalExpr l
  r' <- evalExpr r
  return (p' p l' r')

evalExpr (EAdd (Just p) l op r) = do
  let p' = addOp op
  l' <- evalExpr l
  r' <- evalExpr r
  return (p' p l' r')

evalExpr (ERel _ l op r) = do
  let p' = relOp op
  l' <- evalExpr l
  r' <- evalExpr r
  return (p' l' r')

evalExpr (EVar (Just p) (Ident v)) = do
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

evalExpr (EAnd (Just p) l r) = do
  l' <- evalExpr l
  r' <- evalExpr r
  return (logOp LogOpAnd p l' r')

evalExpr (EOr (Just p) l r) = do
  l' <- evalExpr l
  r' <- evalExpr r
  return (logOp LogOpOr p l' r')

evalExpr (EApp (Just p) (Ident f) values) = do
  s <- ask
  mem <- get
  let x = Map.lookup f (vEnv s)
  case x of
    Nothing -> error $ "function not declared on line " ++ show p
    Just y -> do
      fn <- gets(Map.! y)
      case fn of
        (Closure args b en) -> do
          let real = length args
          let actual = length values
          --traceM("real: " ++ show real ++ " actual: " ++ show actual)
          if (actual /= real) then
            error $ "number of arguments mismatch on line " ++ show p
          else do
            let keys = map (\ (Ar _ (Ident st)) -> st) args
            let size = Map.size mem
            let locs = [size..(size + (length keys) - 1)]
            vals <- mapM evalExpr values
            let newMap = Map.fromList $ zip keys locs
            let newState = Map.fromList $ zip locs vals
            modify (Map.union newState)
            ig <- local (\e -> Env (Map.union newMap (vEnv en)) (retVal e))
              $ execStmt (BStmt (hasPosition b) b)
            return (retVal ig)
        _ -> error $ "function and variable names collide"

collect :: [TopDef] -> RT Env
collect [] = do
  en <- ask
  return en
collect (x : xs) = case x of
  FnDefArgG p s l b -> do
    ret <- execStmt (FnDefArg p s l b)
    nxt <- local (\_ -> ret) (collect xs)
    return nxt
  (Glob _ l) -> do
    ret <- assignAll l
    nxt <- local (\_ -> ret) (collect xs)
    return nxt

interpret :: Program -> RT ()
interpret (Program _ l) = go l where
  go x = do
    mal <- collect x
#ifdef DEBUG
    traceM("finished: " ++ show mal)
    traceM("f state: " ++ show st)
#endif
    let main = Map.lookup "main" (vEnv mal)
    _ <- case main of
      Nothing -> error $ "main function not defined"
      _ -> do
        let start = Just (0, 0)
        local (\_ -> mal) (execStmt (SExp start (EApp start (Ident "main") [])))
    return ()