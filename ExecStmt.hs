module ExecStmt(execStmt) where

import Control.Monad.State
import AbsYAPL
import EvalIntExpr
import Data.Map as Map
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
      go (Block undefined xs)
      return ()

execStmtM (SExp _ e) = do
  en <- get
  -- fixme what to do with this value?
  let val = evalExpr e en
  return ()

execStmtM (Ass _ (Ident s) e) = do
  en <- get
  modify (Map.insert s $ evalExpr e en)
  traceM("assigning: " ++ show s)
  return ()

--execStmtM (SExp _ e) = return (evalExpr e)

execStmt :: Stmt -> VEnv
execStmt s = execState (execStmtM s) Map.empty