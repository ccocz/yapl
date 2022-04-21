-- File generated by the BNF Converter (bnfc 2.9.4).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelYAPL where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsYAPL

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: AbsYAPL.Ident -> Result
transIdent x = case x of
  AbsYAPL.Ident string -> failure x

transProgram :: Show a => AbsYAPL.Program' a -> Result
transProgram x = case x of
  AbsYAPL.Program _ topdefs -> failure x

transTopDef :: Show a => AbsYAPL.TopDef' a -> Result
transTopDef x = case x of
  AbsYAPL.FnDefNoArg _ ident block -> failure x
  AbsYAPL.FnDefArg _ ident args block -> failure x
  AbsYAPL.ExpDef _ expr -> failure x
  AbsYAPL.Glob _ items -> failure x
  AbsYAPL.Stm _ stmt -> failure x
  AbsYAPL.Exp _ expr -> failure x

transArg :: Show a => AbsYAPL.Arg' a -> Result
transArg x = case x of
  AbsYAPL.Ar _ ident -> failure x

transBlock :: Show a => AbsYAPL.Block' a -> Result
transBlock x = case x of
  AbsYAPL.Block _ stmts -> failure x

transStmt :: Show a => AbsYAPL.Stmt' a -> Result
transStmt x = case x of
  AbsYAPL.Empty _ -> failure x
  AbsYAPL.BStmt _ block -> failure x
  AbsYAPL.Decl _ items -> failure x
  AbsYAPL.Ass _ ident expr -> failure x
  AbsYAPL.Incr _ ident -> failure x
  AbsYAPL.Decr _ ident -> failure x
  AbsYAPL.Ret _ expr -> failure x
  AbsYAPL.VRet _ -> failure x
  AbsYAPL.Cond _ expr stmt -> failure x
  AbsYAPL.CondElse _ expr stmt1 stmt2 -> failure x
  AbsYAPL.While _ expr stmt -> failure x
  AbsYAPL.ConstFor _ ident expr1 expr2 stmt -> failure x
  AbsYAPL.SExp _ expr -> failure x

transItem :: Show a => AbsYAPL.Item' a -> Result
transItem x = case x of
  AbsYAPL.Init _ ident expr -> failure x

transType :: Show a => AbsYAPL.Type' a -> Result
transType x = case x of
  AbsYAPL.Int _ -> failure x
  AbsYAPL.Str _ -> failure x
  AbsYAPL.Bool _ -> failure x
  AbsYAPL.Void _ -> failure x
  AbsYAPL.FuncType _ -> failure x

transExpr :: Show a => AbsYAPL.Expr' a -> Result
transExpr x = case x of
  AbsYAPL.EVar _ ident -> failure x
  AbsYAPL.ELitInt _ integer -> failure x
  AbsYAPL.ELitTrue _ -> failure x
  AbsYAPL.ELitFalse _ -> failure x
  AbsYAPL.EApp _ ident exprs -> failure x
  AbsYAPL.EString _ string -> failure x
  AbsYAPL.EList _ -> failure x
  AbsYAPL.Neg _ expr -> failure x
  AbsYAPL.Not _ expr -> failure x
  AbsYAPL.EMul _ expr1 mulop expr2 -> failure x
  AbsYAPL.EAdd _ expr1 addop expr2 -> failure x
  AbsYAPL.ERel _ expr1 relop expr2 -> failure x
  AbsYAPL.EAnd _ expr1 expr2 -> failure x
  AbsYAPL.EOr _ expr1 expr2 -> failure x

transAddOp :: Show a => AbsYAPL.AddOp' a -> Result
transAddOp x = case x of
  AbsYAPL.Plus _ -> failure x
  AbsYAPL.Minus _ -> failure x

transMulOp :: Show a => AbsYAPL.MulOp' a -> Result
transMulOp x = case x of
  AbsYAPL.Times _ -> failure x
  AbsYAPL.Div _ -> failure x
  AbsYAPL.Mod _ -> failure x

transRelOp :: Show a => AbsYAPL.RelOp' a -> Result
transRelOp x = case x of
  AbsYAPL.LTH _ -> failure x
  AbsYAPL.LE _ -> failure x
  AbsYAPL.GTH _ -> failure x
  AbsYAPL.GE _ -> failure x
  AbsYAPL.EQU _ -> failure x
  AbsYAPL.NE _ -> failure x
