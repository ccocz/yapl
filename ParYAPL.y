-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module ParYAPL
  ( happyError
  , myLexer
  , pStmt
  ) where

import Prelude

import qualified AbsYAPL
import LexYAPL

}

%name pStmt_internal Stmt
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'      { PT _ (TS _ 1)  }
  '!='     { PT _ (TS _ 2)  }
  '%'      { PT _ (TS _ 3)  }
  '&&'     { PT _ (TS _ 4)  }
  '('      { PT _ (TS _ 5)  }
  ')'      { PT _ (TS _ 6)  }
  '):'     { PT _ (TS _ 7)  }
  '*'      { PT _ (TS _ 8)  }
  '+'      { PT _ (TS _ 9)  }
  '++'     { PT _ (TS _ 10) }
  ','      { PT _ (TS _ 11) }
  '-'      { PT _ (TS _ 12) }
  '--'     { PT _ (TS _ 13) }
  '/'      { PT _ (TS _ 14) }
  ':'      { PT _ (TS _ 15) }
  ';'      { PT _ (TS _ 16) }
  '<'      { PT _ (TS _ 17) }
  '<='     { PT _ (TS _ 18) }
  '='      { PT _ (TS _ 19) }
  '=='     { PT _ (TS _ 20) }
  '>'      { PT _ (TS _ 21) }
  '>='     { PT _ (TS _ 22) }
  '['      { PT _ (TS _ 23) }
  ']'      { PT _ (TS _ 24) }
  'else:'  { PT _ (TS _ 25) }
  'false'  { PT _ (TS _ 26) }
  'for'    { PT _ (TS _ 27) }
  'if'     { PT _ (TS _ 28) }
  'return' { PT _ (TS _ 29) }
  'to'     { PT _ (TS _ 30) }
  'true'   { PT _ (TS _ 31) }
  'while'  { PT _ (TS _ 32) }
  '{'      { PT _ (TS _ 33) }
  '||'     { PT _ (TS _ 34) }
  '}'      { PT _ (TS _ 35) }
  L_Ident  { PT _ (TV _)    }
  L_integ  { PT _ (TI _)    }
  L_quoted { PT _ (TL _)    }

%%

Ident :: { (AbsYAPL.BNFC'Position, AbsYAPL.Ident) }
Ident  : L_Ident { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Ident (tokenText $1)) }

Integer :: { (AbsYAPL.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Integer) }

String  :: { (AbsYAPL.BNFC'Position, String) }
String   : L_quoted { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), ((\(PT _ (TL s)) -> s) $1)) }

Program :: { (AbsYAPL.BNFC'Position, AbsYAPL.Program) }
Program
  : ListTopDef { (fst $1, AbsYAPL.Program (fst $1) (snd $1)) }

TopDef :: { (AbsYAPL.BNFC'Position, AbsYAPL.TopDef) }
TopDef
  : Ident ':' Block { (fst $1, AbsYAPL.FnDefNoArgG (fst $1) (snd $1) (snd $3)) }
  | Ident '(' ListArg '):' Block { (fst $1, AbsYAPL.FnDefArgG (fst $1) (snd $1) (snd $3) (snd $5)) }
  | Expr { (fst $1, AbsYAPL.ExpDef (fst $1) (snd $1)) }
  | ListItem ';' { (fst $1, AbsYAPL.Glob (fst $1) (snd $1)) }

Arg :: { (AbsYAPL.BNFC'Position, AbsYAPL.Arg) }
Arg : Ident { (fst $1, AbsYAPL.Ar (fst $1) (snd $1)) }

ListArg :: { (AbsYAPL.BNFC'Position, [AbsYAPL.Arg]) }
ListArg
  : {- empty -} { (AbsYAPL.BNFC'NoPosition, []) }
  | Arg { (fst $1, (:[]) (snd $1)) }
  | Arg ',' ListArg { (fst $1, (:) (snd $1) (snd $3)) }

ListTopDef :: { (AbsYAPL.BNFC'Position, [AbsYAPL.TopDef]) }
ListTopDef
  : TopDef { (fst $1, (:[]) (snd $1)) }
  | TopDef ListTopDef { (fst $1, (:) (snd $1) (snd $2)) }

Block :: { (AbsYAPL.BNFC'Position, AbsYAPL.Block) }
Block
  : '{' ListStmt '}' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Block (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1)) (snd $2)) }

ListStmt :: { (AbsYAPL.BNFC'Position, [AbsYAPL.Stmt]) }
ListStmt
  : {- empty -} { (AbsYAPL.BNFC'NoPosition, []) }
  | Stmt ListStmt { (fst $1, (:) (snd $1) (snd $2)) }

Stmt :: { (AbsYAPL.BNFC'Position, AbsYAPL.Stmt) }
Stmt
  : ';' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Empty (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | Block { (fst $1, AbsYAPL.BStmt (fst $1) (snd $1)) }
  | Ident '=' Expr ';' { (fst $1, AbsYAPL.Ass (fst $1) (snd $1) (snd $3)) }
  | Ident '++' ';' { (fst $1, AbsYAPL.Incr (fst $1) (snd $1)) }
  | Ident '--' ';' { (fst $1, AbsYAPL.Decr (fst $1) (snd $1)) }
  | 'return' Expr ';' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Ret (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'return' ';' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.VRet (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | 'if' '(' Expr '):' Stmt { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Cond (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'if' '(' Expr '):' Stmt 'else:' Stmt { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.CondElse (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5) (snd $7)) }
  | 'while' '(' Expr '):' Stmt { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.While (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'for' '(' Ident '=' Expr ';' 'to' Expr '):' Stmt { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.ConstFor (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5) (snd $8) (snd $10)) }
  | Expr ';' { (fst $1, AbsYAPL.SExp (fst $1) (snd $1)) }
  | Ident '(' ListArg '):' Block { (fst $1, AbsYAPL.FnDefArg (fst $1) (snd $1) (snd $3) (snd $5)) }

Item :: { (AbsYAPL.BNFC'Position, AbsYAPL.Item) }
Item
  : Ident '=' Expr ';' { (fst $1, AbsYAPL.Init (fst $1) (snd $1) (snd $3)) }

ListItem :: { (AbsYAPL.BNFC'Position, [AbsYAPL.Item]) }
ListItem
  : Item { (fst $1, (:[]) (snd $1)) }
  | Item ',' ListItem { (fst $1, (:) (snd $1) (snd $3)) }

Expr6 :: { (AbsYAPL.BNFC'Position, AbsYAPL.Expr) }
Expr6
  : Ident { (fst $1, AbsYAPL.EVar (fst $1) (snd $1)) }
  | Integer { (fst $1, AbsYAPL.ELitInt (fst $1) (snd $1)) }
  | 'true' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.ELitTrue (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | 'false' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.ELitFalse (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | Ident '(' ListExpr ')' { (fst $1, AbsYAPL.EApp (fst $1) (snd $1) (snd $3)) }
  | String { (fst $1, AbsYAPL.EString (fst $1) (snd $1)) }
  | '[' ']' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.EList (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | '(' Expr ')' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), (snd $2)) }

Expr5 :: { (AbsYAPL.BNFC'Position, AbsYAPL.Expr) }
Expr5
  : '-' Expr6 { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Neg (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | '!' Expr6 { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Not (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr6 { (fst $1, (snd $1)) }

Expr4 :: { (AbsYAPL.BNFC'Position, AbsYAPL.Expr) }
Expr4
  : Expr4 MulOp Expr5 { (fst $1, AbsYAPL.EMul (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr5 { (fst $1, (snd $1)) }

Expr3 :: { (AbsYAPL.BNFC'Position, AbsYAPL.Expr) }
Expr3
  : Expr3 AddOp Expr4 { (fst $1, AbsYAPL.EAdd (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr4 { (fst $1, (snd $1)) }

Expr2 :: { (AbsYAPL.BNFC'Position, AbsYAPL.Expr) }
Expr2
  : Expr2 RelOp Expr3 { (fst $1, AbsYAPL.ERel (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr3 { (fst $1, (snd $1)) }

Expr1 :: { (AbsYAPL.BNFC'Position, AbsYAPL.Expr) }
Expr1
  : Expr2 '&&' Expr1 { (fst $1, AbsYAPL.EAnd (fst $1) (snd $1) (snd $3)) }
  | Expr2 { (fst $1, (snd $1)) }

Expr :: { (AbsYAPL.BNFC'Position, AbsYAPL.Expr) }
Expr
  : Expr1 '||' Expr { (fst $1, AbsYAPL.EOr (fst $1) (snd $1) (snd $3)) }
  | Expr1 { (fst $1, (snd $1)) }

ListExpr :: { (AbsYAPL.BNFC'Position, [AbsYAPL.Expr]) }
ListExpr
  : {- empty -} { (AbsYAPL.BNFC'NoPosition, []) }
  | Expr { (fst $1, (:[]) (snd $1)) }
  | Expr ',' ListExpr { (fst $1, (:) (snd $1) (snd $3)) }

AddOp :: { (AbsYAPL.BNFC'Position, AbsYAPL.AddOp) }
AddOp
  : '+' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Plus (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | '-' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Minus (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }

MulOp :: { (AbsYAPL.BNFC'Position, AbsYAPL.MulOp) }
MulOp
  : '*' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Times (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | '/' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Div (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | '%' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.Mod (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }

RelOp :: { (AbsYAPL.BNFC'Position, AbsYAPL.RelOp) }
RelOp
  : '<' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.LTH (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | '<=' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.LE (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | '>' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.GTH (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | '>=' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.GE (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | '==' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.EQU (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }
  | '!=' { (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1), AbsYAPL.NE (uncurry AbsYAPL.BNFC'Position (tokenLineCol $1))) }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pStmt :: [Token] -> Err AbsYAPL.Stmt
pStmt = fmap snd . pStmt_internal
}

