module Main where

import LexYAPL
import ParYAPL
import AbsYAPL

import ErrM
import EvalIntExpr
import ExecStmt

main = do
  interact calc
  putStrLn ""

calc s =
  let Ok e = pStmt (myLexer s)
  in show (execStmt e)