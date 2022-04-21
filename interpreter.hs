module Main where

import LexYAPL
import ParYAPL
import AbsYAPL

import ErrM
import EvalIntExpr

main = do
  interact calc
  putStrLn ""

calc s =
  let Ok e = pExpr (myLexer s)
  in show (evalExpr e)