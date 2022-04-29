module Main where

import ParYAPL

import ErrM
import ExecStmt

main :: IO ()
main = do
  interact calc
  putStrLn ""

calc :: [Char] -> String
calc s =
  let Ok e = pStmt (myLexer s)
  in show (execStmt e Map.Empty)