module Main where

import ParYAPL

import ErrM
import ExecStmt
import Data.Map as Map
import EnvYAPL
import Control.Monad.State

main :: IO ()
main = do
  interact calc
  putStrLn ""

start = Env {
  vEnv = Map.empty,
  retVal = NoneVal
}

calc :: [Char] -> String
calc s =
  let Ok e = pStmt (myLexer s)
  in show (evalState (execStmt e) start)