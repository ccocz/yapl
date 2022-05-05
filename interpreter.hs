module Main where

import ParYAPL

import ErrM
import ExecStmt
import Data.Map as Map
import EnvYAPL
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import System.Environment ( getArgs )

start = Env {
  vEnv = Map.empty,
  retVal = NoneVal
}

main :: IO ()
main = do
   file <- getArgs
   case file of
       [] -> error "No args provided!"
       file:_ -> do
           program <- readFile file
           case pProgram (myLexer program) of
               Ok p  -> do
                 r <- runExceptT (runStateT (runReaderT (interpret p) start) Map.empty)
                 case r of
                   (Left e) -> putStrLn $ "Error: " ++ e
                   --(Right r) -> putStrLn $ "last env: " ++ show r
                   (Right r) -> putStrLn $ "finished program"
               Bad e -> error e