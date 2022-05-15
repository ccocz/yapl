module Main where

import ParYAPL

import ErrM
import ExecYAPL
import Data.Map as Map
import EnvYAPL
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import System.Environment (getArgs)

start :: Env
start = Env {
  vEnv = Map.empty,
  retVal = NoneVal
}

main :: IO ()
main = do
   args <- getArgs
   case args of
     [src] -> do
       prog <- readFile src
       case pProgram (myLexer prog) of
         Ok p  -> do
           result <- runExceptT (runStateT (runReaderT (interpret p) start) Map.empty)
           case result of
             (Left e) -> putStrLn $ "Error: " ++ e
             _ -> return ()
         Bad e -> error e
     _ -> error $ "Need single source file argument"