module EnvYAPL where

import Data.Map as Map
import AbsYAPL
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

data Value =
  IntVal Integer
  | BoolVal Bool
  | StringVal String
  | Closure [Arg] Block Env
  | NoneVal
  | VoidVal
  | BreakVal
  | ContVal deriving (Eq, Ord)

instance Show Value where
  show (IntVal v) = show v
  show (BoolVal v) = show v
  show (StringVal v) = v
  show (VoidVal) = "none"
  show (NoneVal) = "none"
  show _ = "Invalid type to show"

type Var = String
type VEnv = Map.Map Var Value

data Env = Env {
  vEnv :: Map.Map Var Loc,
  retVal :: Value
  } deriving (Show, Eq, Ord)

type Loc = Int
type Mem = Map.Map Loc Value

type RT = ReaderT Env (StateT Mem (ExceptT String IO))