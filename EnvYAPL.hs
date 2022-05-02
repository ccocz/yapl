module EnvYAPL where

import Data.Map as Map
import AbsYAPL
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

-- fixme: better to type
data Value =
  IntVal Integer
  | BoolVal Bool
  | StringVal String
  | Closure [Arg] Block
  | NoneVal deriving (Show, Eq, Ord)

type Var = String
type VEnv = Map.Map Var Value

data Env = Env {
  vEnv :: Map.Map Var Value,
  retVal :: Value
  } deriving Show

type RT = ReaderT Env (StateT Env (ExceptT String IO))