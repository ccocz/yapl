module EnvYAPL where

import AbsYAPL
import Data.Typeable
import Data.Map as Map

-- fixme: better to type
data Value =
  IntVal Integer
  | BoolVal Bool
  | StringVal String deriving (Show, Eq, Ord)

type Var = String

type VEnv = Map.Map Var Value
