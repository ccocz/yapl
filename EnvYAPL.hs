module EnvYAPL where

import Data.Map as Map
import AbsYAPL

-- fixme: better to type
data Value =
  IntVal Integer
  | BoolVal Bool
  | StringVal String
  | Closure [Arg] Block deriving (Show, Eq, Ord)
type Var = String

type VEnv = Map.Map Var Value
