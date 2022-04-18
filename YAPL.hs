module YAPL where

import AbsYAPL

interpret :: Program -> Integer
interpret x = case x of
  Program [ExpDef (EAdd exp0 Plus exp)]  ->
    interpret (Program [ExpDef exp0]) + interpret (Program [ExpDef exp])
  Program [ExpDef (EMul exp0 Times exp)]  ->
    interpret (Program [ExpDef exp0]) * interpret (Program [ExpDef exp])
  Program [ExpDef (ELitInt n)] -> n