module Main where

import LexYAPL
import ParYAPL
import AbsYAPL
import YAPL

import ErrM

main = do
  interact calc
  putStrLn ""

calc s =
  let Ok e = pProgram (myLexer s)
  in show (interpret e)