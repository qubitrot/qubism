{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment

import Qubism.QASM.Parser
import Qubism.QASM.Simulation

main :: IO ()
main = do
  file   <- head <$> getArgs
  source <- readFile file 
  parseOpenQASM file source >>= \case
    Left  err  -> putStr err
    Right prog -> do
      st <- runProgram prog
      case st of 
        Left  error  -> print error
        Right progSt -> print progSt
