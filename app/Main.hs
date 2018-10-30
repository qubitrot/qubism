{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment
import qualified Data.Text as T

import Qubism.QASM.Parser
import Qubism.QASM.Simulation

main :: IO ()
main = do
  file   <- head   <$> getArgs
  source <- T.pack <$> readFile file 
  parseOpenQASM file source >>= \case
    Left  err  -> putStr err
    Right prog -> do
      st <- runProgram prog
      case st of 
        Left  err -> print err
        Right _   -> print "Done."
