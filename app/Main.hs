module Main where

import System.Environment
import Text.Megaparsec

import Qubism.QASM.Parser
import Qubism.QASM.Simulation

main :: IO ()
main = do
  file   <- head <$> getArgs
  source <- readFile file
  case runParser mainprogram file source of
    Left  err  -> putStr $ parseErrorPretty' source err
    Right prog -> do
      st <- runProgram prog
      case st of 
        Left  error  -> print error
        Right progSt -> print progSt
