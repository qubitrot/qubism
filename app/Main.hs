module Main where

import System.Environment
import Text.Megaparsec

import Qubism.QASM.Types
import Qubism.QASM.Parser

main :: IO ()
main = do
  file   <- head <$> getArgs
  source <- readFile file
  case runParser mainprogram file source of
    Left  err  -> putStr $ parseErrorPretty' source err
    Right prog -> print prog
