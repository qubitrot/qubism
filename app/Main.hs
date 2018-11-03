{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import System.Environment
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Map  as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Qubism.QASM.Parser
import Qubism.QASM.Simulation
import Qubism.QASM.ProgState

main :: IO ()
main = listToMaybe <$> getArgs >>= \case
  Just filename -> evalFile filename
  Nothing       -> repl

evalFile :: String -> IO ()
evalFile filename = do
  source <- T.pack <$> readFile filename
  parseOpenQASM filename source >>= \case
    Left  err  -> putStr err
    Right prog -> do
      st <- runProgram prog
      case st of 
        Left  err -> print err
        Right _   -> print "Done."

repl :: IO ()
repl = evalStateT loop (initialState, blankState)
  where 
    loop :: StateT (ParserState, ProgState) IO ()
    loop = do
      lift $ putStr "QASM> "
      lift $ hFlush stdout
      input <- lift $ T.pack <$> getLine
      case input of
        ":q" -> pure ()
        _    -> act input
    act input = do
      (idt, ps) <- get
      parsed    <- lift $ parseOpenQASMLn idt input
      case parsed of
        Left  err         -> lift $ print err
        Right (ast, idt') -> do
          result <- lift $ runProgram' ast ps
          case result of
            Left  err -> lift $ print err
            Right ps' -> put (idt', ps')
      loop
