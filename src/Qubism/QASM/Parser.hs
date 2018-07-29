module Qubism.QASM.Parser where

import Control.Monad
import Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Qubism.QASM.Types

type Parser = Parsec Void String

--------- Lexing --------------------------------------

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment  "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

semi :: Parser String
semi = symbol ";"

comma :: Parser String
comma = symbol ","

natural :: Parser Natural
natural = lexeme L.decimal

identifier :: Parser String
identifier = lexeme . try $ (:) <$> letterChar <*> many alphaNumChar

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

---------- Parsing --------------------------------------

program :: Parser Program
program = sepEndBy1 stmt semi

stmt :: Parser Stmt
stmt =  declReg 
    <|> declGate
    <|> measure 
    <|> reset
    <|> (Operation <$> operation)

declReg :: Parser Stmt
declReg = do
  regt  <- symbol "creg" <|> symbol "qreg" 
  ident <- identifier 
  size  <- brackets natural
  let t = if regt == "creg" then CR else QR
  pure $ DeclReg ident t size 

declGate :: Parser Stmt
declGate = do
  symbol "gate"
  ident  <- identifier
  params <- parens $ sepBy identifier comma
  args   <- sepBy1 identifier comma
  body   <- curly $ sepEndBy operation semi
  pure $ DeclGate ident params args body

measure :: Parser Stmt
measure = do
  symbol "measure"
  source <- argument
  symbol "->"
  target <- argument
  pure $ Measure source target

reset :: Parser Stmt
reset = do
  symbol "reset"
  arg <- argument
  pure $ Reset arg

operation :: Parser Op
operation = gate <|> barrier

gate :: Parser Op
gate = do
  g    <- u <|> cx <|> custom
  args <- argument `sepBy1` comma
  pure $ Apply g args

u :: Parser Gate
u = do
  symbol "U"
  symbol "("
  p1 <- param <* comma
  p2 <- param <* comma
  p3 <- param
  symbol ")"
  pure $ U p1 p2 p3

cx :: Parser Gate
cx = do
  symbol "CX"
  pure CX

custom :: Parser Gate
custom = do
  ident  <- identifier
  params <- parens $ param `sepBy` comma
  pure $ Custom ident params

barrier :: Parser Op
barrier = do
  symbol "barrier"
  args <- argument `sepBy` comma
  pure $ Barrier args

argument :: Parser Arg
argument = do
  ident <- identifier
  index <- option Nothing $ Just <$> brackets natural
  pure $ Arg ident index

param :: Parser Param
param = try (lexeme L.float) <|> (fromIntegral <$> natural)
