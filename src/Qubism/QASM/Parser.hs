{-|
Module      : Qubism.QASM.Parser
Description : A Parser for OpenQASM 2.0
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

module Qubism.QASM.Parser where

import Control.Monad
import Data.Void
import Numeric.Natural
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Qubism.QASM.Types

type Parser = Parsec Void String

--------- Lexing --------------------------------------

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment  "//"
        blockCmnt = L.skipBlockComment "/*" "*/" -- Not actually part of the
                                                 -- standard, but why not?
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

double :: RealFloat a => Parser a
double =  try (lexeme L.float) 
      <|> fromIntegral <$> natural

identifier :: Parser String
identifier = lexeme . try $ (:) <$> letterChar <*> many alphaNumChar

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

curly :: Parser a -> Parser a
curly = between (symbol "{") (symbol "}")

list :: Parser a -> Parser [a]
list p = sepEndBy p comma

nonempty :: Parser a -> Parser [a]
nonempty p = sepEndBy1 p comma

---------- Parsing --------------------------------------

mainprogram :: Parser Program
mainprogram = header *> program

header :: Parser ()
header = sc *> symbol "OPENQASM 2.0;" *> pure ()

program :: Parser Program
program = sepEndBy1 stmt (semi <|> symbol "}")

stmt :: Parser Stmt
stmt =  regDecl 
    <|> gateDecl
    <|> QOp <$> qop
    <|> UOp <$> uop
    <|> cond

regDecl :: Parser Stmt
regDecl = do
  prefix <- symbol "qreg" <|> symbol "creg"
  ident  <- identifier
  size   <- brackets natural
  case prefix of
    "qreg" -> pure $ StateVecDecl ident size
    "creg" -> pure $ CRegDecl ident size

gateDecl :: Parser Stmt
gateDecl = do
  symbol "gate"
  ident  <- identifier
  params <- option [] $ parens (list identifier)
  args   <- nonempty identifier
  body   <- symbol "{" *> many (uop <* semi)
  pure $ GateDecl ident params args body

qop :: Parser QuantumOp
qop = measure <|> reset
  where 
    measure = do
      symbol "measure"
      source <- argument
      symbol "->"
      target <- argument
      pure $ Measure source target
    reset = do
      symbol "reset"
      target <- argument
      pure $ Reset target

uop :: Parser UnitaryOp
uop = u <|> cx <|> func <|> barrier
  where
    u = do
      symbol "U"
      params <- parens $ list expr
      arg    <- argument
      pure $ U params arg
    cx = do
      symbol "CX"
      arg1 <- argument
      comma
      arg2 <- argument
      pure $ CX arg1 arg2
    func = do
      ident  <- identifier
      params <- option [] $ parens (list expr)
      args   <- list argument
      pure $ Func ident params args
    barrier = do
      symbol "barrier"
      args <- list argument
      pure $ Barrier args

cond :: Parser Stmt
cond = do
  symbol "if" *> symbol "("
  ident <- identifier
  symbol "=="
  num <- natural
  symbol ")"
  op  <- qop
  pure $ Cond ident num op

argument :: Parser Arg
argument = do
  ident <- identifier
  index <- optional $ brackets natural
  case index of
    Nothing -> pure $ ArgReg   ident
    Just i  -> pure $ ArgQubit ident i

expr :: Parser Expr
expr = makeExprParser term exprOps
  where term =  symbol "pi" *> pure Pi
            <|> Ident <$> identifier 
            <|> Real  <$> double
            <|> parens expr

exprOps :: [[Operator Parser Expr]]
exprOps = 
  [ [ Prefix (Unary  Neg  <$ symbol "-"   ) ]
  , [ Prefix (Unary  Sin  <$ symbol "sin" )
    , Prefix (Unary  Cos  <$ symbol "cos" )
    , Prefix (Unary  Tan  <$ symbol "tan" )
    , Prefix (Unary  Exp  <$ symbol "exp" )
    , Prefix (Unary  Ln   <$ symbol "ln"  )
    , Prefix (Unary  Sqrt <$ symbol "sqrt") ]
  , [ InfixL (Binary Pow  <$ symbol "pow" ) ]
  , [ InfixL (Binary Mul  <$ symbol "*"   )
    , InfixL (Binary Div  <$ symbol "/"   ) ]
  , [ InfixL (Binary Add  <$ symbol "+"   )
    , InfixL (Binary Sub  <$ symbol "-"   ) ]
  ]

