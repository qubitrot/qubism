{-|
Module      : Qubism.QASM.Parser
Description : A Parser for OpenQASM 2.0
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# LANGUAGE LambdaCase #-}

module Qubism.QASM.Parser 
  ( parseOpenQASM )
  where

import Data.Void
import Numeric.Natural
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import qualified Data.Map as Map

import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import           Control.Monad.Combinators.Expr 
import qualified Text.Megaparsec.Char.Lexer as L

import Qubism.QASM.AST

type IdTable = Map.Map Id SourcePos
type Parser  = ParsecT Void String (State IdTable)

parseOpenQASM 
  :: String -- ^ Name of source file 
  -> String -- ^ Input for parser
  -> Either String Program 
parseOpenQASM file input =
  let parsed = runParserT mainprogram file input
  in  case runState parsed Map.empty of
        (Left  err,  _) -> Left $ errorBundlePretty err
        (Right prog, _) -> Right  prog

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
identifier = lexeme . try $ 
  (:) <$> letterChar <*> many alphaNumChar

newIdent :: Parser String
newIdent = do
  i <- identifier
  insertId i =<< getSourcePos
  pure i

knownIdent :: Parser String
knownIdent = do
  i <- identifier
  a <- lookupId i
  case a of
    Just _  -> pure i
    Nothing -> fail $ "Undeclared identifier: " ++ i

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
stmt =  cond
    <|> regDecl
    <|> gateDecl
    <|> QOp <$> qop
    <|> UOp <$> uop

regDecl :: Parser Stmt
regDecl = do
  prefix <- symbol "qreg" <|> symbol "creg"
  ident  <- newIdent
  size   <- brackets natural
  case prefix of
    "qreg" -> pure $ QRegDecl ident size
    "creg" -> pure $ CRegDecl ident size

gateDecl :: Parser Stmt
gateDecl = do
  ident  <- symbol "gate" *> newIdent
  gtable <- lift get
  params <- option [] $ parens (list shadowIdent)
  args   <- nonempty shadowIdent
  body   <- symbol "{" *> many (uop <* semi)
  lift . put $ gtable
  pure $ GateDecl ident params args body    
  where
    shadowIdent = do
      i  <- identifier
      sp <- getSourcePos
      lift . modify $ Map.insert i sp
      pure i

qop :: Parser QuantumOp
qop = measure <|> reset <|>unitary
  where 
    measure = do
      src <- symbol "measure" *> argument
      tgt <- symbol "->"      *> argument
      pure $ Measure src tgt
    reset = do
      tgt <- symbol "reset" *> argument
      pure $ Reset tgt
    unitary = QUnitary <$> uop

uop :: Parser UnitaryOp
uop = u <|> cx <|> func <|> barrier
  where
    u = do
      _   <- symbol "U" *> symbol "("
      p1  <- expr <* symbol ","
      p2  <- expr <* symbol ","
      p3  <- expr <* symbol ")"
      arg <- argument
      pure $ U p1 p2 p3 arg
    cx = do
      _    <- symbol "CX"
      arg1 <- argument
      _    <- comma
      arg2 <- argument
      pure $ CX arg1 arg2
    func = do
      ident  <- knownIdent
      params <- option [] $ parens (list expr)
      args   <- list argument
      pure $ Func ident params args
    barrier = do
      _    <- symbol "barrier"
      args <- list argument
      pure $ Barrier args

cond :: Parser Stmt
cond = do
  _ <- symbol "if" *> symbol "("
  i <- knownIdent
  _ <- symbol "=="
  n <- natural
  _ <- symbol ")"
  o <- qop
  pure $ Cond i n o

argument :: Parser Arg
argument = do
  ident <- knownIdent
  index <- optional $ brackets natural
  case index of
    Nothing -> pure $ ArgReg ident
    Just i  -> pure $ ArgBit ident i

expr :: Parser Expr
expr = makeExprParser term exprOps
  where term =  symbol "pi" *> pure Pi
            <|> Ident <$> knownIdent
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

---------- Utilities -----------

lookupId :: Id -> Parser (Maybe SourcePos)
lookupId i = lift . gets $ Map.lookup i

insertId :: Id -> SourcePos -> Parser ()
insertId i sp = lookupId i >>= \case
    Just _  -> fail $ "Redeclaration of " ++ i
    Nothing -> lift . modify $ Map.insert i sp

deleteId :: Id -> Parser ()
deleteId i = lift . modify $ Map.delete i
