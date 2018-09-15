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
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import qualified Data.Map as Map

import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L

import Qubism.QASM.AST

data IdType 
  = IdQReg Size
  | IdCReg Size
  | IdGate IdTable
  | IdExpr
  deriving Show

type IdTable = Map.Map Id IdType
type Parser  = ParsecT Void String (State IdTable)

parseOpenQASM 
  :: String -- ^ Name of source file 
  -> String -- ^ Input for parser
  -> Either String Program 
parseOpenQASM file input =
  let parsed = runParserT mainprogram file input
  in  case runState parsed Map.empty of
        (Left  err,  _) -> Left $ parseErrorPretty err
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

newIdent :: Parser String
newIdent = lexeme . try $ (:) <$> letterChar <*> many alphaNumChar

knownIdent :: Parser String
knownIdent = do
  id <- newIdent
  a  <- lookupId id
  case a of
    Just _  -> pure id
    Nothing -> fail $ "Undeclared identifier: " ++ id

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
    "qreg" -> do insertId ident $ IdQReg size
                 pure $ QRegDecl ident size
    "creg" -> do insertId ident $ IdCReg size
                 pure $ CRegDecl ident size

gateDecl :: Parser Stmt
gateDecl = do
  symbol "gate"
  ident  <- newIdent
  params <- option [] $ parens (list newIdent)
  args   <- nonempty newIdent
  traverse (flip insertId (IdQReg 1)) args   -- temporarily declare these ids
  traverse (flip insertId  IdExpr   ) params -- in global scope to check for
  body   <- symbol "{" *> many (uop <* semi) -- conflicts or undeclared ids.
  traverse deleteId args                     -- This unfortunately prevents
  traverse deleteId params                   -- name shadowing. There's 
  pure $ GateDecl ident params args body     -- probably a better way.

qop :: Parser QuantumOp
qop = measure <|> reset <|>unitary
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
    unitary = QUnitary <$> uop

uop :: Parser UnitaryOp
uop = u <|> cx <|> func <|> barrier
  where
    u = do
      symbol "U"
      symbol "("
      p1  <- expr <* symbol ","
      p2  <- expr <* symbol ","
      p3  <- expr <* symbol ")"
      arg <- argument
      pure $ U p1 p2 p3 arg
    cx = do
      symbol "CX"
      arg1 <- argument
      comma
      arg2 <- argument
      pure $ CX arg1 arg2
    func = do
      ident  <- knownIdent
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
  ident <- knownIdent
  symbol "=="
  num <- natural
  symbol ")"
  op  <- qop
  pure $ Cond ident num op

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

lookupId :: Id -> Parser (Maybe IdType)
lookupId id = lift . gets $ Map.lookup id

insertId :: Id -> IdType -> Parser ()
insertId id idtype = lookupId id >>= \case
    Just _  -> fail $ "Redeclaration of " ++ id
    Nothing -> lift . modify $ Map.insert id idtype

deleteId :: Id -> Parser ()
deleteId id = lift . modify $ Map.delete id
