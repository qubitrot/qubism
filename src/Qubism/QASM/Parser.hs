{-|
Module      : Qubism.QASM.Parser
Description : A Parser for OpenQASM 2.0
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# LANGUAGE LambdaCase #-}

module Qubism.QASM.Parser 
  ( parseOpenQASM
  , preparse
  , passthrough )
  where

import Data.Void
import Numeric.Natural
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Exception hiding (try)
import System.IO.Error
import qualified Data.Map as Map

import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr 

import Qubism.QASM.AST

data Failure
  = IncludeFail String
  deriving (Eq, Ord, Show)

instance ShowErrorComponent Failure where
  showErrorComponent (IncludeFail file) = 
    "Cannot include: " ++ file ++ " does not exist"

type Parser    = ParsecT Failure String (State IdTable)
type PreParser = ParsecT Failure String IO
type IdTable   = Map.Map Id SourcePos

parseOpenQASM 
  :: String -- ^ Name of source file 
  -> String -- ^ Input for parser
  -> IO (Either String Program)
parseOpenQASM file input = do
  runParserT preparse file input >>= \case
    Left  err -> pure . Left  $ errorBundlePretty err
    Right pp  -> do
      let parsed = runParserT mainprogram file pp
      case runState parsed Map.empty of
        (Left  err,  _) -> pure . Left  $ errorBundlePretty err
        (Right prog, _) -> pure . Right $ prog

-- | The preparser phase only handles includes at the moment, but may be
-- expanded later. Screws up line numbering. TODO.
preparse :: PreParser String
preparse = mconcat <$> manyTill (include <|> passthrough) eof

passthrough :: PreParser String
passthrough = manyTill anySingle done
  where done = eof <|> lookAhead (symbol "include" *> pure ())

include :: PreParser String
include = do
  symbol "include"
  file   <- quotes filepath <* semi
  source <- tryReadFile file
  remain <- getInput
  offset <- getOffset
  setInput  source
  pparse <- preparse
  setInput  remain
  setOffset offset
  pure pparse
    
tryReadFile :: FilePath -> PreParser String
tryReadFile file = do
  r <- lift $ tryJust (guard . isDoesNotExistError) (readFile file)
  case r of Right src -> pure src
            Left  e   -> customFailure . IncludeFail $ file

--------- Lexing --------------------------------------

type LexerT = ParsecT Failure String

sc :: LexerT m ()
sc = L.space space1 lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment  "//"
        blockCmnt = L.skipBlockComment "/*" "*/" -- Not actually part of the
                                                 -- standard, but why not?
lexeme :: LexerT m a -> LexerT m a
lexeme = L.lexeme sc

symbol :: String -> LexerT m String
symbol = L.symbol sc

semi :: LexerT m String
semi = symbol ";"

comma :: LexerT m String
comma = symbol ","

natural :: LexerT m Natural
natural = lexeme L.decimal

double :: RealFloat a => LexerT m a
double =  try (lexeme L.float) 
      <|> fromIntegral <$> natural

parens :: LexerT m a -> LexerT m a
parens = between (symbol "(") (symbol ")")

identifier :: LexerT m String
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

brackets :: LexerT m a -> LexerT m a
brackets = between (symbol "[") (symbol "]")

curly :: LexerT m a -> LexerT m a
curly = between (symbol "{") (symbol "}")

quotes :: LexerT m a -> LexerT m a
quotes = between (symbol "\"") (symbol "\"")

list :: LexerT m a -> LexerT m [a]
list p = sepEndBy p comma

nonempty :: LexerT m a -> LexerT m [a]
nonempty p = sepEndBy1 p comma

filepath :: LexerT m String
filepath = many $  alphaNumChar
               <|> char '.' 
               <|> char '/'

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
uop = u <|> cx <|> barrier <|> func
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
    barrier = do
      _    <- symbol "barrier"
      args <- list argument
      pure $ Barrier args
    func = do
      ident  <- knownIdent
      params <- option [] $ parens (list expr)
      args   <- list argument
      pure $ Func ident params args

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
