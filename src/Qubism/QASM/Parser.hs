{-|
Module      : Qubism.QASM.Parser
Description : A Parser for OpenQASM 2.0
Copyright   : (c) Keith Pearson, 2018
License     : MIT
Maintainer  : keith@qubitrot.org
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Qubism.QASM.Parser 
  ( parseOpenQASM 
  , parseOpenQASM'
  , IdTable
  , ParserState
  , initialState
  ) where

import Data.Void
import Data.Maybe
import Numeric.Natural
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Exception hiding (try)
import System.IO.Error
import System.FilePath

import qualified Data.Map  as Map
import qualified Data.Text as T
import           Data.Text (Text)

import           Text.Megaparsec hiding (State)
import qualified Text.Megaparsec as MP
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

type Parser    = ParsecT Failure Text (StateT ParserState IO)
type IdTable   = Map.Map Id SourcePos

data ParserState = ParserState
  { idTable      :: Map.Map Id SourcePos
  , filePath     :: Maybe FilePath
  , currentInput :: Text
  }

parseOpenQASM 
  :: FilePath -- ^ Name of source file 
  -> Text     -- ^ Input for parser
  -> IO (Either String AST)
parseOpenQASM fp input = 
  let istate = initialState (Just fp) input
      parsed = parseOpenQASM' istate input
  in  (fmap . fmap) fst parsed

parseOpenQASM'
  :: ParserState
  -> Text      
  -> IO (Either String (AST,ParserState))
parseOpenQASM' s input = 
  let path   = fromMaybe "" $ filePath s
      parsed = runParserT program path input
  in  runStateT parsed s >>= \case
        (Left  err,  s') -> pure . Left  $ showError s' err
        (Right prog, s') -> pure . Right $ (prog, s')
  where
    showError s' (ParseErrorBundle b _) = errorBundlePretty $
      ParseErrorBundle b PosState
        { pstateInput = currentInput s'
        , pstateOffset = 0
        , pstateSourcePos = initialPos $ fromMaybe "" $ filePath s'
        , pstateTabWidth = defaultTabWidth
        , pstateLinePrefix = ""
        }
    -- showError is nessisary because megaparsec's errorBundlePretty
    -- assumes that the input stream runParserT begins with is the same
    -- throughout. Unfortunately the use of include below violates this
    -- assumption by loading source from files.

initialState :: Maybe FilePath -> Text -> ParserState
initialState fp s = ParserState 
  { idTable  = Map.empty
  , filePath = fp
  , currentInput = s
  }

--------- Lexing --------------------------------------

type LexerT = ParsecT Failure Text

sc :: LexerT m ()
sc = L.space space1 lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment  "//"
        blockCmnt = L.skipBlockComment "/*" "*/" -- Not actually part of the
                                                 -- standard, but why not?
lexeme :: LexerT m a -> LexerT m a
lexeme = L.lexeme sc

symbol :: Text -> LexerT m Text
symbol = L.symbol sc

semi :: LexerT m Text
semi = symbol ";"

comma :: LexerT m Text
comma = symbol ","

natural :: LexerT m Natural
natural = lexeme L.decimal

double :: RealFloat a => LexerT m a
double =  try (lexeme L.float) 
      <|> fromIntegral <$> natural

parens :: LexerT m a -> LexerT m a
parens = between (symbol "(") (symbol ")")

-- | Reserved words
rws :: [String]
rws = ["if","barrier","gate","measure","reset","creg","qreg","pi"
      ,"sin","cos","tan","exp","ln","sqrt","U","CX","include"];

rword :: Text -> LexerT m Text
rword w = lexeme . try $ string w <* notFollowedBy alphaNumChar

identifier :: LexerT m Text
identifier = lexeme . try $ p >>= check
  where p       = (:) <$> letterChar <*> many alphaNumChar
        check x = if x `elem` rws
                  then fail $ "keyword " ++ x ++
                              " cannot be an identifier"
                  else pure $ T.pack x

newIdent :: Parser Text
newIdent = do
  i <- identifier
  insertId i =<< getSourcePos
  pure i

knownIdent :: Parser Text
knownIdent = do
  i <- identifier
  a <- lookupId i
  case a of
    Just _  -> pure i
    Nothing -> fail $ "Undeclared identifier: " ++ T.unpack i

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

filepath :: LexerT m Text
filepath = T.pack <$> many (alphaNumChar
                            <|> char '.' 
                            <|> char '/')

---------- Parsing --------------------------------------

header :: Parser ()
header = sc *> symbol "OPENQASM 2.0;" *> pure ()

program :: Parser AST
program =  option () (try header) *> sc
        *> sepEndBy1 stmt (semi <|> symbol "}")

stmt :: Parser Stmt
stmt =  attachPos 
     $  cond
    <|> regDecl
    <|> gateDecl
    <|> UOp <$> uop
    <|> QOp <$> qop
    <|> include

regDecl :: Parser Stmt
regDecl = do
  prefix <- rword "qreg" <|> rword "creg"
  ident  <- newIdent
  size   <- brackets natural
  case prefix of
    "qreg" -> pure $ QRegDecl ident size
    "creg" -> pure $ CRegDecl ident size

gateDecl :: Parser Stmt
gateDecl = do
  ident  <- rword "gate" *> newIdent
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
      lift . modify $ \p -> p { idTable = Map.insert i sp (idTable p) }
      pure i

include :: Parser Stmt
include = do
  _      <- rword "include"
  file   <- path =<< T.unpack <$> quotes filepath
  source <- tryReadFile $ file
  prevSt <- getInputState
  putInputState $ (Just file, source, initialStateMP file source)
  ast    <- program
  putInputState prevSt
  pure $ StmtList ast
  where
    getInputState = do
      fp <- lift . gets $ filePath 
      ci <- lift . gets $ currentInput
      mp <- getParserState -- Megaparsec's state, not ours.
      pure (fp,ci,mp)
    putInputState (fp,ci,mp) = do
      lift . modify $ \p -> p { filePath = fp, currentInput = ci }
      setParserState mp -- Megaparsec's state
    path f = lift (gets filePath) >>= \case
      Nothing -> pure f
      Just p  -> let (d,_) = splitFileName p
                 in  pure  $ d ++ f
  
tryReadFile :: FilePath -> Parser Text
tryReadFile file = do
  r <- liftIO $ tryJust (guard . isDoesNotExistError) (readFile file)
  case r of Right src -> pure $ T.pack src
            Left  _   -> customFailure . IncludeFail $ file

qop :: Parser QuantumOp
qop = measure <|> reset <|>unitary
  where 
    measure = do
      src <- rword  "measure" *> argument
      tgt <- symbol "->"      *> argument
      pure $ Measure src tgt
    reset = do
      tgt <- rword "reset" *> argument
      pure $ Reset tgt
    unitary = QUnitary <$> uop

uop :: Parser UnitaryOp
uop = u <|> cx <|> barrier <|> dump <|> func
  where
    u = do
      _   <- rword "U" *> symbol "("
      p1  <- expr <* symbol ","
      p2  <- expr <* symbol ","
      p3  <- expr <* symbol ")"
      arg <- argument
      pure $ U p1 p2 p3 arg
    cx = do
      _    <- rword "CX"
      arg1 <- argument
      _    <- comma
      arg2 <- argument
      pure $ CX arg1 arg2
    barrier = do
      _    <- rword "barrier"
      args <- list argument
      pure $ Barrier args
    func = do
      ident  <- knownIdent
      params <- option [] $ parens (list expr)
      args   <- list argument
      pure $ Func ident params args
    dump = do
      _ <- string ":dump"
      pure $ Dump

cond :: Parser Stmt
cond = do
  _ <- rword "if" *> symbol "("
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
  where term =  rword "pi" *> pure Pi
            <|> Ident <$> knownIdent
            <|> Real  <$> double
            <|> parens expr

exprOps :: [[Operator Parser Expr]]
exprOps = 
  [ [ Prefix (Unary  Neg  <$ symbol "-"   ) ]
  , [ Prefix (Unary  Sin  <$ rword  "sin" )
    , Prefix (Unary  Cos  <$ rword  "cos" )
    , Prefix (Unary  Tan  <$ rword  "tan" )
    , Prefix (Unary  Exp  <$ rword  "exp" )
    , Prefix (Unary  Ln   <$ rword  "ln"  )
    , Prefix (Unary  Sqrt <$ rword  "sqrt") ]
  , [ InfixL (Binary Pow  <$ rword  "pow" ) ]
  , [ InfixL (Binary Mul  <$ symbol "*"   )
    , InfixL (Binary Div  <$ symbol "/"   ) ]
  , [ InfixL (Binary Add  <$ symbol "+"   )
    , InfixL (Binary Sub  <$ symbol "-"   ) ]
  ]

---------- Utilities -----------

lookupId :: Id -> Parser (Maybe SourcePos)
lookupId i = lift . gets $ Map.lookup i . idTable

insertId :: Id -> SourcePos -> Parser ()
insertId i sp = lookupId i >>= \case
    Just _  -> fail $ "Redeclaration of " ++ T.unpack i
    Nothing -> lift . modify $ \ParserState {..} -> ParserState 
                 { idTable  = Map.insert i sp idTable
                 , filePath = filePath 
                 , currentInput = currentInput 
                 }

deleteId :: Id -> Parser ()
deleteId i = lift . modify $ \ParserState {..} -> ParserState 
               { idTable = Map.delete i idTable
               , filePath = filePath 
               , currentInput = currentInput
               }

attachPos :: Parser Stmt -> Parser Stmt
attachPos p = PosInfo <$> getSourcePos <*> p

-- Borrowed from Megaparsec source since it doesn't export it.
initialStateMP :: String -> s -> MP.State s
initialStateMP name s = MP.State
  { stateInput  = s
  , stateOffset = 0
  , statePosState = PosState
    { pstateInput = s
    , pstateOffset = 0
    , pstateSourcePos = initialPos name
    , pstateTabWidth = defaultTabWidth
    , pstateLinePrefix = ""
    }
}
