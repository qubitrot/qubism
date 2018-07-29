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

float :: RealFloat a => Parser a
float = lexeme L.float

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

---------- Parsing --------------------------------------

parseQASM :: Parser Program
parseQASM = header *> program

header :: Parser ()
header = sc *> symbol "OPENQASM" *> float *> semi *> pure ()

program :: Parser Program
program = sepEndBy1 stmt semi

stmt :: Parser Stmt
stmt =  regDef
    <|> gateDef

regDef :: Parser Stmt
regDef = do
  t    <- symbol "qreg" <|> symbol "creg"
  name <- RegIdent <$> identifier
  size <- brackets natural
  let t' = if t == "qreg" then QR else CR
  pure $ RegDef name t' size

gateDef :: Parser Stmt
gateDef = do
  symbol "gate" 
  name   <- GateIdent <$> identifier
  params <- parens $ list (ParamIdent <$> identifier)
  args   <- list (ArgIdent <$> identifier)
  body   <- curly $ sepEndBy gateBody semi
  pure $ GateDef name params args body

gateBody :: Parser GateBody
gateBody = gbApply <|> gbBarrier
  where 
    gbApply = do
      g    <- gate
      args <- list (ArgIdent <$> identifier)
      pure $ GBApply g args
    gbBarrier = do
      symbol "barrier"
      args <- list (ArgIdent <$> identifier)
      pure $ GBBarrier args

gate :: Parser Gate
gate = cx <|> u <|> func
  where
    cx = symbol "CX" *> pure CX
    u  = do
      symbol "U"  *> symbol "("
      p1 <- param <* comma
      p2 <- param <* comma
      p3 <- param <* symbol ")"
      pure $ U p1 p2 p3
    func = do
      ident  <- GateIdent <$> identifier
      params <- parens $ list param
      pure $ Func ident params

param :: Parser Param
param = (Number <$> num)  <|> (Name <$> name)
  where num  = try float  <|> (fromIntegral <$> natural)
        name = ParamIdent <$> identifier
