module Parser where

import Text.Parsec
import Text.Parsec.String

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Lexer

import Syntax

-- Helper function to build the association table
binary sym func assoc = Expr.Infix (reservedOperators sym >> return (BinOp func)) assoc


associationTable = [
  -- Declare * and / to be left associative
  [binary "*" Times Expr.AssocLeft, binary "/" Divide Expr.AssocLeft],
  -- Declare + and - to be left associative
  [binary "+" Plus Expr.AssocLeft, binary "-" Minus Expr.AssocLeft],
  -- Declare >, >=, <, >= to be left associative
  [binary ">" GreaterThan Expr.AssocLeft, binary ">=" GreaterOrEqual Expr.AssocLeft, binary "<" LessThan Expr.AssocLeft, binary "<=" LessOrEqual Expr.AssocLeft] ]

int :: Parser Expr
int = do
  number <- integer
  return $ Interesting number

afloat :: Parser Expr
afloat = do
  number <- float
  return $ Float number

variable :: Parser Expr
variable = do
  name <- identifier
  return $ Var name

funcDef :: Parser Expr
funcDef = do
  name <- identifier
  args <- parentheses $ commaSeparated $ expr
  body <- braces $ many bodyExpr
  return $ FuncDef name args body

funcCall :: Parser Expr
funcCall = do
  name <- identifier
  args <- parentheses $ commaSeparated expr
  return $ FuncCall name args

objectLit :: Parser Expr
objectLit = do
  body <- braces $ commaSeparated $ pair
  return $ Object body

pair :: Parser Expr
pair = do
  name <- identifier
  separator <- colon
  value <- expr
  return $ Pair name value

parseIf :: Parser Expr
parseIf = do
  fi <- reserved "if"
  condition <- parentheses expr
  body <- braces $ many expr
  return $ If condition body

returnExp :: Parser Expr
returnExp = do
  _ <- reserved "return"
  val <- expr
  return $ Return val

stringLit :: Parser Expr
stringLit = do
  stringy <- Lexer.string
  return $ StringLit stringy

expr :: Parser Expr
expr = Expr.buildExpressionParser associationTable factor

bodyExpr :: Parser Expr
bodyExpr = Expr.buildExpressionParser associationTable funcBodyFactor

letStatement :: Parser Expr
letStatement = do
  letItGo <- reserved "let"
  name <- identifier
  equals <- reservedOperators "="
  expression <- expr
  return $ Let name expression

varStatement :: Parser Expr
varStatement = do
  varItGo <- reserved "var"
  name <- identifier
  equals <- reservedOperators "="
  expression <- expr
  return $ VarExp name expression

attrAccessor :: Parser Expr
attrAccessor = do
  variable <- identifier
  dotdot <- dot
  property <- identifier
  return $ Accessor variable property

bareStatement :: Parser Expr
bareStatement = do
  name <- identifier
  equals <- reservedOperators "="
  expression <- expr
  return $ BareAssign name expression


-- The <|> operator is defined in parsec. It means "try this function, but if that doesn't work, try the next one"
factor :: Parser Expr
factor = try attrAccessor
  <|> try afloat
  <|> try int
  <|> try letStatement
  <|> try varStatement
  <|> try bareStatement
  <|> try stringLit
  <|> try funcDef
  <|> try funcCall
  <|> try variable
  <|> try stringLit
  <|> try parseIf
  <|> try objectLit
  <|> parentheses expr

funcBodyFactor :: Parser Expr
funcBodyFactor = try returnExp <|> factor

inputContents contents = do
  Token.whiteSpace lexer
  marrow <- contents
  eof
  return marrow

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (inputContents expr) "<stdin>" s
