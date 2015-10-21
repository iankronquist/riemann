module Parser where

import Text.Parsec
import Text.Parsec.String

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token

import Lexer

import Syntax

-- Helper function to build the association table
binary sym func = Expr.Infix (reservedOperators sym >> return (BinOp func))


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
  _ <- reserved "fun"
  args <- parentheses $ commaSeparated expr
  body <- bodyExpr
  return $ FuncDef args body

funcCall :: Parser Expr
funcCall = do
  name <- identifier
  args <- parentheses $ commaSeparated expr
  return $ FuncCall name args

objectLit :: Parser Expr
objectLit = do
  body <- braces $ commaSeparated pair
  return $ Object body

arrayLit :: Parser Expr
arrayLit = do
  body <- brackets $ commaSeparated expr
  return $ ArrayLiteral body

bracketAccessor :: Parser Expr
bracketAccessor = do
  obj <- identifier
  item <- brackets expr
  return $ BracketAccessor obj item

pair :: Parser Expr
pair = do
  name <- identifier
  separator <- colon
  value <- expr
  return $ Pair name value

parseIf :: Parser Expr
parseIf = do
  _ <- reserved "if"
  condition <- expr
  _ <- reserved "then"
  body <- expr
  --result <- try (reserved "else" *> expr)
  return $ If condition body --result

parseIfElse :: Parser Expr
parseIfElse = do
  _ <- reserved "if"
  condition <- expr
  _ <- reserved "then"
  body <- expr
  _ <- reserved "else"
  elsebody <- expr
  return $ IfElse condition body elsebody


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

bracesExpr :: Parser Expr
bracesExpr = do
  body <- braces $ many expr
  return $ BracesExpr body

assignmentHelper :: Parser (String, Expr)
assignmentHelper = do
  name <- identifier
  equals <- reservedOperators "="
  expression <- expr
  return (name, expression)

letStatement :: Parser Expr
letStatement = do
  letItGo <- reserved "let"
  (name, expression) <- assignmentHelper
  return $ Let name expression

varStatement :: Parser Expr
varStatement = do
  varItGo <- reserved "var"
  (name, expression) <- assignmentHelper
  return $ VarExp name expression

attrAccessor :: Parser Expr
attrAccessor = do
  variable <- identifier
  dotdot <- dot
  property <- identifier
  return $ Accessor variable property

bareStatement :: Parser Expr
bareStatement = do
  (name, expression) <- assignmentHelper
  return $ BareAssign name expression


-- The <|> operator is defined in parsec. It means "try this function, but if that doesn't work, try the next one"
factor :: Parser Expr
factor = try attrAccessor
  <|> try afloat
  <|> try int
  <|> try letStatement
  <|> try varStatement
  <|> try bareStatement
  <|> try bracketAccessor
  <|> try stringLit
  <|> try bracesExpr
  <|> try arrayLit
  <|> try funcDef
  <|> try funcCall
  <|> try variable
  <|> try stringLit
  <|> try parseIfElse
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
parseExpr = parse (inputContents expr) "<stdin>"
