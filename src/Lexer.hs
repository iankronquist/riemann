module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Token as Tok

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    operators = ["=", "*", "/", "+", "-", "<", ">", "<=", ">="]
    names = ["let", "fun", "var", "if", "then", "else"]
    style = emptyDef {
      Tok.commentLine = "//",
      Tok.commentStart = "/*",
      Tok.commentEnd = "*/",
      Tok.reservedOpNames = operators,
      Tok.reservedNames = names
    }

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser Double
float = Tok.float lexer

identifier :: Parser String
identifier = Tok.identifier lexer

string :: Parser String
string = Tok.stringLiteral lexer

parentheses :: Parser a -> Parser a
parentheses = Tok.parens lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

brackets :: Parser a -> Parser a
brackets = Tok.brackets lexer

commaSeparated :: Parser a -> Parser [a]
commaSeparated = Tok.commaSep lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

colon :: Parser String
colon = Tok.colon lexer

dot :: Parser String
dot = Tok.dot lexer

reservedOperators :: String -> Parser ()
reservedOperators = Tok.reservedOp lexer
