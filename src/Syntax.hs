module Syntax where

type Name = String

data Expr = Float Double
  | Interesting Integer
  | Let String Expr
  | VarExp String Expr
  | ArrayLiteral [Expr]
  | BracketAccessor String Expr
  | BareAssign String Expr
  | Accessor String String
  | IfElse Expr Expr Expr
  | If Expr Expr
  | Return Expr
  | Pair String Expr
  | Object [Expr]
  | BinOp Op Expr Expr
  | BracesExpr [Expr]
  | Var String
  | StringLit String
  | FuncDef [Expr] Expr
  | FuncCall Name [Expr]
  deriving (Eq, Ord, Show)

data Op = Plus
  | Minus
  | Times
  | Divide
  | GreaterThan
  | LessThan
  | GreaterOrEqual
  | LessOrEqual
  deriving (Eq, Ord, Show)
