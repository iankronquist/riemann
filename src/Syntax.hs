module Syntax where

type Name = String

data Expr = Float Double
  | Interesting Integer
  | Let String Expr
  | VarExp String Expr
  | BareAssign String Expr
  | If Expr [Expr]
  | Return Expr
  | Pair String Expr
  | Object [Expr]
  | BinOp Op Expr Expr
  | Var String
  | StringLit String
  | FuncDef Name [Expr] [Expr]
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
