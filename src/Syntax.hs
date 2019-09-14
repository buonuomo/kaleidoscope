module Syntax where

type Name = String

data Expr = Float Double
          | BinOp Op Expr Expr
          | Var String
          | Call Name [Expr]
          | Function Name [Name] Expr
          | Extern Name [Name]
          deriving (Show, Eq, Ord)

data Op = Plus
        | Minus
        | Times
        | Divide
        | LessThan
        | GreaterThan
        | LessEquals
        | GreaterEquals
        deriving (Show, Eq, Ord)
